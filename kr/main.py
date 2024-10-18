import pandas as pd
import os
import logging

# Настройка логирования
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')

def load_data(receipt_path, receiptpartner_path):
    try:
        receipt = pd.read_csv(receipt_path, encoding='utf-8')
        receiptpartner = pd.read_csv(receiptpartner_path, encoding='utf-8')
        logging.info('Данные receipt и receiptpartner успешно загружены')
        return receipt, receiptpartner
    except FileNotFoundError as e:
        logging.error(f'Файл не найден: {e}')
        raise
    except pd.errors.EmptyDataError as e:
        logging.error(f'Файл пуст: {e}')
        raise
    except Exception as e:
        logging.error(f'Ошибка при загрузке данных: {e}')
        raise

def split_data(receipt):
    try:
        if 'response' not in receipt.columns:
            raise KeyError('Столбец "response" отсутствует в данных receipt')
        result_not_null = receipt[receipt['response'].notnull()]
        result_null = receipt[receipt['response'].isnull()]
        logging.info('Данные успешно разделены на result_not_null и result_null')
        return result_not_null, result_null
    except KeyError as e:
        logging.error(f'Ошибка в разделении данных: {e}')
        raise
    except Exception as e:
        logging.error(f'Ошибка при разделении данных: {e}')
        raise

def save_csv(df, path):
    try:
        df.to_csv(path, index=False, encoding='utf-8')
        logging.info(f'Файл сохранен: {path}')
    except Exception as e:
        logging.error(f'Ошибка при сохранении файла {path}: {e}')
        raise

def merge_catalogue(verification_data, catalogue_path):
    try:
        catalogue = pd.read_excel(catalogue_path)
        # Проверка наличия необходимых столбцов
        if 'sku_code' not in catalogue.columns or 'corr_sku' not in catalogue.columns:
            raise KeyError('Необходимые столбцы "sku_code" или "corr_sku" отсутствуют в catalogue.xlsx')
        verification_data = verification_data.merge(catalogue[['sku_code', 'corr_sku']], left_on='sku', right_on='sku_code', how='left')
        logging.info('corr_sku успешно подтянут из справочника')
        return verification_data
    except FileNotFoundError as e:
        logging.error(f'Файл catalogue.xlsx не найден: {e}')
        raise
    except KeyError as e:
        logging.error(f'Ошибка в структуре catalogue.xlsx: {e}')
        raise
    except Exception as e:
        logging.error(f'Ошибка при объединении с catalogue: {e}')
        raise

def determine_value(row):
    if pd.notnull(row['corr_sku']):
        if row['sku'] == row['corr_sku']:
            return '1_1'
        else:
            return '1_0'
    else:
        if row['response'] in ['ИНТЕРНЕТ РЕШЕНИЯ', 'МАРКЕТПЛЕЙС', 'ВАЙЛДБЕРРИЗ', 'ЯНДЕКС']:
            return '0_1'
        else:
            return '0_0'

def add_comment(row):
    if row['value'] == '1_0':
        return 'неправильно распознали sku'
    elif row['value'] == '0_0':
        return 'не распознали sku'
    elif row['value'] == '0_1':
        return 'не проходит через нейросеть (отключено)'
    else:
        return ''

def process_verification_data(verification_data, catalogue_path):
    verification_data = merge_catalogue(verification_data, catalogue_path)
    verification_data['value'] = verification_data.apply(determine_value, axis=1)
    verification_data['comment'] = verification_data.apply(add_comment, axis=1)

    # Обработка специальных условий
    conditions = verification_data['response'].isin(['ИНТЕРНЕТ РЕШЕНИЯ', 'МАРКЕТПЛЕЙС', 'ВАЙЛДБЕРРИЗ', 'ЯНДЕКС'])
    verification_data.loc[conditions, 'corr_sku'] = None
    verification_data.loc[conditions, 'value'] = '0_1'
    verification_data.loc[conditions, 'comment'] = 'не проходит через нейросеть (отключено)'

    logging.info('Проверка результатов нейросети завершена')
    return verification_data

def save_final_report(verification_data, path):
    try:
        verification_data.to_excel(path, index=False, encoding='utf-8')
        logging.info(f'Итоговый отчет сохранен: {path}')
    except Exception as e:
        logging.error(f'Ошибка при сохранении итогового отчета: {e}')
        raise

def main():
    # Определение базового пути (корневая папка проекта)
    base_path = os.path.expanduser('~/work/studying_r/kr')

    # Пути к папкам и файлам
    scripts_path = os.path.join(base_path, 'Скрипты')
    reports_path = os.path.join(base_path, 'Отчеты', 'Отчет по нейросети')
    output_path = os.path.join(reports_path, 'Результаты')

    # Создание папки для отчетов, если она не существует
    os.makedirs(output_path, exist_ok=True)

    # Пути к файлам
    receipt_file = os.path.join(scripts_path, 'receipt_vish1.csv')
    receiptpartner_file = os.path.join(scripts_path, 'receiptpartners.csv')  # Проверьте название файла
    catalogue_file = os.path.join(scripts_path, 'Каталог Очкарик (справочник).xlsx')  # Из "Доп. материалы"?
    errors_file = os.path.join(scripts_path, 'Ошибки', 'errors.xlsx')  # Предположительно

    # Проверка существования файла каталога
    if not os.path.exists(catalogue_file):
        # Возможно, каталог находится в "Доп. материалы"
        catalogue_file = os.path.join(base_path, 'Доп. материалы', 'Каталог Очкарик (справочник).xlsx')
        if not os.path.exists(catalogue_file):
            logging.error(f'Файл каталога не найден по путям: {catalogue_file}')
            return

    # Пути к сохраненным файлам
    result_not_null_file = os.path.join(output_path, 'result_not_null.csv')
    result_null_file = os.path.join(output_path, 'result_null.csv')
    final_report_file = os.path.join(output_path, 'stats_result_final.xlsx')

    try:
        # Шаг 1: Загрузка данных
        receipt, receiptpartner = load_data(receipt_file, receiptpartner_file)

        # Шаг 2: Разделение данных
        result_not_null, result_null = split_data(receipt)

        # Шаг 3: Сохранение разделенных данных
        save_csv(result_not_null, result_not_null_file)
        save_csv(result_null, result_null_file)

        # Шаг 4: Объединение данных для проверки
        verification_data = pd.concat([result_not_null, result_null], ignore_index=True)

        # Шаг 5: Обработка данных проверки
        verification_data = process_verification_data(verification_data, catalogue_file)

        # Шаг 6: Сохранение итогового отчета
        save_final_report(verification_data, final_report_file)

        logging.info('Все шаги выполнены успешно')
    except Exception as e:
        logging.error(f'Скрипт завершился с ошибкой: {e}')

if __name__ == '__main__':
    main()
