# Установка и загрузка необходимых пакетов
install.packages(c("jsonlite", "ggplot2", "dplyr", "stringr", "tidyr", "lubridate", "tidytext", "textstem"))
library(jsonlite)
library(ggplot2)
library(dplyr)
library(stringr)
library(tidyr)
library(lubridate)
library(tidytext)
library(textstem)

# Функция для удаления эмодзи и других специальных символов
remove_special_chars <- function(text) {
    # Удаление эмодзи и специальных символов (оставляем только алфавитные символы и пробелы)
    cleaned_text <- str_replace_all(text, "[^\x01-\x7F]+", "") # Удаляем символы Unicode (эмодзи)
    cleaned_text <- str_replace_all(cleaned_text, "[^[:alnum:][:space:][:punct:]]", "") # Удаляем все кроме букв, цифр и знаков препинания
    return(cleaned_text)
}

# Загрузка и обработка JSON-файла чата
json_file <- "/home/pavel/work/studying_r/hw1/data/chat_utf8.json" # Укажите путь к вашему JSON-файлу

# Чтение JSON-файла
chat_data <- fromJSON(json_file, flatten = TRUE)

# Извлечение сообщений
messages <- chat_data$messages

# Преобразование в датафрейм
messages_df <- as.data.frame(messages)

# Преобразование даты в формат DateTime
messages_df$date <- ymd_hms(messages_df$date)

# Выбор основных столбцов
messages_df <- messages_df %>%
    select(date, from, text, id)

# Фильтрация только текстовых сообщений
messages_df <- messages_df %>%
    filter(!is.na(text) & is.character(text))

# Применение функции для удаления эмодзи и специальных символов
messages_df$text_clean <- sapply(messages_df$text, remove_special_chars)

# Приведение текста к верхнему регистру
messages_df$text_clean <- str_to_upper(messages_df$text_clean)

# Токенизация текста (разбиение на слова)
words <- messages_df %>%
    unnest_tokens(word, text_clean) %>%
    anti_join(stop_words, by = "word") # Убираем стоп-слова

# Лемматизация слов
words <- words %>%
    mutate(lemma = lemmatize_words(word))

# Приведение лемм к верхнему регистру
words$lemma <- str_to_upper(words$lemma)

# Частотный анализ
word_frequencies <- words %>%
    count(lemma, sort = TRUE) %>%
    rename(word = lemma) %>%
    mutate(freq = n / sum(n))

# Топ-20 наиболее часто используемых слов
top_words <- word_frequencies %>%
    top_n(20, n)

# Построение графика топ-20 наиболее часто используемых слов
ggplot(top_words, aes(x = reorder(word, n), y = n)) +
    geom_bar(stat = "identity", fill = "coral") +
    coord_flip() +
    labs(
        title = "Топ-20 наиболее часто используемых слов (верхний регистр)",
        x = "Слово",
        y = "Количество"
    ) +
    theme_minimal()

# Вывод первых нескольких строк для проверки результатов
head(words)
