

# 1. Работа со списками

# Сначала загружаем датасет
df <- read.csv("Student_Depression_Dataset.csv")
df <- df[1:1000, ]

# Создаем списки из данных датасета
# Преобразуем колонки датафрейма в списки для дальнейшей работы
student_lists <- list(
    # Базовая демографическая информация
    demographics = list(
        age = df$Age,
        gender = df$Gender,
        city = df$City
    ),
    
    # Академические показатели
    academic = list(
        cgpa = df$CGPA,
        degree = df$Degree,
        study_hours = df$`Work/Study Hours`,
        academic_pressure = df$`Academic Pressure`
    ),
    
    # Показатели здоровья и образа жизни
    lifestyle = list(
        sleep = df$`Sleep Duration`,
        diet = df$`Dietary Habits`
    ),
    
    # Показатели стресса и психического здоровья
    mental_health = list(
        depression = df$Depression,
        suicidal_thoughts = df$`Have you ever had suicidal thoughts ?`,
        family_history = df$`Family History of Mental Illness`
    )
)

#  работы со списками:

print("Структура начального списка:")
str(student_lists)

# 1. Добавление нового элемента в список
# Добавляем показатель стресса как сумму академического и финансового стресса
student_lists$stress <- df$Academic.Pressure + df$Financial.Stress

# Создаем вложенный (рекурсивный) список, группируя возраст по городам
student_lists$city_age_stats <- split(df$Age, df$City)

print("Список после добавления новых элементов:")
str(student_lists)

# 2. Удаление элемента из списка
student_lists$demographics$gender <- NULL
print("Структура списка после удаления пола:")
str(student_lists$demographics)
str(student_lists)

# 3. Применение lapply для анализа числовых данных
# Вычисляем средние значения для всех числовых показателей в academic
# lapply всегда возвращает список
numeric_means <- lapply(student_lists[c("ages", "cgpa", "stress")], mean)
print("\nСредние значения числовых показателей:")
print(numeric_means)

# Демострация sapply - более удобный вывод, чем lapply
# sapply пытается упростить вывод до вектора или матрицы
numeric_stats <- sapply(student_lists[c("ages", "cgpa", "stress")], 
                       function(x) c(mean = mean(x), sd = sd(x)))
print("\nСтатистика числовых показателей:")
print(numeric_stats)

# 4. Применение sapply для подсчета уникальных значений
unique_counts <- sapply(student_lists$lifestyle, function(x) length(unique(x)))
print("Количество уникальных значений для показателей образа жизни:")
print(unique_counts)

# 5. Создание вложенного списка с агрегированными данными
summary_stats <- list(
    age_stats = list(
        mean = mean(student_lists$demographics$age),
        median = median(student_lists$demographics$age),
        sd = sd(student_lists$demographics$age)
    ),
    depression_rate = mean(student_lists$mental_health$depression),
    cities_count = length(unique(student_lists$demographics$city))
)

print("Сводная статистика:")
print(summary_stats)

# 2. Работа с датафреймом
# Создаем графики
library(ggplot2)

# Создаем директорию для графиков, если её нет
if (!dir.exists("plots")) {
    dir.create("plots")
}

# Сохраняем гистограмму возраста
png("plots/age_distribution.png", 
    width = 800,    # ширина в пикселях
    height = 600,   # высота в пикселях
    res = 120)      # разрешение

ggplot(df, aes(x=Age)) +
    geom_histogram(binwidth=2, fill="skyblue", color="black") +
    labs(title="Распределение возраста студентов",
         x="Возраст", 
         y="Количество") +
    theme_minimal() +    # добавляем минималистичную тему
    theme(
        plot.title = element_text(size=16, face="bold"),
        axis.title = element_text(size=12)
    )

dev.off()  # закрываем графическое устройство

# Сохраняем столбчатую диаграмму CGPA
png("plots/cgpa_by_city.png", 
    width = 1000,   # делаем шире для лучшей читаемости названий городов
    height = 600,
    res = 120)

ggplot(df, aes(x=City, y=CGPA)) +
    geom_bar(stat="identity", fill="lightgreen") +
    theme_minimal() +
    theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size=10),
        plot.title = element_text(size=16, face="bold"),
        axis.title = element_text(size=12)
    ) +
    labs(title="CGPA по городам",
         x="Город", 
         y="Средний CGPA")

dev.off()

# Создадим еще один интересный график - точеную диаграмму
png("plots/age_cgpa_depression.png", 
    width = 800,
    height = 600,
    res = 120)

ggplot(df, aes(x=Age, y=CGPA, color=factor(Depression))) +
    geom_point(size=3, alpha=0.6) +
    theme_minimal() +
    labs(title="Зависимость CGPA от возраста",
         x="Возраст",
         y="CGPA",
         color="Депрессия") +
    scale_color_manual(values=c("0"="green", "1"="red"),
                      labels=c("0"="Нет", "1"="Есть")) +
    theme(
        plot.title = element_text(size=16, face="bold"),
        axis.title = element_text(size=12),
        legend.position = "right"
    )

dev.off()

print("Графики сохранены в директории 'plots':")
print(list.files("plots", pattern=".png$"))

# Дополнительный анализ: корреляция между показателями
numeric_data <- df[sapply(df, is.numeric)]
correlations <- cor(numeric_data, use="complete.obs")
print("Корреляции между числовыми показателями:")
print(round(correlations, 2))

# 3. Дополнительное задание: улучшенная функция подсчета слов
count_words <- function(text) {
    # Приведение к нижнему реистру
    text <- tolower(text)
    # Удаление знаков препинания
    text <- gsub("[[:punct:]]", "", text)
    # Удаление лишних пробелов
    text <- gsub("\\s+", " ", text)
    # Разделение на слова
    words <- strsplit(text, "\\s+")[[1]]
    # Подсчет уникальных слов
    unique_words <- length(unique(words))
    return(unique_words)
}

# Пример использования
sample_text <- "Это пример текста. Это ПРИМЕР подсчета слов!"
word_count <- count_words(sample_text)
print(paste("Количество уникальных слов:", word_count))