# analysis.R

# Проверяем и устанавливаем renv, если он не установлен
if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv")
}
library(renv)

# Активируем renv
# renv::activate()

# Список необходимых пакетов
required_packages <- c(
  "rvest",
  "dplyr",
  "stringr",
  "ggplot2",
  "wordcloud",
  "tm",
  "tidytext",
  "textstem",
  "readr",
  "lubridate",
  "tidyr",
  "scales"
)

# Устанавливаем недостающие пакеты
installed_packages <- rownames(installed.packages())
for (pkg in required_packages) {
  if (!pkg %in% installed_packages) {
    renv::install(pkg)
  }
}

# Загружаем библиотеки
library(rvest)
library(dplyr)
library(stringr)
library(ggplot2)
library(wordcloud)
library(tm)
library(tidytext)
library(textstem)
library(readr)
library(lubridate)
library(tidyr)
library(scales)

# Загрузка всех HTML файлов из папки
html_files <- list.files(path = "messages", pattern = "\\.html?$", full.names = TRUE)

# Проверка на наличие HTML файлов
if (length(html_files) == 0) {
  stop("Нет HTML файлов в указанной папке 'messages'. Проверьте путь и наличие файлов.")
}

# Инициализация пустого data.frame для хранения всех сообщений
all_chat_data <- data.frame(
  Date = character(),
  From = character(),
  Text = character(),
  stringsAsFactors = FALSE
)

# Функция для обработки отдельного HTML файла
process_html_file <- function(html_file) {
  chat_page <- read_html(html_file)

  # Извлечение данных о сообщениях
  messages <- chat_page %>% html_nodes(".message.default")

  # Извлечение дат и времени сообщений
  dates <- messages %>% html_nodes(".pull_right.date.details") %>% html_attr("title")

  # Извлечение имен отправителей
  from_names <- messages %>% html_nodes(".from_name") %>% html_text(trim = TRUE)

  # Извлечение текста сообщений
  texts <- messages %>% html_nodes(".text") %>% html_text(trim = TRUE)

  # Приведение всех извлеченных данных к одной длине
  max_length <- max(length(dates), length(from_names), length(texts))
  if (max_length > 0) {
    dates <- c(dates, rep(NA, max_length - length(dates)))
    from_names <- c(from_names, rep("Неизвестный", max_length - length(from_names)))
    texts <- c(texts, rep("", max_length - length(texts)))

    # Создание таблицы с результатами для текущего файла
    chat_data <- data.frame(
      Date = dates,
      From = from_names,
      Text = texts,
      stringsAsFactors = FALSE
    )

    # Фильтрация пустых сообщений
    chat_data <- chat_data %>% filter(Text != "")

    return(chat_data)
  } else {
    return(NULL)
  }
}

# Обработка всех HTML файлов и объединение данных
for (html_file in html_files) {
  cat("Обработка файла:", html_file, "\n")
  chat_data <- process_html_file(html_file)
  if (!is.null(chat_data)) {
    all_chat_data <- bind_rows(all_chat_data, chat_data)
  }
}

# Проверка на наличие данных для анализа
if (nrow(all_chat_data) == 0) {
  stop("Нет данных для анализа. Проверьте корректность HTML файлов.")
}

# Преобразование даты в POSIXct
all_chat_data <- all_chat_data %>%
  mutate(
    Date = dmy_hms(Date, tz = "UTC"),
    MessageLength = nchar(Text)
  )

# Просмотр таблицы
print(head(all_chat_data))

# Сохранение данных в CSV файл
write_csv(all_chat_data, "all_chat_data.csv")

# Анализ текста: создание облака слов
# Объединение всех сообщений в один текст
all_text <- paste(all_chat_data$Text, collapse = " ")

# Создание корпуса и очистка текста
corpus <- VCorpus(VectorSource(all_text)) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeWords, stopwords("russian")) %>%
  tm_map(content_transformer(lemmatize_strings))

# Создание матрицы частоты слов и построение облака слов
dtm <- TermDocumentMatrix(corpus)
m <- as.matrix(dtm)
word_freqs <- sort(rowSums(m), decreasing = TRUE)
df_word_freq <- data.frame(word = names(word_freqs), freq = word_freqs, row.names = NULL)

# Проверка на наличие слов для построения облака
if (nrow(df_word_freq) > 0) {
  # Построение облака слов
  set.seed(1234)
  png("wordcloud.png", width = 800, height = 600)
  wordcloud(
    words = df_word_freq$word,
    freq = df_word_freq$freq,
    min.freq = 2,
    max.words = 100,
    random.order = FALSE,
    colors = brewer.pal(8, "Dark2")
  )
  dev.off()
} else {
  warning("Нет слов для построения облака слов.")
}

# Построение графиков частоты слов
# Топ-10 самых частотных слов
if (nrow(df_word_freq) > 0) {
  top_words <- head(df_word_freq, 10)
  png("top_words.png", width = 800, height = 600)
  ggplot(top_words, aes(x = reorder(word, -freq), y = freq)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    theme_minimal() +
    labs(title = "Топ-10 самых частотных слов", x = "Слово", y = "Частота") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  dev.off()
}

# График слов, распределенных по пользователям
words_by_user <- all_chat_data %>%
  unnest_tokens(word, Text) %>%
  filter(!word %in% stopwords("russian")) %>%
  mutate(word = lemmatize_strings(word)) %>%
  count(From, word, sort = TRUE)

if (nrow(words_by_user) > 0) {
  top_words_by_user <- words_by_user %>%
    group_by(From) %>%
    slice_max(order_by = n, n = 5)

  png("words_by_user.png", width = 1200, height = 800)
  ggplot(top_words_by_user, aes(x = reorder(word, -n), y = n, fill = From)) +
    geom_bar(stat = "identity", show.legend = FALSE) +
    facet_wrap(~From, scales = "free") +
    theme_minimal() +
    labs(title = "Топ-5 слов по пользователям", x = "Слово", y = "Частота") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  dev.off()
}

# Метрики анализа текста
# Общее количество сообщений
total_messages <- nrow(all_chat_data)

# Количество сообщений по пользователям
messages_by_user <- all_chat_data %>%
  group_by(From) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))

cat("Общее количество сообщений:", total_messages, "\n")
print(messages_by_user)

# Построение графика количества сообщений по пользователям
if (nrow(messages_by_user) > 0) {
  png("messages_by_user.png", width = 800, height = 600)
  ggplot(messages_by_user, aes(x = reorder(From, -Count), y = Count)) +
    geom_bar(stat = "identity", fill = "tomato") +
    theme_minimal() +
    labs(title = "Количество сообщений по пользователям", x = "Пользователь", y = "Количество сообщений") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  dev.off()
}

# Дополнительные графики
# Распределение длины сообщений
if (nrow(all_chat_data) > 0) {
  png("message_length_distribution.png", width = 800, height = 600)
  ggplot(all_chat_data, aes(x = MessageLength)) +
    geom_histogram(binwidth = 10, fill = "skyblue", color = "black") +
    theme_minimal() +
    labs(title = "Распределение длины сообщений", x = "Длина сообщения (символы)", y = "Количество сообщений")
  dev.off()
}

# Средняя длина сообщения по пользователям
avg_message_length_by_user <- all_chat_data %>%
  group_by(From) %>%
  summarise(AvgLength = mean(MessageLength, na.rm = TRUE)) %>%
  arrange(desc(AvgLength))

if (nrow(avg_message_length_by_user) > 0) {
  png("avg_message_length_by_user.png", width = 800, height = 600)
  ggplot(avg_message_length_by_user, aes(x = reorder(From, -AvgLength), y = AvgLength)) +
    geom_bar(stat = "identity", fill = "purple") +
    theme_minimal() +
    labs(title = "Средняя длина сообщения по пользователям", x = "Пользователь", y = "Средняя длина сообщения") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  dev.off()
}

# Часовая активность сообщений
all_chat_data <- all_chat_data %>%
  mutate(Hour = hour(Date))

messages_by_hour <- all_chat_data %>%
  group_by(Hour) %>%
  summarise(Count = n()) %>%
  arrange(Hour)

if (nrow(messages_by_hour) > 0) {
  png("messages_by_hour.png", width = 800, height = 600)
  ggplot(messages_by_hour, aes(x = Hour, y = Count)) +
    geom_line(group = 1, color = "darkgreen") +
    geom_point(color = "darkgreen") +
    scale_x_continuous(breaks = 0:23) +
    theme_minimal() +
    labs(title = "Часовая активность сообщений", x = "Час", y = "Количество сообщений")
  dev.off()
}

# Доля сообщений каждого пользователя в общем объёме
if (nrow(messages_by_user) > 0) {
  png("user_message_share.png", width = 800, height = 600)
  ggplot(messages_by_user, aes(x = "", y = Count, fill = From)) +
    geom_bar(width = 1, stat = "identity") +
    coord_polar(theta = "y") +
    theme_minimal() +
    labs(title = "Доля сообщений каждого пользователя", x = NULL, y = NULL) +
    theme(axis.text.x = element_blank(), axis.ticks = element_blank())
  dev.off()
}
