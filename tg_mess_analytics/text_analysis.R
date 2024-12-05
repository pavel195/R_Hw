# Установка и загрузка необходимых пакетов
library(tidyverse)
library(tidytext)
library(wordcloud2)
library(udpipe)
library(lubridate)
library(ggplot2)
library(htmlwidgets)

# Загрузка русской модели для лемматизации
udmodel <- udpipe_download_model(language = "russian")
udmodel <- udpipe_load_model(udmodel$file_model)

# Чтение данных
df <- read.csv("all_chat_data.csv", 
               encoding = "UTF-8",
               col.names = c("datetime", "user", "text"))

# Конвертация времени
df$datetime <- as.POSIXct(df$datetime, format = "%d.%m.%Y %H:%M:%S")

# Фильтрация сообщений пользователя Zh
zh_messages <- df %>%
  filter(user == "Zh")

# Токенизация и лемматизация сообщений Zh
zh_text_analysis <- zh_messages %>%
  select(text) %>%
  unnest_tokens(word, text) %>%
  # Удаление стоп-слов
  anti_join(get_stopwords(language = "ru")) %>%
  # Удаление цифр и коротких слов
  filter(!str_detect(word, "\\d+"),
         str_length(word) > 2)

# Лемматизация с помощью udpipe
annotations <- udpipe_annotate(udmodel, x = unique(zh_text_analysis$word))
lemmas <- as.data.frame(annotations) %>%
  select(token, lemma)

# Объединение с лемматизированными словами
zh_text_analysis <- zh_text_analysis %>%
  left_join(lemmas, by = c("word" = "token"))

# Подсчет частоты слов
zh_word_frequencies <- zh_text_analysis %>%
  count(lemma, sort = TRUE) %>%
  filter(!is.na(lemma))

# Создание облака слов для моих сообщений
wordcloud <- wordcloud2(zh_word_frequencies[1:50,], 
                       size = 0.5,
                       color = 'random-dark',
                       backgroundColor = "white",
                       rotateRatio = 0.3,
                       minRotation = -pi/4,
                       maxRotation = pi/4)
# Сохранение облака слов в HTML
saveWidget(wordcloud, "zh_wordcloud.html", selfcontained = TRUE)

# Топ-15 самых частых слов Zh
zh_top_words_plot <- zh_word_frequencies[1:15,] %>%
  ggplot(aes(x = reorder(lemma, n), y = n, fill = lemma)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA)
  ) +
  scale_fill_brewer(palette = "Set3") +  # Использование яркой цветовой палитры
  labs(title = "Топ-15 самых частых слов от меня",
       x = "Слово",
       y = "Частота",
       fill = "Слова")

# Анализ активности Zh по часам
zh_activity_by_hour <- zh_messages %>%
  mutate(hour = hour(datetime)) %>%
  count(hour) %>%
  ggplot(aes(x = hour, y = n)) +
  geom_line(color = "#FF69B4", size = 1) +  # Розовая линия
  geom_point(aes(color = n), size = 3) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA)
  ) +
  scale_color_gradient(low = "#87CEEB", high = "#FF1493") +  # Градиент от голубого к розовому
  labs(title = "Активность меня по часам",
       x = "Час",
       y = "Количество сообщений",
       color = "Количество\nсообщений")

# Анализ средней длины моих сообщений 
zh_message_length <- zh_messages %>%
  mutate(
    length = str_length(text),
    date = as.Date(datetime)
  ) %>%
  group_by(date) %>%
  summarise(avg_length = mean(length)) %>%
  ggplot(aes(x = date, y = avg_length)) +
  geom_line(color = "#006400", size = 1) +
  geom_smooth(method = "loess", color = "#98FB98", fill = "#98FB98", alpha = 0.2) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA)
  ) +
  labs(title = "Средняя длина сообщений от меня по дням",
       x = "Дата",
       y = "Средняя длина сообщения")

# Сохранение графиков с белым фоном
ggsave("zh_top_words.png", zh_top_words_plot, width = 10, height = 8, bg = "white")
ggsave("zh_activity_by_hour.png", zh_activity_by_hour, width = 10, height = 6, bg = "white")
ggsave("zh_message_length.png", zh_message_length, width = 10, height = 6, bg = "white")

# Создание отчета в HTML
zh_report <- c(
  "# Анализ сообщений от меня\n\n",
  paste("Всего сообщений от :", nrow(zh_messages), "\n"),
  paste("Уникальных слов:", nrow(zh_word_frequencies), "\n"),
  "\n## Самые частые слова :\n",
  paste(capture.output(head(zh_word_frequencies, 10)), collapse = "\n"),
  "\n\nГрафики сохранены в файлах:\n",
  "- zh_top_words.png\n",
  "- zh_activity_by_hour.png\n",
  "- zh_message_length.png\n"
)

writeLines(zh_report, "zh_report.md") 