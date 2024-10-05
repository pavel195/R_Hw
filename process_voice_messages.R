#!/usr/bin/env Rscript

# Автоматическая установка и загрузка необходимых пакетов
required_packages <- c("tidyverse", "ggplot2", "wordcloud", "voskr", "tidytext", "RussianStemmer", "stopwords")

# Функция для установки пакета, если он отсутствует
install_if_missing <- function(pkg){
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE, repos = "https://cloud.r-project.org/")
    if(!require(pkg, character.only = TRUE)) {
      stop(paste("Не удалось установить пакет:", pkg))
    }
  }
}

# Установка отсутствующих пакетов
invisible(sapply(required_packages, install_if_missing))

# Загрузка библиотек
library(tidyverse)
library(ggplot2)
library(wordcloud)
library(voskr)
library(tidytext)
library(RussianStemmer)
library(stopwords)

# Установка рабочего каталога
setwd("~/work/studying_r")

# Путь к папке с голосовыми сообщениями
voice_msgs_dir <- "/mnt/c/Users/paulc/Downloads/Telegram Desktop/telega_temp/voice_messages"

# Получение списка .ogg файлов
ogg_files <- list.files(path = voice_msgs_dir, pattern = "\\.ogg$", full.names = TRUE)

# Проверка наличия .ogg файлов
if(length(ogg_files) == 0){
  stop("В указанной директории нет файлов с расширением .ogg")
}

# Создание папки для .wav файлов
wav_dir <- file.path(voice_msgs_dir, "wav_files")
dir.create(wav_dir, showWarnings = FALSE)

# Функция для конвертации .ogg в .wav
convert_to_wav <- function(ogg_file, wav_file) {
  command <- sprintf('sox "%s" "%s"', ogg_file, wav_file)
  system(command, intern = TRUE)
}

# Конвертация всех .ogg файлов в .wav
for (ogg_file in ogg_files) {
  wav_file <- file.path(wav_dir, paste0(basename(tools::file_path_sans_ext(ogg_file)), ".wav"))

  # Проверка, существует ли уже .wav файл
  if(!file.exists(wav_file)){
    cat("Конвертация файла:", ogg_file, "в", wav_file, "\n")
    convert_to_wav(ogg_file, wav_file)
  } else {
    cat("Файл уже конвертирован:", wav_file, "\n")
  }
}

# Получение списка .wav файлов
wav_files <- list.files(path = wav_dir, pattern = "\\.wav$", full.names = TRUE)

# Проверка наличия .wav файлов
if(length(wav_files) == 0){
  stop("В папке для .wav файлов нет файлов для обработки")
}

# Инициализация модели Vosk для русского языка
model_path <- "model"  # Путь к папке с моделью
if (!dir.exists(model_path)) {
  cat("Скачивание модели Vosk для русского языка...\n")
  download.file("https://alphacephei.com/vosk/models/vosk-model-small-ru-0.22.zip", destfile = "model.zip")
  unzip("model.zip", exdir = ".")
  file.rename("vosk-model-small-ru-0.22", model_path)
  file.remove("model.zip")
  cat("Модель Vosk загружена и распакована.\n")
}

# Инициализация модели
model <- tryCatch({
  voskr::vosk_model$new(model_path)
}, error = function(e){
  stop("Не удалось инициализировать модель Vosk: ", e$message)
})

# Функция для распознавания речи
recognize_speech <- function(wav_file) {
  recognizer <- voskr::vosk_recognizer$new(model, sample_rate = 16000)
  audio_data <- voskr::read_audio(wav_file)
  recognizer$accept_waveform(audio_data)
  result <- recognizer$result()
  text <- result$text
  return(text)
}

# Распознавание речи для всех файлов и сбор текста
all_text <- ""
for (wav_file in wav_files) {
  cat("Обработка файла:", wav_file, "\n")
  text <- tryCatch({
    recognize_speech(wav_file)
  }, error = function(e){
    warning(paste("Ошибка при обработке файла", wav_file, ":", e$message))
    ""
  })
  all_text <- paste(all_text, text, sep = " ")
}

# Проверка наличия распознанного текста
if(nchar(all_text) == 0){
  stop("Не удалось распознать текст из голосовых сообщений.")
}

# Обработка текста

# Преобразование текста в нижний регистр
all_text <- tolower(all_text)

# Разбиение на слова
words_df <- tibble(text = all_text) %>%
  unnest_tokens(word, text)

# Удаление стоп-слов
stop_words_ru <- stopwords("ru")
words_df <- words_df %>%
  filter(!word %in% stop_words_ru)

# Подсчет частот слов
word_counts <- words_df %>%
  count(word, sort = TRUE)

# Проверка наличия слов для визуализации
if(nrow(word_counts) == 0){
  stop("После удаления стоп-слов не осталось слов для анализа.")
}

# Построение и сохранение гистограммы топ-20 слов
hist_plot <- word_counts %>%
  top_n(20, n) %>%
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Топ-20 наиболее частотных слов",
       x = "Слово", y = "Частота") +
  theme_minimal()

ggsave("top20_words_histogram.png", plot = hist_plot, width = 8, height = 6)
cat("Гистограмма топ-20 слов сохранена как 'top20_words_histogram.png'\n")

# Построение и сохранение облака слов
png("wordcloud.png", width = 800, height = 600)
wordcloud(words = word_counts$word, freq = word_counts$n,
          min.freq = 2, max.words = 100, random.order = FALSE,
          colors = brewer.pal(8, "Dark2"))
dev.off()
cat("Облако слов сохранено как 'wordcloud.png'\n")
