# Завантажуємо необхідні пакети
install.packages("circlize")  # Якщо ще не встановлено
library(circlize)

# Зчитуємо дані з CSV
file_path <- "/Users/anastasiiakostyrka/Desktop/Навчання/7семестр/Візуалізація_даних/Global Missing Migrants Dataset.csv"  # Ваш файл має бути у тій же директорії, що й скрипт
data <- read.csv(file_path)

# Попередній перегляд даних
View(data)

chord_data <- data.frame(
  source = data$Region.of.Origin,
  target = data$Region.of.Incident,
  value = data$Total.Number.of.Dead.and.Missing
)

library(dplyr)

chord_data <- chord_data %>%
  group_by(source, target) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

chord_data <- chord_data %>%
  filter(!is.na(source) & !is.na(target) & !is.na(value)) %>%  # Видаляємо рядки з NA
  filter(value > 0)  # Видаляємо рядки з некоректʼ

chord_data <- chord_data %>%
  mutate(source = ifelse(source == "" | is.na(source), "Unknown", source),
         target = ifelse(target == "" | is.na(target), "Unknown", target))

# CHORD 
chordDiagram(chord_data, transparency = 0.5, annotationTrack = "grid", reduce = 0)






install.packages("networkD3")  # Якщо ще не встановлено
library(networkD3)

sankey_data <- data %>%
  filter(Region.of.Origin == "Southern Asia") %>%
  filter(!is.na(Region.of.Incident) & !is.na(Total.Number.of.Dead.and.Missing))

# Формуємо таблицю з трьома колонками: Region.of.Origin, Region.of.Incident, Total.Number.of.Dead.and.Missing
sankey_data <- sankey_data %>%
  select(source = Region.of.Origin, target = Region.of.Incident, value = Total.Number.of.Dead.and.Missing)

# Згрупуємо дані за Region.of.Incident і підсумуємо Total.Number.of.Dead.and.Missing
sankey_data_grouped <- sankey_data %>%
  group_by(target) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

# Перевірка результату
View(sankey_data_grouped)

# Створюємо таблицю для джерел та цілей
nodes <- data.frame(name = c("Southern Asia", unique(sankey_data_grouped$target)))

# Формуємо таблицю для лінків
links <- sankey_data_grouped %>%
  mutate(source = match("Southern Asia", nodes$name) - 1,  # індекс для "Southern Asia"
         target = match(target, nodes$name) - 1) %>%  # target - регіон інциденту
  select(source, target, value)  # Обираємо необхідні стовпці

sankey <- sankeyNetwork(Links = links, Nodes = nodes, Source = "source", 
                        Target = "target", Value = "value", NodeID = "name", 
                        units = "Migrants")

# Виведення графіка
print(sankey)