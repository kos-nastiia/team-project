# Install necessary libraries
install.packages("dendextend")
install.packages("ggplot2")
install.packages("tidyverse")
install.packages("ggraph")

# Load libraries
library(dendextend)
library(ggplot2)
library(tidyverse)


book1 <- read.csv("~/Desktop/Навчання/7семестр/Візуалізація_даних/Team Project/book1.csv")
book2 <- read.csv("~/Desktop/Навчання/7семестр/Візуалізація_даних/Team Project/book2.csv")
book3 <- read.csv("~/Desktop/Навчання/7семестр/Візуалізація_даних/Team Project/book3.csv")
book4 <- read.csv("~/Desktop/Навчання/7семестр/Візуалізація_даних/Team Project/book4.csv")
book5 <- read.csv("~/Desktop/Навчання/7семестр/Візуалізація_даних/Team Project/book5.csv")












# Dendrogram chart
# Завантаження необхідних бібліотек
library(dplyr)
library(tidyr)
library(ggplot2)

# Функція для визначення топ-30 персонажів
get_top_characters <- function(data, top_n = 30) {
  characters <- data %>%
    select(Source, Target) %>%
    pivot_longer(cols = everything(), names_to = "type", values_to = "character") %>%
    count(character, sort = TRUE) %>%
    slice_head(n = top_n) %>%
    pull(character)
  return(characters)
}

# Отримання топ-30 персонажів
top_characters <- get_top_characters(bind_rows(book1, book5))

# Функція для створення матриці ваг (матриця суміжності)
create_interaction_matrix <- function(data, characters) {
  matrix <- matrix(0, nrow = length(characters), ncol = length(characters))
  colnames(matrix) <- characters
  rownames(matrix) <- characters
  
  for (i in 1:nrow(data)) {
    source <- data$Source[i]
    target <- data$Target[i]
    weight <- data$weight[i]
    if (source %in% characters & target %in% characters) {
      matrix[source, target] <- matrix[source, target] + weight
      matrix[target, source] <- matrix[target, source] + weight  # Неорієнтований граф
    }
  }
  return(as.data.frame(matrix))
}

# Створення матриць ваг для 1 та 5 книги
interaction_matrix_book1 <- create_interaction_matrix(book1, top_characters)
interaction_matrix_book5 <- create_interaction_matrix(book5, top_characters)

# Нова метрика: сума ваг для кожного персонажа
add_total_interactions <- function(matrix) {
  matrix$total_interactions <- rowSums(matrix)
  return(matrix)
}

interaction_matrix_book1 <- add_total_interactions(interaction_matrix_book1)
interaction_matrix_book5 <- add_total_interactions(interaction_matrix_book5)

# Функція для побудови стилізованої дендрограми
plot_dendrogram <- function(matrix, title) {
  # Нормалізація даних
  matrix_scaled <- scale(matrix[,-ncol(matrix)]) # Виключаємо колонку з "total_interactions"
  
  # Виконання кластеризації
  hc <- hclust(dist(matrix_scaled), method = "ward.D2")
  
  # Побудова дендрограми з кастомізацією кольорів
  dend <- as.dendrogram(hc)
  
  # Стилізація дендрограми (червоні гілки, чорний текст)
  dend <- dend %>%
    dendextend::set("branches_k_color", value = c("black", "red")) %>%
    dendextend::set("branches_lwd", 1.5) %>%
    dendextend::set("labels_col", value = "black") %>%
    dendextend::set("labels_cex", 0.8)
  
  # Візуалізація
  plot(dend, main = title, xlab = "Characters", sub = "", cex.main = 1.5)
}

# Побудова дендрограм
plot_dendrogram(interaction_matrix_book1, "Dendrogram of Top 30 Characters (Book 1)")
plot_dendrogram(interaction_matrix_book5, "Dendrogram of Top 30 Characters (Book 5)")





# Load necessary libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(dendextend)

# Function to get the top characters from all books
get_top_characters <- function(data_list, top_n = 30) {
  combined_data <- bind_rows(data_list)
  characters <- combined_data %>%
    select(Source, Target) %>%
    pivot_longer(cols = everything(), names_to = "type", values_to = "character") %>%
    count(character, sort = TRUE) %>%
    slice_head(n = top_n) %>%
    pull(character)
  return(characters)
}

# Function to create a combined interaction matrix for all books
create_combined_interaction_matrix <- function(data_list, characters) {
  combined_matrix <- matrix(0, nrow = length(characters), ncol = length(characters))
  colnames(combined_matrix) <- characters
  rownames(combined_matrix) <- characters
  
  for (book_index in seq_along(data_list)) {
    data <- data_list[[book_index]]
    for (i in 1:nrow(data)) {
      source <- data$Source[i]
      target <- data$Target[i]
      weight <- data$weight[i]
      if (source %in% characters & target %in% characters) {
        combined_matrix[source, target] <- combined_matrix[source, target] + (weight * book_index)
        combined_matrix[target, source] <- combined_matrix[target, source] + (weight * book_index)
      }
    }
  }
  return(as.data.frame(combined_matrix))
}

# Load all books
book1 <- read.csv("~/Desktop/Навчання/7семестр/Візуалізація_даних/Team Project/book1.csv")
book2 <- read.csv("~/Desktop/Навчання/7семестр/Візуалізація_даних/Team Project/book2.csv")
book3 <- read.csv("~/Desktop/Навчання/7семестр/Візуалізація_даних/Team Project/book3.csv")
book4 <- read.csv("~/Desktop/Навчання/7семестр/Візуалізація_даних/Team Project/book4.csv")
book5 <- read.csv("~/Desktop/Навчання/7семестр/Візуалізація_даних/Team Project/book5.csv")

# Combine book data into a list
book_data <- list(book1, book2, book3, book4, book5)

# Get the top 30 characters across all books
top_characters <- get_top_characters(book_data)

# Create a combined interaction matrix
interaction_matrix_combined <- create_combined_interaction_matrix(book_data, top_characters)

print(interaction_matrix_combined)
interaction_matrix_combined_scaled <- scale(interaction_matrix_combined)

plot_colored_dendrogram <- function(matrix, title) {
  # Нормалізація матриці
  matrix_scaled <- scale(matrix)
  
  # Кластеризація
  hc <- hclust(dist(matrix_scaled), method = "ward.D2")
  
  # Конвертація у дендрограму
  dend <- as.dendrogram(hc)
  
  # Визначення кольорів для гілок за домінуючою книгою
  branch_colors <- apply(matrix, 1, function(row) {
    max_book <- which.max(row)  # Знаходимо домінуючу книгу
    return(max_book)
  })
  
  # Встановлення кольорів для гілок
  book_colors <- c("black", "darkgray", "gray", "lightcoral", "red")  # Кольори для книг 1-5
  branch_colors <- book_colors[branch_colors]
  
  # Налаштування дендрограми
  dend <- dend %>%
    dendextend::set("branches_k_color", value = branch_colors) %>%
    dendextend::set("branches_lwd", 1.2) %>%         # Товщина гілок
    dendextend::set("labels_col", value = "black") %>% # Колір підписів
    dendextend::set("labels_cex", 0.8)               # Розмір шрифта підписів
  
  # Створення полотна для дендрограми
  par(mar = c(10, 4, 4, 2))  # Розширення полів: збільшуємо нижнє поле для підписів
  
  # Побудова дендрограми
  plot(dend, main = title, xlab = "", ylab = "Height",
       cex.axis = 0.8, cex.lab = 1, cex.main = 1.5,
       las = 2,  # Поворот підписів осі X вертикально
       sub = "")  # Видаляємо зайвий підзаголовок
  
  # Легенда
  legend("topright", legend = paste("Book", 1:5),
         fill = book_colors, title = "Book Colors", cex = 0.8)
}
# Plot the combined dendrogram
plot_colored_dendrogram(interaction_matrix_combined_scaled, "Dendrogram of Top 30 Characters Across All Books")

interaction_matrix_combined_log <- log1p(interaction_matrix_combined)  # log(1 + x)
dist_matrix <- dist(interaction_matrix_combined_log)
plot_colored_dendrogram(dist_matrix, "Dendrogram of Top 30 Characters Across All Books")










install.packages("scico")
library(scico)

# Load data
dataUU <- read.table("https://raw.githubusercontent.com/holtzy/data_to_viz/master/Example_dataset/13_AdjacencyUndirectedUnweighted.csv", header=TRUE)

# Transform the adjacency matrix in a long format
connect <- dataUU %>%
  gather(key="to", value="value", -1) %>%
  mutate(to = gsub("\\.", " ",to)) %>%
  na.omit()

# Number of connection per person
c( as.character(connect$from), as.character(connect$to)) %>%
  as.tibble() %>%
  group_by(value) %>%
  summarize(n=n()) -> coauth
colnames(coauth) <- c("name", "n")
#dim(coauth)

# Create a graph object with igraph
mygraph <- graph_from_data_frame( connect, vertices = coauth, directed = FALSE )

# Find community
com <- walktrap.community(mygraph)
#max(com$membership)

#Reorder dataset and make the graph
coauth <- coauth %>%
  mutate( grp = com$membership) %>%
  arrange(grp) %>%
  mutate(name=factor(name, name))

# keep only 10 first communities
coauth <- coauth %>%
  filter(grp<16)

# keep only this people in edges
connect <- connect %>%
  filter(from %in% coauth$name) %>%
  filter(to %in% coauth$name)

# Create a graph object with igraph
mygraph <- graph_from_data_frame( connect, vertices = coauth, directed = FALSE )

# prepare a vector of n color in the viridis scale
mycolor <- scico(n = max(coauth$grp), palette = "roma")
mycolor <- sample(mycolor, length(mycolor))

# Make the graph
ggraph(mygraph, layout="linear") +
  geom_edge_arc(edge_colour="black", edge_alpha=0.2, edge_width=0.3, fold=TRUE) +
  geom_node_point(aes(size=n, color=as.factor(grp), fill=grp), alpha=0.5) +
  scale_size_continuous(range=c(0.5,8)) +
  scale_color_manual(values=mycolor) +
  geom_node_text(aes(label=name), angle=65, hjust=1, nudge_y = -1.1, size=2.3) +
  theme_void() +
  theme(
    legend.position="none",
    plot.margin=unit(c(0,0,0.4,0), "null"),
    panel.spacing=unit(c(0,0,3.4,0), "null")
  ) +
  expand_limits(x = c(-1.2, 1.2), y = c(-5.6, 1.2))


# Завантаження бібліотек
library(igraph)
library(ggraph)
library(dplyr)
library(scico)

# Об'єднуємо всі таблиці в один датасет
data <- bind_rows(book1, book2, book3, book4, book5)

# Перейменовуємо колонки для зручності
colnames(data) <- c("from", "to", "type", "weight", "book")

# Вибираємо персонажів з останньої книги (book5)
characters_last_book <- unique(c(data %>% filter(book == "book5") %>% pull(from), 
                                 data %>% filter(book == "book5") %>% pull(to)))

# Фільтруємо дані, щоб залишити тільки персонажів, які є в останній книзі
data_filtered <- data %>%
  filter(from %in% characters_last_book, to %in% characters_last_book)

# Об'єднуємо однакові імена та агрегуємо ваги
data_filtered <- data_filtered %>%
  group_by(from, to) %>%
  summarize(weight = sum(weight, na.rm = TRUE), .groups = "drop")

# Підраховуємо кількість з'єднань для кожного персонажа
node_stats <- data_filtered %>%
  select(from, to) %>%
  pivot_longer(cols = everything(), values_to = "name", names_to = NULL) %>%
  group_by(name) %>%
  summarize(n = n())

# Створюємо графік за допомогою igraph
mygraph <- graph_from_data_frame(data_filtered, vertices = node_stats, directed = FALSE)

# Знаходимо спільноти
com <- cluster_walktrap(mygraph)

# Додаємо інформацію про групу до вузлів
node_stats <- node_stats %>%
  mutate(grp = membership(com)[name]) %>% # Додаємо спільноти
  arrange(grp) %>%
  mutate(name = factor(name, levels = name))

# Вибираємо лише вузли, які належать до перших 15 спільнот
node_stats <- node_stats %>%
  filter(grp < 16)
node_stats <- node_stats %>%
  mutate(grp = ifelse(is.na(grp), -1, grp))

# Фільтруємо зв'язки, щоб залишити тільки релевантні
data_filtered <- data_filtered %>%
  filter(from %in% node_stats$name, to %in% node_stats$name)

# Перестворюємо графік з очищеними даними
mygraph <- graph_from_data_frame(data_filtered, vertices = node_stats, directed = FALSE)

# Підготовка кольорової палітри
if (max_grp == -Inf) {
  mycolor <- scico(n = 10, palette = "roma")  # Встановлюємо 10 кольорів як дефолт
} else {
  mycolor <- scico(n = max_grp, palette = "roma")
}

# Перемішуємо кольори
mycolor <- sample(mycolor, length(mycolor))

# Візуалізація графу
ggraph(mygraph, layout = "linear") +
  geom_edge_arc(edge_colour = "black", edge_alpha = 0.2, edge_width = 0.3, fold = TRUE) +
  geom_node_point(aes(size = n, color = as.factor(grp), fill = grp), alpha = 0.5) +
  scale_size_continuous(range = c(0.5, 8)) +
  scale_color_manual(values = mycolor) +
  geom_node_text(aes(label = name), angle = 65, hjust = 1, nudge_y = -1.1, size = 2.3) + 
  theme_void() +
  theme(
    legend.position = "none",
    plot.margin = unit(c(0, 0, 0.4, 0), "null"),
    panel.spacing = unit(c(0, 0, 3.4, 0), "null")
  ) +
  expand_limits(x = c(-1.2, 1.2), y = c(-5.6, 1.2))

















# Network diagram
# Встановлення та підключення необхідних пакетів
install.packages("igraph")
install.packages("ggraph")
install.packages("tidygraph")

# Завантаження бібліотек
library(igraph)
library(RColorBrewer)
library(scales)

# Завантаження даних
book1 <- read.csv("~/Desktop/Навчання/7семестр/Візуалізація_даних/Team Project/book1.csv")
book1_filtered <- subset(book1, weight > 3)
# Створення графа
network <- graph_from_data_frame(d=book1_filtered, directed=FALSE)

# Визначення ступеня вершини
vertex_degree <- degree(network)

# Вибір 50 найважливіших персонажів за кількістю зв'язків
top_vertices <- order(vertex_degree, decreasing=TRUE)[1:50]
sub_network <- induced_subgraph(network, vids=top_vertices)

# Визначення кольорів для родин
family_colors <- c(
  "Snow" = "#7F7F7F",
  "Targaryen" = "darkviolet",
  "Lannister" = "red",
  "Stark" = "blue",
  "Baratheon" = "darkgreen",
  "Baelish" = "gray"
)

# Виділення родин і призначення кольорів вершинам
V(sub_network)$family <- sapply(V(sub_network)$name, function(x) {
  last_name <- sub(".*-(.*)", "\\1", x)
  if (last_name %in% names(family_colors)) {
    return(family_colors[last_name])
  } else {
    # Для персонажів, які не належать до жодної з родин, колір тимчасово NA
    return(NA)
  }
})

# Призначення кольору для "не сімейних" вершин
for (v in V(sub_network)) {
  if (is.na(V(sub_network)$family[v])) {
    # Визначення кольору сім'ї вершини, з якою найбільше зв'язків
    neighbor_colors <- V(sub_network)$family[neighbors(sub_network, v)]
    dominant_color <- names(sort(table(neighbor_colors), decreasing=TRUE))[1]
    V(sub_network)$family[v] <- dominant_color
  }
}

# Налаштування розміру вершини з мінімальним і максимальним розмірами
min_size <- 5
max_size <- 12
V(sub_network)$size <- rescale(degree(sub_network), to = c(min_size, max_size))

# Налаштування розміру тексту
V(sub_network)$label.cex <- rescale(degree(sub_network), to = c(0.5, 1.0))

# Призначення кольорів ребрам
E(sub_network)$color <- ifelse(
  V(sub_network)[ends(sub_network, E(sub_network))[, 1]]$family == 
    V(sub_network)[ends(sub_network, E(sub_network))[, 2]]$family,
  V(sub_network)[ends(sub_network, E(sub_network))[, 1]]$family, 
  "gray80"
)

# Призначення товщини ребра на основі ваги
E(sub_network)$width <- rescale(E(sub_network)$weight, to = c(1, 5))

# Використання алгоритму Fruchterman-Reingold
layout_fr <- layout_with_fr(sub_network, niter = 2000, repulserad = vcount(sub_network)^3, area = vcount(sub_network)^4, coolexp = 0.5)
layout_kk <- layout_with_kk(sub_network)

# Виведення покращеного графа
plot(
  sub_network, 
  vertex.color=V(sub_network)$family, 
  vertex.label.color="black", 
  vertex.label.cex=V(sub_network)$label.cex, 
  vertex.label.family="Helvetica",
  vertex.label.font=2,
  vertex.frame.color="black", 
  edge.color=E(sub_network)$color, 
  edge.width=E(sub_network)$width,
  layout=layout_fr, 
  main="Граф для 50 найважливіших персонажів Першої книги"
)

legend(
  "bottomleft", legend=names(family_colors), col=family_colors, pch=21, pt.bg=family_colors,
  pt.cex=2, cex=1, bty="n"
)