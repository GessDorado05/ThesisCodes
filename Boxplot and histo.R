library(dplyr)
library(ggplot2)
library(readr)
library(readxl)
library(scales)
library(patchwork)

# Read and preprocess data
data <- read_excel("D:/Up Acads/Assignments/4th Year/First Sem/AMAT 200b/Data/sample.xlsx", sheet = 1)

data <- data %>%
  mutate(Year = as.numeric(format(as.Date(Month, origin = "1899-12-30"), "%Y"))) %>%
  filter(!is.na(Year))

data$`IEC Participants` <- as.numeric(gsub(",", "", data$`IEC Participants`))

# Summarize for bar plot
annual <- data %>%
  group_by(Year) %>%
  summarise(Total = sum(`IEC Participants`, na.rm = TRUE)) %>%
  filter(Year >= 2006 & Year <= 2022)

# Common theme settings — no grid, visible axis lines
common_theme <- theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.5)
  )

# Bar Plot
bar_plot <- ggplot(annual, aes(x = factor(Year), y = Total)) +
  geom_bar(stat = "identity", fill = "#2c7fb8", width = 0.7) +
  geom_text(aes(label = comma(Total)), vjust = -0.3, size = 3.5) +
  scale_y_continuous(labels = comma) +
  labs(
    x = "Year",
    y = "IEC Participants"
  ) +
  common_theme

# Boxplot
filtered_data <- data %>% filter(Year >= 2006 & Year <= 2022)

box_plot <- ggplot(filtered_data, aes(x = factor(Year), y = `IEC Participants`)) +
  geom_boxplot(fill = "#2c7fb8", color = "black", outlier.color = "red", outlier.shape = 16) +
  scale_y_continuous(labels = comma) +
  labs(
    x = "Year",
    y = "IEC Participants"
  ) +
  common_theme

# Combine plots vertically with tags
(bar_plot / box_plot) +
  plot_layout(heights = c(1.2, 1)) +
  plot_annotation(tag_levels = 'a', tag_prefix = "", tag_suffix = ".)")


