# Load necessary libraries
if (!require("readxl")) {
  install.packages("readxl")
}
if (!require("ggplot2")) {
  install.packages("ggplot2")
}

library(readxl)
library(ggplot2)

# Read the data from the first sheet of the Excel file
data <- read_excel("C:/Users/DORADO/OneDrive/Desktop/Up Acads/Assignments/4th Year/First Sem/AMAT 200b/Data/sample.xlsx", sheet = 1)
head(data)

# Define a custom color palette for 16 distinct colors
custom_colors <- c(
  "2006" = "#1f77b4", "2007" = "#ff7f0e", "2008" = "#2ca02c", "2009" = "#d62728",
  "2010" = "#9467bd", "2011" = "#8c564b", "2012" = "#e377c2", "2013" = "#7f7f7f",
  "2014" = "#bcbd22", "2015" = "#17becf", "2016" = "#e41a1c", "2017" = "#377eb8",
  "2018" = "#4daf4a", "2019" = "#984ea3", "2020" = "#ff7f00", "2021" = "#a65628"
)

# Create the boxplot directly referencing the columns
ggplot(data, aes(x = factor(`Year`), y = `IEC Participants`, fill = factor(`Year`))) +
  geom_boxplot(color = "black", alpha = 0.7) +
  scale_fill_manual(values = custom_colors) +
  labs(title = "IEC Participants by Year (2006-2017)", 
       x = "Year", 
       y = "IEC Participants") +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",  # No legend
    axis.text.x = element_text(angle = 45, hjust = 1),  # Tilt x-axis labels
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),  # Center title
    axis.title.x = element_text(size = 12, face = "bold"),  # Bold x-axis title
    axis.title.y = element_text(size = 12, face = "bold")   # Bold y-axis title
  )
