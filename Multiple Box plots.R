# Load necessary libraries
library(ggplot2)
library(readxl)
library(dplyr)

# Read the data from an Excel file
data <- read_excel("C:/Users/DORADO/OneDrive/Desktop/Up Acads/Assignments/4th Year/First Sem/AMAT 200b/Data/sample.xlsx", sheet = 6)  # Replace with your file path

# Ensure the 'Period' is a factor with the correct order
data$Period <- factor(data$Period, 
                      levels = c("No Intensification", "Pre-Covid Intensification", "Post-Covid Intensification +"))

# Check the number of rows
print(nrow(data))  # Should print the total number of rows in the dataset

# Calculate mean and variance for each period
summary_stats <- data %>%
  group_by(Period) %>%
  summarise(
    mean_value = mean(`Positive Dog Rabies Cases`, na.rm = TRUE),
    variance_value = var(`Positive Dog Rabies Cases`, na.rm = TRUE)
  )

# Define a modern color palette
custom_colors <- c("No Intensification" = "#3498db",  # Light blue
                   "Pre-Covid Intensification" = "#e74c3c",  # Red
                   "Post-Covid Intensification +" = "#2ecc71")  # Green

# Create the box plot
box_plot <- ggplot(data, aes(x = Period, y = `Rainfall (mm)`, fill = Period)) +
  geom_boxplot() +
  scale_fill_manual(values = custom_colors) +
  labs(title = "Box Plot of Rainfall per Period",
       x = "Period",
       y = "Rainfall (mm)") +
  theme_minimal(base_size = 14) +  # Set base size
  theme(
    axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5, size = 10, face = "bold", color = "#2c3e50"),  
    axis.text.y = element_text(size = 10, face = "bold", color = "#2c3e50"),
    axis.title.x = element_text(margin = margin(t = 10), size = 12, face = "bold"),
    axis.title.y = element_text(margin = margin(r = 10), size = 12, face = "bold"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5, color = "#34495e"),
    panel.grid.major = element_line(color = "#bdc3c7", size = 0.5),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "#ecf0f1", color = NA),
    legend.position = "top",
    legend.background = element_rect(fill = "#fdfefe"),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10)
  ) +

  # Add annotations for both mean and variance
  geom_text(data = summary_stats, aes(x = Period, y = mean_value + 1, 
                                      label = paste("Mean:", round(mean_value, 2), "\nVariance:", round(variance_value, 2))),
            size = 4, color = "black", position = position_nudge(y = 250), vjust = 0) 

# Display the box plot
print(box_plot)

