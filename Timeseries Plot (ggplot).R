# Install required packages (if not already installed)
# install.packages("readxl")
# install.packages("ggplot2")

# Load required libraries
library(readxl)
library(ggplot2)

# Step 1: Read data from the Excel file
# Replace 'your_file.xlsx' and 'Sheet1' with the actual file and sheet names
original_data <- read_excel("your_file.xlsx", sheet = "Sheet1")

# Assuming 'time' is in the first column and 'original' values in the second column
# Adjust according to your actual column names or positions
head(original_data)  # View the first few rows to verify

# Step 2: Create the predicted data
# Replace this with your actual predicted values and time values
predicted_data <- data.frame(
  time = original_data$time,  # Assuming time is the same as in the original data
  predicted = c(2.1, 4.1, 5.9, 7.8, 10.2, 12.3, 13.8)  # Example predicted data
)

# Step 3: Combine original and predicted data into one data frame
# Assuming 'original_data' has columns 'time' and 'original'
comparison_data <- data.frame(
  time = original_data$time,  # Time variable from the Excel sheet
  original = original_data$original,  # Original data from Excel
  predicted = predicted_data$predicted  # Predicted data
)

# Step 4: Create a line plot to compare original and predicted data
ggplot(comparison_data, aes(x = time)) +
  geom_line(aes(y = original, color = "Original Data"), size = 1) +
  geom_line(aes(y = predicted, color = "Predicted Data"), linetype = "dashed", size = 1) +
  labs(title = "Comparison of Original and Predicted Data",
       x = "Time",
       y = "Value") +
  scale_color_manual(values = c("Original Data" = "blue", "Predicted Data" = "red")) +
  theme_minimal()
