# Step 1: Install and load the necessary package
# Install 'readxl' if you haven't already
install.packages("readxl")

# Load the 'readxl' package
library(readxl)

# Step 2: Load the dataset from an Excel file
# Use the read_excel function to load your dataset
# Replace "your_file_path.xlsx" with the actual path to your Excel file
data <- read_excel("C:/Users/DORADO/OneDrive/Desktop/Up Acads/Assignments/4th Year/First Sem/AMAT 200b/Data/sample.xlsx", sheet = 6)

# Print the first few rows of the data to ensure it's loaded correctly
head(data)

# Step 3: Perform One-Way ANOVA
# Assuming your dataset has a 'Group' column (independent variable) 
# and a 'Value' column (dependent variable)

anova_result <- aov(`Positive Dog Rabies Cases` ~ `Period`, data = data)

# Step 4: Detailed ANOVA Table
anova_table <- anova(anova_result)
print(anova_table)

# Step 5: Perform Tukey's HSD post hoc test (if ANOVA is significant)
# Tukey's Honest Significant Difference (HSD) post hoc test
tukey_result <- TukeyHSD(anova_result)

# Print Tukey's HSD results
print(tukey_result)

# Step 6: Visualize Tukey's HSD result (optional)
plot(tukey_result)

