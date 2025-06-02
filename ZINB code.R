#install.packages("pscl")
#install.packages("dplyr")

rm(list = ls())

# Load necessary libraries
library(pscl)
library(dplyr)

# Load data from an Excel file
data <- read_excel("C:/Users/DORADO/OneDrive/Desktop/Up Acads/Assignments/4th Year/First Sem/AMAT 200b/Data/sample.xlsx")

# Display the first few rows of the data
head(data)
View(rab)
rab=data

# Initialize dependent variable
y=rab[["Positive Dog Rabies Cases"]]

# Initialize indepdendent variable
x1=rab[["Vaccinated Dogs"]]
x2=rab[["Dog Impounded"]]
x3=rab[["Castrated Dogs"]]
x4=rab[["IEC Participants"]]
x5=rab[["Mean Temp (Celsius)"]]
x6=rab[["Rel Humidity (%)"]]
x7=rab[["Rainfall (mm)"]]
x8=rab[["COVID"]]
View(rab)

# Convert x1 to binary based on a condition (e.g., x1 > 25)
x1_binary <- ifelse(x1 > 1, 1, 0)
summary(x1_binary)

x1_cat <- as.factor(x1_binary)
x8_cat <- as.factor(x8)

# Check the class of x1_cat and x8_cat
print(class(x1_cat))
print(class(x8_cat))

# Check for missing values
print(sum(is.na(x1_cat)))
print(sum(is.na(x8_cat)))

View(data)

# Display the updated dataset
print(data)

# Fit a Zero-Inflated Negative Binomial model
zinb_model <- zeroinfl(y ~ x1_cat + x2 + x3 + x4 + x5 + x6 + x7 + x8_cat|x1_cat + x2 + x3 + x4 + x5 + x6 + x7 + x8_cat, data = rab, dist = "negbin")

# Summarize the model
summary(zinb_model)

vif_values <- vif(zinb_model)
print(vif_values)

# Predicted values
predictions <- predict(zinb_model, type = "response")

# Display the first few predicted values
head(predictions)
View(predictions)
