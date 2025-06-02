install.packages("carData")
install.packages("car")

# Load necessary libraries
library(car)
library(carData)
library(readxl)

# Load data from an Excel file
data <- read_excel("C:/Users/DORADO/OneDrive/Desktop/Up Acads/Assignments/4th Year/First Sem/AMAT 200b/Data/Cleaned Data.xlsx")

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

# Fit a linear regression model
model <- lm(y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7)

# Calculate VIF for each predictor
vif_values <- vif(model)

# Display VIF values
print(vif_values)

#------------------------------------------------------------------------

# Remove x2 and fit the model again
model_reduced <- lm(y ~ x1 + x3, data = data)

# Calculate VIF for the reduced model
vif_values_reduced <- vif(model_reduced)

# Display VIF values for the reduced model
print(vif_values_reduced)
