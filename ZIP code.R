install.packages("pscl")
 
# Load necessary libraries
library(pscl)

# Load data from an Excel file
data <- read_excel("C:/Users/DORADO/OneDrive/Desktop/Up Acads/Assignments/4th Year/First Sem/AMAT 200b/Data/Cleaned Data.xlsx")

# Display the first few rows of the data
head(data)
view(rab)
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

# Convert x1 to binary based on a condition (e.g., x1 > 25)
x1_binary <- ifelse(x1 > 1, 1, 0)
summary(x1_binary)

# Fit a Zero-Inflated Poisson model
zip_model <- zeroinfl(y ~ x1_binary + x2 + x3 + x4 + x5 + x6 + x7 | x1_binary + x2 + x3 + x4 + x5 + x6 + x7, data = rab, dist = "poisson")


# Summarize the model
summary(zip_model)

# Predicted values
predictions <- predict(zip_model, type = "response")

# Display the first few predicted values
head(predictions)
View(predictions)
