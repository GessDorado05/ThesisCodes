# Load necessary libraries
library(MASS)

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

# Fit a Poisson regression model
poisson_model <- glm(y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7, data = data, family = poisson())

# Calculate residual deviance and degrees of freedom
residual_deviance <- poisson_model$deviance
df_residual <- poisson_model$df.residual

# Calculate ratio
ratio <- residual_deviance / df_residual
print(ratio)

# Check for overdispersion
if (ratio > 1.5) {
  print("There is evidence of overdispersion.")
} else {
  print("No evidence of overdispersion.")
}

#--------------------------------------------------------------------------

# Fit a Negative Binomial regression model
negbin_model <- glm.nb(y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7, data = data)

# Compare models using AIC
aic_poisson <- AIC(poisson_model)
aic_negbin <- AIC(negbin_model)

print(c("AIC of Poisson model:", aic_poisson))
print(c("AIC of Negative Binomial model:", aic_negbin))

if (aic_negbin < aic_poisson) {
  print("Negative Binomial model fits better, indicating overdispersion.")
} else {
  print("No evidence of overdispersion.")
}
