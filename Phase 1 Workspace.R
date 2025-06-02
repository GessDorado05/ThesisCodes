install.packages("pscl")
install.packages("dplyr")
install.packages("carData")
install.packages("MASS")

rm(list = ls())

# Load necessary libraries
library(carData)
library(car)
library(readxl)
library(pscl)
library(dplyr)
library(MASS)
library(ggplot2)

# Load data from an Excel file
data <- read_excel("C:/Users/DORADO/OneDrive/Desktop/Up Acads/Assignments/4th Year/First Sem/AMAT 200b/Data/sample.xlsx", sheet = 2)

# Display the first few rows of the data
head(data)
rab=data
View(rab)

# Initialize dependent variable
y=rab[["Positive Dog Rabies Cases"]]

# Initialize indepdendent variable
x1=rab[["Vaccinated Dogs"]]
x2=rab[["Dog Impounded"]]
x3=rab[["Castrated Dogs"]]
x4=rab[["IEC Participants"]]
x5=rab[["Mean Temperature"]]
x6=rab[["Rel Humidity (%)"]]
x7=rab[["Rainfall (mm)"]]
View(rab)

# Function to standardize x4
standardize <- function(x) {
  return ((x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE))
}
# Standardize Predictors
x1_vac <- standardize(x1)
x2_imp <- standardize(x2)
x3_cast <- standardize(x3)
x4_iec <- standardize(x4)
x5_temp <- standardize(x5)
x6_hum <- standardize(x6)
x7_rain <- standardize(x7)

# Convert x1 to binary based on a condition (e.g., x1 > 25)
x1_binary <- ifelse(x1 > 1, 1, 0)
summary(x1_binary)

# Alternative to standardization
x4_category <- cut(x4,
                     breaks = c(-Inf, 334, 666, Inf),  # Define the breaks for ranges
                     labels = c(1, 2, 3),  # Labels for the categories
                     right = TRUE)  # Include the right endpoint in intervals
# Print the data frame with the new category column
print(x4_category)

# Alternative to standardization and categorization
x4_binary <- ifelse(x4 > 1, 1, 0)
summary(x4_binary)

#Convert numeric to factor
x1_vac <- as.factor(x1_binary)
x4_iec <- as.factor(x4_category) #only applies for alternatives
x8_cov <- as.factor(x8)

#------------------------------------------------------------------------------------------------------------------

# Fit a linear regression model
model <- lm(y ~ x1_vac + x2_imp + x3_cast + x4_iec + x5_temp + x6_hum + x7_rain)

# Calculate VIF for each predictor
vif_values <- vif(model)

# Display VIF values
print(vif_values)

# Analyze VIF results
analyze_vif <- function(vif_values) {
  high_vif <- vif_values[vif_values > 10]
  
  if (length(high_vif) > 0) {
    cat("High multicollinearity detected in the following variables:\n")
    print(high_vif)
  } else {
    cat("No high multicollinearity detected.\n")
  }
  
  # Recommendations based on VIF values
  for (name in names(vif_values)) {
    vif_value <- vif_values[name]
    cat(name, ": VIF = ", vif_value, "\n")
    if (vif_value > 10) {
      cat("  - Consider removing, combining, or transforming this variable to address multicollinearity.\n")
    } else if (vif_value > 5) {
      cat("  - Be cautious, potential moderate multicollinearity.\n")
    } else {
      cat("  - Low multicollinearity.\n")
    }
  }
}

# Call the analyze_vif function
analyze_vif(vif_values)

#----------------------------------------------------------------------------------------------------------------------------------------------

# Fit a Poisson regression model
p_model <- glm(y ~ x1_vac + x2_imp + x3_cast + x4_iec + x5_temp + x6_hum + x7_rain, family = poisson(link = "log"), data = data)

# Display the model summary
summary(model)

# Calculate Variance Inflation Factors (VIF) for multicollinearity check
vif_values <- vif(model)
print(vif_values)

# Interpretation of VIF
if (any(vif_values > 5)) {
  print("Warning: Some predictors have high VIF values (> 5), indicating potential multicollinearity issues.")
} else {
  print("VIF values are below the typical threshold of 5, suggesting no severe multicollinearity.")
}

# Overdispersion check: Compute the ratio of the residual deviance to the degrees of freedom
overdispersion <- sum(residuals(model, type = "pearson")^2) / model$df.residual
print(paste("Overdispersion statistic (Pearson Chi-squared / df):", round(overdispersion, 2)))

# Interpretation of Overdispersion
if (overdispersion > 1.5) {
  print("Warning: There is evidence of overdispersion (statistic > 1.5), indicating that a Poisson model may not be appropriate. Consider using a Negative Binomial model.")
} else {
  print("No strong evidence of overdispersion. The Poisson model appears to be appropriate.")
}

# Optional: To get exponentiated coefficients (rate ratios):
exp(coef(model))

#----------------------------------------------------------------------------------------------------------------------------------------------

# Fit a Zero-Inflated Poisson model
zip_model <- zeroinfl(y ~ x1_vac + x2_imp + x3_cast + x4_iec + x5_temp + x6_hum + x7_rain | x1_vac + x2_imp + x3_cast + x4_iec + x5_temp + x6_hum + x7_rain, dist = "poisson")

# Summarize the model for ZIP
summary(zip_model)

# Check VIF
zip_vif <- vif(zip_model)
print(zip_vif)

# Extract residuals and deviance
zipresiduals <- residuals(zip_model, type = "pearson")
zipdeviance <- sum(zipresiduals^2)

# Calculate degrees of freedom
zipdf <- nrow(rab) - length(coef(zip_model))

# Calculate overdispersion statistic
overdispersion_stat <- zipdeviance / zipdf

# Print the results
cat("Residual Deviance: ", zipdeviance, "\n")
cat("Degrees of Freedom: ", zipdf, "\n")
cat("Overdispersion Statistic: ", overdispersion_stat, "\n")

# Check for overdispersion
if (overdispersion_stat > 1) {
  print("Overdispersion detected in the zero-inflated Poisson model.")
} else {
  print("No significant overdispersion detected for ZIP.")
}

#---------------------------------------------------------------------------------------------------------------------------------------------------

# Perform Vuong's test comparing Poisson and ZIP models
vuong_test <- vuong(p_model, zip_model)

#---------------------------------------------------------------------------------------------------------------------------------------------------

# Fit a Negative Binomial regression model
nb_model <- glm.nb(y ~ x1_vac + x2_imp + x3_cast + x4_iec + x5_temp + x6_hum + x7_rain, data = data)
summary(nb_model)

# Calculate Variance Inflation Factors (VIF) for multicollinearity check
vif_values <- vif(nb_model)
print(vif_values)

# Interpretation of VIF
if (any(vif_values > 5)) {
  print("Warning: Some predictors have high VIF values (> 5), indicating potential multicollinearity issues.")
} else {
  print("VIF values are below the typical threshold of 5, suggesting no severe multicollinearity.")
}

# Overdispersion check: Compute the ratio of the residual deviance to the degrees of freedom
overdispersion <- sum(residuals(nb_model, type = "pearson")^2) / nb_model$df.residual
print(paste("Overdispersion statistic (Pearson Chi-squared / df):", round(overdispersion, 2)))

# Interpretation of Overdispersion
if (overdispersion > 1.5) {
  print("Warning: There may still be some overdispersion (statistic > 1.5), but the Negative Binomial model is better suited for overdispersed data.")
} else {
  print("No strong evidence of overdispersion. The Negative Binomial model appears to be appropriate.")
}

# Optional: To get exponentiated coefficients (rate ratios)
exp_coef <- exp(coef(nb_model))
print("Exponentiated coefficients (rate ratios):")
print(exp_coef)

#---------------------------------------------------------------------------------------------------------------------------------------------------

# Fit a Zero-Inflated Negative Binomial model (all predictors included)
zinb_model <- zeroinfl(y ~ x1_vac + x2_imp + x3_cast + x4_iec + x5_temp + x6_hum + x7_rain | x1_vac + x2_imp + x3_cast + x4_iec + x5_temp + x6_hum + x7_rain, dist = "negbin")

# Summarize the model for ZINB
summary(zinb_model)

# Check Multicolinearity
zinb_vif <- vif(zinb_model)
print(zinb_vif)


# Fit a Zero-Inflated Negative Binomial model (interventions only)
zinb_model1 <- zeroinfl(y ~ x1_vac + x2_imp + x3_cast + x4_iec|x1_vac + x2_imp + x3_cast + x4_iec, dist = "negbin")

# Summarize the model for ZINB
summary(zinb_model1)

zinb_vif1 <- vif(zinb_model1)
print(zinb_vif1)

# Fit a Zero-Inflated Negative Binomial model (weather only)
zinb_model2 <- zeroinfl(y ~ x5_temp + x6_hum + x7_rain|x5_temp + x6_hum + x7_rain, data = rab, dist = "negbin")

# Summarize the model for ZINB
summary(zinb_model2)

zinb_vif2 <- vif(zinb_model)
print(zinb_vif2)

#---------------------------------------------------------------------------------------------------------------------------------------------------

# Perform Vuong's test comparing Negative Binomial and ZINB models
vuong_test <- vuong(nb_model, zinb_model)

# Futher Comparison
# Goodness of Fit Comparisons
# AIC
cat("AIC of Negative Binomial Model: ", AIC(nb_model), "\n")
cat("AIC of Zero-Inflated Negative Binomial Model: ", AIC(zinb_model), "\n")

# BIC
cat("BIC of Negative Binomial Model: ", BIC(nb_model), "\n")
cat("BIC of Zero-Inflated Negative Binomial Model: ", BIC(zinb_model), "\n")

# Residual Analysis: Plotting Residuals for Both Models
data$nb_residuals <- residuals(nb_model, type = "pearson")
data$zinb_residuals <- residuals(zinb_model, type = "pearson")

# Plotting residuals for Negative Binomial model
ggplot(data, aes(x = nb_residuals)) +
  geom_histogram(bins = 30, fill = "lightblue", color = "black") +
  labs(title = "Residuals of Negative Binomial Model", x = "Pearson Residuals", y = "Frequency") +
  theme_minimal()

# Plotting residuals for Zero-Inflated Negative Binomial model
ggplot(data, aes(x = zinb_residuals)) +
  geom_histogram(bins = 30, fill = "lightgreen", color = "black") +
  labs(title = "Residuals of Zero-Inflated Negative Binomial Model", x = "Pearson Residuals", y = "Frequency") +
  theme_minimal()

#---------------------------------------------------------------------------------------------------------------------------------------------------

# Evaluation Metrics

# Compute log-likelihood
log_likelihood <- logLik(zinb_model)
log_likelihood1 <- logLik(zinb_model1)
log_likelihood2 <- logLik(zinb_model2)

# Print LL
print(log_likelihood)
print(log_likelihood1)
print(log_likelihood2)
print(log_likelihood3)

# Compute AIC for the fitted models
aic_zinb <- AIC(zinb_model)
aic_zinb1 <- AIC(zinb_model1)
aic_zinb2 <- AIC (zinb_model2)

# Print the AIC value
print(aic_zinb)
print(aic_zinb1)
print(aic_zinb2)

# Compute BIC for the fitted models
bic_zinb <- BIC(zinb_model)
bic_zinb1 <- BIC(zinb_model1)
bic_zinb2 <- BIC (zinb_model2)

# Print the AIC value
print(bic_zinb)
print(bic_zinb1)
print(bic_zinb2)

# Extract log-likelihood and degrees of freedom
zinbloglik <- logLik(zinb_model2)
zinbdf <- attr(zinbloglik, "df")

# Extract theta (dispersion parameter)
zinbtheta <- summary(zinb_model)$theta
print(zinbtheta)

# Assess overdispersion
if (zinbtheta > zinbdf) {
  print("Overdispersion detected in the zero-inflated negative binomial model.")
} else {
  print("No significant overdispersion detected for ZINB.")
}

#---------------------------------------------------------------------------------------------------------------------------------------------------

# Predicted values
predictions <- predict(zinb_model, type = "response")

# Display the first few predicted values
head(predictions)
View(predictions)

# Compare rab and prediction
predicted_data <- data.frame(
  time = data$Month,  # Assuming time is the same as in the original data
  predicted = predictions 
)

# Combine original and predicted data into one data frame
comparison_data <- data.frame(
  time = data$Month,  # Time variable from the Excel sheet
  original = data$`Positive Dog Rabies Cases`,  # Original data from Excel
  predicted = predicted_data$predicted)  # Predicted data
  
# Create a line plot to compare original and predicted data
ggplot(comparison_data, aes(x = time)) +
  geom_line(aes(y = original, color = "Original Data"), size = 0.8) +
  geom_line(aes(y = predicted, color = "Predicted Data"), size = 0.8) +
  labs(title = "Comparison of Original and Predicted Data",
       x = "Month",
       y = "Value") +
  scale_color_manual(values = c("Original Data" = "blue", "Predicted Data" = "red")) +
  theme_minimal()

