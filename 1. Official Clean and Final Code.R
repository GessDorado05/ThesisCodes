install.packages("pscl")
install.packages("dplyr")
install.packages("carData")
install.packages("MASS")
install.packages("glmnet")
install.packages("readxl")
install.packages("car")
install.packages("ggplot2")
install.packages("AER")
install.packages("patchwork")
install.packages("Metrics")
install.packages(c("pROC", "DescTools", "ResourceSelection", "MLmetrics", "caret"))

rm(list = ls()) # To clear environment in R

# Load necessary libraries
library(carData)
library(car)
library(readxl)
library(pscl)
library(dplyr)
library(MASS)
library(ggplot2)
library(glmnet)
library(AER)
library(patchwork)
library(Metrics)

# Initialization ---------------------------------------------------------------------------------------------------------------------

# Load data from an Excel file
data <- read_excel("D:/Up Acads/Assignments/4th Year/First Sem/AMAT 200b/Data/sample.xlsx", sheet = 1)
rab <- data

# Display the first few rows of the data
head(rab)

# Check for Zero Variance Predictors
apply(rab[, c("Positive Dog Rabies Cases", "Vaccinated Dogs", "Dog Impounded", "Castrated Dogs", "IEC Participants", "Mean Temperature", "Rel Humidity (%)", "Rainfall (mm)")], 2, var)

# Check for missing values and duplicates in the dataset
colSums(is.na(rab))
sum(duplicated(rab))

# Initialize dependent variable
y=rab[["Positive Dog Rabies Cases"]]

# Initialize indepdendent variable
x1=rab[["Vaccinated Dogs"]]
x2=rab[["IEC Participants"]]
x3=rab[["Castrated Dogs"]]
x4=rab[["Dog Impounded"]]
x5=rab[["Mean Temperature"]]
x6=rab[["Rel Humidity (%)"]]
x7=rab[["Rainfall (mm)"]]

# Create Data Frame for the dataset
data.rab <- data.frame(y,x1, x2, x3, x4, x5, x6, x7)
data.rab

# Check the length of all variables
rab_len <- sapply(list(x1, x2, x3, x4, x5, x6, x7), length)
rab_len

# Compute Mean, Variance, and SD of variables
rab_mean <- apply(data.rab, 2, mean)
rab_mean
rab_var <- apply(data.rab, 2, var)
rab_var
rab_sd <- apply(data.rab, 2, sd)
rab_sd

# Function to standardize predictors x_i
standardize <- function(x) {
  return ((x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE))
}
# Standardize Predictors
x1_s <- standardize(x1)
x2_s <- standardize(x2)
x3_s <- standardize(x3)
x4_s <- standardize(x4)
x5_s <- standardize(x5)
x6_s <- standardize(x6)
x7_s <- standardize(x7)

# Function to normalize predictors x_i
normalize <- function(x) {
  return ((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}

x1_n <- normalize (x1)
x2_n <- normalize (x2)
x3_n <- normalize (x3)
x4_n <- normalize (x4)
x5_n <- normalize (x5)
x6_n <- normalize (x6)
x7_n <- normalize (x7)

# Create data frame for standardized and normalized predictors
rab.s <- data.frame(x1_s, x2_s, x3_s, x4_s, x5_s, x6_s, x7_s)
rab.s
rab.n <- data.frame(x1_n, x2_n, x3_n, x4_n, x5_n, x6_n, x7_n)
rab.n

# Compute Mean, Variance, and SD of standardized and normalized predictors
rab.s_mean <- apply(rab.s, 2, mean)
rab.s_mean
rab.s_var <- apply(rab.s, 2, var)
rab.s_var
rab.s_sd <- apply(rab.s, 2, sd)
rab.s_sd

rab.n_mean <- apply(rab.n, 2, mean)
rab.n_mean
rab.n_var <- apply(rab.n, 2, var)
rab.n_var
rab.n_sd <- apply(rab.n, 2, sd)
rab.n_sd

# Binarize x1 and x4 (x_i > 0)
x1_bin <- ifelse(x1 > 0, 1, 0)
summary(x1_bin)
x2_bin <- ifelse(x2 > 0, 1, 0)
summary(x4_bin)

# Convert x1 and x4 from numeric to factor
x1_b <- as.factor(x1_bin)
x2_b <- as.factor(x2_bin)

# Check length and class of all data types
sapply(list(x1_b, x2_b, 
            x1_s, x2_s, x3_s, x4_s, x5_s, x6_s, x7_s, 
            x1_n, x2_n, x3_n, x4_n, x5_n, x6_n, x7_n), length) # Should be 195
sapply(list(x1_b, x2_b), class)                                # Should be factor
sapply(list(x1_s, x2_s, x3_s, x4_s, x5_s, x6_s, x7_s), class)  # Should be numeric
sapply(list(x1_n, x2_n, x3_n, x4_n, x5_n, x6_n, x7_n), class)  # Should be numeric

# Standard Poisson Model -------------------------------------------------------------------------------------------------------

# GLMs considered for this model (Copy and Paste GLM to test each model)
# Preliminary Models
# 1. SP(O) - y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 (All original Scale)
# 2. SP(S) - y ~ x1_s + x2_s + x3_s + x4_s + x5_s + x6_s + x7_s (All standardized Scale)
# 3. SP(N) - y ~ x1_n + x2_n + x3_n + x4_n + x5_n + x6_n + x7_n (All normalized Scale)
# Final Models
# 4. SP(sb1) - y ~ x1_b + x2_s + x3_s + x4_s + x5_s + x6_s + x7_s (Only x1 is binarized and the rest are standardized)
# 5. SP(sb2) - y ~ x1_s + x2_b + x3_s + x4_s + x5_s + x6_s + x7_s (Only x2 is binarized and the rest are standardized)

# Fit a Poisson regression model
sp_model <- glm(y ~ x1_s + x2_b + x3_s + x4_s + x5_s + x6_s + x7_s, family = poisson(link = "log"))
# Display the model summary
summary(sp_model)
vif(sp_model)

# Calculate Pearson Chi-squared statistic
pearson_chi_sp <- sum(residuals(sp_model, type = "pearson")^2)

# Degrees of freedom
n_sp <- nrow(data)  # Number of observations
k_sp <- length(coef(sp_model))  # Number of model parameters
df_sp <- n_sp - k_sp

# Dispersion statistic
dispersion_statistic_sp <- pearson_chi_sp / df_sp

# Print results
cat("Pearson Chi-squared statistic:", round(pearson_chi_sp, 4), "\n")
cat("Degrees of Freedom:", df_sp, "\n")
cat("Dispersion Statistic:", round(dispersion_statistic_sp, 4), "\n")

# Interpretation
if (dispersion_statistic_sp > 1.2) {  # Arbitrary threshold for overdispersion
  cat("Evidence of overdispersion in Standard Poisson.\n")
} else {
  cat("No strong evidence of overdispersion in Standard Poisson.\n")
}

# Extract predicted values from model
pred_sp <- predict(sp_model, type = "response")
pred_sp

# Display the first few predicted values
head(pred_sp)

# Round predictions to nearest ones
Rpred_sp <- round(pred_sp, 0)
Rpred_sp

#Check zeros captured by the prediction
sum(Rpred_sp == 0)

# Convert Months to date format
data$Month <- as.Date(data$Month)

# Combine Original and Predicted data into one data frame
comp.data_sp <- data.frame(
  time = data$Month,  # Time variable from the Excel sheet
  original = data$`Positive Dog Rabies Cases`,  # Original data from Excel
  predicted = pred_sp)  # Predicted data

# Create a line graph comparing Original and Predicted data
sp_line <- ggplot(comp.data_sp, aes(x = time)) +
  geom_line(aes(y = original, color = "Original Data"), size = 1) +
  geom_line(aes(y = predicted, color = "Predicted Data"), size = 1) +
  labs(
       x = "Year",
       y = "Number of Canine Rabies Cases",
       color = "Legend") +
  scale_color_manual(values = c("Original Data" = "#1f77b4", "Predicted Data" = "#d62728")) +
  
  # X-axis: Show only years
  scale_x_date(
    date_labels = "%Y",       # Only show year
    date_breaks = "1 year",   # Breaks at each year
    expand = c(0.01, 0.01)
  ) +
  
  # Y-axis: Breaks at every 1 unit
  scale_y_continuous(breaks = seq(0, max(comp.data_sp$original, comp.data_sp$predicted), by = 1)) +
  
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),  # Remove all grid lines
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    axis.text = element_text(color = "black", size = 15),
    axis.title = element_text(color = "black", size = 15),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "top",  # Position legend at the top
    legend.justification = c(1, 1),  # Align the legend to the upper-right corner
    legend.box.margin = margin(0, 150, 0, 0),  # Move the legend outside the plot area to the right
    legend.text = element_text(size = 15),     # Increased legend text size
    legend.title = element_text(size = 15),    # Increase legend title
    axis.text.x = element_text(angle = 0, hjust = 0.5)
  )

# Show generated graph
sp_line

# Compute AIC and BIC for SP
AIC(sp_model)
BIC(sp_model)

# Compute Residuals using MAE, MSE, and RMSE for SP
# MSE
MSEsp <- mean((y - pred_sp)^2)
print(paste("MSEsp:", round(MSEsp, 4)))

# RMSE
RMSEsp <- sqrt(MSEsp)
print(paste("RMSEsp:", round(RMSEsp, 4)))

# MAE
MAEsp <- mean(abs(y - pred_sp))
print(paste("MAEsp:", round(MAEsp, 4)))

# Standard Negative Binomial Model -------------------------------------------------------------------------------------------------------

# GLMs considered for this model (Copy and Paste GLM to test each model)
# Preliminary Models
# 1. SNB(O) - y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 (All original Scale)
# 2. SNB(S) - y ~ x1_s + x2_s + x3_s + x4_s + x5_s + x6_s + x7_s (All standardized Scale)
# 3. SNB(N) - y ~ x1_n + x2_n + x3_n + x4_n + x5_n + x6_n + x7_n (All normalized Scale)
# Final Models
# 4. SNB(sb1) - y ~ x1_b + x2_s + x3_s + x4_s + x5_s + x6_s + x7_s (Only x1 is binarized and the rest are standardized)
# 5. SNB(sb2) - y ~ x1_s + x2_b + x3_s + x4_s + x5_s + x6_s + x7_s (Only x2 is binarized and the rest are standardized)

# Fit a Negative Binomial regression model
snb_model <- glm.nb(y ~ x1_s + x2_b + x3_s + x4_s + x5_s + x6_s + x7_s)
summary(snb_model)
vif(snb_model)

# Calculate Pearson Chi-squared statistic
pearson_chi_snb <- sum(residuals(snb_model, type = "pearson")^2)

# Degrees of freedom
n_snb <- nrow(data)  # Number of observations
k_snb <- length(coef(snb_model))  # Number of model parameters
df_snb <- n_snb - k_snb

# Dispersion statistic
dispersion_statistic_snb <- pearson_chi_snb / df_snb

# Print results
cat("Pearson Chi-squared statistic:", round(pearson_chi_snb, 4), "\n")
cat("Degrees of Freedom:", df_snb, "\n")
cat("Dispersion Statistic:", round(dispersion_statistic_snb, 4), "\n")

# Interpretation
if (dispersion_statistic_snb > 1.2) {  # Arbitrary threshold for overdispersion
  cat("Evidence of overdispersion in negative binomial.\n")
} else {
  cat("No strong evidence of overdispersion in negative binomial.\n")
}

# Extract predicted values from model
pred_snb <- predict(snb_model, type = "response")
pred_snb

# Display the first few predicted values
head(pred_snb)

# Round predictions to nearest ones
Rpred_snb <- round(pred_snb, 0)
Rpred_snb

#Check zeros captured by the prediction
sum(Rpred_snb == 0)

# Convert Months to date format
data$Month <- as.Date(data$Month)

# Combine Original and Predicted data into one data frame
comp.data_snb <- data.frame(
  time = data$Month,  # Time variable from the Excel sheet
  original = data$`Positive Dog Rabies Cases`,  # Original data from Excel
  predicted = pred_snb)  # Predicted data

# Create a line graph comparing Original and Predicted data
snb_line <- ggplot(comp.data_snb, aes(x = time)) +
  geom_line(aes(y = original, color = "Original Data"), size = 1) +
  geom_line(aes(y = predicted, color = "Predicted Data"), size = 1) +
  labs(
       x = "Year",
       y = "Number of Canine Rabies Cases",
       color = "Legend") +
  scale_color_manual(values = c("Original Data" = "#1f77b4", "Predicted Data" = "#d62728")) +
  
  # X-axis: Show only years
  scale_x_date(
    date_labels = "%Y",       # Only show year
    date_breaks = "1 year",   # Breaks at each year
    expand = c(0.01, 0.01)
  ) +
  
  # Y-axis: Breaks at every 1 unit
  scale_y_continuous(breaks = seq(0, max(comp.data_snb$original, comp.data_snb$predicted), by = 1)) +
  
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),  # Remove all grid lines
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    axis.text = element_text(color = "black", size = 15),
    axis.title = element_text(color = "black", size = 15),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "top",  # Position legend at the top
    legend.justification = c(1, 1),  # Align the legend to the upper-right corner
    legend.text = element_text(size = 15),     # Increased legend text size
    legend.title = element_text(size = 15),    # Increase legend title
    legend.box.margin = margin(0, 150, 0, 0),  # Move the legend outside the plot area to the right
    axis.text.x = element_text(angle = 0, hjust = 0.5)
  )

# Show generated graph
snb_line

# Compute AIC and BIC for SNB
AIC(snb_model)
BIC(snb_model)

# Compute Residuals using MAE, MSE, and RMSE for SNB
# MSE
MSEsnb <- mean((y - pred_snb)^2)
print(paste("MSEsnb:", round(MSEsnb, 4)))

# RMSE
RMSEsnb <- sqrt(MSEsnb)
print(paste("RMSEsnb:", round(RMSEsnb, 4)))

# MAE
MAEsnb <- mean(abs(y - pred_snb))
print(paste("MAEsnb:", round(MAEsnb, 4)))

# Zero-inflated Poisson Model -------------------------------------------------------------------------------------------------------

# GLMs considered for this model - Same for count and ZI (Copy and Paste GLM to test each model)
# Preliminary Models 
# 1. SNB(S) - y ~ x1_s + x2_s + x3_s + x4_s + x5_s + x6_s + x7_s (All standardized Scale)
# 2. SNB(N) - y ~ x1_n + x2_n + x3_n + x4_n + x5_n + x6_n + x7_n (All normalized Scale)
# Final Models
# 3. SNB(sb1) - y ~ x1_b + x2_s + x3_s + x4_s + x5_s + x6_s + x7_s (Only x1 is binarized and the rest are standardized)
# 4. SNB(sb2) - y ~ x1_s + x2_b + x3_s + x4_s + x5_s + x6_s + x7_s (Only x2 is binarized and the rest are standardized)

# Fit a Zero-Inflated Poisson model
zip_model <- zeroinfl(y ~ x1_s + x2_b + x3_s + x4_s + x5_s + x6_s + x7_s | x1_s + x2_b + x3_s + x4_s + x5_s + x6_s + x7_s, dist = "poisson")
summary(zip_model)
vif(zip_model)

# Calculate Pearson Chi-squared statistic
pearson_chi_zip <- sum(residuals(zip_model, type = "pearson")^2)

# Degrees of freedom
n_zip <- nrow(data)  # Number of observations
k_zip <- length(coef(zip_model))  # Number of model parameters
df_zip <- n_zip - k_zip

# Dispersion statistic
dispersion_statistic_zip <- pearson_chi_zip / df_zip

# Print results
cat("Pearson Chi-squared statistic:", round(pearson_chi_zip, 4), "\n")
cat("Degrees of Freedom:", df_zip, "\n")
cat("Dispersion Statistic:", round(dispersion_statistic_zip, 4), "\n")

# Interpretation
if (dispersion_statistic_zip > 1.2) {  # Arbitrary threshold for overdispersion
  cat("Evidence of overdispersion in zero-inflated Poisson.\n")
} else {
  cat("No strong evidence of overdispersion in zero-inflated Poisson.\n")
}

# Extract predicted values from model
pred_zip <- predict(zip_model, type = "response")
pred_zip

# Display the first few predicted values
head(pred_zip)

# Round predictions to nearest ones
Rpred_zip <- round(pred_zip, 0)
Rpred_zip

#Check zeros captured by the prediction
sum(Rpred_zip == 0)

# Convert Months to date format
data$Month <- as.Date(data$Month)

# Combine Original and Predicted data into one data frame
comp.data_zip <- data.frame(
  time = data$Month,  # Time variable from the Excel sheet
  original = data$`Positive Dog Rabies Cases`,  # Original data from Excel
  predicted = pred_zip)  # Predicted data

# Create a line graph comparing Original and Predicted data
zip_line <- ggplot(comp.data_zip, aes(x = time)) +
  geom_line(aes(y = original, color = "Original Data"), size = 1) +
  geom_line(aes(y = predicted, color = "Predicted Data"), size = 1) +
  labs(
       x = "Year",
       y = "Number of Canine Rabies Cases",
       color = "Legend") +
  scale_color_manual(values = c("Original Data" = "#1f77b4", "Predicted Data" = "#d62728")) +
  
  # X-axis: Show only years
  scale_x_date(
    date_labels = "%Y",       # Only show year
    date_breaks = "1 year",   # Breaks at each year
    expand = c(0.01, 0.01)
  ) +
  
  # Y-axis: Breaks at every 1 unit
  scale_y_continuous(breaks = seq(0, max(comp.data_zip$original, comp.data_zip$predicted), by = 1)) +
  
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),  # Remove all grid lines
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    axis.text = element_text(color = "black", size = 15),
    axis.title = element_text(color = "black", size = 15),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "top",  # Position legend at the top
    legend.justification = c(1, 1),  # Align the legend to the upper-right corner
    legend.text = element_text(size = 15),     # Increased legend text size
    legend.title = element_text(size = 15),    # Increase legend title
    legend.box.margin = margin(0, 150, 0, 0),  # Move the legend outside the plot area to the right
    axis.text.x = element_text(angle = 0, hjust = 0.5)
  )

# Show generated graph
zip_line

# Compute AIC and BIC for ZIP
AIC(zip_model)
BIC(zip_model)

# Compute Residuals using MAE, MSE, and RMSE for ZIP
# MSE
MSEzip <- mean((y - pred_zip)^2)
print(paste("MSEzip:", round(MSEzip, 4)))

# RMSE
RMSEzip <- sqrt(MSEzip)
print(paste("RMSEzip:", round(RMSEzip, 4)))

# MAE
MAEzip <- mean(abs(y - pred_zip))
print(paste("MAEzip:", round(MAEzip, 4)))


# Zero-inflated Negative Binomial Model -------------------------------------------------------------------------------------------------------

# GLMs considered for this model - Same for count and ZI (Copy and Paste GLM to test each model)
# Preliminary Models 
# 1. ZINB(sBoth) - y ~ x1_b + x2_b + x3_s + x4_s + x5_s + x6_s + x7_s (Both x1 and x2 are binarized and the rest are standardized)
# 2. ZINB(nb1) - y ~ x1_b + x2_n + x3_n + x4_n + x5_n + x6_n + x7_n (Only x1 is binarized and the rest are normalized)
# Final Models
# 3. ZINB(sb1) - y ~ x1_b + x2_s + x3_s + x4_s + x5_s + x6_s + x7_s (Only x1 is binarized and the rest are standardized)
# 4. ZINB(sb2) - y ~ x1_s + x2_b + x3_s + x4_s + x5_s + x6_s + x7_s (Only x2 is binarized and the rest are standardized)

# Fit a Zero-Inflated Negative binomial model
zinb_model <- zeroinfl(y ~ x1_s + x2_b + x3_s + x4_s + x5_s + x6_s + x7_s | x1_s + x2_b + x3_s + x4_s + x5_s + x6_s + x7_s, dist = "negbin")
summary(zinb_model)
vif(zinb_model)

# Calculate Pearson Chi-squared statistic
pearson_chi_zinb <- sum(residuals(zinb_model, type = "pearson")^2)

# Degrees of freedom
n_zinb <- nrow(data)  # Number of observations
k_zinb <- length(coef(zinb_model))  # Number of model parameters
df_zinb <- n_zinb - k_zinb

# Dispersion statistic
dispersion_statistic_zinb <- pearson_chi_zinb / df_zinb

# Print results
cat("Pearson Chi-squared statistic:", round(pearson_chi_zinb, 4), "\n")
cat("Degrees of Freedom:", df_zinb, "\n")
cat("Dispersion Statistic:", round(dispersion_statistic_zinb, 4), "\n")

# Interpretation
if (dispersion_statistic_zinb > 1.2) {  # Arbitrary threshold for overdispersion
  cat("Evidence of overdispersion in zero-inflated negative binomial.\n")
} else {
  cat("No strong evidence of overdispersion in zero-inflated negative binomial.\n")
}

# Extract predicted values from model
pred_zinb <- predict(zinb_model, type = "response")
pred_zinb

# Display the first few predicted values
head(pred_zinb)

# Round predictions to nearest ones
Rpred_zinb <- round(pred_zinb, 0)
Rpred_zinb

#Check zeros captured by the prediction
sum(Rpred_zinb == 0)

# Convert Months to date format
data$Month <- as.Date(data$Month)

# Combine Original and Predicted data into one data frame
comp.data_zinb <- data.frame(
  time = data$Month,  # Time variable from the Excel sheet
  original = data$`Positive Dog Rabies Cases`,  # Original data from Excel
  predicted = pred_zinb)  # Predicted data

# Create a line graph comparing Original and Predicted data
zinb_line <- ggplot(comp.data_zinb, aes(x = time)) +
  geom_line(aes(y = original, color = "Original Data"), size = 1) +
  geom_line(aes(y = predicted, color = "Predicted Data"), size = 1) +
  labs(
       x = "Year",
       y = "Number of Canine Rabies Cases",
       color = "Legend") +
  scale_color_manual(values = c("Original Data" = "#1f77b4", "Predicted Data" = "#d62728")) +
  
  # X-axis: Show only years
  scale_x_date(
    date_labels = "%Y",       # Only show year
    date_breaks = "1 year",   # Breaks at each year
    expand = c(0.01, 0.01)
  ) +
  
  # Y-axis: Breaks at every 1 unit
  scale_y_continuous(breaks = seq(0, max(comp.data_zinb$original, comp.data_zinb$predicted), by = 1)) +
  
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),  # Remove all grid lines
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    axis.text = element_text(color = "black", size = 15),
    axis.title = element_text(color = "black", size = 15),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "top",  # Position legend at the top
    legend.justification = c(1, 1),  # Align the legend to the upper-right corner
    legend.text = element_text(size = 15),     # Increased legend text size
    legend.title = element_text(size = 15),    # Increase legend title
    legend.box.margin = margin(0, 150, 0, 0),  # Move the legend outside the plot area to the right
    axis.text.x = element_text(angle = 0, hjust = 0.5)
  )

# Show generated graph
zinb_line

# The Binary Reconstruction -------------------------------------------------------------------------------------------------------

# Predict the probability of zero for each observation
prob <- predict(zinb_model, type = "prob")

# Extract the probability of zero
prob_zero <- prob[,1]

# Display the first few probabilities of zero
head(prob_zero)

# Create a data frame from the probabilities
y_prob_df <- data.frame(y, prob_zero = prob_zero)
y_prob_df

# Add a column where values close to 1 get 1, otherwise 0 (with high variance, use 0.5 threshold)
y_prob_df$binary_class <- ifelse(y_prob_df$prob_zero < 0.9, 1, 0)
# Display the first few rows of the data frame
y_prob_df

# Add a column where values greater than 0 from the original cases get 1, otherwise 0
data_rab <- data.frame(data$"Positive Dog Rabies Cases")
data_rab$binarize <- ifelse(data_rab > 0, 1, 0)
data_rab
# Rename Column
names(data_rab$binarize)[1] <- "Actual"

# Vectorize
Actual <- data_rab$binarize
Predicted <- y_prob_df$binary_class

# Confusion matrix for the Actual and Predicted Values
table(Predicted = Predicted, Actual = Actual)

# Filter only rows where y is 0
zero_prob_df <- y_prob_df %>%
  filter(y == 0)
zero_prob_df

# Count structured and sampled zeros
structured_count <- sum(zero_prob_df$binary_class == 0)
structured_count
sampled_count <- sum(zero_prob_df$binary_class == 1)
sampled_count

# Create a graph showing the distribution of probabilities
hist <- ggplot(zero_prob_df, aes(x = prob_zero, fill = as.factor(binary_class))) +
  geom_histogram(binwidth = 0.05, alpha = 0.8, position = "identity", color = "black") +
  labs(
    x = "Probability of Zero",
    y = "Count",
    fill = "Category"
  ) +
  scale_fill_manual(
    values = c("0" = "#E74C3C", "1" = "#3498DB"),
    labels = c("Structured Zero", "Sampled Zero")
  ) +
  annotate("text", x = 0.25, y = 30, label = paste("Sampled Zeros:", sampled_count),
           color = "#3498DB", size = 5, fontface = "bold") +
  annotate("text", x = 0.65, y = 30, label = paste("Structured Zeros:", structured_count),
           color = "#E74C3C", size = 5, fontface = "bold") +
  scale_x_continuous(breaks = seq(0, 1, by = 0.05), limits = c(0, 1.05)) +  # Finer x-axis
  scale_y_continuous(breaks = seq(0,35, by = 5), limits = c(0, 35)) +  # Finer y-axis
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),  # No full box
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    axis.text = element_text(color = "black", size=15),
    axis.title = element_text(color = "black", size = 15),
    legend.position = c(0.95, 0.90),
    legend.justification = c("right", "top"),
    legend.text = element_text(size = 15),     # Increased legend text size
    legend.title = element_text(size = 15),    # Increase legend title
    legend.background = element_rect(fill = "white", color = "black"),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

hist

# Put reconstructed data into a vector (8th Predictor - Uncertainty)
x8 <- y_prob_df$binary_class # This data will be added as a new predictor for the phase 3
x8
# Convert class to factor
x8_b <- as.factor(x8)


# Create data frame for the new set of predictors
new_pred <- data.frame(x1_s, x2_b, x3_s, x4_s, x5_s, x6_s, x7_s, x8_b)
new_pred

# Check for the data types of predictors (x4 and x8 shoul be factor, the rest is numeric)
sapply(list(x1_s, x2_b, x3_s, x4_s, x5_s, x6_s, x7_s, x8_b), class)

# Check length for all predictors (should be the same length)
sapply(list(x1_s, x2_b, x3_s, x4_s, x5_s, x6_s, x7_s, x8_b), length)

# Logistic regression -------------------------------------------------------------------------------------------------------

# Convert y to binary based on a condition (e.g., y > 0, 1) and convert to factor
y_bin <- ifelse(y > 0, 1, 0)
y_b <- as.factor(y_bin)
# Check class
class(y_b)

# GLMs considered for this model (Copy and Paste GLM to test each model)
# 1. L1 - y_b ~ x1_s + x2_b + x3_s + x4_s + x5_s + x6_s + x7_s + x8_b (All predictors)
# 2. L2 - y_b ~ x1_s + x2_b + x3_s + x4_s + x8_b                      (Rabies interventions + uncertainty)
# 3. L3 - y_b ~ x1_s + x2_b + x3_s + x4_s                             (Rabies interventions only)
# 4. L4 - y_b ~ x5_s + x6_s + x7_s + x8_b                             (Weather variables + uncertainty)
# 5. L5 - y_b ~ x5_s + x6_s + x7_s                                    (Weather variables only)
# 6. L6 - y_b ~ x1_s + x3_s + x6_s + x7_s + x8_b                      (Significant variables by ZINB-sb2 + uncertainty)
# 7. L7 - y_b ~ x1_s + x3_s + x6_s + x7_s                             (Significant variables by ZINB-sb2 only)

# Fit Logistic Regression model
logit_model <- glm(y_b ~ x5_s + x6_s + x7_s + x8_b, family = binomial(link = "logit"))
summary(logit_model)
vif(logit_model)

# Extract Predicted values
pred_logit <- predict(logit_model, type = "response")
head(pred_logit)

# Predicted classes (using 0.5 threshold as default)
pred_class <- ifelse(pred_logit > 0.2, 1, 0)
head(pred_class)

#Check zeros in predicted class
sum(pred_class == 0)

comp.data_logit <- data.frame(
  time = data$Month,  # Time variable from the Excel sheet
  original = y_bin,  # Original data from Excel
  predicted = pred_logit)  # Predicted data
comp.data_logit

# Add a column to indicate predicted class
comp.data_logit$pred_class <- ifelse(comp.data_logit$predicted >= 0.2, "Predict 1", "Predict 0")

# Create a plot comparing the original and predicted values for logistic regression
logit_graph <- ggplot(comp.data_logit, aes(x = time)) +
  # Original data as a thin line
  geom_line(aes(y = original, color = "Original Data"), size = 0.7) +
  
  # Predicted points, color-coded by binary classification
  geom_point(aes(y = predicted, color = pred_class), size = 2) +
  
  # Threshold line at 0.5
  geom_hline(yintercept = 0.2, linetype = "dashed", color = "gray50", size = 0.6) +
  
  labs(
    title = NULL,
    x = "Year",
    y = "Probability / Binary Outcome",
    color = "Legend"
  ) +
  
  # Colors: line + points (red/blue)
  scale_color_manual(
    values = c(
      "Original Data" = "#5DA5A4",  # a soft teal
      "Predict 1" = "#d73027",      # red-ish
      "Predict 0" = "#1c91d0"       # blue-ish
    )
  ) +
  
  # x-axis as years (assuming 'time' is Date)
  scale_x_date(
    date_labels = "%Y",
    date_breaks = "1 year",
    expand = c(0.01, 0.01)
  ) +
  
  # y-axis breaks every 0.1
  scale_y_continuous(
    breaks = seq(0, 1, by = 0.1),
    limits = c(0, 1)
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),  # remove grid lines
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    axis.text = element_text(color = "black", size = 20),
    axis.title = element_text(size = 15),
    legend.position = "top",
    legend.justification = "right",
    legend.text = element_text(size = 15),     # Increased legend text size
    legend.title = element_text(size = 15),    # Increase legend title
    plot.title = element_text(hjust = 0.5)
  )

# Show graph
logit_graph

# Compute AIC and BIC for Logistic
AIC(logit_model)
BIC(logit_model)

# Compute Residuals using MAE, MSE, and RMSE for Logistic
# MSE
MSElogit <- mean((y - pred_logit)^2)
print(paste("MSElogit:", round(MSElogit, 4)))

# RMSE
RMSElogit <- sqrt(MSElogit)
print(paste("RMSElogit:", round(RMSElogit, 4)))

# MAE
MAElogit <- mean(abs(y - pred_logit))
print(paste("MAElogit:", round(MAElogit, 4)))

