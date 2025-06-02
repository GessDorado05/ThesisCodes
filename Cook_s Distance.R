# Load necessary libraries
library(carData)
library(car)
library(ggplot2)

# Generate example data
set.seed(123)
n <- 100
x1 <- rnorm(n)
x2 <- rnorm(n)
x3 <- rnorm(n)
y <- 3 + 2 * x1 + 3 * x2 + rnorm(n)

# Create a data frame
data <- data.frame(y = y, x1 = x1, x2 = x2, x3 = x3)

# Fit a linear regression model
model <- lm(y ~ x1 + x2 + x3, data = data)
print(model)

# Calculate Cook's Distance
cooks_d <- cooks.distance(model)

# Add Cook's Distance to the data frame
data$cooks_d <- cooks_d

# Display observations with high Cook's Distance
high_cooks_d <- data[data$cooks_d > (4 / n), ]
print(high_cooks_d)

# Plot Cook's Distance
plot(cooks_d, type = "h", main = "Cook's Distance", ylab = "Cook's Distance")
abline(h = 4 / n, col = "red")  # Threshold line for Cook's Distance

# Optional: Enhanced visualization using ggplot2
ggplot(data, aes(x = 1:n, y = cooks_d)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 4 / n, color = "red", linetype = "dashed") +
  labs(title = "Cook's Distance", x = "Observation", y = "Cook's Distance") +
  theme_minimal()
