# Load necessary library
library(pscl)

# Generate example data
set.seed(123)
n <- 1000
x <- rnorm(n)
# Generate response variable with excess zeros
mu <- exp(1 + 0.5 * x)
size <- 2  # Dispersion parameter for NB
y <- rnbinom(n, size = size, mu = mu)
y[sample(1:n, size = 0.5*n)] <- 0

# Create a data frame
data <- data.frame(y = y, x = x)
View(data)

# Fit a Zero-Inflated Negative Binomial model
zinb_model <- zeroinfl(y ~ x | x, data = data, dist = "negbin")

# Summarize the model
summary(zinb_model)

# Predict the probability of zero for each observationprob_zero
predicted_probs <- predict(zinb_model, type = "prob")

# Extract the probability of zero
prob_zero <- predicted_probs[, 1]

# Display the first few probabilities of zero
head(prob_zero)
View(prob_zero)

