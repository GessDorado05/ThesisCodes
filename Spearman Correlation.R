# Load necessary library
library(ggplot2)
library(gridExtra)  # for arranging plots

# Define the data
data <- data.frame(
  rab = c(4, 2, 0, 0, 0, 2, 2, 1, 1, 1, 1, 2, 1, 0, 5, 0, 0, 0, 0, 0, 3, 3, 1, 0, 1, 1, 2, 2, 1, 2, 0, 2, 0, 1, 0, 2, 1, 0, 1, 0, 2, 0, 1, 1, 2, 2, 1, 0, 1, 1, 0, 1, 1, 1, 1, 2, 1, 0, 0, 3, 1, 1, 3, 1, 3, 1, 1, 2, 0, 2, 0, 0, 1, 1, 1, 1, 5, 0, 0, 3, 3, 4, 2, 1, 3, 1, 3, 8, 8, 13, 8, 7, 7, 7, 6, 3, 2, 2, 0, 2, 2, 1, 6, 3, 6, 5, 5, 4, 4, 4, 6, 0, 1, 2, 3, 5, 3, 4, 0, 3, 3, 5, 0, 4, 3, 1, 0, 0, 2, 3, 1, 1, 5, 6, 2),
  vac = c(3845, 5491, 7944, 7138, 5575, 5225, 3690, 1115, 3454, 4137, 3478, 6070, 761, 870, 10531, 8915, 4542, 8537, 13729, 3408, 3155, 852, 1086, 2553, 3706, 8574, 7283, 10646, 7088, 4655, 7013, 3904, 7040, 3950, 4302, 2795, 2800, 10867, 6585, 6841, 6605, 7846, 11840, 5790, 7283, 4373, 1413, 6025, 4071, 4757, 4462, 1631, 2132, 2988, 4639, 7915, 5963, 5985, 7550, 2876, 20758, 10950, 20852, 4811, 15643, 17959, 4897, 5425, 3436, 2725, 1967, 1379, 12585, 14566, 13228, 8872, 11133, 9438, 6246, 9474, 5640, 4972, 2907, 2632, 10998, 11734, 10107, 14702, 17723, 16943, 9816, 7271, 5135, 12677, 8356, 2417, 4211, 5356, 4320, 8725, 11512, 3500, 8883, 11110, 20486, 26704, 9183, 6240, 13707, 12727, 13896, 480, 597, 5203, 17485, 19048, 29087, 9341, 22298, 1240, 4686, 22316, 19117, 2034, 778, 776, 1601, 21681, 14948, 21146, 4619, 8435, 1072, 912, 5662),
  cast = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 79, 16, 37, 68, 29, 48, 124, 90, 32, 44, 12, 32, 18, 14, 56, 33, 22, 13, 0, 0, 57, 14, 23, 24, 20, 74, 19, 15, 0, 0, 24, 47, 5, 38, 17, 12, 14, 32, 68, 8, 10, 19, 0, 0, 10, 0, 5, 18, 74, 88, 31, 58, 19, 6, 10, 146, 60, 38, 4, 0, 0, 17, 0, 0, 0, 21, 39, 86, 30, 0, 0, 58, 58, 800, 17, 0, 0, 85, 166, 154, 141, 45, 87, 245, 0, 82, 113, 80, 11, 123, 67, 0, 0, 4, 7, 45, 228, 48, 36, 3, 198, 75, 0, 224, 321, 57, 0, 0, 202, 82, 84, 19, 35, 52, 620, 673)
)

# Compute Spearman correlation matrix
spearman_cor <- cor(data, method = "spearman")

# Print the correlation matrix
print(spearman_cor)

# Create an empty matrix to store p-values
var_names <- names(data)
n <- length(var_names)
pval_matrix <- matrix(NA, nrow = n, ncol = n, dimnames = list(var_names, var_names))
cor_matrix <- matrix(NA, nrow = n, ncol = n, dimnames = list(var_names, var_names))

# Loop through each pair of variables
for (i in 1:n) {
  for (j in 1:n) {
    test <- cor.test(data[[i]], data[[j]], method = "spearman", exact = FALSE)
    cor_matrix[i, j] <- test$estimate
    pval_matrix[i, j] <- test$p.value
  }
}

# Display the correlation matrix
print("Spearman correlation matrix:")
print(cor_matrix)

# Display the matrix of p-values
print("P-value matrix:")
print(pval_matrix)


library(ggplot2)

# Rabies vs Vaccination
ggplot(data, aes(x = vac, y = rab)) +
  geom_point(color = "blue") +
  labs(title = "Rabies Cases vs Vaccination",
       x = "Number of Vaccinations",
       y = "Number of Rabies Cases") +
  theme_minimal()

# Rabies vs Castration
ggplot(data, aes(x = cast, y = rab)) +
  geom_point(color = "darkgreen") +
  labs(title = "Rabies Cases vs Castration",
       x = "Number of Castrations",
       y = "Number of Rabies Cases") +
  theme_minimal()
