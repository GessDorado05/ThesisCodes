# Function to standardize a variable
standardize <- function(x) {
  return ((x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE))
}

# Example usage
data <- c(1, 2, 3, 4, 5)
standardized_data <- standardize(data)
print(standardized_data)

#------------------------------------------------------------------------------------------

# Function to normalize a variable between 0 and 1
min_max_normalize <- function(x) {
  return ((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}

# Example usage
data <- c(1, 2, 3, 4, 5)
normalized_data <- min_max_normalize(data)
print(normalized_data)

#------------------------------------------------------------------------------------------

# Apply standardization to all numeric columns in a data frame
df <- data.frame(a = 1:5, b = c(2, 4, 6, 8, 10))
df_standardized <- as.data.frame(lapply(df, function(x) if(is.numeric(x)) standardize(x) else x))
print(df_standardized)

# Apply min-max normalization to all numeric columns in a data frame
df_normalized <- as.data.frame(lapply(df, function(x) if(is.numeric(x)) min_max_normalize(x) else x))
print(df_normalized)
