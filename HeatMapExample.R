# Sample dataset
set.seed(123)  # For reproducibility
data <- data.frame(
  Category1 = rep(c("A", "B", "C", "D"), times = 5),
  Category2 = rep(c("W", "X", "Y", "Z"), each = 5),
  Value = rnorm(20, mean = 50, sd = 10)
)


# Create the heat map
ggplot(data, aes(x = Category1, y = Category2, fill = Value)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_minimal() +
  labs(title = "Heat Map of Values by Category1 and Category2",
       x = "Category1",
       y = "Category2",
       fill = "Value")

# Sample dataset
summary_data <- data.frame(
  antibioticType = rep(c("Antibiotic1", "Antibiotic2", "Antibiotic3"), each = 3),
  bacteria = rep(c("Bacteria1", "Bacteria2", "Bacteria3"), times = 3),
  fitness = runif(9, min = 0, max = 1)  # Random fitness values between 0 and 1
)

ggplot(summary_data, aes(x = antibioticType, y = bacteria, fill = fitness)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_minimal() +
  labs(title = "Heat Map of Fitness by Antibiotic Type and Bacteria",
       x = "Antibiotic",
       y = "Bacteria",
       fill = "Fitness")
