# Load necessary libraries
library(lavaan)
library(readr)

# Load the dataset
data <- read_csv("3rd CLPM Combination.csv")

# Define the Cross-Lagged Panel Model (CLPM)
model <- '
  # Stability paths within each variable across waves
  R2TTF ~ R1TTF
  R3TTF ~ R2TTF
  R2AU ~ R1I
  R3CI ~ R2AU
  R2PhWB ~ R1PhWB
  R3PhWB ~ R2PhWB

  # Cross-lagged paths
  R2TTF ~ R1I + R1PhWB
  R2AU ~ R1TTF + R1PhWB
  R2PhWB ~ R1TTF + R1I
  R3TTF ~ R2AU + R2PhWB
  R3CI ~ R2TTF + R2PhWB
  R3PhWB ~ R2TTF + R2AU

  # Covariances at Wave 1
  R1TTF ~~ R1I + R1PhWB
  R1I ~~ R1PhWB

  # Covariances at Wave 2
  R2TTF ~~ R2AU + R2PhWB
  R2AU ~~ R2PhWB

  # Covariances at Wave 3
  R3TTF ~~ R3CI + R3PhWB
  R3CI ~~ R3PhWB
'

# Fit the model using the dataset
fit <- sem(model, data = data)

# Summarize the model results
summary(fit, fit.measures = TRUE, standardized = TRUE)

# Extract model evaluation metrics
AIC_value <- AIC(fit)
fitmeasures(fit, c("rmsea", "cfi", "tli", "chi.sq", "df", "pvalue"))

# Extract R-squared values for Wave 2 and Wave 3
r_squared <- inspect(fit, "r2")
print("R-squared for Wave 2 and Wave 3:")
print(r_squared[c("R2TTF", "R2PhWB", "R2AU", "R3TTF", "R3PhWB", "R3CI")])