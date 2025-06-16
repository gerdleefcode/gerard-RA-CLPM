# Load necessary libraries
library(lavaan)
library(readr)

# Load the dataset
data <- read_csv("5th CLPM R2CI Combination.csv")

# Define the Cross-Lagged Panel Model (CLPM)
model <- '
  # Stability paths within each variable across waves
  R2HSTF ~ R1HSTF
  R3HSTF ~ R2HSTF
  R2CI ~ R1I
  R3CI ~ R2CI
  R2PhWB ~ R1PhWB
  R3PhWB ~ R2PhWB

  # Cross-lagged paths
  R2HSTF ~ R1I + R1PhWB
  R2CI ~ R1HSTF + R1PhWB
  R2PhWB ~ R1HSTF + R1I
  R3HSTF ~ R2CI + R2PhWB
  R3CI ~ R2HSTF + R2PhWB
  R3PhWB ~ R2HSTF + R2CI

  # Covariances at Wave 1
  R1HSTF ~~ R1I + R1PhWB
  R1I ~~ R1PhWB

  # Covariances at Wave 2
  R2HSTF ~~ R2CI + R2PhWB
  R2CI ~~ R2PhWB

  # Covariances at Wave 3
  R3HSTF ~~ R3CI + R3PhWB
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
print(r_squared[c("R2HSTF", "R2PhWB", "R2CI", "R3HSTF", "R3PhWB", "R3CI")])