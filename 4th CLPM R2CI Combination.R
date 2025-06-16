# Load necessary libraries
library(lavaan)
library(readr)

# Load the dataset
data <- read_csv("4th CLPM R2CI Combination.csv")

# Define the Cross-Lagged Panel Model (CLPM)
model <- '
  # Stability paths within each variable across waves
  R2TTF ~ R1TTF
  R3TTF ~ R2TTF
  R2CI ~ R1I
  R3CI ~ R2CI
  R2PsWB ~ R1PsWB
  R3PsWB ~ R2PsWB

  # Cross-lagged paths
  R2TTF ~ R1I + R1PsWB
  R2CI ~ R1TTF + R1PsWB
  R2PsWB ~ R1TTF + R1I
  R3TTF ~ R2CI + R2PsWB
  R3CI ~ R2TTF + R2PsWB
  R3PsWB ~ R2TTF + R2CI

  # Covariances at Wave 1
  R1TTF ~~ R1I + R1PsWB
  R1I ~~ R1PsWB

  # Covariances at Wave 2
  R2TTF ~~ R2CI + R2PsWB
  R2CI ~~ R2PsWB

  # Covariances at Wave 3
  R3TTF ~~ R3CI + R3PsWB
  R3CI ~~ R3PsWB
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
print(r_squared[c("R2TTF", "R2PsWB", "R2CI", "R3TTF", "R3PsWB", "R3CI")])