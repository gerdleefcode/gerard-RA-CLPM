# Load necessary libraries
library(lavaan)
library(readr)

# Load the dataset
data <- read_csv("2nd CLPM Combination.csv")

# Define the Cross-Lagged Panel Model (CLPM)
model <- '
  # Stability paths within each variable across waves
  R2HSTF ~ R1HSTF
  R3HSTF ~ R2HSTF
  R2AU ~ R1I
  R3CI ~ R2AU
  R2PsWB ~ R1PsWB
  R3PsWB ~ R2PsWB

  # Cross-lagged paths
  R2HSTF ~ R1I + R1PsWB
  R2AU ~ R1HSTF + R1PsWB
  R2PsWB ~ R1HSTF + R1I
  R3HSTF ~ R2AU + R2PsWB
  R3CI ~ R2HSTF + R2PsWB
  R3PsWB ~ R2HSTF + R2AU

  # Covariances at Wave 1
  R1HSTF ~~ R1I + R1PsWB
  R1I ~~ R1PsWB

  # Covariances at Wave 2
  R2HSTF ~~ R2AU + R2PsWB
  R2AU ~~ R2PsWB

  # Covariances at Wave 3
  R3HSTF ~~ R3CI + R3PsWB
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
print(r_squared[c("R2HSTF", "R2PsWB", "R2AU", "R3HSTF", "R3PsWB", "R3CI")])