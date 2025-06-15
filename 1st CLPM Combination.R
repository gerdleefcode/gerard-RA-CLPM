# Load necessary libraries
library(lavaan)
library(readr)

# Load the dataset
data <- read_csv("1st CLPM Combination.csv")

# Define the Cross-Lagged Panel Model (CLPM)
model <- '
  # Stability paths within each variable across waves
  R2TTF ~ R1TTF
  R3TTF ~ R2TTF
  R2HSTF ~ R1HSTF
  R3HSTF ~ R2HSTF
  R2AU ~ R1I
  R3CI ~ R2AU

  # Cross-lagged paths
  R2TTF ~ R1HSTF + R1I
  R2HSTF ~ R1TTF + R1I
  R2AU ~ R1TTF + R1HSTF
  R3TTF ~ R2HSTF + R2AU
  R3HSTF ~ R2TTF + R2AU
  R3CI ~ R2TTF + R2HSTF

  # Covariances at Wave 1
  R1TTF ~~ R1HSTF + R1I
  R1HSTF ~~ R1I

  # Covariances at Wave 2
  R2TTF ~~ R2HSTF + R2AU
  R2HSTF ~~ R2AU

  # Covariances at Wave 3
  R3TTF ~~ R3HSTF + R3CI
  R3HSTF ~~ R3CI
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
print(r_squared[c("R2TTF", "R2HSTF", "R2AU", "R3TTF", "R3HSTF", "R3CI")])