# -------------------------------------------------------------------------
# R Script to Estimate Backscatter Fraction from Junge Slope (mu)
# Based on Fournier-Forand Phase Function Approximations (Boss & Pegau)
# -------------------------------------------------------------------------

# 1. Define the estimation functions
# Valid for typical marine environments where 3.0 < mu < 4.5
estimate_bp_organic <- function(mu) {
  # Approximation for refractive index n = 1.05 (soft/organic particles)
  bp <- 0.0012 * mu^2 - 0.0021 * mu + 0.0015
  return(bp)
}

estimate_bp_inorganic <- function(mu) {
  # Approximation for refractive index n = 1.15 (hard/mineral particles)
  bp <- 0.0115 * mu^2 - 0.0210 * mu + 0.0145
  return(bp)
}

# 2. Example Data Setup
# Replace this vector with your actual Junge slope values
junge_slopes <- c(3.2, 3.5, 3.8, 4.0, 4.2, 4.5)

# 3. Calculate Backscatter Fractions
data_results <- data.frame(
  Junge_Slope = junge_slopes,
  Organic_bp   = estimate_bp_organic(junge_slopes),
  Inorganic_bp = estimate_bp_inorganic(junge_slopes)
)

# Print results table to the console
print("Calculated Backscatter Fractions:")
print(data_results)

# -------------------------------------------------------------------------
# 4. Optional: Plot the results using base R graphics
# -------------------------------------------------------------------------
# Define a continuous sequence for smooth plotting lines
mu_seq <- seq(3.0, 4.5, by = 0.05)
bp_org_line <- estimate_bp_organic(mu_seq)
bp_ino_line <- estimate_bp_inorganic(mu_seq)

# Initialize plot window
plot(mu_seq, bp_ino_line, type = "l", col = "red", lwd = 2,
     xlab = expression(paste("Junge Slope (", mu, ")")),
     ylab = expression(paste("Backscatter Fraction (", tilde(b)[p], ")")),
     main = "Backscatter Fraction vs. Junge Slope",
     ylim = c(0, max(bp_ino_line) * 1.1))

# Add the organic line
lines(mu_seq, bp_org_line, col = "blue", lwd = 2)

# Add custom points from your dataset
points(data_results$Junge_Slope, data_results$Inorganic_bp, col = "red", pch = 19)
points(data_results$Junge_Slope, data_results$Organic_bp, col = "blue", pch = 19)

# Add a legend
legend("topleft", legend = c("Inorganic (n = 1.15)", "Organic (n = 1.05)"),
       col = c("red", "blue"), lwd = 2, pch = 19, bty = "n")
grid()
