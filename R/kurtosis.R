kurtosisPDF <- function(f, lower, upper, excess = TRUE, tol = 1e-8) {
  # Compute mean (μ)
  mean_val <- integrate(function(x) x * f(x), lower, upper, rel.tol = tol)$value
  
  # Compute variance (σ²)
  second_moment <- integrate(function(x) (x^2) * f(x), lower, upper, rel.tol = tol)$value
  var_val <- second_moment - mean_val^2
  
  # Compute 4th central moment (μ₄)
  fourth_central_moment <- integrate(function(x) (x - mean_val)^4 * f(x), lower, upper, rel.tol = tol)$value
  
  # Kurtosis
  kurt <- fourth_central_moment / (var_val^2)
  
  if (excess) {
    return(kurt - 3)
  } else {
    return(kurt)
  }
}

# Example: kurtosis_from_pdf(dnorm, -Inf, Inf)  # Should return ≈ 0
# Example: 
# f_unif <- function(x) dunif(x, min = 1, max = 3)
# kurtosis_from_pdf(f_unif, 1, 3)  # Returns ≈ -1.2 (excess kurtosis of uniform)
#
# Example:
# a <- sqrt(24)
# b <- 2 - 0.5 * a
# f_beta_u <- function(x) {
#   ifelse(x >= b & x <= (b + a),
#          dbeta((x - b) / a, 0.5, 0.5) / a,
#          0)
# }
# kurtosis_from_pdf(f_beta_u, b, b + a)  # Should be > 0 (high kurtosis)

