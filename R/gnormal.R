dgnormal <- function(x, mean, alpha, beta){
  # Generalised normal distribution (or, exponential power distribution)
  # 
  # beta = 2: normal, so (excess) kurtosis = 0
  # beta < 2: heavy tails (big kurtosis)
  # beta > 2: light tails (small kurtosis)
  
  if (beta <= 0) stop("beta must be a positive value")

  scaling <- beta / (2 * alpha * gamma(1/beta) )
  scaling * exp( -abs( (x - mean)/alpha )^beta )
}

