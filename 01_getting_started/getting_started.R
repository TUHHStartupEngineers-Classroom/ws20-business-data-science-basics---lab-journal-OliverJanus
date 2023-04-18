calcQ <- function(D=1000, K=5, h=0.25){
  return(sqrt(2*D*K/h))
}

prob_vect <- c(0.1, 0.1, 0.1, 0.1, 0.1, 0.5)
sample(x = 6, size = 1, replace = TRUE, prob = prob_vect)
