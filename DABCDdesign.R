DABCD <- function(lambda_A, lambda_B, D, R, n){
  epsilon_A <- 1 - (lambda_A/D) + exp(-D/lambda_A)*(lambda_A/(D*R))*(exp(R/lambda_A)*(2*lambda_A - R) - 2*lambda_A)
  epsilon_B <- 1 - (lambda_B/D) + exp(-D/lambda_B)*(lambda_B/(D*R))*(exp(R/lambda_B)*(2*lambda_B - R) - 2*lambda_B)
  rho <- (sqrt((lambda_A^3)*epsilon_B))/((sqrt((lambda_A^3)*epsilon_B))+(sqrt((lambda_B^3)*epsilon_A)))
  N_a <- n*rho
  N_a}


DABCD(16.7, 13.6, 34, 29.5, 476)


  