#Function for DABCD mathematical formula using optimal allocation ratio, rho, that minimises expected hazard
DABCD <- function(lambda_A, lambda_B, D, R, n){
  #The epsilon values below are those computed using the censoring scheme derived in my report
  epsilon_A <- 1 - (lambda_A/D) + exp(-D/lambda_A)*(lambda_A/(D*R))*(exp(R/lambda_A)*(2*lambda_A - R) - 2*lambda_A)
  epsilon_B <- 1 - (lambda_B/D) + exp(-D/lambda_B)*(lambda_B/(D*R))*(exp(R/lambda_B)*(2*lambda_B - R) - 2*lambda_B)
  #Optimal allocation ratio
  rho <- (sqrt((lambda_A^3)*epsilon_B))/((sqrt((lambda_A^3)*epsilon_B))+(sqrt((lambda_B^3)*epsilon_A)))
  #Multiplying rho by the number of of subjects in the trial, n, gives you the number of subjects to be allocated to treatment A
  N_a <- n*rho
  N_a }


DABCD(16.7, 13.6, 34, 28.5, 476)


  