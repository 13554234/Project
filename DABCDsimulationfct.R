#DABCD function that is repeated m times and returns mean values of subjects assigned to A and B and their standard error values
DABCDfct <- function(lambda_A, lambda_B, D, R, m, n){
  A <- c()
  B <- c()

  for(j in 1:m){
    #The epsilon values below are those computed using the censoring scheme derived in my report
    epsilon_A <- 1 - (lambda_A/D) + exp(-D/lambda_A)*(lambda_A/(D*R))*(exp(R/lambda_A)*(2*lambda_A - R) - 2*lambda_A)
    epsilon_B <- 1 - (lambda_B/D) + exp(-D/lambda_B)*(lambda_B/(D*R))*(exp(R/lambda_B)*(2*lambda_B - R) - 2*lambda_B)
    epsilon_A
    epsilon_B 
    #Normal of subjects in each treatment arm A and B at the beginning of the trial are 0:
    nA <- 0
    nB <- 0 
    x <- 0.5
  
    counta <- c()
    countb <- c()
    #rho is the optimal allocation ratio, which in our case is the one that minimises expected hazard
    rho <- (sqrt((lambda_A^3)*epsilon_B))/((sqrt((lambda_A^3)*epsilon_B))+(sqrt((lambda_B^3)*epsilon_A)))
    #Split into the two cases of the DABCD allocation function
    for (i in 1:n){
      if(0 < x && x < 1){
        g <- (rho*(rho/x)^2)/(rho*(rho/x)^2 + (1-rho)*((1-rho)/(1-x))^2)
      }else{
        if(x==1){
          g<-1-x
        }else{
          if(x==0){
            g<-1-x
          }
        }
      }
      #Assign the next subject to treatment arm A with probability g(x,rho)
      subject <- sample(c("A", "B"), 1, prob = c(g, 1-g))
      if(subject == 'A'){
        nA <- nA + 1
      }else{
        if(subject == 'B'){
          nB <- nB + 1
        }}
      x <- nA/(nA+nB)
      counta <- c(counta, nA)
      countb <- c(countb, nB)
    }
    A <- c(A, counta[476])
    B <- c(B, countb[476])
}
sem_tA=sqrt(var(A))/sqrt(m)
sem_tB=sqrt(var(B))/sqrt(m)
result <- c(mean(A), mean(B), sem_tA, sem_tB)
result
}


DABCDfct(lambda_A=16.7, lambda_B=13.6, D=34, R=28.5, m=500, n=476)

