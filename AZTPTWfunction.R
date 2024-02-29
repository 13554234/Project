#SIMULATION OF THE 1995 AZT TRIAL INTO MATERNAL-TO-INFANT HIV TRANSMISSION
#So we create a function with inputs:
# 1) success_A - the probability that receiving treatment A is a success
# 2) success_B - the probability that receiving treatment B is a success
# 3) m - the number of simulations eg. 500
# 4) nR - the number of subjects to be randomised
# 5) gammaR - the number of balls of each type in the initial urn composition
# 6) alphaR - the number of balls added into the urn dependent on the treatment and outcome of the previous subject
PTWsimulation <- function(success_A, success_B, m, nR, gammaR, alphaR){
  #Initialise counts in arm A and arm B in each simulation
  countA<- rep(0,m)
  countB<- rep(0,m)
  #Initialise number of treatment assignments to A and B in each simulation
  assignedA<- rep(0,m)
  assignedB<- rep(0,m)
  #This for loop:
  #(1) sets the initial counts for the number of subjects in each treatment arm to empty vectors which will be added to after every ball is pulled out the urn so we can see how the count changes over time
  #(2) sets the initial number of balls/subjects in each treatment arm to alphaR
  #(3) ball samples a ball/subject from the urn/pool 
  for(j in 1:m){
    countnA <- c()
    countnB <- c()
    nA <- gammaR
    nB <- gammaR
    #Ignoring first alphaR patients, number of patients assigned to treatment A and B.
    assignednA<- 0 
    assignednB<- 0
    n <- nR
    alpha <- alphaR
    ball <- sample(c("A", "B"), 1, prob = c(nA, nB))
    # This for loop is then used to check whether the ball is from treatment A (AZT) and then to check whether this treatment was a success,
    # in which case we add the next betaR subjects to enter the trial into that treatment arm. However if the treatment is unsuccessful then we 
    # add the next betaR subjects into the opposite treatment arm. Similarly, if the ball we pull is from treatment B, we do the opposite.
    # The function 'treat' samples a 1 with probability pA and a 0 with probability 1-pA to imitate the treatment being a success or a failure.
    for (i in 1:n) {
      ball <- sample(c("A", "B"), 1, prob = c(nA, nB))
      if(ball == 'A'){
        assignednA <- assignednA + 1
        treat <- sample(c(1,0), 1, prob = c(success_A, 1-success_A))
        if(treat == 1){
          nA <- nA + alpha
          nB <- nB 
        }else{
          nA <- nA 
          nB <- nB + alpha
        }               
      }else{
        assignednB <- assignednB + 1
        treat <- sample(c(1,0), 1, prob = c(success_B, 1-success_B))
        if(treat == 1){
          nA <- nA 
          nB <- nB + alpha
        }else{
          nA <- nA + alpha
          nB <- nB 
        }
      }
      countnA <- c(countnA, nA)
      countnB <- c(countnB, nB)
    }
    #Store number in arm A and B at this simulation (jth)
    countA[j] <- countnA[n]
    countB[j] <- countnB[n]
    #Store number assigned to treatment A and B in this simulation
    assignedA[j] <- assignednA
    assignedB[j] <- assignednB 
  }
  # Estimated mean count of type A and B in urn at end of trial should be close to 300:176 split for the AZT trial
  mean_A=mean(countA)
  mean_B=mean(countB)
  # Estimated mean number of treatment assignments in bin A and B 
  mean_tA=mean(assignedA)
  mean_tB=mean(assignedB)
  # Estimated variance of number of treatment assignments to A. 
  var_tA=var(assignedA)
  var_tB=var(assignedB)
  
  # Standard error of the mean for estimates of mean above
  sem_tA=sqrt(var_tA)/sqrt(m)
  sem_tB=sqrt(var_tB)/sqrt(m)
  sem_tA
  sem_tB
  
  #Next we want to show that, had we implemented this Play-The-Winner method, 11 children would have avoided contracting HIV
  #Originally, 60 children in the placebo group contracted HIV and 20 in the AZT group
  proportionA <- 20/238
  proportionB <- 60/238
  childrenHIVA <- mean_A*proportionA
  childrenHIVB <- mean_B*proportionB
  #The following value should be very close to 11 - number of children saved!
  children_saved <- 80-(childrenHIVA+childrenHIVB)
  
  result <- c(mean_A, mean_B, mean_tA, mean_tB, children_saved, sem_tA, sem_tB)
  result
}
PTWsimulation(success_A=0.9160, success_B=0.7479, m=500, nR=426, gammaR=25, alphaR=1)


#This is the function that was previously written to represent my mathematical representation of PTW
ExpectedNa <- function(gamma, pB, pA, n){
  ENai <- 0
  ENatotal <- 0
  Bk <- 0
  for(i in 1:n){
    Ai <- (gamma+(i-1)*(1-pB))/((2*gamma)+(i-1))
    B <- 1
    for (k in (i+1):n){
      Bk <- (1 + (pA+pB-1)/((2*gamma)+(k-1)))
      B <- B*Bk
    }
    ENai <- Ai*B
    ENatotal <- ENatotal + ENai}
  ENatotal
}
# Computed mean values from formula: these should be roughly the same as mean_tA and mean_tB
E_tA = ExpectedNa(gamma=25, pB=0.7479, pA=0.9160, n=426)
E_tB = ExpectedNa(gamma=25, pB=0.9160, pA=0.7479, n=426)
E_tA
E_tB






