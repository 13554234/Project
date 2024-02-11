#SIMULATION OF THE 1995 AZT TRIAL INTO MATERNAL-TO-INFANT HIV TRANSMISSION
#First let's assign the treatment arms with the appropriate success probabilities, see ‘Randomised Response-Adaptive Designs in Clinical Trials’ – Anthony C Atiknson, Atanu Biswas
success_A <- 0.9160
success_B <- 0.7479

#Set the total for the number of subjects in each treatment arm to 0 and let m = 500, where m is the number of times we repeat the Play-The-Winner trial
countnAtotal <- 0 
countnBtotal <- 0
m <- 500

#Initialise counts in bin A and bin B in each simulation
countA<- rep(0,m)
countB<- rep(0,m)

#Initialise number of treatment assignments to A and B in each simulation
assignedA<- rep(0,m)
assignedB<- rep(0,m)

#This for loop:
#(1) sets the initial counts for the number of subjects in each treatment arm to empty vectors which will be added to after every ball is pulled out the urn so we can see how the count changes over time
#(2) sets the initial number of balls/subjects in each treatment arm to 25 (alpha)
#(3) ball samples a ball/subject from the urn/pool 
for(j in 1:m){
  countnA <- c()
  countnB <- c()
  
  nA <- 25
  nB <- 25
  
  #Ignoring first alpha patients, number of patients assigned to treatment A and B.
  assignednA<- 0 
  assignednB<- 0
  
  n <- 426
  beta <- 1
  ball <- sample(c("A", "B"), 1, prob = c(nA, nB))
  # This for loop is then used to check whether the ball is from treatment A (AZT) and then to check whether this treatment was a success,
  # in which case we add the next beta subjects to enter the trial into that treatment arm. However if the treatment is unsuccessful then we 
  # add the next beta subjects into the opposite treatment arm. Similarly, if the ball we pull is from treatment B, we do the opposite.
  # The function 'treat' samples a 1 with probability pA and a 0 with probability 1-pA to imitate the treatment being a success or a failure.   
  for (i in 1:n) {
    ball <- sample(c("A", "B"), 1, prob = c(nA, nB))
    if(ball == 'A'){
      assignednA <- assignednA + 1
      treat <- sample(c(1,0), 1, prob = c(success_A, 1-success_A))
      if(treat == 1){
        nA <- nA + beta
        nB <- nB 
      }else{
        nA <- nA 
        nB <- nB + beta
      }               
    }else{
      assignednB <- assignednB + 1
      treat <- sample(c(1,0), 1, prob = c(success_B, 1-success_B))
      if(treat == 1){
        nA <- nA 
        nB <- nB + beta
      }else{
        nA <- nA + beta
        nB <- nB 
      }
    }
    countnA <- c(countnA, nA)
    countnB <- c(countnB, nB)
  }
  
  countnA[426]
  countnB[426]
  #The following two lines add up all the final values as to how many subjects are in each treatment arm once the sample size is exhausted (476 people).
  countnAtotal <- countnAtotal + countnA[426]
  countnBtotal <- countnBtotal + countnB[426]
  
  #Store number in bin A and B at this simulation (jth)
  countA[j] <- countnA[n]
  countB[j] <- countnB[n]
  
  #Store number assigned to treatment A and B in this simulation
  assignedA[j] <- assignednA
  assignedB[j] <- assignednB 
}

# Estimated mean count of type A and B in urn at end of trial should be close to 300:176 split
mean_A=mean(countA)
mean_B=mean(countB)
mean_A
mean_B

# Estimated variance of bin A and B count
var_A=var(countA)
var_B=var(countB)

# Standard error of the mean for estimates of mean above
sem_A=sqrt(var_A)/sqrt(m)
sem_B=sqrt(var_B)/sqrt(m)


# Estimated mean number of treatment assignments in bin A and B 
mean_tA=mean(assignedA)
mean_tB=mean(assignedB)
mean_tA
mean_tB

# Estimated variance of number of treatment assignments to A. 
var_tA=var(assignedA)
var_tB=var(assignedB)

# Standard error of the mean for estimates of mean above
sem_tA=sqrt(var_tA)/sqrt(m)
sem_tB=sqrt(var_tB)/sqrt(m)
sem_tA
sem_tB


#This code presents a mathematical understanding behind the urn composition in a Randomised Play-The-Winner trial using the equation that appears in my thesis for E(Na(n)):

#Firstly let's introduce the variables that we are going to input into the function:
#- alpha is the number of balls of each type initially in the urn
#- pB and pA are the relevant success probabilites of treatments B and A respectively
#- n is the number of subjects in the trial

#We can now introduce our other variables which we are going to use: ENai which is the value of the summation at each value of i and ENatotal which is these values added together over time.
ExpectedNa <- function(alpha, pB, pA, n){
  ENai <- 0
  ENatotal <- 0
  Bk <- 0
  for(i in 1:n){
    Ai <- (alpha+(i-1)*(1-pB))/((2*alpha)+(i-1))
    B <- 1
    for (k in (i+1):n){
      Bk <- (1 + (pA+pB-1)/((2*alpha)+(k-1)))
      B <- B*Bk
    }
    ENai <- Ai*B
    ENatotal <- ENatotal + ENai}
  ENatotal}

# Computed mean values from formula: these should be roughly the same as mean_tA and mean_tB
E_tA = ExpectedNa(alpha=25, pB=0.7479, pA=0.9160, n=426)
E_tB = ExpectedNa(alpha=25, pB=0.9160, pA=0.7479, n=426)
E_tA
E_tB






#Next we want to show that, had we implemented this Play-The-Winner method, 11 children would have avoided contracting HIV
#Originally, 60 children in the placebo group contracted HIV and 20 in the AZT group
proportionA <- 20/238
proportionB <- 60/238
childrenHIVA <- mean_A*proportionA
childrenHIVB <- mean_B*proportionB
#The following value should be very close to 11 - number of children saved!
80-(childrenHIVA+childrenHIVB)



