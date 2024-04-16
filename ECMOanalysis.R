#Start with an urn with an equal number of red and black balls where red represents subjects in the treatment group (A) and black represents subjects in the control group (B)
nA <- 1
nB <- 1

#Count
countnA <- c()
countnB <- c()
#Count of how many times we see the 11:1 split
countn1 <- 0
#Count of how many times we see the 8:4 split
countn2 <- 0

#We need probabilities for the treatments being successful depending on whether they're from group A or B
success_A <- 0.7
success_B <- 0.2
#Number of iterations of the trial we want
m <- 1000

#So from the urn we pull a ball and follow our usual PTW rules with alpha=1, beta=0.
for(j in 1:m){
  nA <- 1
  nB <- 1
  countnA <- c()
  countnB <- c()
  n <- 10
  beta <- 0
  alpha <- 1
  characters <- c(rep('A', nA), rep('B', nB))
  random_characters <- sample(characters)
  random_ball <- sample(random_characters, size = 1)
  #Or this does the same thing
  ball <- sample(c("A", "B"), 1, prob = c(nA, nB))
  for (i in 1:n) {
    ball <- sample(c("A", "B"), 1, prob = c(nA, nB))
    if(ball == 'A'){
      treat <- sample(c(0,1), 1, prob = c(success_A, 1-success_A))
      if(treat == 0){
        nA <- nA + alpha
        nB <- nB + beta
      }else{
        nA <- nA + beta
        nB <- nB + alpha
      }               
    }else{
      treat <- sample(c(0,1), 1, prob = c(success_B, 1-success_B))
      if(treat == 0){
        nA <- nA + beta
        nB <- nB + alpha
      }else{
        nA <- nA + alpha
        nB <- nB + beta
      }
    }
    countnA <- c(countnA, nA)
    countnB <- c(countnB, nB)
  }
  
  countnA
  countnB
  #This identifies whether each trial has the 11:1 split found in the 1985 ECMO trial
  if(countnA[10]==11){
    countn1 = countn1 + 1
  }else{
    if(countnA[10]==8){
      countn2 = countn2 +1}
  }
}
#We can compare these two counts to see how often the 11:1 and 8:4 splits are in this context.
countn1
countn2


#If you want to plot the counts of the final run:
plot(1:n, countnA, cex=.7)
points(countnB, col = "red", cex=.7)
lines(countnA)
lines(countnB, col='red')






#Now let's see what would have happened if we had started with 3 balls of each colour:
#Start with an urn with an equal number of red and black balls where red represents subjects in the treatment group (A) and black represents subjects in the control group (B)
nA <- 3
nB <- 3

#Count
countnA <- c()
countnB <- c()

#We need probabilities for the treatments being successful depending on whether they're from group A or B
success_A <- 0.7
success_B <- 0.2

#So from the urn we pull a ball:
n <- 6
beta <- 0
alpha <- 1
ball <- sample(c("A", "B"), 1, prob = c(nA, nB))
for (i in 1:n) {
  ball <- sample(c("A", "B"), 1, prob = c(nA, nB))
  if(ball == 'A'){
    treat <- sample(c(0,1), 1, prob = c(success_A, 1-success_A))
    if(treat == 0){
      nA <- nA + alpha
      nB <- nB + beta
    }else{
      nA <- nA + beta
      nB <- nB + alpha
    }               
  }else{
    treat <- sample(c(0,1), 1, prob = c(success_B, 1-success_B))
    if(treat == 0){
      nA <- nA + beta
      nB <- nB + alpha
    }else{
      nA <- nA + alpha
      nB <- nB + beta
    }
  }
  countnA <- c(countnA, nA)
  countnB <- c(countnB, nB)
}

countnA
countnB
proportion <- c(countnA/(countnA+countnB))
proportion[6]
plot(1:n, countnA, cex=.7)
points(countnB, col = "red", cex=.7)
lines(countnA)
lines(countnB, col='red')

>  countn1
[1] 34
> countn2
[1] 308

