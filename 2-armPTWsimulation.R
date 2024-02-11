#Start with an urn with an equal number of white and black balls where white represents subjects in the treatment group (A) and black represents subjects in the control group (B)
nA <- 5
nB <- 5

#Count
countnA <- c()
countnB <- c()

#We need probabilities for the treatments being successful depending on whether they're from group A or B
success_A <- 0.8
success_B <- 0.5

#So from the urn we pull a ball, if ball is from group A and we observe it's a success we add beta balls back into urn
n <- 60
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
proportion <- c(countnA/(countnA+countnB))
proportion
proportion[n]
plot(1:n, countnA, cex=.7, xlab = 'Nth iteration of randomised play-the-winner rule', ylab = 'No. of subjects in each treatment arm')
points(countnB, col = "red", cex=.7)
lines(countnA)
lines(countnB, col='red')