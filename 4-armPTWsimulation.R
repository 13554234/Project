#Instead of a binary clinical trial we are now looking at multi-arm trials
nA <- 10
nB <- 10
nC <- 10
nD <- 10

#Count
countnA <- c()
countnB <- c()
countnC <- c()
countnD <- c()


#We need probabilities for the treatments being successful depending on whether they're from group A or B
success_A <- 0.9
success_B <- 0.4
success_C <- 0.5
success_D <- 0.7

#So from the urn we pull a ball, if ball is from group A and we observe it's a success we add beta balls back into urn
n <- 100
beta <- 0
alpha <- 1
characters <- c(rep('A', nA), rep('B', nB))
random_characters <- sample(characters)
random_ball <- sample(random_characters, size = 1)
#Or this does the same thing
ball <- sample(c("A", "B", "C", "D"), 1, prob = c(nA, nB, nC, nD))
for (i in 1:n) {
  ball <- sample(c("A", "B", "C", "D"), 1, prob = c(nA, nB, nC, nD))
  if(ball == 'A'){
    treat <- sample(c(0,1), 1, prob = c(success_A, 1-success_A))
    if(treat == 0){
      nA <- nA + alpha
      nB <- nB + beta
      nC <- nC + beta
      nD <- nD + beta
    }else{
      nA <- nA + beta
      nB <- nB + beta
      nC <- nC + beta
      nD <- nD + beta
    }               
  }else{
    if(ball == 'B'){
      treat <- sample(c(0,1), 1, prob = c(success_B, 1-success_B))
      if(treat == 0){
        nA <- nA + beta
        nB <- nB + alpha
        nC <- nC + beta
        nD <- nD + beta
      }else{
        nA <- nA + beta
        nB <- nB + beta
        nC <- nC + beta
        nD <- nD + beta
      }
    }else{
      if(ball == 'C'){
        treat <- sample(c(0,1), 1, prob = c(success_C, 1-success_C))
        if(treat == 0){
          nA <- nA + beta
          nB <- nB + beta
          nC <- nC + alpha
          nD <- nD + beta
        }else{
          nA <- nA + beta
          nB <- nB + beta
          nC <- nC + beta
          nD <- nD + beta
        }
      }else{
        treat <- sample(c(0,1), 1, prob = c(success_D, 1-success_D))
        if(treat == 0){
          nA <- nA + beta
          nB <- nB + beta
          nC <- nC + beta
          nD <- nD + alpha
        }else{
          nA <- nA + beta
          nB <- nB + beta
          nC <- nC + beta
          nD <- nD + beta
        }
      }}}
  countnA <- c(countnA, nA)
  countnB <- c(countnB, nB)
  countnC <- c(countnC, nC)
  countnD <- c(countnD, nD)
}

countnA
countnB
countnC
countnD
proportion <- c(countnA/(countnA+countnB+countnC+countnD))
proportion
proportion[100]
plot(1:n, countnA, cex=.4)
points(countnB, col = "red", cex=.4)
points(countnC, col = "blue", cex=.4)
points(countnD, col = "green", cex=.4)
lines(countnA)
lines(countnB, col='red')
lines(countnC, col = "blue")
lines(countnD, col='green')

