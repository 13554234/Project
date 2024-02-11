#Start with an urn with an equal number of red, black and white balls where red represents subjects in the treatment group (A) and black represents subjects in the control group (B) and white balls are migration balls
nA <- 100
nB <- 100
n0 <- 10

#Count
countnA <- c()
countnB <- c()
countn0 <- c()

#We need probabilities for the treatments being successful depending on whether they're from group A or B
success_A <- 0.8
success_B <- 0.2

#So from the urn we pull a ball:
n <- 100
alpha <- 1
for (i in 1:n) {
  ball <- sample(c("A", "B","0"), 1, prob = c(nA, nB, n0))
  if(ball == 'A'){
    treat <- sample(c(0,1), 1, prob = c(success_A, 1-success_A))
    if(treat == 0){
      nA <- nA 
    }else{
      nA <- nA - 1
    }               
  }else{
    if(ball == 'B'){
      treat <- sample(c(0,1), 1, prob = c(success_B, 1-success_B))
      if(treat == 0){
        nB <- nB 
      }else{
        nB <- nB - 1
      }}else{
        nA <- nA + alpha
        nB <- nB + alpha
      }
  }
  countnA <- c(countnA, nA)
  countnB <- c(countnB, nB)
}

countnA
countnB
proportion <- c(countnA/(countnA+countnB))
proportion[n]
plot(1:n, countnA, cex=.4, ylim=c(70,100))
points(countnB, col = "red", cex=.4)
lines(countnA)
lines(countnB, col='red')
