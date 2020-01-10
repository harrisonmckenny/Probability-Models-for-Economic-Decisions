#Probability Models for Economic Decisions 
#1/5/2019 
#Harrison McKenny and M. Wolfetone Dempsey
#Chapter 2 Discrete Random Variables

#rm(list=ls())

pctr <-  function(x,xo) {length(x[x<= xo])/length(x)}

#Creating Figure 2.1 Discrete probability distribution for the number of entrants (K)
probability <- c(0.1,0.25,0.3,0.25,0.1)
NumOfCompetitiveEntrants <- c(1,2,3,4,5)
plot(NumOfCompetitiveEntrants,probability,type="h",xlim=c(0,6),
     ylim=c(0,1),xlab="Number Of Competitive Entrants (K)",ylab="Probability",
     main="Figure 2.1: Discrete Probability Distribution For The Number Of Entrants (K)",lwd=3,yaxt="n")
ticks <- c(0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0) #customizing y axis
axis(2,at=ticks,labels=ticks) #appending y axis
lines(x=c(0,1),y=c(0,0),lty=2)
lines(x=c(1,2),y=c(0.1,0.1),lty=2)
lines(x=c(2,3),y=c(0.35,0.35),lty=2)
lines(x=c(3,4),y=c(0.65,0.65),lty=2)
lines(x=c(4,5),y=c(0.9,0.9),lty=2)
lines(x=c(5,6),y=c(1,1),lty=2)
lines(x=c(2,2),y=c(0.2,0.35),lty=2)
lines(x=c(3,3),y=c(0.35,0.65),lty=2)
lines(x=c(4,4),y=c(0.65,0.9),lty=2)
lines(x=c(5,5),y=c(0.9,1),lty=2)

text(x=1.5,y=0.5,"Dashed lines: Cumulative probability",cex=0.6)

#Figure 2.2-----------Inverse cumulative-probability curve for number of entrants (K)

plot(x=0,ylim=c(0,6),xlim=c(0,1),ylab="Number Of Competitive Entrants (K)",xlab="Cumulative Probability",
     main="Figure 2.2: Inverse Cumulative Probability Distribution For The Number Of Entrants (K)",
     cex.main=.9,col='white',xaxt="n")
ticks <- c(0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0) #customizing y axis
axis(1,at=ticks,labels=ticks) #appending y axis
lines(y=c(0,1),x=c(0,0))
lines(y=c(1,2),x=c(0.1,0.1))
lines(y=c(2,3),x=c(0.35,0.35))
lines(y=c(3,4),x=c(0.65,0.65))
lines(y=c(4,5),x=c(0.9,0.9))
lines(y=c(1,1),x=c(0,.1))
lines(y=c(2,2),x=c(0.1,0.35))
lines(y=c(3,3),x=c(0.35,0.65))
lines(y=c(4,4),x=c(0.65,0.9))
lines(y=c(5,5),x=c(0.9,1))

#Figure 2.3 Simulation with a discrete probability distribution (four equivalent formulas)
a13 <- runif(1)
a13
a5_9 <- c(0.00,0.10,0.35,0.65,0.90) #P(K<k)
k <- seq(1,5,1) #K
c5_9 <- c(0.10,0.25,0.30,0.25,0.10)
c10 <- sum(c5_9) #should be 1 as this is the sum of probabilities
c10

g6_8 <- c()
for (i in 1:3){
  if (a5_9[i+1] <= a13 & a13 < a5_9[i+2]){
    g6_8[i] <- k[i+1]}
  else{
    g6_8[i] <- 0
  }
}
#Not needed reading further i can do this with this
g5 <- ifelse(a13<a5_9[2],k[1],0)
g5
g9 <- ifelse(a5_9[5]<=a13,k[5],0)
g9
g6_8

g5_9 <- c(g5,g6_8,g9) #(for B16)
g5_9
#running line 48-61 over will yeild different results each time. Equivilent to pressing f9


b13 <- sum(g5_9) #simulated value
b13
?ecdf
length(g5_9)-1
#Creating Myersons function DISCINV 
install.packages('FSA')
library(FSA)
pcumsum(c5_9)

values=k

DISCRINV <- function(x,values,probabilities){
  precumsum <- pcumsum(probabilities)
  middle <- c()
    for (i in 1:(length(values)-2)){
    if (precumsum[i+1] <= x & x < precumsum[i+2]){
      middle[i] <- values[i+1]}
      else{
        middle[i] <- 0
      }
    }
     
      firstrow <- ifelse(x < precumsum[2], values[1], 0)
      lastrow <- ifelse(precumsum[length(precumsum)] <= x , values[length(precumsum)] , 0)
      Gvector <- c(firstrow,middle,lastrow)
      print(firstrow)
      print(middle)
      print(lastrow)
      print(Gvector)
        simulatedvalue <-  sum(Gvector)
       
          return(simulatedvalue)
          }

# here-----------------

results <- DISCRINV(a13,k,c5_9)

results

#Figure 2.4 Making a simple simulation model of competitors and profit
fixedcosts <- 26
marketvalue <- 100
k <- seq(1,5,1) #K
c5_9 <- c(0.10,0.25,0.30,0.25,0.10)

profit <- marketvalue/(1+k)-fixedcosts
profit

table <- data.frame(k,profit,c5_9)
colnames(table) <- c("Competitors","Profit","Probability")
table

#Model 1 (Correct)
NumOfCompetitors1 <- 2
Model1Profit <- marketvalue/(1+NumOfCompetitors)-fixedcosts
Model1Profit
Model1 <- data.frame(NumOfCompetitors1,Model1Profit)
colnames(Model1) <- c("# Competitors entering","Profit")
Model1


#Model 2 (WRONG!!!)
NumOfCompetitors2 <- DISCRINV(runif(1),table$Competitors,table$Probability)
NumOfCompetitors2
Model2Profit <- DISCRINV(runif(1),profit,c5_9)
Model2Profit
Model2 <- data.frame(NumOfCompetitors2,Model2Profit)
colnames(Model2) <- c("# Competitors entering","Profit")
Model2

#run this model over several times and your profit will differ even though num of firms stays the same
#this is because this model treats number of competitors and profit as independent when they are not!!!!!!!!

#2.4 Expected  Value and Standard Deviation
#Figure 2.5 Expected values and standard deviations of discrete random variables
k <- seq(1,5,1) #K
c5_9 <- c(0.10,0.25,0.30,0.25,0.10)
fixedcosts <- 26
marketvalue <- 100
profit <- marketvalue/(1+k)-fixedcosts
profit

#SUMPRODUCT = crossprod
b13<- crossprod(k,c5_9) #Mean or E(K) Stdev(K)
b13

d5_9 <- c()
for (i in 1:length(k)){

  d5_9[i] <- (k[i]-b13)^2

  }

d5_9
table <- data.frame(k,c5_9,d5_9,profit)
colnames(table) <- c("k","P(K=k)","(k-E(K))^2","Profit")
table

d13 <- (crossprod(d5_9,c5_9))^0.5 #Stdev(K) non STDEVPR way
d13

#---------------------Creating STDEVPR Simtools function
STDEVPR <- function(values,probabilities){
  d <- c()
  meank<- crossprod(values,probabilities)
  for (i in 1:length(values)){
     d[i] <- (values[i]-meank)^2
  }
  stdevk <- (crossprod(d,probabilities)^0.5)
  
  return(stdevk)
}
#---------------------------------------------------
c13 <- STDEVPR(k,c5_9) #Stdevpr way

f13 <- crossprod(profit,c5_9) #E(Y)
f13
g13 <- STDEVPR(profit,c5_9) #Stdev(Y)
g13


#----------------Figure 2.6 Estimating an expected value and standard deviation from simulation data
k <- seq(1,5,1) #K
b3_7 <- c(0.10,0.25,0.30,0.25,0.10)
d7 <- crossprod(k,b3_7)
d7 #E(K)

e7 <- STDEVPR(k,b3_7)
e7 # Stdev(K)

b14 <- DISCRINV(runif(1),k,b3_7)
b14
Simtable <- seq(0,1,0.0025)
Simtable
samplesize <- length(Simtable)
samplesize

b15_415 <- c()

#Simulating 401 times
for (i in 1:401){
  b15_415[i] <- DISCRINV(runif(1),k,b3_7)
}
b15_415

b9 <- mean(b15_415) #Mean estimated from Simtable
b9

b10 <- sd(b15_415) #Stdev(K) estimated from Simtable
b10

d15_415 <- c()
for (i in 1:length(b15_415)){
  d15_415[i] <- (b15_415[i]-b9)^2
}
d15_415 #Squared deviations from sample mean
sum(d15_415)/(samplesize-1) #statistician recommended
d13 <- (sum(d15_415)/(samplesize-1))^0.5 #sample standard deviation
d13 #same as using the sd() function from above

table <- data.frame(Simtable,b15_415,d15_415)
colnames(table) <- c("Simtable","Sim'd K","Squared deviations from sample mean")
table

sorted <- table[order(table$`Sim'd K`),] #sorting sim values from smallest to largest

Fig2_7 <- data.frame(Simtable,sorted$`Sim'd K`)

#Figure 2.7 Estimate of an inverse cumulative distrivution from simulation data
#This plot will change slightly each time you run the above code
plot(x=Fig2_7$Simtable,y=sorted$`Sim'd K`,xlab="Cumulative Probability",
     ylab="Number Of Competitive Entrants (K)",xaxt="n",ylim=c(0,6),type="b",
     main="Estimate Of An Inverse Cumulative Distribution From Simulation Data",cex.main=.9)
ticks <- c(0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0) #customizing y axis
axis(1,at=ticks,labels=ticks)
#lines(y=c(1,2),x=c(0.1,0.1)) Maybe not needed? talk over with Dempsey
#lines(y=c(2,3),x=c(0.35,0.35))
#lines(y=c(3,4),x=c(0.65,0.65))
#lines(y=c(4,5),x=c(0.9,0.9))    

#Figure 2.8 A Spreadsheet for studying the properties of a sample mean
a3_7 <- seq(1,5,1) #K
b3_7 <- c(0.10,0.25,0.30,0.25,0.10)
samplesize <- 30

i30rv <- c()

#Simulating 30 times
for (i in 1:30){
  i30rv[i] <- DISCRINV(runif(1),a3_7,b3_7)
}
i30rv
sss30 <- matrix(i30rv,ncol=3,nrow = 10)
sss30

e8 <- crossprod(a3_7,b3_7) #E(K)
e8
e9 <- STDEVPR(a3_7,b3_7)#E(Stdev(K))
e9
e10 <- e9/(samplesize^0.5)
e10 #Stdev of sample mean

e15 <- mean(i30rv) #sample mean or average
e15

#In R you can make a random variable that has a normal probability distribution with mean m and std. dev sig. by
#qnorm(runif(1),m,sig)
#qnorm() = norm.inv()

#what should a normal dist with mean of 100 and standard dev of 20 look like?
test <- c()
for (i in 1:1000){
test[i] <- qnorm(runif(1),100,20) #looping to get 1000 random examples
}
test #examples


h12 <- qnorm(runif(1),e8,) # Calculated from probability distribution
h12

e16 <- sd(sss30) #Sample standard deviation
e16

e17 <- e16/(samplesize^0.5) #Estimated stdev of sample mean
e17 

e20 <- e15-1.96*e17 #lower bound of confidence interval
e20
f20 <- e15+1.96*e17#upper bound of confidence interval
f20

conint <- c(e20,f20) #95% confidence interval
conint

e22<- (e20<8 & e8 <f20) #E(K) actually in the interval?
e22

#rerunning lines 257 to 312 multiple times is equivalent to pressing the f9 key to look at e22 change
#this should be true about 95% of the time

#----------------------------------------Figure 2.9 Analysis iof the Superior Semiconductor case
fixedcosts <- 26
marketvalue <- 100
b6_10 <- seq(1,5,1) #K
c6_10 <- c(0.10,0.25,0.30,0.25,0.10)

profit <- marketvalue/(1+k)-fixedcosts
e6_10 <- profit
e6_10

#Computations from probability distribution
b14 <- crossprod(b6_10,c6_10) #E(K)
b14

e14 <- crossprod(profit,c6_10) #E(profit)
e14

#Simulation model
b18 <- DISCRINV(runif(1),b6_10,c6_10) #K in simulation 
b18

e18 <- marketvalue/(1+b18)-fixedcosts
e18

b28_428 <- c()

#some stuff i need to sort here

#Simulating 401 times
samplesize <- 401

for (i in 1:401){
  b28_428[i] <- DISCRINV(runif(1),b6_10,c6_10)
}
b28_428

profitsim <- c()
for (i in 1:length(b28_428)){
profitsim[i] <- marketvalue/(1+b28_428[i])-fixedcosts
}
profitsim

b23 <- quantile(profitsim,0.05)
b23

#Figure 2.10 cumulative risk profile from simulation data

Simtable <- seq(0,1,0.0025)
table <- data.frame(Simtable,profitsim)
colnames(table) <- c("Simtable","Profitsim")
table

sorted <- table[order(table$Profitsim),] #sorting sim values from smallest to largest

Fig2_7 <- data.frame(Simtable,sorted$Profitsim)

plot(Fig2_7,type="b",xlab="Cumulative Probability",ylab="Profit ($ Millions)",xaxt="n",
     main="Cumulative Risk Profile From Simulation Data",cex.main=.9)
ticks <- c(0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0) #customizing y axis
axis(1,at=ticks,labels = ticks)







