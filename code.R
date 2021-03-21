library(stats)
library(nloptr)
# input the bond data
BondData <- matrix(ncol=4,nrow=29)
colnames(BondData) <- c('Time to nexr payment','Time to maturity','Coupon rate','Clean price')
BondData[1,] <- c(0.4356,0.4356,0.875,100.3)
BondData[2,]<-c(0.2644,0.7644,0.875,100.48)
BondData[3,]<-c(0.2658,1.2658,0.75,100.5)
BondData[4,]<-c(0.4342,1.9342,0.625,100.31)
BondData[5,]<-c(0.0192,2.0192,0.375,99.78)
BondData[6,]<-c(0.4753,2.9753,0.75,100.16)
BondData[7,]<-c(0.3534,3.3534,1.5,102.34)
BondData[8,]<-c(0.1,3.6,1.75,103.8)
BondData[9,]<-c(0.2685,4.2685,2.125,104.19)
BondData[10,]<-c(0.4342,4.9342,1.75,102.06)
BondData[11,]<-c(0.2274,5.2274,4.5,115.91)
BondData[12,]<-c(0.1027,5.6027,2.375,104.36)
BondData[13,]<-c(0.2712,6.2712,2.750,105.86)
BondData[14,]<-c(0.4370,6.9370,2.375,102.97)
BondData[15,]<-c(0.4822,7.4822,3.5,110.53)
BondData[16,]<-c(0.2260,7.7260,3.875,113.09)
BondData[17,]<-c(0.4822,8.4822,2.750,103.98)
BondData[18,]<-c(0.2260,8.7260,3.125,106.50)
BondData[19,]<-c(0.2301,9.2301,3.375,108)
BondData[20,]<-c(0.4808,9.9808,2.625,101.19)
BondData[21,]<-c(0.4932,25.4932,4.5,117.58)
BondData[22,]<-c(0.4959,26.4959,4.75,122.28)
BondData[23,]<-c(0.2397,26.7397,5,126.97)
BondData[24,]<-c(0.4959,27.4959,4.375,115.19)
BondData[25,]<-c(0.2397,27.7397,4.5,117.47)
BondData[26,]<-c(0.4959,28.4959,3.5,98.98)
BondData[27,]<-c(0.2397,28.7397,4.25,112.44)
BondData[28,]<-c(0.2438,29.2438,4.375,114.67)
BondData[29,]<-c(0.4945,29.9945,2.875,105.75)

# calculate the dirty price
DirtyPrice<-c()
for (i in 1:29) {
  DirtyPrice[i]<- (0.5 - BondData[i,1]) * BondData[i,3] + BondData[i,4]
}

# define a function to obtain the objective, which should be minimized
Objective <- function(x){
  
  # take 4 input parameters
  beta0 <- x[1]
  beta1 <- x[2]
  beta2 <- x[3]
  tau1 <- x[4]
  
  # the function for R(0, t)
  R <- function(t, beta0, beta1, beta2, tau1){
    result <- beta0 + beta1 * ((1 - exp(-t / tau1)) / (t / tau1)) + beta2 * ((1 - exp(-t / tau1)) / (t / tau1) - exp(-t / tau1))
    return(result)
  }
  
  # calculate the theoretical price for each bond
  TheoPrice <- function(i){
    
    eR_sum <- 0
    PayTimes <- (BondData[i, 2] - BondData[i, 1]) / 0.5 + 1
    ti <- c()
    
    for(j in (1 : PayTimes)){
      ti[j] <- BondData[i, 1] + 0.5 * (j - 1)
      eR_sum <- eR_sum + exp(-R(ti[j], beta0, beta1, beta2, tau1) * ti[j])
    }
    
    TPresult <- eR_sum * 0.5 * BondData[i, 3] + exp(-R(BondData[i, 2], beta0, beta1, beta2, tau1) * BondData[i, 2]) * 100
    return(TPresult)
  }
  
# calculate the deviation and obtain the objective
  List <- c()
  tmp <- 0
  Objsum <- 0
  for(k in 1 : 29){
    List[k] <- TheoPrice(k)
    tmp <- (1 / BondData[k, 2]) * (List[k] - DirtyPrice[k])^2
    Objsum <- Objsum + tmp
  }
  return(Objsum)
}

# result for 4 parameters
Solution1 <- bobyqa(c(0, 0, 0, 2), Objective, lower = c(-1, -1, -1, 1), upper = c(1, 1, 1, 5))
Solution1$par
Solution1$value

Solution2 <- optim(c(0, 0, 0, 2), Objective)
Solution2$par
Solution2$value


