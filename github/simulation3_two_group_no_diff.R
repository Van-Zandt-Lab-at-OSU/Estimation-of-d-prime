
# This script provides the code and plot for two groups with no differences


# use functions
source("t_test_functions.R")

# seed 117
set.seed(117)


# reference group

M1 <- 60
Nn1 <- 20
Ns1 <- 20
d_mean1 <- 2.5
d_sd1 <- 0.5
c_mean1 <- 0
c_sd1 <- 0
sim_number <- 1000

par(mfrow=c(2,2))

####################################
# Plot 1: different number of trials

# group for comparison
M2 <- 60
Nn2 <- c(20,40,60,80,100)
Ns2 <- c(20,40,60,80,100)
d_mean2 <- 2.5
d_sd2 <- 0.5
c_mean2 <- 0
c_sd2 <- 0

# get simulation results and make the plot

prop.t <- rep(0,length(Ns2))
prop.rep <- rep(0,length(Ns2))
prop.ll <- rep(0,length(Ns2))

for(i in 1:length(Nn2)){
  result <- group_comparison_t_test(d_mean1=d_mean1,
                                    c_mean1=c_mean1,
                                    d_sd1=d_sd1,
                                    c_sd1=c_sd1,
                                    d_mean2=d_mean2,
                                    c_mean2=c_mean2,
                                    d_sd2=d_sd2,
                                    c_sd2=c_sd2,
                                    Nn1=Nn1,
                                    Ns1=Ns1,
                                    M1=M1,
                                    Nn2=Nn2[i],
                                    Ns2=Ns2[i],
                                    M2=M2,
                                    sim_number=sim_number,
                                    replacement_value=0.5, 
                                    two.groups=1) 
  prop.t[i] <- sum(ifelse(result$p.t<0.05,1,0))/sim_number
  prop.rep[i] <- sum(ifelse(result$p.rep<0.05,1,0))/sim_number
  prop.ll[i] <- sum(ifelse(result$p.ll<0.05,1,0))/sim_number
}

# plot
plot.new()
plot.window(xlim=c(0,120),ylim=c(0,1))  
axis(1)
axis(2)
lines(Ns2,prop.rep,type="o")
lines(Ns2,prop.ll,lty=3,type="o")
lines(Ns2,prop.t,col="grey",type="o")
abline(v=Ns1,col="red")
title(xlab="Trials per signal/noise",ylab="Prop of significance",
      main=paste("Trial size"))
legend(0,0.95, legend=c("Rep05", "LogL","True"),
       col=c("black","black","grey"),
       lty=c(1,3,1), cex=0.8)






####################################
# Plot 2: different number of samples

# group for comparison
M2 <- c(20,40,60,80,100)
Nn2 <- 20
Ns2 <- 20
d_mean2 <- 2.5
d_sd2 <- 0.5
c_mean2 <- 0
c_sd2 <- 0

# get simulation results and make the plot

prop.t <- rep(0,length(M2))
prop.rep <- rep(0,length(M2))
prop.ll <- rep(0,length(M2))

for(i in 1:length(M2)){
  result <- group_comparison_t_test(d_mean1=d_mean1,
                                    c_mean1=c_mean1,
                                    d_sd1=d_sd1,
                                    c_sd1=c_sd1,
                                    d_mean2=d_mean2,
                                    c_mean2=c_mean2,
                                    d_sd2=d_sd2,
                                    c_sd2=c_sd2,
                                    Nn1=Nn1,
                                    Ns1=Ns1,
                                    M1=M1,
                                    Nn2=Nn2,
                                    Ns2=Ns2,
                                    M2=M2[i],
                                    sim_number=sim_number,
                                    replacement_value=0.5, 
                                    two.groups=1) 
  prop.t[i] <- sum(ifelse(result$p.t<0.05,1,0))/sim_number
  prop.rep[i] <- sum(ifelse(result$p.rep<0.05,1,0))/sim_number
  prop.ll[i] <- sum(ifelse(result$p.ll<0.05,1,0))/sim_number
}

# plot
plot.new()
plot.window(xlim=c(0,120),ylim=c(0,1))  
axis(1)
axis(2)
lines(M2,prop.rep,type="o")
lines(M2,prop.ll,lty=3,type="o")
lines(M2,prop.t,col="grey",type="o")
abline(v=M1,col="red")
title(xlab="Participants in Group 2",ylab="Prop of significance",
      main=paste("Sample size"))
legend(0,0.95, legend=c("Rep05", "LogL","True"),
       col=c("black","black","grey"),
       lty=c(1,3,1), cex=0.8)



####################################
# Plot 3: standard deviation

# group for comparison
M2 <- 60
Nn2 <- 20
Ns2 <- 20
d_mean2 <- 2.5
d_sd2 <- c(0.2,0.5,0.8,1.1,1.4)
c_mean2 <- 0
c_sd2 <- 0

# get simulation results and make the plot

prop.t <- rep(0,length(d_sd2))
prop.rep <- rep(0,length(d_sd2))
prop.ll <- rep(0,length(d_sd2))

for(i in 1:length(d_sd2)){
  result <- group_comparison_t_test(d_mean1=d_mean1,
                                    c_mean1=c_mean1,
                                    d_sd1=d_sd1,
                                    c_sd1=c_sd1,
                                    d_mean2=d_mean2,
                                    c_mean2=c_mean2,
                                    d_sd2=d_sd2[i],
                                    c_sd2=c_sd2,
                                    Nn1=Nn1,
                                    Ns1=Ns1,
                                    M1=M1,
                                    Nn2=Nn2,
                                    Ns2=Ns2,
                                    M2=M2,
                                    sim_number=sim_number,
                                    replacement_value=0.5, 
                                    two.groups=1) 
  prop.t[i] <- sum(ifelse(result$p.t<0.05,1,0))/sim_number
  prop.rep[i] <- sum(ifelse(result$p.rep<0.05,1,0))/sim_number
  prop.ll[i] <- sum(ifelse(result$p.ll<0.05,1,0))/sim_number
}

# plot
plot.new()
plot.window(xlim=c(0,1.5),ylim=c(0,1))  
axis(1)
axis(2)
lines(d_sd2,prop.rep,type="o")
lines(d_sd2,prop.ll,lty=3,type="o")
lines(d_sd2,prop.t,col="grey",type="o")
abline(v=d_sd1,col="red")
title(xlab="Group 2: standard deviation",ylab="Prop of significance",
      main=paste("Standard deviation"))
legend(0,0.95, legend=c("Rep05", "LogL","True"),
       col=c("black","black","grey"),
       lty=c(1,3,1), cex=0.8)





####################################
# Plot 4: response bias

# group for comparison
M2 <- 60
Nn2 <- 20
Ns2 <- 20
d_mean2 <- 2.5
d_sd2 <- 0.5
c_mean2 <- c(0,0.1,0.2,0.3,0.4,0.5)
c_sd2 <- 0

# get simulation results and make the plot

prop.t <- rep(0,length(c_mean2))
prop.rep <- rep(0,length(c_mean2))
prop.ll <- rep(0,length(c_mean2))

for(i in 1:length(c_mean2)){
  result <- group_comparison_t_test(d_mean1=d_mean1,
                                    c_mean1=c_mean1,
                                    d_sd1=d_sd1,
                                    c_sd1=c_sd1,
                                    d_mean2=d_mean2,
                                    c_mean2=c_mean2[i],
                                    d_sd2=d_sd2,
                                    c_sd2=c_sd2,
                                    Nn1=Nn1,
                                    Ns1=Ns1,
                                    M1=M1,
                                    Nn2=Nn2,
                                    Ns2=Ns2,
                                    M2=M2,
                                    sim_number=sim_number,
                                    replacement_value=0.5, 
                                    two.groups=1) 
  prop.t[i] <- sum(ifelse(result$p.t<0.05,1,0))/sim_number
  prop.rep[i] <- sum(ifelse(result$p.rep<0.05,1,0))/sim_number
  prop.ll[i] <- sum(ifelse(result$p.ll<0.05,1,0))/sim_number
}

# plot
plot.new()
plot.window(xlim=c(-0.1,0.6),ylim=c(0,1))  
axis(1)
axis(2)
lines(c_mean2,prop.rep,type="o")
lines(c_mean2,prop.ll,lty=3,type="o")
lines(c_mean2,prop.t,col="grey",type="o")
abline(v=c_mean1,col="red")
title(xlab="Group 2: response bias",ylab="Prop of significance",
      main=paste("Response bias"))
legend(0,0.95, legend=c("Rep05", "LogL","True"),
       col=c("black","black","grey"),
       lty=c(1,3,1), cex=0.8)
