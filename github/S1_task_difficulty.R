


require(ggplot2)
require(ggpubr)

# This code is to generate results for Simulation 1: Individuals
# Seed 117 is used

set.seed(117)

# source the function

source("signal_detection_theory_model.R")

# Distributions to be used in the simulation
d_mean <- seq(0.1,3.9,0.3)
c_mean <- 0
d_sd <- 0
c_sd <- 0

# Sample size: 20+20 and 100+100
N <- 40

# number of simulations
sim_number <- 1000

x <- sort(rep(d_mean,sim_number))
y <- rep(0,length(d_mean)*sim_number)

y_mean <- d_mean

# simulation
for(i in 1:length(N)){
  
  # signal and noise trials
  Ns <- Nn <- N[i]/2
  
  for(j in 1:length(d_mean)){
    result <- sdt_simulation(Nn=Nn,
                             Ns=Ns,
                             sim_number=sim_number,
                             d_mean.t=d_mean[j],
                             c_mean.t=c_mean,
                             d_sd.t=d_sd,
                             c_sd.t=c_sd,
                             replacement_values=c(0.5),
                             replacement=1,
                             log_linear=1)
    y[(sim_number*(j-1)+1):(sim_number*j)] <- result$d.rep
    y_mean[j] <- mean(result$d.rep)
  }
  
  df <- data.frame(x = x,y = y)
  sub <- data.frame(x=d_mean,y=y_mean)
  
  p <- ggplot(data = df,aes(x = x,y = y)) + 
    stat_sum() + ggtitle("Impact of task difficulty (d' values)") +
    xlab("True d'") + ylab("Estimated d'") + theme(legend.position = "none") +
    geom_abline(slope=1, intercept=0,col="blue") +
    geom_point(data=sub,aes(x=x,y=y),col='red')
  
}

p




