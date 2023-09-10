
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
N <- 200

# number of simulations
sim_number <- 1000

x <- sort(rep(d_mean,sim_number))
y1 <- rep(0,length(d_mean)*sim_number)
y2 <- rep(0,length(d_mean)*sim_number)


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
      y1[(sim_number*(j-1)+1):(sim_number*j)] <- result$d.rep
      y2[(sim_number*(j-1)+1):(sim_number*j)] <- result$d.ll
  }
  
  df <- data.frame(x = x,y = y1)
  p1 <- ggplot(data = df,aes(x = x,y = y)) + 
    stat_sum() + ggtitle(paste0("Replacement:",Ns,"+",Nn)) +
    xlab("True d'") + ylab("Calculated d'") + theme(legend.position = "none")
  df <- data.frame(x = x,y = y2)
  p2 <- ggplot(data = df,aes(x = x,y = y)) + 
    stat_sum() + ggtitle(paste0("Log-linear:",Ns,"+",Nn)) +
    xlab("True d'") + ylab("Calculated d'") + theme(legend.position = "none")
}

ggarrange(p1,p2,nrow=1,ncol=2)









