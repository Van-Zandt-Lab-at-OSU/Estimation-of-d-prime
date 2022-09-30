

# This code is to generate results for Simulation 1: Individuals
# Seed 117 is used

set.seed(117)

# source the function

source("signal_detection_theory_model.R")

# Distributions to be used in the simulation
d_mean <- seq(0.1,3.9,0.2)
c_mean <- 0
d_sd <- 0
c_sd <- 0

# Sample size: 20+20 and 100+100
N <- c(40,200)

# array and function to be used to create the pixel plot
d_units <- c(-99,seq(-1,5,0.2),99)

unit_prob <- function(d,d_units){
  prob <- rep(0,length(d_units)-1)
  for(i in 1:length(d)){
    j <- 1
    while(d[i]>d_units[j]){
      j <- j+1
    }
    prob[j-1] <- prob[j-1]+1
  }
  prob <- prob/length(d)
  return(prob)
}

# number of simulations
sim_number <- 1000

# examine (1) replacement with 0.5; (2) log-linear
prob_t <- array(dim=c(length(d_mean),length(d_units)-1))        # true
prob_rep05 <- array(dim=c(length(d_mean),length(d_units)-1))
prob_ll <- array(dim=c(length(d_mean),length(d_units)-1))

# plot 2*2
par(mfrow=c(2,2))

# plotting functions: color
grades <- c(0,0.01,seq(0.05,0.5,0.05),1.1)
colors <- c("white",paste0("grey",seq(88,8,-8)),"black")

find_color <- function(p,grades,colors){
  j <- 1
  while(p>grades[j]){
    j <- j+1
  }
  return(colors[j])
}

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
      prob_t[j,] <- unit_prob(result$d.t,d_units)
      prob_rep05[j,] <- unit_prob(result$d.rep[1,],d_units)
      prob_ll[j,] <- unit_prob(result$d.ll,d_units)
  }
  
  
# make Plot 1: true d' against estimated d'
  plot.new()
  plot.window(xlim=c(0,4),ylim=c(-1,5))
  
  for(i in 1:(dim(prob_rep05)[1])){
    for(j in 2:(dim(prob_rep05)[2]-1)){
      col <- find_color(prob_rep05[i,j],grades,colors)
      rect(d_mean[i]-0.1,d_units[j],d_mean[i]+0.1,d_units[j+1],col=col,border=NA)
    }
  }
  axis(1)
  axis(2)
  abline(0,1)
  title(main=paste0("Replacement: ",Ns,"+",Nn),xlab="True d'",ylab="Estimated d'")
  
# make Plot 2: true d' against estimated differences
  plot.new()
  plot.window(xlim=c(0,4),ylim=c(-2,2))
  
  for(i in 1:(dim(prob_rep05)[1])){
    for(j in 2:(dim(prob_rep05)[2]-1)){
      col <- find_color(prob_rep05[i,j],grades,colors)
      rect(d_mean[i]-0.1,d_units[j]-d_mean[i],
           d_mean[i]+0.1,d_units[j+1]-d_mean[i],col=col,border=NA)
    }
  }
  axis(1)
  axis(2)
  abline(h=0)
  title(main=paste0("Replacement: ",Ns,"+",Nn),xlab="True d'",
        ylab="Estimation-true difference")  

}











