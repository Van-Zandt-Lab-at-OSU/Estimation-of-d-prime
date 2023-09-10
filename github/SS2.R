
require(ggplot2)
require(ggpubr)

source("signal_detection_theory_model.R")

# This function performs simulations of group

group_simulation <- function(d_mean.t=seq(0.1,3.9,0.3), # expecting a sequence
                             c_mean.t,  # expecting a value
                             d_sd.t,    # expecting a value
                             c_sd.t,    # expecting a value
                             Nn = 20,
                             Ns = 20,
                             M = 20,    # number of participants in the group
                             sim_number = 100,
                             replacement_value=0.5, # expecting one value
                             ll_value=0.5
                             # probs to be plotted on the ribbon plot
){
  
  
  
  # recording true and estimated d' values
  d.values.t <- array(dim=c(M,sim_number))
  d.est.rep <- array(dim=c(M,sim_number))
  d.est.ll <- array(dim=c(M,sim_number))
  
  # simulation
  for(j in 1:length(d_mean.t)){
    for(k in 1:M){
      result <- sdt_simulation(Nn=Nn,
                               Ns=Ns,
                               sim_number=sim_number,
                               d_mean.t=d_mean.t[j],
                               c_mean.t=c_mean.t,
                               d_sd.t=d_sd.t,
                               c_sd.t=c_sd.t,
                               replacement_values=replacement_value,
                               ll_values=ll_value)
      
      d.values.t[k,] <- result$d.t - d_mean.t[j]
      d.est.rep[k,] <- result$d.rep[1,] - d_mean.t[j]
      d.est.ll[k,] <- result$d.ll - d_mean.t[j]
    }
  }
  
  # result
  result <- list(d.t=d.values.t ,
                 d.rep=d.est.rep,
                 d.ll=d.est.ll,
                 d_mean=d_mean.t)
  
  return(result)
  
}


################ sample

# specify values

set.seed(117)

d_mean <- c(1,2,3)
c_mean <- c(0,0.3,0.6)
d_sd <- c(0.4,0.8,1.2)
Nn <- 20
Ns <- 20
M <- 60
sim_number <- 1000
replacement_value <- 0.5


d_dist <- array(dim=c(3,3,3))

for(i in 1:3){
  for(j in 1:3){
    for(k in 1:3){
      result <- group_simulation(d_mean.t=d_mean[i],
                                  c_mean.t=c_mean[j],
                                  d_sd.t=d_sd[k],
                                  c_sd.t=0,
                                  Nn=Nn,
                                  Ns=Ns,
                                  M=M,
                                  sim_number=sim_number,
                                  replacement_value=replacement_value)    
      d_dist[i,j,k] <- mean(result$d.rep)
    }
  }
  print(c(i,j,k))
}




par(mfrow=c(1,3))

for(j in 1:3){
  plot.new()
  plot.window(xlim=c(0,4),ylim=c(-0.4,0.5))
  axis(1)
  axis(2)
  lines(d_mean,d_dist[,j,1],type='o',col="blue")
  lines(d_mean,d_dist[,j,2],type='o',col="purple")
  lines(d_mean,d_dist[,j,3],type='o',col='red')
  abline(h=0)
  title(main=paste0("Response bias c=",c_mean[j]),
        xlab="d' mean value",ylab="Estimated d' mean - true d' mean")
  legend(2.5,0.4, legend=c("d' SD=0.4","d' SD=0.8","d' SD=1.2"),
         col=c("blue","purple",'red'),lty=1, cex=1)
}

