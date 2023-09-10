
# This script provides the code and plot for one-sample t-test simulations


# use functions
source("t_test_functions.R")

# seed 117
set.seed(117)

# simulation values
M <- c(20,60,100)
Nn <- c(20,100)
Ns <- c(20,100)
d_mean <- seq(0.1,3.9,0.3)
c_mean <- 0
d_sd <- c(0.5,1,1.5)
c_sd <- 0
sim_number <- 1000
colors <- c("red","blue","orange")

# get simulation results and make plots

par(mfrow=c(2,3))

for(i in 1:length(Nn)){
  for(j in 1:length(M)){
    
    # specify proportions of significant results
    prop.t <- rep(0,length(d_mean))
    prop.rep <- rep(0,length(d_mean))
    prop.ll <- rep(0,length(d_mean))
    
    # start the plot
    plot.new()
    plot.window(xlim=c(0,4),ylim=c(0,0.3))
    
    # simulation
    for(l in 1:length(d_sd)){
      # simulate for each sd value
      for(k in 1:length(d_mean)){
        result <- group_comparison_t_test(d_mean1=d_mean[k],
                                          c_mean1=c_mean,
                                          d_sd1=d_sd[l],
                                          c_sd1=c_sd,
                                          Nn1=Nn[i],
                                          Ns1=Ns[i],
                                          M1=M[j],
                                          sim_number=sim_number,
                                          replacement_value=0.5, 
                                          two.groups=0) 
        prop.t[k] <- sum(ifelse(result$p.t<0.05,1,0))/sim_number
        prop.rep[k] <- sum(ifelse(result$p.rep<0.05,1,0))/sim_number
        prop.ll[k] <- sum(ifelse(result$p.ll<0.05,1,0))/sim_number
      }
      # make lines in this plot
      lines(d_mean,prop.rep,col=colors[l],type="o")
      lines(d_mean,prop.ll,col=colors[l],lty=3,type="o")
    }
    # add other elements to the plot
    abline(h=0.05,col="grey")
    axis(1)
    axis(2)
    title(xlab="d' values",ylab="Prop of significance",
          main=paste("Sample",M[j],"trial",Ns[i],"+",Nn[i]))
    legend(0.5,0.3, legend=c("Rep05", "LogL",paste0("SD:",d_sd)),
           col=c("black","black",colors),
           lty=c(1,3,rep(1,length(d_sd))), cex=0.8)
  }
}



