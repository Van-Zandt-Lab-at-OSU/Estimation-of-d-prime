
# This script provides the code and plot for two groups with true differences


# use functions
source("t_test_functions.R")

# seed 117
set.seed(117)

# simulation values
M <- 60
Nn <- c(20,100)
Ns <- c(20,100)
d_mean <- seq(0.1,3.9,0.3)
c_mean <- 0
d_sd <- 0.5
c_sd <- 0
sim_number <- 1000
effect_size <- c(0.2,0.5,0.8)

# get simulation results and make plots

par(mfrow=c(2,3))

for(i in 1:length(Nn)){
    
    # specify proportions of significant results
    prop.t <- rep(0,length(d_mean))
    prop.rep <- rep(0,length(d_mean))
    prop.ll <- rep(0,length(d_mean))
    
    for(k in 1:length(effect_size)){
      # start the plot
      plot.new()
      plot.window(xlim=c(0,4),ylim=c(0,1))     
      for(j in 1:length(d_mean)){
        result <- group_comparison_t_test(d_mean1=d_mean[j],
                                          c_mean1=c_mean,
                                          d_sd1=d_sd,
                                          c_sd1=c_sd,
                                          d_mean2=d_mean[j] + effect_size[k]*sqrt(d_sd^2),
                                          c_mean2=c_mean,
                                          d_sd2=d_sd,
                                          c_sd2=c_sd,
                                          Nn1=Nn[i],
                                          Ns1=Ns[i],
                                          M1=M,
                                          Nn2=Nn[i],
                                          Ns2=Ns[i],
                                          M2=M,
                                          sim_number=sim_number,
                                          replacement_value=0.5, 
                                          two.groups=1) 
        prop.t[j] <- sum(ifelse(result$p.t<0.05,1,0))/sim_number
        prop.rep[j] <- sum(ifelse(result$p.rep<0.05,1,0))/sim_number
        prop.ll[j] <- sum(ifelse(result$p.ll<0.05,1,0))/sim_number
      }
      # make lines
      lines(d_mean,prop.rep)
      lines(d_mean,prop.ll,lty=3)
      lines(d_mean,prop.t,col="grey")
      # other elements
      axis(1)
      axis(2)
      title(xlab="d' values",ylab="Prop of significance",
            main=paste("Effect",effect_size[k],"trial",Ns[i],"+",Nn[i]))
      legend(0.5,ifelse(k==1,0.95,0.4), legend=c("Rep05", "LogL","True"),
             col=c("black","black","grey"),
             lty=c(1,3,1), cex=0.8)
    }
}



