
# check the impact of different replacement values

source("simulation2_group_ribbon_functions.R")

set.seed(117)

# specify values

d_mean <- seq(0.1,3.9,0.3)
c_mean <- 0
d_sd <- 0.5
c_sd <- 0
Nn <- 20
Ns <- 20
M <- 60
sim_number <- 1000
probs <- c(0.025,0.1,0.25,0.5,0.75,0.9,0.975)

replacement_values <- c(0.1,0.5,1,2)

# store plots in a list

plots <- list()

for(i in 1:length(replacement_values)){
  result <- group_simulation(d_mean.t=d_mean,
                             c_mean.t=c_mean,
                             d_sd.t=d_sd,
                             c_sd.t=c_sd,
                             Nn=Nn,
                             Ns=Ns,
                             M=M,
                             sim_number=sim_number,
                             replacement_value=replacement_values[i],
                             probs = probs)
  
  plots[[i]] <- ribbon_plots(p=result$p.rep,
                             d_mean=d_mean,
                             d_sd=d_sd,
                             M=M,
                             title=paste("Replacement value:",replacement_values[i]),
                             reference=1,
                             p_ref=result$p.t)
 print(i)
}

# plot using ggarrange

ggarrange(plots[[1]],plots[[2]],
          plots[[3]],plots[[4]],
          nrow=2,ncol=2)

