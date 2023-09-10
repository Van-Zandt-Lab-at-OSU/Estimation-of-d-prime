
# make plots for Simulation 2: the group

source("simulation2_group_ribbon_functions.R")

set.seed(117)

# specify values

d_mean <- seq(0.1,3.9,0.3)
c_mean <- 0
d_sd <- 0.5
c_sd <- 0
Nn <- 100
Ns <- 100
M <- 20
sim_number <- 1000
replacement_value <- 0.5
probs <- c(0.025,0.1,0.25,0.5,0.75,0.9,0.975)


# get the simulation results and ribbon lines
result <- group_simulation(d_mean.t=d_mean,
                           c_mean.t=c_mean,
                           d_sd.t=d_sd,
                           c_sd.t=c_sd,
                           Nn=Nn,
                           Ns=Ns,
                           M=M,
                           sim_number=sim_number,
                           replacement_value=replacement_value,
                           probs = probs)

# ribbon plot 1: true d' values
p1 <- ribbon_plots(p=result$p.t,
                   d_mean=d_mean,
                   d_sd=d_sd,
                   M=M,
                   title=paste("True - Sample:",M,",trials:",Ns,"+",Nn),
                   reference=0,
                   p_ref=result$p.t)

# ribbon plot 2: replacement estimations

p2 <- ribbon_plots(p=result$p.rep,
                   d_mean=d_mean,
                   d_sd=d_sd,
                   M=M,
                   title=paste("Rep 0.5 - Sample:",M,",trials:",Ns,"+",Nn),
                   reference=1,
                   p_ref=result$p.t)

# ribbon plot 3: log-linear estimations

p3 <- ribbon_plots(p=result$p.ll,
                   d_mean=d_mean,
                   d_sd=d_sd,
                   M=M,
                   title=paste("log linear - Sample:",M,",trials:",Ns,"+",Nn),
                   reference=1,
                   p_ref=result$p.t)

# plot using ggarrange

ggarrange(p1,p2,p3,nrow=1,ncol=3)

