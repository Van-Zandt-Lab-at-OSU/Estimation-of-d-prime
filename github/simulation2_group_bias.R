
# make plots for Simulation 2: the group

source("simulation2_group_ribbon_functions.R")

set.seed(117)




################ sample

# specify values

d_mean <- seq(0.1,3.9,0.3)
c_mean <- 0
d_sd <- 0.5
Nn <- 20
Ns <- 20
M <- 60
sim_number <- 1000
replacement_value <- 0.5
probs <- c(0.025,0.1,0.25,0.5,0.75,0.9,0.975)


# get the simulation results and ribbon lines
result1 <- group_simulation(d_mean.t=d_mean,
                           c_mean.t=c_mean,
                           d_sd.t=d_sd,
                           c_sd.t=0,
                           Nn=Nn,
                           Ns=Ns,
                           M=M,
                           sim_number=sim_number,
                           replacement_value=replacement_value,
                           probs = probs)

# ribbon plot 2: replacement estimations

p1.1 <- ribbon_plots(p=result1$p.rep,
                   d_mean=d_mean,
                   d_sd=d_sd,
                   M=M,
                   title=paste("Rep 0.5 - bias:",0),
                   reference=0,
                   p_ref=result$p.t)

# ribbon plot 3: log-linear estimations

p1.2 <- ribbon_plots(p=result1$p.ll,
                   d_mean=d_mean,
                   d_sd=d_sd,
                   M=M,
                   title=paste("log linear - bias:",0),
                   reference=0,
                   p_ref=result$p.t)

# get the simulation results and ribbon lines
result2 <- group_simulation(d_mean.t=d_mean,
                           c_mean.t=c_mean,
                           d_sd.t=d_sd,
                           c_sd.t=0.25,
                           Nn=Nn,
                           Ns=Ns,
                           M=M,
                           sim_number=sim_number,
                           replacement_value=replacement_value,
                           probs = probs)

# ribbon plot 2: replacement estimations

p2.1 <- ribbon_plots(p=result2$p.rep,
                     d_mean=d_mean,
                     d_sd=d_sd,
                     M=M,
                     title=paste("Rep 0.5 - bias:",0.25),
                     reference=0,
                     p_ref=result$p.t)

# ribbon plot 3: log-linear estimations

p2.2 <- ribbon_plots(p=result2$p.ll,
                     d_mean=d_mean,
                     d_sd=d_sd,
                     M=M,
                     title=paste("log linear - bias:",0.25),
                     reference=0,
                     p_ref=result$p.t)


# get the simulation results and ribbon lines
result3 <- group_simulation(d_mean.t=d_mean,
                           c_mean.t=c_mean,
                           d_sd.t=d_sd,
                           c_sd.t=0.5,
                           Nn=Nn,
                           Ns=Ns,
                           M=M,
                           sim_number=sim_number,
                           replacement_value=replacement_value,
                           probs = probs)

# ribbon plot 2: replacement estimations

p3.1 <- ribbon_plots(p=result3$p.rep,
                     d_mean=d_mean,
                     d_sd=d_sd,
                     M=M,
                     title=paste("Rep 0.5 - bias:",0.5),
                     reference=0,
                     p_ref=result$p.t)

# ribbon plot 3: log-linear estimations

p3.2 <- ribbon_plots(p=result3$p.ll,
                     d_mean=d_mean,
                     d_sd=d_sd,
                     M=M,
                     title=paste("log linea - bias:",0.5),
                     reference=0,
                     p_ref=result$p.t)



# plot using ggarrange

ggarrange(p1.1,p2.1,p3.1,p1.2,p2.2,p3.2,nrow=2,ncol=3)

