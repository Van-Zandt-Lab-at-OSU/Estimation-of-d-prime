

require(ggplot2)
require(ggpubr)
# check the impact of different replacement values

source("simulation2_group_ribbon_functions.R")

set.seed(117)

# specify values

d_mean <- seq(0.1,3.9,0.3)
c_mean <- 0
d_sd <- 0
c_sd <- 0
Nn <- 20
Ns <- 20
M <- 60
sim_number <- 1000
probs <- c(0.5)

replacement_values <- c(0.1,0.5,0.75,1)

# store plots in a list

x1 <- rep(0,length(d_mean)*4)
y1 <- rep(0,length(d_mean)*4)
value <- rep(0,length(d_mean)*4)
k <- 1
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
  x1[k:(k+length(d_mean)-1)] <- d_mean
  y1[k:(k+length(d_mean)-1)] <- result$p.rep
  value[k:(k+length(d_mean)-1)] <- rep(replacement_values[i],length(d_mean))
  k <- k + length(d_mean)
}

dat1 <- data.frame(x=x1,y=y1,Correction.value=as.factor(value))

p1 <- ggplot(dat1, aes(x = x, y = y, colour = Correction.value, group = Correction.value)) +  
  geom_line() + geom_point() + ggtitle("Replacement") +
  xlab("True d'") + ylab("Difference between estimation and true d'") +
  ylim(-0.8,0.5) + grids(color="grey")


set.seed(117)

# specify values

d_mean <- seq(0.1,3.9,0.3)
c_mean <- 0
d_sd <- 0
c_sd <- 0
Nn <- 20
Ns <- 20
M <- 60
sim_number <- 1000
probs <- c(0.5)

ll_values <- c(0.25,0.4,0.5,0.75)

# store plots in a list

x1 <- rep(0,length(d_mean)*4)
y1 <- rep(0,length(d_mean)*4)
value <- rep(0,length(d_mean)*4)
k <- 1

for(i in 1:length(ll_values)){
  result <- group_simulation(d_mean.t=d_mean,
                             c_mean.t=c_mean,
                             d_sd.t=d_sd,
                             c_sd.t=c_sd,
                             Nn=Nn,
                             Ns=Ns,
                             M=M,
                             sim_number=sim_number,
                             ll_value=ll_values[i],
                             probs = probs)
  x1[k:(k+length(d_mean)-1)] <- d_mean
  y1[k:(k+length(d_mean)-1)] <- result$p.ll
  value[k:(k+length(d_mean)-1)] <- rep(ll_values[i],length(d_mean))
  k <- k + length(d_mean)
}

dat2 <- data.frame(x=x1,y=y1,Correction.value=as.factor(value))

p2 <- ggplot(dat2, aes(x = x, y = y, colour = Correction.value, group = Correction.value)) +  
  geom_line() + geom_point() + ggtitle("Log-linear") +
  xlab("True d'") + ylab("Difference between estimation and true d'") + 
  ylim(-0.8,0.5)  + grids(color="grey")

ggarrange(p1,p2,nrow=1,ncol=2)