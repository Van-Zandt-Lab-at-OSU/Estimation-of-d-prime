
# This function is used to provide one-group and two-group t-test simulations

source("signal_detection_theory_model.R")

group_comparison_t_test <- function(d_mean1,
                                    c_mean1,
                                    d_sd1,
                                    c_sd1,
                                    d_mean2=0,      # there isn't necessarily a second group
                                    c_mean2=0,
                                    d_sd2=0,
                                    c_sd2=0,
                                    Nn1,
                                    Ns1,
                                    Nn2=0,
                                    Ns2=0,
                                    M1,
                                    M2=10,
                                    sim_number=100,
                                    replacement_value=0.5,     # expecting one value
                                    two.groups=1               # whether they are two groups
                                    ){

  library(pwr)  

# specify the true values for simulation
  d.t1 <- array(dim=c(M1,sim_number))
  d.t2 <- array(dim=c(M2,sim_number))
  
# specify simulated results
  d.est.rep1 <- array(dim=c(M1,sim_number))
  d.est.rep2 <- array(dim=c(M2,sim_number))  
  
  d.est.ll1 <- array(dim=c(M1,sim_number))
  d.est.ll2 <- array(dim=c(M2,sim_number)) 
  
# The simulation
# Simulate the first group  
  for(k in 1:M1){
    result <- sdt_simulation(Nn=Nn1,
                             Ns=Ns1,
                             sim_number=sim_number,
                             d_mean.t=d_mean1,
                             c_mean.t=c_mean1,
                             d_sd.t=d_sd1,
                             c_sd.t=c_sd1,
                             replacement_values=replacement_value)
    d.t1[k,] <- result$d.t
    d.est.rep1[k,] <- result$d.rep[1,]
    d.est.ll1[k,] <- result$d.ll
  }
  
# simulate the second group if application
  if(two.groups==1){
    for(k in 1:M2){
      result <- sdt_simulation(Nn=Nn2,
                               Ns=Ns2,
                               sim_number=sim_number,
                               d_mean.t=d_mean2,
                               c_mean.t=c_mean2,
                               d_sd.t=d_sd2,
                               c_sd.t=c_sd2,
                               replacement_values=replacement_value)
      d.t2[k,] <- result$d.t
      d.est.rep2[k,] <- result$d.rep[1,]
      d.est.ll2[k,] <- result$d.ll
    }    
  }
 
  
# specify t-test results
  p.t <- rep(0,sim_number)
  p.rep <- rep(0,sim_number) 
  p.ll <- rep(0,sim_number) 
  d.t <- rep(0,sim_number)       # this variable record the true effect size between 2 groups
  
  for(i in 1:sim_number){
    
    if(two.groups==1){
      p.t[i] <- t.test(d.t1[,i],d.t2[,i])[[3]]
      p.rep[i] <- t.test(d.est.rep1[,i],d.est.rep2[,i])[[3]]
      p.ll[i] <- t.test(d.est.ll1[,i],d.est.ll2[,i])[[3]]    
    }
    
    else{
      p.t[i] <- t.test(d.t1[,i],mu=d_mean1)[[3]]
      p.rep[i] <- t.test(d.est.rep1[,i],mu=d_mean1)[[3]]
      p.ll[i] <- t.test(d.est.ll1[,i],mu=d_mean1)[[3]]
    }
  }
  
# result
  result <- list(p.t=p.t,p.rep=p.rep,p.ll=p.ll)
  
  return(result)
  
}


