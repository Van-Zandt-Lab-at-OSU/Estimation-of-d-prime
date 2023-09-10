
# This is meant to simulate and make ribbon plots for Simulation 2: the group

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
                                         ll_value=0.5,
                                         probs = c(0.025,0.1,0.25,0.5,0.75,0.9,0.975)
                                         # probs to be plotted on the ribbon plot
                                         ){
 
  
# ribbon lines
  p.t <- array(dim=c(length(probs),length(d_mean.t))) #true
  p.rep <- array(dim=c(length(probs),length(d_mean.t))) #replacement
  p.ll <- array(dim=c(length(probs),length(d_mean.t))) #tlog_linear

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
# compute the ribbon lines
    for(l in 1:length(probs)){
      p.t[l,j] <- quantile(apply(d.values.t,2,mean),probs=probs[l])
      p.rep[l,j] <- quantile(apply(d.est.rep,2,mean),probs=probs[l])
      p.ll[l,j] <- quantile(apply(d.est.ll,2,mean),probs=probs[l])
    }
  }
  
# result
  result <- list(p.t=p.t,
                 p.rep=p.rep,
                 p.ll=p.ll,
                 d_mean=d_mean.t)

  return(result)
  
}


# This function makes ribbon plots

ribbon_plots <- function(p,        # the ribbon lines
                         d_mean,
                         d_sd,
                         M=20,
                         title=title,
                         reference=1, # whether to add reference lines
                         p_ref=0     # reference ribbon lines
                         ){
  # packages needed 
  library(dplyr)
  library(ggplot2)
  library(distributional)
  library(ggdist)
  require(ggpubr)
  
  # theme for the ribbon plots  
  theme_set(theme_ggdist())  
  
  # make a data frame
  wideDF <- as.data.frame(t(p))
  names(wideDF) <- paste0("p",c("025","1","25","5","75","9","975"))
  wideDF$d <- d_mean
  
  # the plot
  # reference = 1: add red lines from the true d' distribution as references
  if(reference ==1){
    wideDF$ref025 <- p_ref[1,]
    wideDF$ref1 <- p_ref[2,] 
    wideDF$ref25 <- p_ref[3,]  
    wideDF$ref5 <- p_ref[4,] 
    wideDF$ref75 <- p_ref[5,] 
    wideDF$ref9 <- p_ref[6,]
    wideDF$ref975 <- p_ref[7,]  
    rib_plots <- ggplot( wideDF , aes(x=d) ) +
      geom_ribbon( aes( ymin=p025 , ymax=p975 ) , fill="grey80" , alpha=0.5 ) +
      geom_ribbon( aes( ymin=p1 , ymax=p9 ) , fill="grey50" , alpha=0.5 ) +
      geom_ribbon( aes( ymin=p25 , ymax=p75 ) , fill="grey30" , alpha=0.5 ) +
      geom_line( aes( y=p5 ) , col="black" ) +
      geom_line( aes( y=d_sd*qnorm(0.975)/sqrt(M)) , col="green" ) +
      geom_line( aes( y=d_sd*qnorm(0.025)/sqrt(M)) , col="green" ) +
      geom_line( aes( y=ref025) , col="red" ) +
      geom_line( aes( y=ref975) , col="red" ) +
      geom_line( aes( y=ref5) , col="red" ) +
      labs( title=title,
            y="Difference between estimation and true d'" , x="True d'" ) +
      theme(plot.title = element_text(size=10),axis.title=element_text(size=10))   
  }
  else{
    rib_plots <- ggplot( wideDF , aes(x=d) ) +
      geom_ribbon( aes( ymin=p025 , ymax=p975 ) , fill="grey80" , alpha=0.5 ) +
      geom_ribbon( aes( ymin=p1 , ymax=p9 ) , fill="grey50" , alpha=0.5 ) +
      geom_ribbon( aes( ymin=p25 , ymax=p75 ) , fill="grey30" , alpha=0.5 ) +
      geom_line( aes( y=p5 ) , col="black" ) +
      geom_line( aes( y=d_sd*qnorm(0.975)/sqrt(M)) , col="green" ) +
      geom_line( aes( y=d_sd*qnorm(0.025)/sqrt(M)) , col="green" ) +
      labs( title=title,
            y="Difference between estimation and true d'" , x="True d'" ) +
      theme(plot.title = element_text(size=10),axis.title=element_text(size=10)) +
      coord_cartesian(ylim = c(-0.5,0.5)) 
  }
  return(rib_plots)
}

