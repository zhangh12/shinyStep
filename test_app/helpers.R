


action1 <- function(trial){
  
  locked_data <- trial$get_locked_data('dose selection')
  
  fit <- fitCoxph(Surv(pfs, pfs_event)~arm, placebo = 'placebo', 
                  data = locked_data, 
                  alternative = 'less', scale = 'hazard ratio')
  
  # browser() ## if you want to see what does fit look like
  z_l <- fit$z[fit$arm == 'low dose']
  z_h <- fit$z[fit$arm == 'high dose']
  if(z_l > 1.28){
    trial$remove_arms('high dose')
    trial$save(value = 'low', name = 'kept_arm')
  }else if(z_h > 1.28){
    trial$remove_arms('low dose')
    trial$save(value = 'high', name = 'kept_arm')
  }else{
    trial$save(value = 'both', name = 'kept_arm')
  }
  
}



