#' Week 8b Answer Function
#' 
#' This function computes the answer to the second (really third) question of Week 8's practice script
#' @param data The data from which you will bootstrap
#' @param stat The statistic you will bootstrap
#' @export

answerWeek8b <- function(data,stat) {
  
  #Generate a vector to store bootstrapped statistics
  bootStat<-rep(NA,10000)
  
  for(i in 1:10000){
    
    #Resample from the data
    bootSamp<-sample(data,length(data),replace=T)
    
    #Calculate the statistics of interest:
    if(stat=="variance"){
      bootStat[i]<-var(bootSamp)
    }
    
    if(stat=="median"){
      bootStat[i]<-median(bootSamp)
    }
    
    if(stat=="mean"){
      bootStat[i]<-mean(bootSamp)
    }
  }
  
  #Estimate the 95% CIs
  lci<-quantile(bootStat,.025)
  uci<-quantile(bootStat,.975)
  
  #Output the answers 
  return(paste0("95% CI = [",round(lci,3),", ",round(uci,3),"]"))

}
