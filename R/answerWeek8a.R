#' Week 8a Answer Function
#' 
#' This function computes the answer to the first question of Week 8's practice script
#' @param type The type of chi-squared test to simulate
#' @export

answerWeek8a <- function(type="gof") {
    
    #For goodness of fit test
    if(type=="gof"){
      
      #Determine a random k value
      k<-sample(2:5,1)
      
      #Generate random observations
      obs<-sample(5:20,k,replace=T)
      
      #Generate random expectations
      exp<-runif(k,0,1)
      
      exp<-exp/sum(exp)
      
      exp<-sum(obs)*exp
      
      #Compute chi-squared statistic
      chi<-sum((obs-exp)^2/exp)
      
      #Compute degrees of freedom
      df<-k-1
      
      #Compute p-value
      p<-pchisq(chi,k-1,lower.tail=F)
      
      
    }
    
    #For test of independence
    if(type=="ind"){
      
      #Generate a random k value
      k<-sample(c(4,6,8),1)
      
      #Generate an observation matrix
      obs<-matrix(sample(5:20,k,replace=T),2,k/2)
      
      #Compute expectations
      exp<-outer(rowSums(obs),colSums(obs))/sum(obs)
      
      #Compute chi-squared
      chi<-sum((obs-exp)^2/exp)
      
      #Determine df
      df<-(nrow(obs)-1)*(ncol(obs)-1)
      
      #Compute p-value
      p<-pchisq(chi,df,lower.tail=F)
    }
   
  #Output the answers 
  return(list(obs,exp,chi,df,p))

}
