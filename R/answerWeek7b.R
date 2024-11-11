#' Week 7b Answer Function
#' 
#' This function computes the answer to the second question of Week 7's practice script
#' @param d The randomly generated Cohen's d
#' @param n The randomly generated sample size
#' @export

answerWeek7b <- function(d,n) {
  
  #Compute SDpooled based on d
  pooledVar<-(1/abs(d))^2
  
  #Compute SE based on SDpooled
  se<-sqrt(2*pooledVar/n)
  
  #Compute t
  tStat<-1/se
  
  # #Note, this returns the same thing if ns are the same:
  # t<-abs(d)*sqrt(n/2)
  
  #Compute the t-critical value under the null
  tcrit<-qt(.975,n*2-2)
  
  #Compute beta (the proportion of Ha that is within tcrit in the H0 distribution)
  beta<-pt(tcrit,n*2-2,tStat)-pt(-1*tcrit,n*2-2,tStat)
  
  #Compute power
  power<-1-beta
  
  #Output the answers
  return(power)
  
}
