#' Week 6 Answer Function
#' 
#' This function computes the answers to Week 5's practice script
#' @param data The randomly generated problem data
#' @export

answerWeek6 <- function(data) {

  #Determine size of each group
  n<-nrow(data)
  
  #Determine the number of groups
  k<-ncol(data)
  
  #Determine total sample size
  N<-n*k
  
  #Calculate group means
  groupMeans<-colMeans(data)
  
  #Determine group variances
  groupVars<-apply(data,2,var)
  
  #Determine variance of group means
  varMeans<-var(groupMeans)
  
  #Calculate MSb
  msb<-varMeans*n
  
  #Calculate MSw
  msw<-mean(groupVars)
  
  #Calculate F statistic
  fStat<-msb/msw
  
  #Determine p-value
  p<-pf(fStat,k-1,N-k,lower.tail=F)
  
  #Determine result of significance test
  result<-ifelse(p<.05,"Reject H0","Fail to Reject H0")
  
  #Calculate eta-squared
  eta<-(msb*(k-1))/((msb*(k-1))+msw*(N-k))
  
  #Calculate Tukey's HSD
  hsd<-qtukey(.95,k,N-k)*sqrt(msw/n)
    
  return(list(
    "F statistic" = round(fStat, 3),
    "p-value" = round(p, 3),
    "result" = result,
    "eta squared" = round(eta,3),
    "Tukey's HSD" = round(hsd,3)
  ))
}
