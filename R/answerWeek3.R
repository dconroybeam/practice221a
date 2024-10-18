#' Week 3 Answer Function
#' 
#' This function computes the answers to Week 3's practice script
#' @param data The randomly generated question data
#' @param popMean The randomly generated population mean
#' @param popSD The randomly generated population SD
#' @export

answerWeek3 <- function(data, popMean, popSD) {
  
  #Calculate the standard error
  se<-popSD/sqrt(length(data))
  
  #Compute the z-score
  z<-(mean(data)-popMean)/se
  
  #Compute p-value
  p<-2*pnorm(abs(z),0,1,lower.tail=F)
  
  #Determine the significance test result
  result<-ifelse(p<.05,"Reject H0","Fail to Reject H0")
  
  #Compute the 95% CI:
  lci<-mean(data)-qnorm(.975,0,1)*se
  uci<-mean(data)+qnorm(.975,0,1)*se
  
  
  return(list(
    "z statistic" = round(z, 3),
    "p-value" = round(p, 3),
    "result" = result,
    "95% CI" = c(round(lci,3),round(uci,3))
  ))
}
