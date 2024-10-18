#' Week 4 Answer Function
#' 
#' This function computes the answers to Week 4's practice script
#' @param data The randomly generated question data
#' @param popMean The randomly generated population mean
#' @export

answerWeek4 <- function(data, popMean) {
  
  sampSD<-sd(data)
  
  #Calculate the standard error
  se<-sampSD/sqrt(length(data))
  
  #Compute the z-score
  t<-(mean(data)-popMean)/se
  
  #Compute p-value
  p<-2*pt(abs(t),length(data)-1,lower.tail=F)
  
  #Determine the significance test result
  result<-ifelse(p<.05,"Reject H0","Fail to Reject H0")
  
  #Compute the 95% CI:
  lci<-mean(data)-qt(.975,length(data)-1)*se
  uci<-mean(data)+qt(.975,length(data)-1)*se
  
  
  return(list(
    "t statistic" = round(t, 3),
    "p-value" = round(p, 3),
    "result" = result,
    "95% CI" = c(round(lci,3),round(uci,3))
  ))
}
