#' Week 5 Answer Function
#' 
#' This function computes the answers to Week 5's practice script
#' @param sample1 The randomly generated data for sample 1
#' @param sample2 The randomly generated data for sample 1
#' @param type The type of t-test you want to conduct
#' @export

answerWeek5 <- function(sample1, sample2, type) {
  
  #Calculate answers for independent samples t
  if(type=="independent"){
    
    #Determine sample size
    n<-length(sample1)
    
    #Calculate means
    m1<-mean(sample1)
    m2<-mean(sample2)
    
    #Calculate SDs
    sd1<-sd(sample1)
    sd2<-sd(sample2)
    
    #Calculate pooled variance
    pooledVar<-(sd1^2*(n-1) + sd2^2*(n-1)) / (n+n-2)
    
    #Calculate standard error
    se<-sqrt(2*(pooledVar/n))
    
    #Calculate 2
    t<-(m1-m2)/se
    
    #Determine p
    p<-2*pt(abs(t),2*(n-1),lower.tail=F)
    
    #Determine result of significance test
    result<-ifelse(p<.05,"Reject H0","Fail to Reject H0")
    
    #95% CI
    lci<-abs(m1-m2)-qt(.975,2*(n-1))*se
    uci<-abs(m1-m2)+qt(.975,2*(n-1))*se
    
    #Cohen's d
    d<-abs(m1-m2)/sqrt(pooledVar)
    
  }

  #Calculate answers for dependent samples t
  if(type=="dependent"){
    
    #Convert scores to different scores
    diffs<-(sample1-sample2)
    
    #Determine sample size
    n<-length(diffs)
    
    #Compute mean difference
    mDiff<-mean(diffs)
    
    #Compute SD of difference scores
    sdDiff<-sd(diffs)
    
    #Compute SE
    se<-sdDiff/sqrt(n)
    
    #Compute t
    t<-mDiff/se
    
    #Determine p-value
    p<-2*pt(abs(t),n-1,lower.tail=F)
    
    #Determine result of significance test
    result<-ifelse(p<.05,"Reject H0","Fail to Reject H0")
    
    #95% CI
    lci<-abs(mDiff)-qt(.975,n-1)*se
    uci<-abs(mDiff)+qt(.975,n-1)*se
    
    #Cohen's d
    d<-abs(mDiff)/sdDiff
    
  }
  
  
  return(list(
    "t statistic" = round(t, 3),
    "p-value" = round(p, 3),
    "result" = result,
    "95% CI" = c(round(lci,3),round(uci,3)),
    "Cohen's d" = round(d,3)
  ))
}
