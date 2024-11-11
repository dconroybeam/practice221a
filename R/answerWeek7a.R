#' Week 7a Answer Function
#' 
#' This function computes the answer to the first question of Week 7's practice script
#' @export

answerWeek7a <- function() {
  
  #Randomly decide what kind of dataset to generate
  rand<-sample(1:3,1)
  
  #Generate normal data with no violations
  if(rand==1){
    data<-data.frame("sample"=rep(1:2,each=100),"y"=c(rnorm(100,10,1),rnorm(100,11,1)))
  }
  
  #Generate normal data with homogeneity of variance issues
  if(rand==2){
    data<-data.frame("sample"=rep(1:2,each=100),"y"=c(rnorm(100,10,1),rnorm(100,11,3)))
  }
  
  #Generate non-normal data
  if(rand==3){
    data<-data.frame("sample"=rep(1:2,each=100),"y"=c(rchisq(100,1)*1.75,rchisq(100,1)*2))
  }
  
  #Calculate sample variances
  vars<-tapply(data$y,data$sample,var)
  
  #Calculate the ratio of variances
  varRatio<-max(vars)/min(vars)
  
  varDecision<-paste0("Variance Ratio = ",
                      round(varRatio,3),
                      ifelse(varRatio>3,
                             "; Too high",
                             "; Not too high"))
  
  #Compute data for QQ plot for sample 1
  qq1<-data.frame("actual"=sort(data$y[data$sample==1]))
  qq1$ideal<-scale(qnorm(ppoints(data$y[data$sample==1]),
                         mean(data$y[data$sample==1],na.rm=T),
                         sd(data$y[data$sample==1],na.rm=T)))

  
  #Compute data for QQ plot for sample 2
  qq2<-data.frame("actual"=sort(data$y[data$sample==2]))
  qq2$ideal<-scale(qnorm(ppoints(data$y[data$sample==2]),
                         mean(data$y[data$sample==2],na.rm=T),
                         sd(data$y[data$sample==2],na.rm=T)))
  #Label the qq dataframes
  qq1$sample<-1
  qq2$sample<-2
  
  #Rbind them together
  qqData<-rbind(qq1,qq2)
  
  #Generate QQ plots for each sample
  qqPlot1<-ggplot(data=qqData[qqData$sample==1,],
                  aes(x=scale(actual),y=ideal))+
    geom_abline(intercept=0,slope=1,lwd=1,color="red")+
    geom_point()
  
  qqPlot2<-ggplot(data=qqData[qqData$sample==2,],
                  aes(x=scale(actual),y=ideal))+
    geom_abline(intercept=0,slope=1,lwd=1,color="red")+
    geom_point()
  
  #Output the answers
  answer<-ifelse(rand==1,"Normal Data, No Variance Issues",
                 ifelse(rand==2,"Normal Data, Variances not Homogenous",
                        "Non-Normal Data"))
  
  #Output the answers
  return(list(data,answer,varDecision,qqPlot1,qqPlot2))
  
}
