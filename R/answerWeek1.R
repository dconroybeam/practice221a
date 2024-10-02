#' Week 1 Anwer Function
#' 
#' This function computes the answer to Week 1's practice script
#' @param data The randomly generated question data
#' @export

answerWeek1 <- function(data) {
  #Compute mean
  m <- sum(data) / length(data)
  
  #Compute SS
  ss <- sum((data - m) ^ 2)
  
  #Compute variance and SD using population formulas
  popVar <- ss / length(data)
  popSd <- sqrt(popVar)
  
  return(c(
    paste0("mean = ", round(m, 3)),
    paste0("SS = ", round(ss, 3)),
    paste0("variance = ", round(popVar, 3)),
    paste0("SD = ", round(popSd, 3))
  ))
}