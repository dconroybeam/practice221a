#' Week 2a Answer Function
#' 
#' This function computes the answer to the first question of Week 2's practice script
#' @param data The randomly generated question data
#' @export

answerWeek2a <- function(data) {
  #Compute mean
  m <- sum(data) / length(data)
  
  #Compute SS
  ss <- sum((data - m) ^ 2)
  
  #Compute variance and SD using population formulas
  sampVar <- ss / (length(data) - 1)
  sampSd <- sqrt(sampVar)
  
  return(c(
    paste0("mean = ", round(m, 3)),
    paste0("SS = ", round(ss, 3)),
    paste0("variance = ", round(sampVar, 3)),
    paste0("SD = ", round(sampSd, 3))
  ))
}