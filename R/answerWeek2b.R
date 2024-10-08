#' Week 2b Answer Function
#' 
#' This function computes the answer to the second question of Week 2's practice script
#' @param data The randomly generated question data
#' @param newM The randomly generated new sample mean
#' @param newSd The randomly generated new sample SD
#' @export

answerWeek2b <- function(data, newM, newSD) {
  #Compute sample mean
  m <- mean(data)
  
  #Compute sample sd
  sd <- sqrt(sum((data - m) ^ 2) / (length(data) - 1))
  
  #Compute z-scores
  z <- (data - m) / sd
  
  #Transform
  dataNew <- (z * newSD + newM)
  
  
  return(list(
    "z-scores" = round(z, 3),
    "Transformed Scores" = round(dataNew, 3)
  ))
}
