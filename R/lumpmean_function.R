#' Running mean using a time span instead of the number of observations.
#'
#' This function performs a running mean, but instead of
#' averaging based on the number of samples/observations, it
#' take the mean based on a specified time span.
#'
#' @param x A numeric vector to use as a time indicator.
#' @param y A numeric vector of the data to average.
#' @param tspan A numeric value to use as the time span.
#'
#' @export
#' @examples
#' ### Create Test data
#' testx <- 1
#' while(testx[length(testx)]<30){
#'   hold <- testx[length(testx)]+sample(seq(0.1,1,by=0.1),1)
#'   testx <- c(testx,hold)
#'   rm(hold)
#' }
#' testdf <- data.frame(time=testx,val=sample(1:100,length(testx),replace=T))
#'
#' ### Get the running mean of val for every 5 units of time:
#' testdf$avg <- lumpmean(x=testdf$time,y=testdf$val,tspan = 5)
#'
#' @return A numeric vector of the running mean. NAs provided on tail when the span is not meet.


lumpmean <- function(x, y, tspan) {
  checktspan <- unlist(lapply(seq_along(x), function(k)
    x[k + 1] - x[k]))
  if (min(checktspan, na.rm = TRUE) > tspan) {
    message("Warning: If difference in x is > tspan, exact value taken.")
  }
  if (min(checktspan, na.rm = TRUE) <= 0) {
    stop("x[i+1] must always be > x[i]")
  }
  out <- unlist(lapply(seq_along(x), function(j) {
    h <- j
    while (x[h + 1] - x[j] <= tspan & h != length(x)) {
      h <- h + 1
    }
    if (x[h] - x[j] <= tspan & h == length(x)) {
      chunkmean <- NA
    } else {
      chunkmean <- mean(y[j:h])
    }
  }))
  return(out)
}
