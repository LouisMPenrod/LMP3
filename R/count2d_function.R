#' Count the number of observation in 2d space.
#'
#' This function will count the number of observations
#' in 2d space with the ability to set the x and y begin,
#' end, and width(len) values. Provide either begin, end, and len,
#' or xbegin, xend, xlen, ybegin, yend, and ylen.
#'
#' @param x A numeric vector with the x positions.
#' @param y A numeric vector with the y positions.
#' @param begin A numeric value to begin the x and y bins.
#' @param end A numeric value to end the x and y bins.
#' @param len A numeric value to use and the x and y bin width.
#' @param xbegin A numeric value to begin the x bins.
#' @param xend A numeric value to end the x bins.
#' @param xlen A numeric value to use and the x bin width.
#' @param ybegin A numeric value to begin the y bins.
#' @param yend A numeric value to end the y bins.
#' @param ylen A numeric value to use and the y bin width.
#'
#' @export
#' @examples
#' ### Create Test data
#' x1 <- runif(1000,0,10)
#' y1 <- runif(1000,0,10)
#' data1 <- data.frame(x=x1,y=y1)
#'
#' ### Count the number of observations from 0 to 10 by 1.
#' dfout <- count2d(x=data1$x,y=data1$y,begin=0,end=10,len=1)
#' # or
#' dfout <- count2d(x=data1$x,y=data1$y,
#'                  xbegin=0,xend=10,xlen=1,
#'                  ybegin=0,yend=10,ylen=1)
#'
#' @return A dataframe with the x and y bin number, centers, range, and the count.

count2d <- function(x,y,begin=NA,end=NA,len=NA,xbegin=NA,xend=NA,xlen=NA,ybegin=NA,yend=NA,ylen=NA){
  if(!is.numeric(c(x,y,xbegin,xend,xlen,ybegin,yend,ylen,begin,end,len))){
    stop("All values must be numeric")
  }
  if((any(sapply(c(xbegin,xend,xlen,ybegin,yend,ylen),is.na))&any(sapply(c(begin,end,len),is.na)))|
     (any(!sapply(c(xbegin,xend,xlen,ybegin,yend,ylen),is.na))&all(!sapply(c(begin,end,len),is.na)))|
     (all(!sapply(c(xbegin,xend,xlen,ybegin,yend,ylen),is.na))&any(!sapply(c(begin,end,len),is.na)))){
    stop("Provide either 'begin', 'end', and 'len' or provide 'xbegin', 'xend', 'xlen', 'ybegin', 'yend', and 'ylen'")
  }
  if(all(sapply(c(xbegin,xend,xlen,ybegin,yend,ylen),is.na))&all(!sapply(c(begin,end,len),is.na))){
    xbegin <- ybegin <- begin
    xend <- yend <- end
    xlen <- ylen <- len
    message("'begin', 'end', and 'len' applied to 'x' and 'y'")
  }
  xbin <- 1:(length(seq(xbegin,xend,by=xlen))-1)
  xcen <- seq(xbegin+(xlen/2),xend-(xlen/2),by=xlen)
  ybin <- 1:(length(seq(ybegin,yend,by=ylen))-1)
  ycen <- seq(ybegin+(ylen/2),yend-(ylen/2),by=ylen)
  df <- data.frame(xbin=rep(xbin,times=length(ybin)),
                   ybin=rep(ybin,each=length(xbin)),
                   xcen=rep(xcen,times=length(ycen)),
                   ycen=rep(ycen,each=length(xcen)),
                   xran=paste0(rep(xcen,times=length(ycen))-xlen/2,"-",rep(xcen,times=length(ycen))+xlen/2),
                   yran=paste0(rep(ycen,each=length(xcen))-ylen/2,"-",rep(ycen,each=length(xcen))+ylen/2),
                   count=NA)
  data <- data.frame(x=x,y=y)
  invisible(lapply(seq_along(df$xbin),function(j){
    count <- data %>%
      dplyr::filter(x>=(df$xcen[j]-xlen/2)&x<=(df$xcen[j]+xlen/2)&y>=(df$ycen[j]-ylen/2)&y<=(df$ycen[j]+ylen/2)) %>%
      dplyr::count()
    df$count[j] <<- count
  }))
  return(df)
}
