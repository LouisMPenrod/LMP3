#' An Inequality Function
#'
#' This function allows the user to count the number of values/rows
#' that match the inequality.
#' @param x Data to pass through function. Vector with multiple values
#' @param val Value to evaluate with inequality
#' @param eq inequality symbol as a character.
#'     Options are ">", "<", ">=", "<=", or "=".
#'
#' @return Value of observations that fit the condition given.
#'
#' @examples
#' ### Create Test data
#' a<-seq(1,5,by=1)
#' b<-seq(1,10,by=2)
#' c<-seq(1,15,by=3)
#' data<-data.frame(a,b,c)
#'
#' #for vector:
#' ineq(a,val=3,eq="<")
#'
#' #for dataframe:
#' apply(data,2,function(x) ineq(x,val=4,eq=">"))
#'
#' @export
ineq<-function(x,val,eq){
  if(eq==">"){
    sm<-length(x[x>val])
    return(sm)
  }
  else if(eq=="<"){
    sm<-length(x[x<val])
    return(sm)
  }
  else if(eq==">="){
    sm<-length(x[x>=val])
    return(sm)
  }
  else if(eq=="<="){
    sm<-length(x[x<=val])
    return(sm)
  }
  else if(eq=="="){
    sm<-length(x[x==val])
    return(sm)
  }
  else {
    return(NA)
  }
}
