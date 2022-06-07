#' rcorr filter
#'
#' This function takes an rcorr list, converts in into a data.frame, and allows the user to filter the correlations based on significance and correlation coefficient.
#'
#' @param rcorr_obj The rcorr list.
#' @param signif A logical value to filter the p-value by based on a alpha of a. Default is set to NA.
#' @param a The numeric alpha value used to filter by significance. Default is set to 0.05.
#' @param ccdir A character string of the correlation coefficient direction used to filter. Options are "all", "zero", "pos", or "neg".
#' @param min_abs_cc A numeric minimum absolute value of the correlation coefficient to return.
#'
#' @return A data.frame indicating the pairs of variables with correlation coefficients or p-values which match the conditions provided.
#'
#' @export
#' @examples
#' library(tidyverse)
#' library(Hmisc)
#'
#' # create rcorr object
#' out <- mtcars %>%
#'   select(mpg:qsec) %>%
#'   as.matrix() %>%
#'   rcorr()
#'
#' # filter by signif
#' rcorr_filter(rcorr_obj = out, signif = NA, a = 0.05, ccdir = "all", min_abs_cc = NA)
#' rcorr_filter(rcorr_obj = out, signif = TRUE, a = 0.05, ccdir = "all", min_abs_cc = NA)
#' rcorr_filter(rcorr_obj = out, signif = FALSE, a = 0.05, ccdir = "all", min_abs_cc = NA)
#'
#' # with different alpha values
#' rcorr_filter(rcorr_obj = out, signif = TRUE, a = 0.05, ccdir = "all", min_abs_cc = NA)
#' rcorr_filter(rcorr_obj = out, signif = TRUE, a = 0.01, ccdir = "all", min_abs_cc = NA)
#' rcorr_filter(rcorr_obj = out, signif = TRUE, a = 0.001, ccdir = "all", min_abs_cc = NA)
#'
#' # filter by ccdir
#' rcorr_filter(rcorr_obj = out, signif = TRUE, a = 0.05, ccdir = "all", min_abs_cc = NA)
#' rcorr_filter(rcorr_obj = out, signif = TRUE, a = 0.05, ccdir = "pos", min_abs_cc = NA)
#' rcorr_filter(rcorr_obj = out, signif = TRUE, a = 0.05, ccdir = "neg", min_abs_cc = NA)
#' rcorr_filter(rcorr_obj = out, signif = TRUE, a = 0.05, ccdir = "zero", min_abs_cc = NA)
#'
#' # filter by min_abs_cc
#' rcorr_filter(rcorr_obj = out, signif = TRUE, a = 0.05, ccdir = "all", min_abs_cc = NA)
#' rcorr_filter(rcorr_obj = out, signif = TRUE, a = 0.05, ccdir = "all", min_abs_cc = 0.5)
#' rcorr_filter(rcorr_obj = out, signif = TRUE, a = 0.05, ccdir = "pos", min_abs_cc = 0.5)

rcorr_filter <- function(rcorr_obj,
                         signif=NA,
                         a=0.05,
                         ccdir="all",
                         min_abs_cc=NA){

  require(dplyr)

  ## Check inputs

  # check rcorr_obj class is 'rcorr'
  if(class(rcorr_obj)!="rcorr"){
    stop("rocrr_obj provided does not have the class 'rcorr'")
  }

  # check signif is NA, TRUE, or FALSE
  if(!is.logical(signif)){
    stop("signif must be either a NA, TRUE, or FALSE logical value")
  }

  # check alpha is a positive numeric, non-zero value
  if(!is.numeric(a)|a<=0){
    stop("alpha must be a positive, non-zero numeric value")
  }

  # check ccdir is all, pos, neg, or zero
  if(!ccdir %in% c("all","pos","neg","zero")){
    stop("ccdir must be either 'all', 'pos', 'neg', or 'zero")
  }

  # check that min_abs_cc is either NA or numeric
  if(!is.na(min_abs_cc)&!is.numeric(min_abs_cc)){
    stop("min_abs_cc must be either a logical NA or a numeric value")
  }

  ## Extract and pivot correlation coefficients
  corrdf <- rcorr_obj %>%
    pluck(1) %>%
    as_tibble()%>%
    mutate(var1=names(.))%>%
    relocate(var1) %>%
    pivot_longer(cols = -var1,names_to = "var2",values_to = "cc")%>%
    mutate(key=paste0(pmin(var1,var2),pmax(var1,var2),sep="")) %>%
    distinct(key,.keep_all = TRUE)

  ## Extract and pivot p-values
  corrpdf <- rcorr_obj %>%
    pluck(3) %>%
    as_tibble()%>%
    mutate(var1=names(.))%>%
    relocate(var1) %>%
    pivot_longer(cols = -var1,names_to = "var2",values_to = "p.val")%>%
    mutate(key=paste0(pmin(var1,var2),pmax(var1,var2),sep="")) %>%
    distinct(key,.keep_all = TRUE)%>%
    dplyr::select(key,p.val)

  ## Combine correlation coefficients and p-values
  corrfinaldf <- merge(corrdf,corrpdf,by="key",all=TRUE) %>%
    dplyr::select(-key) %>% arrange(var1) %>%
    filter(!is.na(p.val))

  ## Reduce data to meet conditions provided
  reduced <- corrfinaldf %>%
    filter(case_when(
      ccdir=="pos" ~ cc>0,
      ccdir=="neg" ~ cc<0,
      ccdir=="zero" ~ cc==0,
      ccdir=="all" ~ cc<=1|cc>=-1)) %>%
    filter(case_when(
      is.na(signif) ~ p.val>=0,
      !is.na(signif)&signif==TRUE ~ p.val<=a,
      !is.na(signif)&signif==FALSE ~ p.val>a)) %>%
    filter(case_when(
      !is.na(min_abs_cc) ~ abs(cc)>=abs(min_abs_cc),
      TRUE ~ abs(cc)<=1))

  ## return table
  return(reduced)
}
