#' Generate a dose design of interest for dose finding study.
#'
#' This function will create key variables for a specific dose design.
#' @details Author: Qing Zhao
#' @param dls A vector that contains the dose levels of interest. MCPMod requires at least three active doses for the Mod step.
#' @param ngrp.equal An optional numeric value indicating the number of subjects in each dose group when the dose groups are of equal size.
#' @param ntot An optional numeric value indicating the total sample size.
#' @param ratio An optional vector indicating the allocation ratio of sample size between the different dose groups when the dose groups are of unequal size. The vector requires to be the same length as the dose level vector `dls`.
#' @return A list of key variables that contains the dose levels, the total sample size, and the number of subjects in each dose group.
#' @export
#'
#' @examples
#' \dontrun{
#' # when the dose groups are of equal size 
#' gd.obj <- gen_design(dls = c(0, 6, 12, 18, 30), ngrp.equal = 60)
#' # when the dose groups are of unequal size
#' gd.obj <- gen_design(dls = c(0, 4, 7, 10), ntot = 300, ratio =  c(2, 1, 2, 2))
#'}

gen_design <- function(dls, ngrp.equal = NULL, ntot = NULL, ratio = NULL){
  if (is.null(ngrp.equal) == F & is.null(ntot) == T){
    if (length(ngrp.equal) != 1){
      stop("The output assumes equal allocation for each of the dose groups. Please enter one number representing the number of participants in any dose level.")
    } else {
      ndls <- length(dls)
      ntot <- ngrp.equal * ndls
      ngrp <- rep(ngrp.equal, ndls)
    }
  } else if (is.null(ngrp.equal) == T & is.null(ntot) == F & is.null(ratio) == F){
    if(length(ratio) == length(dls)){
      ngrp <- round(ntot/sum(ratio))*ratio
      ntot <- round(ntot/sum(ratio))*sum(ratio)
    } else {
      stop("The length of the allocation ratio is required to be the same as the number of dose levels.")
    }
  } else if (is.null(ngrp.equal) == T & ((is.null(ntot) == T) != (is.null(ratio) == T))){
    stop("Please enter both the total number of participants and the allocation ratio.") 
  } 
  return(list(dls = dls, ntot = ntot, ngrp = ngrp))
}
