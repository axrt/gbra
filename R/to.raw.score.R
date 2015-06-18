#' Converts a given bit BLAST score to raw score. Designed for protein alignments as by default
#' lambda and kappa are given for such.
#' @param \code{lambda} parameter, default 0.267 (as estimated by BLAST of 06.17.2015)
#' @param \code{kappa} parameter, default 0.041 (as estimated by BLAST of 06.17.2015)
#' @return \code{raw score} for the given raw score and parameters
#' @example
#' > to.raw.score(bit.score = 491.115)
#' [1] 1262.999
to.raw.score<-function(bit.score, lambda=0.267, kappa=0.041){
 
  score.raw=(bit.score*log(2)+log(ka))/lambda
  return(score.raw)
}