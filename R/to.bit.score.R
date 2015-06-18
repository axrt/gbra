#' Converts a given raw BLAST score to bit score. Designed for protein alignments as by default
#' lambda and kappa are given for such.
#' @param \code{lambda} parameter, default 0.267 (as estimated by BLAST of 06.17.2015)
#' @param \code{kappa} parameter, default 0.041 (as estimated by BLAST of 06.17.2015)
#' @return \code{bit score} for the given raw score and parameters
#' @examples
#' > to.bit.score(1263)
#' [1] 491.1153
to.bit.score<-function(raw.score, lambda=0.267, kappa=0.041){
  bit.score=(lambda*raw.score-log(kappa))/log(2)
  return(bit.score)
}