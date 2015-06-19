#' Use to calculate the maximum likelihood estimate of a hitset (presumably - from one genome)
#' 
#' @param \code{df} plain dataframe(no genome and names column) of raw score coordinates
#' @param maximum \code{steps} until the convergence of the ML, default 50 (more than enough actually)
#' @param \code{epsilon} minimum improvement in the coordinate after which the ML is considered to have converged, default 1e-6
#' @return a \code{list} of \code{track} (a two column table, contains every stage of convergence process) and \code{gprime} (the actual ML estimate)
#' @examples
#' df<- read.bhs(folder) %>% data.frame()
#' MLE<-getMLE(df[,3:ncol(df)])
#'
getMLE<-function(df, steps=50, epsilon=1e-6){
  
  E<-function(df){
    row.means<-rowMeans(x = df)
    row.means<-jitter(x = row.means)
    return(row.means)
  }
  
  diff.dist<-function(i,gprime){
    return(sqrt(sum((i - gprime)^2)))
  }
  sum.dist<-function(df,gprime){
    return(sum(apply(X = df, MARGIN = 2, FUN = diff.dist, gprime=gprime)))
  }
  diff.dist.ratio<-function(i,gprime){
    return(i/diff.dist(i,gprime))          
  }
  diff.dist.inverse<-function(i, gprime) {
    return(1/diff.dist(i,gprime))
  }
  
  gprime<-E(df)
  
  update.track<-matrix(0, nrow = steps, ncol = 2)
  update.track<-data.frame(update.track)
  colnames(update.track) <- c("g", "delta_g")
  
  g.dist <- sum.dist(df,gprime)
  delta.g<-1
  counter<-0
  
  while ((counter < steps) && (delta.g > epsilon)) {
    
    counter<-counter+1
    update.track[counter,]<-c(g.dist,delta.g)
    
    gprime <- rowSums(apply(df,2,diff.dist.ratio,gprime=gprime))/
      sum(apply(df, 2, diff.dist.inverse, gprime = gprime))
    
    g.dist <- sum.dist(df,gprime)
    delta.g<-abs(update.track[counter,1]-g.dist)
  }
  
  return(list(track=update.track,gprime=gprime))
}

