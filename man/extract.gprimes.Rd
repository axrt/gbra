% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/extract.gprimes.R
\name{extract.gprimes}
\alias{extract.gprimes}
\title{Use to extravt gprime MLE values from a list of MLEs}
\usage{
extract.gprimes(mle.list)
}
\arguments{
\item{\code{mle.list}}{a list of MLE values (from getMLEs())}
}
\value{
a plain data.frame of MLE gprime values
}
\description{
Use to extravt gprime MLE values from a list of MLEs
}
\examples{
data<-master.table.raw \%>\% data.frame() \%>\% restrict.minimal.hits(minhit = 40) \%>\% normalize.scores() \%>\% group_by(ID_QUERY_GENOME) \%>\% split(f=.$ID_QUERY_GENOME)
mles.raw<-lapply(data,FUN = function(i){
message(paste("Calculating MLE for genome",i[1,2]))
return(getMLE(t(i[,3:ncol(i)])))
})
mle.df<-extract.gprimes(mles.raw)
}

