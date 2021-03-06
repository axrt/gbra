% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/normalize.scores.R
\name{normalize.scores}
\alias{normalize.scores}
\title{Use to normalize the bitscore values in the RAW(!) table from read.bhs()}
\usage{
normalize.scores(raw.df)
}
\arguments{
\item{\code{raw.df}}{data.frame from read.bhs(); keep in mind that read.bhs() returns a data.table, not data.frame, so conversion is due}
}
\value{
\code{data.frame} with bit score values converted to [0,1] scale
}
\description{
Use to normalize the bitscore values in the RAW(!) table from read.bhs()
}
\examples{
master.table.raw<-read.bhs(bh.folder = "/gBLASTer/bh")
master.table.raw<-as.data.frame(master.table.raw)
master.table.norm<- normalize.scores(master.table.raw)
}

