% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/restrict.minimal.hits.R
\name{restrict.minimal.hits}
\alias{restrict.minimal.hits}
\title{Use to restrict a given data.frame with RAW(!) bitscores to a certain number of
non-zero bitscores per ORF.}
\usage{
restrict.minimal.hits(df, minhit = 10)
}
\arguments{
\item{\code{df}}{data.frame, presumably from read.bhs().}

\item{\code{minhit}}{minimal number of non-zero hits that an ORF must have in order remain in the table, default is 10.}
}
\value{
a data.frame with only those ORFs that had non-zero hits of \code{minhit} threshold and greater.
}
\description{
Use to restrict a given data.frame with RAW(!) bitscores to a certain number of
non-zero bitscores per ORF.
}
\examples{
master.table.raw<-read.bhs(bh.folder = "gBLASTer/bh")
master.table.raw<-as.data.frame(master.table.raw)
master.table.raw.10hcut<-restrict.minimal.hits<-function(df=master.table.raw)
}

