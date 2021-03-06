% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/read.orf.base.fasta.R
\name{read.orf.base.fasta.single}
\alias{read.orf.base.fasta.single}
\title{Use to read a protein fasta file into a data frame}
\usage{
read.orf.base.fasta.single(file)
}
\arguments{
\item{\code{file}}{a file, that contains multiple fasta-formatted records}
}
\value{
data.frame with tow columns, the first contains sequences, the second - sequence ids
}
\description{
Use to read a protein fasta file into a data frame
}
\examples{
file<-"gBLASTer/orfs/Acidilobus_saccharovorans"
test.fasta.df<-read.orf.base.fasta.single(file = file)
}

