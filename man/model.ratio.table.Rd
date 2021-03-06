% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/model.ratio.table.R
\name{model.ratio.table}
\alias{model.ratio.table}
\title{Use to calculate tp/tn/fp/fn ratio for a given logistic regression, which uses the given data to predict}
\usage{
model.ratio.table(model, predict.data, cut = 0.5)
}
\arguments{
\item{\code{model}}{logistic regression model}

\item{\code{predict.data}}{data to use for model assessment}

\item{\code{cut}}{probability margin, default is 0.5 (equally probable to get in both clusters)}
}
\value{
a \code{table} funciton output
}
\description{
Use to calculate tp/tn/fp/fn ratio for a given logistic regression, which uses the given data to predict
}

