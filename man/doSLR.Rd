% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/doSLR.R
\name{doSLR}
\alias{doSLR}
\title{doSLR performs a Stepwise Linear Regression (SLR) model in both directions on the training data,
and calculates the Root Mean Square Error (RMSE) and Mean Absolute Percentage Error (MAPE) from the predicted values of the testing data.}
\usage{
doSLR(train, test)
}
\arguments{
\item{train}{Dataframe of training data, the first column is the response factor to be modelled by the remaining columns}

\item{test}{Dataframe of testing data, the first column is the response factor, the rest of the columns contain factors used to predict the response column with the built model}
}
\value{
Returns the output of the function getModelError, which is a list consisting of:
\itemize{
\item mape - the Mean Absolute Percentage Error (MAPE) of the current model
\item rmse - the Root Mean Square Error (RMSE) of the current model
}
}
\description{
doSLR performs a Stepwise Linear Regression (SLR) model in both directions on the training data,
and calculates the Root Mean Square Error (RMSE) and Mean Absolute Percentage Error (MAPE) from the predicted values of the testing data.
}
\details{
This function applies the SLR Step algorithm to the training data (see \code{\link[stats]{step}}) with derection set to "both",
and then calculates the RMSE and MAPE with \code{\link[StompR]{getModelError}} using the predicted values calculated from the testing data.
}
