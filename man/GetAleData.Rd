\name{GetAleData}
\alias{GetAleData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Accumulated Local Effects for a conditional random forest.
}
\description{
This function computes the Accumulated Local Effects for several covariates in a conditional random forest and gathers them into a single data frame.
}
\usage{
GetAleData(object, xnames=NULL, order=1, grid.size=20, parallel=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{object}{
An object as returned by \code{cforest} (or \code{fastcforest}).
}
\item{xnames}{
A character vector of the covariates for which to compute the Accumulated Local Effects. If NULL (default), ALE are computed for all the covariates in the model. Should be of length 2 for 2nd order ALE.
}
\item{order}{
An integer indicating whether to compute 1st order ALE (1, default) or 2nd order ALE (2).
}
\item{grid.size}{
The size of the grid for evaluating the predictions. Default is 20.
}
\item{parallel}{
Logical indicating whether or not to run the function in parallel using a backend provided by the \code{foreach} package. Default is \code{FALSE}.
}
}
\details{
The computation of Accumulated Local Effects uses \code{FeatureEffect} function from \code{iml} package. The results are then gathered and reshaped into a friendly data frame format.
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A data frame with covariates values and accumulated local effects.
}
\references{
Apley, D. W., Zhu J. "Visualizing the Effects of Predictor Variables in Black Box Supervised Learning Models". arXiv:1612.08468v2, 2019.

Molnar, Christoph. "Interpretable machine learning. A Guide for Making Black Box Models Explainable", 2019.
https://christophm.github.io/interpretable-ml-book/.}
\author{
Nicolas Robette
}
%\note{
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%\code{ctree}
%}
\examples{
  data(iris)
  iris2 = iris
  iris2$Species = factor(iris$Species == "versicolor")
  iris.cf = party::cforest(Species ~ ., data = iris2, controls = cforest_unbiased(mtry=2))
  GetAleData(iris.cf)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{tree}% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
