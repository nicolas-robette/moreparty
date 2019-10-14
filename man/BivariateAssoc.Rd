\name{BivariateAssoc}
\alias{BivariateAssoc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Bivariate association measures for supervised learning tasks.
}
\description{
Computes bivariate association measures between a response and predictor variables (and, optionnaly, between every couples of predictor variables.)
}
\usage{
BivariateAssoc(Y, X, xx = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Y}{the response variable}

  \item{X}{the predictor variables}

  \item{xx}{whether the association measures should be computed for couples of predictor variables (default) or not. With a lot of predictors, consider setting xx to FALSE (for reasons fo computation time).}

}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A list of the following items :
\item{YX}{A table with the association measures between the response and predictor variables}
\item{XX}{A table with the association measures between every couples of predictor variables}
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Nicolas Robette
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
  data(iris)
  iris2 = iris
  iris2$Species = factor(iris$Species == "versicolor")
  BivariateAssoc(iris2$Species,iris2[,1:4])
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
