\name{FeatureSelection}
\alias{FeatureSelection}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Recursive and non-recursive feature elimination for conditional random forests.
}
\description{
Performs recursive or non-recursive feature algorithm fo a conditional random forest model.}

\usage{
FeatureSelection(Y, X, recompute = F, ntree = 3000, measure = NULL, parallel = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Y}{response vector. Must be of class \code{factor} or \code{numeric}}

  \item{X}{matrix or data frame containing the predictors}

  \item{recompute}{whether the variable importances should be recomputed after each rejection step (recursive algorithm) or not (non-recursive algorithm)}

  \item{ntree}{number of trees contained in a forest}

  \item{measure}{The name of the measure of the \code{measures} package that should be used for the variable importance calculation.}
  
  \item{parallel}{Logical indicating whether or not to run \code{fastvarImp} in parallel using a backend provided by the \code{foreach} package. Default is \code{FALSE}.}
  
  \item{dots}{Further arguments (like positive or negativ class) that are needed by the measure.}
}

\details{
To be developed soon !
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A list with the following elements :
\item{selection.0se}{selected variables with the 0 standard error rule}
\item{forest.0se}{forest corresponding the variables selected with the 0 standard error rule}
\item{oob.error.0se}{OOB error of the forest with 0 standard error rule}
\item{selection.1se}{selected variables with the 1 standard error rule}
\item{forest.1se}{forest corresponding the variables selected with the 1 standard error rule}
\item{oob.error.1se}{OOB error of the forest with 1 standard error rule}
}
\references{
B. Gregorutti, B. Michel, and P. Saint Pierre. "Correlation and variable importance in random forests". arXiv:1310.5726, 2017.

A. Hapfelmeier and K. Ulm. "A new variable selection approach using random forests". \emph{Computational Statistics and Data Analysis}, 60:50–69, 2013.
}
\author{
Nicolas Robette
}
\note{
The code is adapted from Hapfelmeier & Ulm (2013).

Only works for regression and binary classification.

}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
  data(iris)
  iris2 = iris
  iris2$Species = factor(iris$Species == "versicolor")
  iris.cf = party::cforest(Species ~ ., data = iris2,
                           control = party::cforest_unbiased(mtry = 2, ntree = 50))
  featsel <- FeatureSelection(iris2$Species, iris2[,1:4],ntree=200)
  featsel$selection.0se
  featsel$selection.1se
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
