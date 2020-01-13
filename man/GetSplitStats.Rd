\name{GetSplitStats}
\alias{GetSplitStats}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Permutation tests results for each split in a conditional tree.
}
\description{
This function displays the results the selection variable process for each split of a conditional tree, i.e. the p-values from permutation tests of independence between every predictor and the dependent variable. This may help to assess the stability of the tree.
}
\usage{
GetSplitStats(ct)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ct}{A tree of class \code{BinaryTree} (as returned by \code{ctree} from \code{party} package) or \code{constparty} (as returned by \code{ctree} from \code{partykit} package)}.}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A list of elements, one for each split in the tree. For each split, the vector corresponds to are log(1-p) for every predictors, with p the p-value of the permutation test of independence. Variables are sorted by decreasing degree of association with the dependent variable.
}
\references{
Hothorn T, Hornik K, Van De Wiel MA, Zeileis A. "A lego system for conditional inference". \emph{The American Statistician}. 60:257–263, 2006.

Hothorn T, Hornik K, Zeileis A. "Unbiased Recursive Partitioning: A Conditional Inference Framework". \emph{Journal of Computational and Graphical Statistics}, 15(3):651-674, 2006.}
\author{
Nicolas Robette
}
\note{
see also https://stats.stackexchange.com/questions/171301/interpreting-ctree-partykit-output-in-r
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{ctree}
}
\examples{
  data(iris)
  iris2 = iris
  iris2$Species = factor(iris$Species == "versicolor")
  iris.ct = party::ctree(Species ~ ., data = iris2)
  GetSplitStats(iris.ct)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{tree}% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
