# moreparty

## an R package for conditional inference random forests

This package aims at complementing the party package, with parallelization and interpretation tools.

It provides functions for :

* parallelized conditional random forest
* parallelized variable importance
* feature selection : recursive and non-recursive feature elimination, algorithms based on permutation tests
* surrogate tree
* bivariate association measures
* prototypes
* getting any tree from a random forest


## installation

Execute the following code within `R`:

```r
if (!require(devtools)){
	install.packages('devtools')
	library(devtools)
}
install_github("nicolas-robette/moreparty")
```


## References

Altmann A., Toloşi L., Sander O., and Lengauer T.. "Permutation importance: a corrected feature importance measure". *Bioinformatics*, 26(10):1340-1347, 2010.

Gregorutti B., Michel B., and Saint Pierre P. "Correlation and variable importance in random forests". arXiv:1310.5726, 2017.

Hapfelmeier A. and Ulm K. "A new variable selection approach using random forests". *Computational Statistics and Data Analysis*, 60:50–69, 2013.

Hothorn T., Hornik K., Van De Wiel M.A., Zeileis A. "A lego system for conditional inference". *The American Statistician*. 60:257–263, 2006.

Hothorn T., Hornik K., Zeileis A. "Unbiased Recursive Partitioning: A Conditional Inference Framework". *Journal of Computational and Graphical Statistics*, 15(3):651-674, 2006.

Molnar, C. *Interpretable machine learning. A Guide for Making Black Box Models Explainable*, 2019.
[https://christophm.github.io/interpretable-ml-book/]

Strobl, C., Malley, J., and Tutz, G. "An Introduction to Recursive Partitioning: Rationale, Application, and Characteristics of Classification and Regression Trees, Bagging, and Random Forests". *Psychological methods*, 14(4):323-348, 2009.

