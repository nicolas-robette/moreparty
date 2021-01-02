# **moreparty** <img src="man/figures/moreparty.png" height=140px width=120px alt="" align="right" />

## Tools for conditional inference random forests

  <!-- badges: start -->
  [![R build status](https://github.com/nicolas-robette/moreparty/workflows/R-CMD-check/badge.svg)](https://github.com/nicolas-robette/moreparty/actions)
  [![](https://www.r-pkg.org/badges/version/moreparty?color=blue)](https://cran.r-project.org/package=moreparty)
  [![](http://cranlogs.r-pkg.org/badges/last-month/moreparty?color=orange)](https://cran.r-project.org/package=moreparty)
  [![CodeFactor](https://www.codefactor.io/repository/github/nicolas-robette/moreparty/badge)](https://www.codefactor.io/repository/github/nicolas-robette/moreparty)

  <!-- badges: end -->

This package aims at complementing the [`party`](https://cran.r-project.org/package=party) package with parallelization and interpretation tools.

It provides functions for :

-   parallelized conditional random forest
-   parallelized variable importance
-   feature selection : recursive and non-recursive feature elimination,
    algorithms based on permutation tests
-   accumulated local effects (ALE), partial dependence and interaction
    strength
-   surrogate tree
-   prototypes
-   getting any tree from a forest
-   assessing the stability of a conditional tree
-   bivariate association measures
-   dot plots for variable importance and effects


## Installation

Execute the following code within `R`:

``` r
if (!require(devtools)){
    install.packages('devtools')
    library(devtools)
}
install_github("nicolas-robette/moreparty")
```

## References

Altmann A., Toloşi L., Sander O., and Lengauer T. “Permutation importance: a corrected feature importance measure”. *Bioinformatics*, 26(10):1340-1347, 2010.

Apley, D. W., Zhu J. “Visualizing the Effects of Predictor Variables in Black Box Supervised Learning Models”. arXiv:1612.08468v2, 2019.
 
Gregorutti B., Michel B., and Saint Pierre P. “Correlation and variable importance in random forests”. arXiv:1310.5726, 2017.

Hapfelmeier A. and Ulm K. “A new variable selection approach using random forests”. *Computational Statistics and Data Analysis*, 60:50–69, 2013.

Hothorn T., Hornik K., Van De Wiel M.A., Zeileis A. “A lego system for conditional inference”. *The American Statistician*. 60:257–263, 2006.

Hothorn T., Hornik K., Zeileis A. “Unbiased Recursive Partitioning: A Conditional Inference Framework”. *Journal of Computational and Graphical Statistics*, 15(3):651-674, 2006.

Molnar, C. *Interpretable machine learning. A Guide for Making Black Box Models Explainable*, 2019.
(<a href="https://christophm.github.io/interpretable-ml-book/" class="uri">https://christophm.github.io/interpretable-ml-book/</a>)

Strobl, C., Malley, J., and Tutz, G. “An Introduction to Recursive Partitioning: Rationale, Application, and Characteristics of
Classification and Regression Trees, Bagging, and Random Forests”. *Psychological methods*, 14(4):323-348, 2009.
