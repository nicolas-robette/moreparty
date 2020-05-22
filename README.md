moreparty
=========

an R package for conditional inference random forests
-----------------------------------------------------

This package aims at complementing the
[`party`](https://cran.r-project.org/package=party) package with
parallelization and interpretation tools.

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

installation
------------

Execute the following code within `R`:

``` r
if (!require(devtools)){
    install.packages('devtools')
    library(devtools)
}
install_github("nicolas-robette/moreparty")
```

references
----------

Altmann A., Toloşi L., Sander O., and Lengauer T. “Permutation
importance: a corrected feature importance measure”. *Bioinformatics*,
26(10):1340-1347, 2010.

Apley, D. W., Zhu J. “Visualizing the Effects of Predictor Variables in
Black Box Supervised Learning Models”. arXiv:1612.08468v2, 2019.

Gregorutti B., Michel B., and Saint Pierre P. “Correlation and variable
importance in random forests”. arXiv:1310.5726, 2017.

Hapfelmeier A. and Ulm K. “A new variable selection approach using
random forests”. *Computational Statistics and Data Analysis*, 60:50–69,
2013.

Hothorn T., Hornik K., Van De Wiel M.A., Zeileis A. “A lego system for
conditional inference”. *The American Statistician*. 60:257–263, 2006.

Hothorn T., Hornik K., Zeileis A. “Unbiased Recursive Partitioning: A
Conditional Inference Framework”. *Journal of Computational and
Graphical Statistics*, 15(3):651-674, 2006.

Molnar, C. *Interpretable machine learning. A Guide for Making Black Box
Models Explainable*, 2019.
(<a href="https://christophm.github.io/interpretable-ml-book/" class="uri">https://christophm.github.io/interpretable-ml-book/</a>)

Strobl, C., Malley, J., and Tutz, G. “An Introduction to Recursive
Partitioning: Rationale, Application, and Characteristics of
Classification and Regression Trees, Bagging, and Random Forests”.
*Psychological methods*, 14(4):323-348, 2009.

tutorial
--------

To begin with, let’s create a binary classification problem from the
`iris` data set.

``` r
iris2 = iris
iris2$Species = factor(iris$Species=="versicolor")
```

Now we can fit a conditional forest to the data. We use
[`doParallel`](https://cran.r-project.org/package=doParallel) package
for parallelization, here with 2 cores. The syntax of `fastcforest`
function is exactly the same as `cforest` from
[`party`](https://cran.r-project.org/package=party) package, with an
additional option for parallelization.

``` r
set.seed(123)

library(moreparty)
library(doParallel)

registerDoParallel(cores=2)
iris.cf = fastcforest(Species~., data=iris2, parallel=TRUE)
stopImplicitCluster()
```

------------------------------------------------------------------------

#### variable importance

We may now compute the variable importances.

``` r
registerDoParallel(cores=2)
vi = fastvarImp(iris.cf, measure='ACC', parallel=TRUE)
stopImplicitCluster()
rev(sort(vi))
```

    ##  Petal.Width  Sepal.Width Petal.Length Sepal.Length 
    ##   0.18247273   0.14970909   0.06509091   0.01960000

Petal width and sepal width seem notably more important than the other
two variables.

------------------------------------------------------------------------

#### surrogate trees

A surrogate tree is a simple tree that tries to approximate a more
complex (and less interpretable) model, such as random forests.

``` r
surro = SurrogateTree(iris.cf)
surro$r.squared
```

    ## [1] 0.9269712

``` r
plot(surro$tree)
```

![](README_files/figure-markdown_github/unnamed-chunk-5-1.png)

This surrogate tree approximates our forest’s predictions, but in a far
from perfect way (R2 = 0.82), so it should probably be interpreted
cautiously.

------------------------------------------------------------------------

#### prototypes

Prototypes are ‘representative’ cases of a group of data points, here
versicolor vs non versicolor species, according the proximity matrix
derived from the forest.

``` r
prox = proximity(iris.cf)
Prototypes(iris2$Species, iris2[,1:4], prox, nProto=3)
```

    ## $`FALSE`
    ##      Sepal.Length Sepal.Width Petal.Length Petal.Width
    ## [1,] "6.75"       "3.25"      "5.7"        "2.3"      
    ## [2,] "5.05"       "3.5"       "1.45"       "0.2"      
    ## [3,] "5.1"        "3.45"      "1.5"        "0.2"      
    ## 
    ## $`TRUE`
    ##      Sepal.Length Sepal.Width Petal.Length Petal.Width
    ## [1,] "5.8"        "2.8"       "4.25"       "1.3"      
    ## [2,] "6.3"        "2.95"      "4.5"        "1.45"     
    ## [3,] "6.2"        "2.95"      "4.5"        "1.4"

The prototypes of versicolor species all have sepal length about 5,
sepal width about 3, petal length about 4.5 and petal width about 1.4.
The prototypes of non versicolor species are more heterogeneous, in
particular in terms of petal length and width.
