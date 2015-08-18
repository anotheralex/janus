<!-- README.md is generated from README.Rmd. Please edit that file -->
janus
=====

Overview
========

janus is a package for feature selection and classification. It serves as a wrapper for several underlying packages that handle the technical implementations while providing a uniform interface for interacting with them.

By necessity, some choices have been made about sensible defaults. The documentation will make clear where these defaults differ from those present in the underlying packages.

Among the packages currently supported are:

-   [e1071](https://cran.r-project.org/web/packages/e1071/index.html) - support vector machine classifier
-   [FSelector](https://cran.r-project.org/web/packages/FSelector/index.html) - feature selection using a range of measures
-   [glm]() - two-class logistic regression classifier
-   [glmnet](https://cran.r-project.org/web/packages/glmnet/index.html) - two- and multi-class logistic regression classifier
-   [randomForest](https://cran.r-project.org/web/packages/randomForest/index.html) - random forest classifier

Installation
============

We assume here that you are planning to install this from scratch. The general order is therefore:

1.  install R
2.  install RStudio
3.  install devtools
4.  install janus

These are described in turn.

Installing R
------------

R ([homepage](http://www.r-project.org/)) is an open source statistical computing language. R is extended through packages that are available through, among other places, the Comprehensive R Archive Network, or CRAN.

To download R, visit the site for the [R Project](https://www.r-project.org/), choose a [CRAN mirror](https://cran.r-project.org/mirrors.html) (such as [this one](http://cran.ma.imperial.ac.uk/) at Imperial College London), and download and install the correct version for your platform.

Installing RStudio
------------------

RStudio ([homepage](https://www.rstudio.com)) is an open source integrated development environment for working with R. RStudio makes a lot of working with R that much easier, including such essential features as code completion, syntax checking and version control support. We highly recommend using RStudio.

Both free and commercial versions of RStudio are available. We recommend simply downloading the appropriate installer for the free desktop version from RStudio [download page](https://www.rstudio.com/products/rstudio/download/) and following the installation instructions.

Installing devtools
-------------------

devtools ([homepage](https://github.com/hadley/devtools/)) is a tool that simplifies using R as a programming language rather than just a statistics environment. In our case, we are using devtools to aid deployment, through a function that allows installing packages hosted on GitHub with a minimum of fuss.

To install devtools, run the following command from the console within RStudio.

    install.packages("devtools")

Installing janus
----------------

To install janus, run the following commands at the console in RStudio.

    library(devtools)
    install_github("anotheralex/janus")

Running janus
=============
