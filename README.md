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
-   [glm]() - two-class logistic regression
-   [glmnet](https://cran.r-project.org/web/packages/glmnet/index.html) - two- and multi-class logistic regression
-   [randomForest](https://cran.r-project.org/web/packages/randomForest/index.html) - random forest and bagged random forest models

Installation
============

We assume here that you are planning to install this from scratch. The general order is therefore:

1.  install R
2.  install RStudio
3.  install janus

These are described in turn.
