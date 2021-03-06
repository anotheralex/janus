% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/janus.R
\name{janus}
\alias{janus}
\title{Constructor for janus object}
\usage{
janus(object, package, classifier, interface = c("formula", "default"),
  constructed = TRUE)
}
\arguments{
\item{object}{A trained model object.}

\item{package}{Character string indicating package origin of classifier.}

\item{classifier}{Character string indicating the classifier used to train
the model in object.}

\item{interface}{String indicating whether the object was created using the
formula method interface or the default interface.}

\item{constructed}{Logical indicating whether this object was created using
  the janus constructor.}
}
\value{
A janus object containing the trained model object with additional
  metadata.
}
\description{
A constructor for creating a janus object. The principle argument is the
trained model object, which is packaged inside a janus object along with
metadata derived from the fitting process.
}
\author{
Alex Wollenschlaeger, \email{alexw@panix.com}
}

