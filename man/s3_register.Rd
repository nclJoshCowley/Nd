% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/register-s3.R
\name{s3_register}
\alias{s3_register}
\title{Register a method for a suggested dependency}
\usage{
s3_register(generic, class, method = NULL)
}
\arguments{
\item{generic, class, method}{See \code{\link[vctrs:s3_register]{vctrs::s3_register()}}.}
}
\description{
This is a reimplementation of \code{\link[vctrs:s3_register]{vctrs::s3_register()}}, implemented here
to avoid having to take a dependency on vctrs.
}
\keyword{internal}
