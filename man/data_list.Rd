\name{data_list}
\alias{data_list}
\title{A data_list for input into a Bayesian sampler}
\description{
Used by \code{\link{compose_data}} to create lists of data suitable for input into a Bayesian sampler. 
This typically should not be called directly (instead use \code{\link{compose_data}}) unless you need to implement your own conversion routines for a
data type not already supported.
}
\usage{
data_list(...)
}
\arguments{
  \item{...}{
Items to include in the list.
}
}
\details{
This function creates a list with class \code{c("data_list", "list")} instead of \code{c("list")}, but otherwise acts like the \code{\link{list}} function.   
It is used by \code{\link{as.data_list}} to create data lists for input into Bayesian samplers. See the implementations of \code{as.data_list.numeric}, 
\code{as.data_list.logical}, etc for examples.
}
\value{
An object of class \code{c("data_list", "list")}}
\author{
Matthew Kay
}
\seealso{
\code{\link{compose_data}}, \code{\link{extract_samples}}.
}
\keyword{manip}
