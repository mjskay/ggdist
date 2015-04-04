\name{compose_data}
\alias{compose_data}
\title{Compose data for input into a Bayesian sampler}
\description{
Compose data into a list suitable to be passed into an MCMC sampler (JAGS, BUGS, etc).
}
\usage{
compose_data(..., 
    .n_name=function(name) paste0("n_", name))
}
\arguments{
  \item{...}{
Data to be composed into a list suitable for being passed into JAGS, etc.
}
  \item{.n_name}{
A function that is used to form index variables (a variable whose value is the length of a factor or a data 
frame in \code{...}). For example, if a data frame with factor \code{"foo"} (having three levels)
is passed to \code{compose_data}, the list returned by \code{compose_data} will include a column named \code{.n_name("foo")}, 
which by default would be "n_foo".
}
}
\details{

This function recursively translates each argument into list elements using \code{\link{as.data_list}}, concatenating
all resulting lists together. By default this means that:
\itemize{
    \item numerics are included as-is.
    \item logicals are translated into numeric using \code{\link{as.numeric}}.
    \item factors are translated into numeric using \code{\link{as.numeric}}, and an additional
          elemnt named \code{.n_name(argument_name)} is added with the number of levels in the factor.
    \item lists are translated by translating all elements of the list (recursively) and adding them to the result.
    \item data.frames are translated by translating every column of the data.frame and adding them to the result.
          A variable named \code{"n"} (or \code{.n_name(argument_name)} if the data.frame is passed as
          a named argument \code{argument_name}) is also added containing the number of rows in the data frame.
    \item all other types are dropped (and a warning given)
}
If you wish to add support for additional types not described above, provide an implementation
of \code{\link{as.data_list}} for the type. See the implementations of \code{as.data_list.numeric}, 
\code{as.data_list.logical}, etc for examples.
}
\value{
An object of class \code{c("data_list", "list")}, where each element is a translated variable as described above.}
\author{
Matthew Kay
}
\seealso{
\code{\link{as.data_list}}, \code{\link{extract_samples}}.
}
\examples{
##TODO
}
\keyword{manip}
