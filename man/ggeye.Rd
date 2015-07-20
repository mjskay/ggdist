\name{ggeye}
\alias{ggeye}
\title{Shortcut for eye plots with ggplot2}
\description{
This function is a shortcut for ggplot() + geom_eye() + coord_flip.
}
\usage{
ggeye(data=NULL, mapping=NULL, ...)
}
\arguments{
  \item{data}{
Passed to \code{\link{ggplot}}. Default data set.
}
  \item{mapping}{
Passed to \code{\link{ggplot}}. The aesthetic mapping, usually constructed with \code{\link{aes}} or \code{\link{aes_string}}.
}
  \item{...}{
Other arguments passed to \code{\link{geom_eye}}.
}
}
\details{
\code{ggeye(data, mapping, ...)} is a shortcut for \code{ggplot(data, mapping) + geom_eye(...) + coord_flip()}.
}
\author{
Matthew Kay
}
\seealso{
See \code{\link{geom_eye}} for more information.
}
\examples{
##TODO
}
\keyword{manip}
