\name{compare_levels}
\alias{compare_levels}
\title{Compare the value of some variable extracted from a Bayesian posterior sample for different levels of a factor}
\description{
Given a posterior sample from a Bayesian sampler in long format (e.g. as returned by extract_samples), compare
the value of a variable in that sample across different paired combinations of levels of a factor.
}
\usage{
compare_levels(samples, variable, by, fun=`-`, comparison=default)
}
\arguments{
  \item{samples}{
Long-format \code{data.frame} of samples such as returned by \code{\link{extract_samples}}. 
}
  \item{variable}{
Bare (unquoted) name of a column in samples representing the variable to compare across levels. 
}
  \item{by}{
Bare (unquoted) name of a column in samples that is a \code{factor} or \code{ordered}. The value of
\code{variable} will be compared across pairs of levels of this \code{factor}.
}
  \item{fun}{
Binary function to use for comparison. For each pair of levels of \code{by} we are comparing (as determined by \code{comparison}),
compute the result of this function.
}
  \item{comparison}{
One of (a) the comparison types \code{ordered}, \code{control}, \code{pairwise}, or \code{default} (may
also be given as strings, e.g. \code{"ordered"}), see `Details`; (b) a user-specified function that takes a 
\code{factor} and returns a list of pairs of names of levels to compare (as strings); or (c) a list of 
pairs of names of levels to compare (as strings), e.g.: \code{list(c("a", "b"), c("b", "c"))}, which
would compare level \code{"a"} against \code{"b"} and level \code{"b"} against \code{"c"}. 
}
}
\details{
This function simplifies conducting comparisons across levels of some variable returned from
a Bayesian sample. It applies \code{fun} to all samples of \code{variable} for each pair of
levels of \code{by} as selected by \code{comparison}. By default, all pairwise comparisons
are generated if \code{by} is an unordered \code{factor} and ordered comparisons are made
if \code{by} is \code{ordered}. 

The included \code{comparison} types are:
\itemize{
    \item \code{ordered}: compare each level \code{i} with level \code{i - 1}; e.g. \code{fun(i, i - 1)}
    \item \code{pairwise}: compare each level of \code{by} with every other level.
    \item \code{control}: compare each level of \code{by} with the first level of \code{by}. 
        If you wish to compare with a different level, you can first apply \code{\link{relevel}} to \code{by}
        to set the control (reference) level.
    \item \code{default}: use \code{ordered} if \code{is.ordered(by)} and \code{pairwise} otherwise. 
}

}
\value{
A \code{data.frame} with the same columns as \code{samples}, except that the \code{by} column contains a symbolic
representation of the comparison of pairs of levels of \code{by} in \code{samples}, and \code{variable}
contains the result of that comparison.
}
\author{
Matthew Kay
}
\seealso{
\code{\link{extract_samples}}.
}
\examples{
##TODO
}
\keyword{manip}
