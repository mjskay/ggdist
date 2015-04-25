\name{extract_samples}
\alias{extract_samples}
\title{Extract samples from a Bayesian sampler in a tidy data format}
\description{
Extract samples from an MCMC chain for a variable with the given named indices into a long-format data frame.
}
\usage{
extract_samples(model, variable_spec)
}
\arguments{
  \item{model}{
A supported Bayesian model fit / MCMC object. Currently supported models include \code{\link[coda]{mcmc}}. 
}
  \item{variable_spec}{
An expression in the form of \code{variable_name[index_1, index_2, ...] | wide_index}. See `Details`.
}
}
\details{
Imagine a variable b[i,v] with i in [1..100] and v in [1..3]. An MCMC sample returned from JAGS 
(for example) would have columns with names like "b[1,1]", "b[2,1]", etc. 

\code{extract_samples(mcmc_chain, b[i,v])} would return a data frame with:)
\itemize{
    \item column \code{".sample"}: value in \code{[1..nrow(mcmcChain)]}
    \item column \code{"i"}: value in \code{[1..20]}
    \item column \code{"v"}: value in \code{[1..3]}
    \item column \code{"b"}: value of \code{"b[i,v]"} for sample number \code{".sample"} in \code{mcmc_chain}.
}

The shorthand \code{..} can be used to specify one column that should be put into a wide format and whose
names will be the base variable name plus the value of the index at \code{..}. For example:

\code{extract_samples(mcmcChain, b[i,..])} would return a data frame with:)
\itemize{
    \item column \code{".sample"}: value in \code{[1..nrow(mcmcChain)]}
    \item column \code{"i"}: value in \code{[1..20]}
    \item column \code{"b1"}: value of \code{"b[i,1]"} for sample number \code{".sample"} in mcmc_chain
    \item column \code{"b2"}: value of \code{"b[i,2]"} for sample number \code{".sample"} in mcmc_chain 
    \item column \code{"b3"}: value of \code{"b[i,3]"} for sample number \code{".sample"} in mcmc_chain 
}

An optional clause in the form \code{| wide_index} can also be used to put the data frame into
a wide format based on wide_index. For example, this:

\code{extract_samples(mcmcChain, b[i,v] | v)}

is equivalent to this:

\code{extract_samples(mcmcChain, b[i,v]) \%>\% spread(v,b)}

The main difference between using the \code{|} syntax instead of the \code{..} syntax is that the 
\code{|} syntax respects prototypes applied to indices with \code{\link{apply_prototypes}}, and thus
can be used to get columns with nicer names.

Finally, any expression containing multiple variable names can be used in place of the variable
indexed over in order to extract multiple variables at the same time (so long as they share 
exactly the same subscripts). For example, if we have a variable a[i,v] with the same subscripts
as b[i,v], we could do this:

\code{extract_samples(mcmcChain, cbind(a, b)[i,v])}

Which is equivalent to this:

\code{extract_samples(mcmcChain, a[i,v]) \%>\% left_join(extract_samples(mcmcChain, b[i,v]))}

 
}
\value{
A data frame.
}
\author{
Matthew Kay
}
\seealso{
\code{\link{apply_prototypes}}, \code{\link{compose_data}}.
}
\examples{
##TODO
}
\keyword{manip}
