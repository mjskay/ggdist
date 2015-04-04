\name{extract_samples}
\alias{extract_samples}
\title{Extract samples from a Bayesian sampler in a tidy data format}
\description{
Extract samples from an MCMC chain for a variable with the given named indices into a long-format data frame.
}
\usage{
extract_samples(model, variable_spec, prototypes=NULL)
}
\arguments{
  \item{model}{
A supported Bayesain model fit / MCMC object. Currently supported models include \code{\link[coda]{mcmc}}. 
}
  \item{variable_spec}{
A formula in the form of \code{~ variable_name[index_1, index_2, ...]}. See `Details`.
}
  \item{prototypes}{
A list or data.frame providing data prototypes used to convert returned columns back into useful data types. See `Details`. 
}
}
\details{
Imagine a variable b[i,v] with i in [1..100] and v in [1..3]. An MCMC sample returned from JAGS 
(for example) would have columns with names like "b[1,1]", "b[2,1]", etc. 

\code{extract_samples(mcmc_chain, ~ b[i,v])} would return a data frame with:)
\itemize{
    \item column \code{".sample"}: value in \code{[1..nrow(mcmcChain)]}
    \item column \code{"i"}: value in \code{[1..20]}
    \item column \code{"v"}: value in \code{[1..3]}
    \item column \code{"b"}: value of \code{"b[i,v]"} for sample number \code{".sample"} in \code{mcmc_chain}.
}

The shorthand \code{".."} can be used to specify one column that should be put into a wide format. For example:

\code{extract_samples(mcmcChain, ~ b[i,..])} would return a data frame with:)
\itemize{
    \item column \code{".sample"}: value in \code{[1..nrow(mcmcChain)]}
    \item column \code{"i"}: value in \code{[1..20]}
    \item column \code{"b1"}: value of \code{"b[i,1]"} for sample number \code{".sample"} in mcmc_chain
    \item column \code{"b2"}: value of \code{"b[i,2]"} for sample number \code{".sample"} in mcmc_chain 
    \item column \code{"b3"}: value of \code{"b[i,3]"} for sample number \code{".sample"} in mcmc_chain 
}

\code{prototypes} optionally specifies a list or data.frame. Each entry in prototypes with the same name
as the variable or an index in varible_spec is a used as a prototype for that variable or index ---
i.e., its type is taken to be the expected type of that variable or index. Those types are used to translate 
numeric values of variables back into useful values (usually levels of a factor). 

The most common use of prototypes is to automatically translate indices that correspond to levels of a factor
in the original data back into levels of that factor. Names in prototypes that are not in 
found in variable_spec are ignored.

The usual use of prototypes is to pass in the data frame from which the original data came. 
Supported types of prototypes are factor, ordered, and logical. For example:

\itemize{
    \item if \code{prototypes$v} is a factor, the v column in the returned samples is translated into 
        a factor using \code{factor(v, labels=levels(prototypes$v), ordered=is.ordered(prototypes$v))}.
    \item if \code{prototypes$v} is a logical, the v column is translated into a logical using \code{as.logical(v)}.
}
}
\value{
A data frame.
}
\author{
Matthew Kay
}
\seealso{
\code{\link{compose_data}}.
}
\examples{
##TODO
}
\keyword{manip}
