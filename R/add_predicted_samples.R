# add_predicted_samples
# 
# Author: mjskay
###############################################################################

# Names that should be suppressed from global variable check by codetools
# Names used broadly should be put in _global_variables.R
globalVariables(c(".iteration", ".pred"))


#' Add samples from the posterior prediction from a model to a data frame
#' 
#' Given a data frame, adds samples from the posterior predictions of the model to the data
#' in a long format.
#' 
#' \code{add_predicted_samples} and \code{predict_samples} are alternate spellings of the 
#' same function with opposite order of the first two arguments to facilitate use in data
#' processing pipelines that start either with a data frame or a model.
#' 
#' Given equal choice between the two, \code{add_predicted_samples} is the preferred spelling.
#' 
#' @param newdata Data frame to generate predictions from. If omitted, most model types will
#' generate predictions from the data used to fit the model.
#' @param model A supported Bayesian model fit / MCMC object. Currently
#' supported models include \code{\link[coda]{mcmc}}, \code{\link[coda]{mcmc.list}},
#' \code{\link[runjags]{runjags}}, \code{\link[rstan]{stanfit}}, \code{\link[rethinking]{map}},
#' \code{\link[rethinking]{map2stan}}, and anything with its own \code{\link[coda]{as.mcmc.list}}
#' implementation.
#' @param ... Additional arguments passed to the underlying prediction method for the type of
#' model given.
#' @return A data frame (actually, a \code{\link[tibble]{tibble}}) with a \code{.row} column (a
#' factor grouping rows from the input \code{newdata}), \code{.chain} column (the chain
#' each sample came from, or \code{NA} if the model does not provide chain information),
#' \code{.iteration} column (the iteration the sample came from), and \code{.pred} column (a
#' sample from the posterior predictive distribution).
#' @author Matthew Kay
#' @seealso \code{\link{gather_samples}}
#' @keywords manip
#' @importFrom magrittr %>%
#' @importFrom tidyr gather
#' @importFrom dplyr mutate
#' @export
add_predicted_samples = function(newdata, model, ...) {
    predict_samples(model, newdata, ...)
}
    
#' @rdname add_predicted_samples
#' @export
predict_samples = function(model, newdata, ...) UseMethod("predict_samples")

#' @rdname add_predicted_samples
#' @export
predict_samples.default = function(model, newdata, ...) {
    stop(paste0("Models of type ", deparse0(class(model)), " are not currently supported by add_predicted_samples and predict_samples"))
}

#' @rdname add_predicted_samples
#' @export
predict_samples.stanreg = function(model, newdata, ...) {
    if (!requireNamespace("rstan", quietly = TRUE)) {
        stop('The `rstantools` package is needed for `predict_samples` to support `stanreg` objects.'
            , call. = FALSE)
    }
    
    newdata %>%
        data.frame(
            #for some reason calculating row here instead of in a subsequent mutate()
            #is about 3 times faster
            .row = factor(1:nrow(.)),
            .chain = as.numeric(NA),
            t(rstantools::posterior_predict(model, newdata = .)), 
            check.names=FALSE
        ) %>%
        gather(.iteration, .pred, (ncol(newdata)+3):ncol(.)) %>%
        mutate(
            .iteration = as.numeric(.iteration)
        ) %>%
        group_by_(".row", .dots = colnames(newdata))
}
