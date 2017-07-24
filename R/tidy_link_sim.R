# tidy_link and tidy_sim
#
# Author: mjskay
###############################################################################

check_for_rethinking = function(name) {
    if (!requireNamespace("rethinking", quietly = TRUE)) {
        stop(paste0(
            "The `rethinking` package is needed for `', name, '` function to work.\n",
            "Install it via devtools::install_github(\"rmcelreath/rethinking\")\n",
            "For more information see https://github.com/rmcelreath/rethinking"
        ), call. = FALSE)
    }
}

#' Tidy predictions for map and map2stan models
#'
#' Computes inverse-link linear model samples for \code{\link[rethinking]{map}} and
#' \code{\link[rethinking]{map2stan}} models in a tidy data format (a tidy version of
#' \code{\link[rethinking]{link}}).
#'
#' @param data Optional list of data to compute predictions over. When missing, uses data found inside fit object.
#' @param fit Object of class \code{\link[rethinking]{map}} or \code{\link[rethinking]{map2stan}}.
#' @param ... Additional arguments for \code{\link[rethinking]{link}}.
#'
#' Computes samples for all link functions in \code{fit} and returns a tidy-format data frame
#' consisting of multiple samples for each row of \code{data}.
#'
#' @seealso \code{\link[rethinking]{link}}
#'
#' @export
tidy_link = function(data, fit, ...) {
    check_for_rethinking("tidy_link")

    tidy_link_sim_(rethinking::link, data, fit,
        #if there is only a single link in the model, link() will
        #return a matrix instead of a list of matrices, so we must give the name
        #of the link variable here so that tidy_link_sim_ can include the name
        #in the results
        single_response_name = first_link_name(fit),
        ...)
}

#' Tidy posterior observations for map and map2stan models
#'
#' Simulates posterior observations for \code{\link[rethinking]{map}} and \code{\link[rethinking]{map2stan}} models
#' in a tidy data format (a tidy version of \code{\link[rethinking]{sim}}).
#'
#' @param data Optional list of data to compute predictions over. When missing, uses data found inside fit object.
#' @param fit Object of class \code{\link[rethinking]{map}} or \code{\link[rethinking]{map2stan}}.
#' @param name If \code{NULL}, uses the name of the first response specification in the model with ".predicted"
#' appended as the column representing predictions. Otherwise, gives a bare (unquoted) name to assign
#' predicted observations to.
#' @param ... Additional arguments for \code{\link[rethinking]{sim}}.
#'
#' Simulates posterior observations for all link functions in \code{fit} and returns a tidy-format data frame
#' consisting of multiple samples for each row of \code{data}.
#'
#' @seealso \code{\link[rethinking]{sim}}
#'
#' @export
tidy_sim = function(data, fit, name = NULL, ...) {
    check_for_rethinking("tidy_sim")

    name = substitute(name)
    if (is.null(name)) {
        name = paste0(deparse0(first_y_expr(fit)), ".predicted")
    }
    else {
        name = deparse0(name)
    }
    tidy_link_sim_(rethinking::sim, data, fit, single_response_name = name, ...)
}

#' @import dplyr
tidy_link_sim_ = function(fun, data, fit,
    single_response_name, #name used when only one response is returned
    ...) {

    if (missing(data)) {
        data = fit@data
    }

    # some of the  functions below don't always play well with tbl_dfs, so convert `data`
    # to a data.frame on the way in
    data = as.data.frame(data)

    # get link/sim results as a matrix or list of matrices
    responses = fun(fit, data, ...)

    if (!is.list(responses)) {
        #if there is only one link definition in the model, or if we are using sim(),
        #only a single matrix will be returned, instead of a list of matrices indexed
        #by the link parameter name. Thus we must convert it into a list
        responses = list(responses)
        names(responses) = single_response_name
    }

    #tidy it up: make it long format with and a single column for each response
    #TODO: properly recover chain and iteration information here
    cbind(
        .chain = 1,
        .iteration = 1:nrow(responses[[1]]),
        #flatten each response matrix into a single vector; the sample index above
        #will be repeated to fit the length of the flattened vector, thus becomming
        #equivalent to the row index in the original matrix
        as.data.frame(lapply(responses, function(r) as.vector(r)))
    ) %>%
        #add the predictors back in for each sample
        group_by_(".chain", ".iteration") %>%
        do(cbind(., data)) %>%
        #group by the predictors so that functions like mean_qi() "just work" on the
        #resulting data frame
        group_by_(.dots = names(data))
}
