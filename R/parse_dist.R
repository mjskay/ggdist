# A stat designed for use with geom_slabinterval
#
# Author: mjskay
###############################################################################


#' Parse distribution specifications into columns of a data frame
#'
#' Parses simple string distribution specifications, like \code{"normal(0, 1)"}, into two columns of
#' a data frame, suitable for use with \code{\link{stat_dist_slabinterval}} and its shortcut stats
#' (like \code{stat_dist_halfeye}). This format is output
#' by \code{\link[brms]{get_prior}}, making it particularly useful for visualizing priors from
#' brms models.
#'
#' \code{parse_dist()} can be applied to character vectors or to a data frame + bare column name of the
#' column to parse, and returns a data frame with \code{".dist"} and \code{".args"} columns added.
#' \code{parse_dist()} uses \code{r_dist_name()} to translate distribution names into names reconized
#' by R.
#'
#' \code{r_dist_name()} takes a character vector of names and translates common names into R
#' distribution names. Character case, \code{.}, \code{_}, \code{-}, and spaces are ignored when
#' translating names, so \code{"lognormal"}, \code{"LogNormal"}, \code{"log_normal"},
#' \code{"log-Normal"}, and any number of other variants all get translated into \code{"lnorm"}.
#'
#' @param object A character vector containing distribution specifiations or a data frame with a column
#'  containing distribution specifications.
#' @param dist_col A bare (unquoted) column or column expression that resolves to a character vector
#'  of distribution specifications.
#' @param ...  Arguments passed to other implementations of \code{parse_dist}.
#' @param dist The name of the output column to contain the distribution name
#' @param args The name of the output column to contain the arguments to the distribution
#' @param to_r_names If \code{TRUE} (the default), certain common aliases for distribution names are
#'   automatically translated into names that R can recognize (i.e., names which have functions starting
#'   with \code{r}, \code{p}, \code{q}, and \code{d} representing random number generators, distribution
#'   functions, etc. for that distribution), using the \code{r_dist_name} function. For example,
#'   \code{"normal"} is translated into \code{"norm"} and \code{"lognormal"} is translated into \code{"lnorm"}.
#' @param dist_name For \code{r_dist_name}, a character vector of distribution names to be translated into
#'   distribution names R recognizes. Unrecognized names are left as-is.
#' @seealso See \code{\link{stat_dist_slabinterval}} and its shortcut stats, which can easily make use of
#' the output of this function using the \code{dist} and \code{args} aesthetics.
#' @examples
#'
#' library(dplyr)
#'
#' # parse dist can operate on strings directly...
#' parse_dist(c("normal(0,1)", "student_t(3,0,1)"))
#'
#' # ... or on columns of a data frame, where it adds the
#' # parsed specs back on as columns
#' data.frame(prior = c("normal(0,1)", "student_t(3,0,1)")) %>%
#'   parse_dist(prior)
#'
#' if (
#'   require("brms", quietly = TRUE)
#' ) {
#'
#'   # parse_dist is particularly useful with brms prior specifications,
#'   # which follow the same format...
#'
#'   # get priors for a brms model
#'   priors = get_prior(mpg ~ log(hp), data = mtcars, family = lognormal)
#'
#'   # The `prior` column output by `get_prior()` is a character vector
#'   # of distribution specifications. We can parse this directly...
#'   parse_dist(priors$prior)
#'
#'   # ... or we can parse it and have it added back onto the original data frame
#'   # (this form is likely more useful for plotting, since the other columns
#'   # are retained)
#'   parse_dist(priors, prior)
#'
#' }
#'
#' @importFrom dplyr tibble
#' @importFrom purrr map_dfr
#' @export
parse_dist = function(object, ..., dist = ".dist", args = ".args", to_r_names = TRUE) {
  UseMethod("parse_dist")
}

#' @rdname parse_dist
#' @export
parse_dist.default = function(object, ...) {
  stop(
    "Objects of type ", deparse0(class(object)), " are not currently supported by `parse_dist`.\n",
    "A character vector or a data frame are expected.\n"
  )
}

#' @rdname parse_dist
#' @export
parse_dist.data.frame = function(object, dist_col, ..., dist = ".dist", args = ".args", to_r_names = TRUE) {
  dists = eval_tidy(enquo(dist_col), object)
  parsed_dists = parse_dist(dists, ..., to_r_names = to_r_names)

  object[[dist]] = parsed_dists$.dist
  object[[args]] = parsed_dists$.args
  object
}

#' @rdname parse_dist
#' @export
parse_dist.character = function(object, ..., dist = ".dist", args = ".args", to_r_names = TRUE) {
  result = map_dfr(object, function(dist_spec) {
    parsed_dist = tryCatch(str2lang(dist_spec), error = function(...) NULL)

    if (is.null(parsed_dist)) {
      tibble(
        dist = NA,
        args = list(NA)
      )
    } else{
      dist_name = as.character(parsed_dist[[1]])
      if (to_r_names) {
        dist_name = r_dist_name(dist_name)
      }

      args_list = parsed_dist
      args_list[[1]] = quote(list)

      tibble(
        dist = dist_name,
        args = list(eval(args_list))
      )
    }
  })

  names(result) = c(dist, args)
  result
}

#' @rdname parse_dist
#' @export
parse_dist.factor = function(object, ..., dist = ".dist", args = ".args", to_r_names = TRUE) {
  parse_dist(as.character(object))
}

r_dist_lookup = c(
  normal = "norm",
  lognormal = "lnorm",
  exponential = "exp",
  chisquare = "chisq",
  uniform = "unif"
)

#' @rdname parse_dist
#' @export
r_dist_name = function(dist_name) {
  r_name = r_dist_lookup[tolower(gsub("[ ._-]+", "", dist_name))]
  names(r_name) = names(dist_name)
  ifelse(is.na(r_name), dist_name, r_name)
}
