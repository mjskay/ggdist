# A stat designed for use with geom_slabinterval
#
# Author: mjskay
###############################################################################


#' Parse distribution specifications into columns of a data frame
#'
#' Parses simple string distribution specifications, like `"normal(0, 1)"`, into two columns of
#' a data frame, suitable for use with [stat_dist_slabinterval()] and its shortcut stats
#' (like `stat_dist_halfeye`). This format is output
#' by [brms::get_prior()], making it particularly useful for visualizing priors from
#' brms models.
#'
#' `parse_dist()` can be applied to character vectors or to a data frame + bare column name of the
#' column to parse, and returns a data frame with `".dist"` and `".args"` columns added.
#' `parse_dist()` uses `r_dist_name()` to translate distribution names into names recognized
#' by R.
#'
#' `r_dist_name()` takes a character vector of names and translates common names into R
#' distribution names. Names are first made into valid R names using [make.names()],
#' then translated (ignoring character case, `"."`, and `"_"`). Thus, `"lognormal"`,
#' `"LogNormal"`, `"log_normal"`, `"log-Normal"`, and any number of other variants
#' all get translated into `"lnorm"`.
#'
#' @param object A character vector containing distribution specifications or a data frame with a column
#'  containing distribution specifications.
#' @param dist_col A bare (unquoted) column or column expression that resolves to a character vector
#'  of distribution specifications.
#' @param ...  Arguments passed to other implementations of `parse_dist`.
#' @param dist The name of the output column to contain the distribution name
#' @param args The name of the output column to contain the arguments to the distribution
#' @param to_r_names If `TRUE` (the default), certain common aliases for distribution names are
#'   automatically translated into names that R can recognize (i.e., names which have functions starting
#'   with `r`, `p`, `q`, and `d` representing random number generators, distribution
#'   functions, etc. for that distribution), using the `r_dist_name` function. For example,
#'   `"normal"` is translated into `"norm"` and `"lognormal"` is translated into `"lnorm"`.
#' @param dist_name For `r_dist_name`, a character vector of distribution names to be translated into
#'   distribution names R recognizes. Unrecognized names are left as-is.
#' @seealso See [stat_dist_slabinterval()] and its shortcut stats, which can easily make use of
#' the output of this function using the `dist` and `args` aesthetics.
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
#' @importFrom tibble tibble
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
  na_spec = tibble( # for unparseable specs
    dist = NA,
    args = list(NA)
  )

  result = map_dfr(object, function(dist_spec) {
    # split into name and arg list components
    # e.g. "Normal(0,1)" => "Normal", "0,1)"
    parsed_dist = strsplit(dist_spec, "(", fixed = TRUE)[[1]]

    # check for at least "XXX(YYY..." format
    if (length(parsed_dist) != 2) return(na_spec)

    dist_name = parsed_dist[[1]]
    if (to_r_names) {
      dist_name = r_dist_name(dist_name)
    }

    args_spec = paste0("list(", parsed_dist[[2]])
    args_list = tryCatch(eval(str2lang(args_spec)), error = function(...) NULL)

    if (is.null(args_list)) {
      na_spec
    } else {
      tibble(
        dist = dist_name,
        args = list(args_list)
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



# r_dist_name -------------------------------------------------------------

r_dist_lookup = c(
  normal = "norm",
  lognormal = "lnorm",
  exponential = "exp",
  chisquare = "chisq",
  uniform = "unif",
  studentt = "student_t",  # for brms::student_t
  studentst = "student_t",
  binomial = "binom",
  geometric = "geom",
  hypergeometric = "hyper",
  multinomial = "multinom",
  negbinomial = "nbinom",
  negativebinomial = "nbinom",
  poisson = "pois",
  weibull = "weibull",
  lkj = "lkjcorr"
)

#' @rdname parse_dist
#' @export
r_dist_name = function(dist_name) {
  # clean up names and make lowercase
  dist_name[!is.na(dist_name)] = make.names(dist_name[!is.na(dist_name)])
  r_name = r_dist_lookup[tolower(gsub("[_.]", "", dist_name))]
  names(r_name) = names(dist_name)
  ifelse(is.na(r_name), dist_name, r_name)
}



# check_dist_name ---------------------------------------------------------

# check that distribution names are valid and replace
# invalid ones with NA
check_dist_name = function(dist) {
  invalid =
    is.na(mget(paste0("d", dist), inherits = TRUE, ifnotfound = NA)) |
    is.na(mget(paste0("p", dist), inherits = TRUE, ifnotfound = NA)) |
    is.na(mget(paste0("q", dist), inherits = TRUE, ifnotfound = NA))
  failed_names = dist[invalid & !is.na(dist)]
  if (length(failed_names) > 0) {
    warning(
      "The following distribution names were not recognized and were ignored: \n",
      "    ", paste(failed_names, collapse = ", "), "\n",
      "  See help('stat_dist_slabinterval') for information on specifying distribution names.",
      if ("lkjcorr" %in% failed_names) "\n  See help('marginalize_lkjcorr') for help visualizing LKJ distributions."
    )
  }
  dist[invalid] = NA
  dist
}
