# A stat designed for use with geom_slabinterval
#
# Author: mjskay
###############################################################################


# Names that should be suppressed from global variable check by codetools
# Names used broadly should be put in _global_variables.R
globalVariables("prior")


#' Parse distribution specifications into columns of a data frame
#'
#' Parses simple string distribution specifications, like `"normal(0, 1)"`, into two columns of
#' a data frame, suitable for use with the `dist` and `args` aesthetics of [stat_slabinterval()]
#' and its shortcut stats (like [stat_halfeye()]). This format is output
#' by `brms::get_prior`, making it particularly useful for visualizing priors from
#' brms models.
#'
#' [parse_dist()] can be applied to character vectors or to a data frame + bare column name of the
#' column to parse, and returns a data frame with `".dist"` and `".args"` columns added.
#' [parse_dist()] uses [r_dist_name()] to translate distribution names into names recognized
#' by R.
#'
#' [r_dist_name()] takes a character vector of names and translates common names into R
#' distribution names. Names are first made into valid R names using [make.names()],
#' then translated (ignoring character case, `"."`, and `"_"`). Thus, `"lognormal"`,
#' `"LogNormal"`, `"log_normal"`, `"log-Normal"`, and any number of other variants
#' all get translated into `"lnorm"`.
#'
#' @param object <[character] | [data.frame]> One of:
#'  - A character vector containing distribution specifications, like `c("normal(0,1)", "exp(1)")`
#'  - A data frame with a column containing distribution specifications.
#' @param dist_col <bare [language]> Column or column expression of `object` that resolves to a
#' character vector of distribution specifications (when `object` is a [data.frame()]).
#' @param ...  Arguments passed to other implementations of `parse_dist()`.
#' @param dist <[string][character]> The name of the output column to contain the distribution name.
#' @param args <[string][character]> The name of the output column to contain the arguments to the distribution.
#' @param dist_obj <[string][character]> The name of the output column to contain a \pkg{distributional}
#' object representing the distribution.
#' @param package <[string][character] | [environment] | [NULL]> The package or environment to search for
#' distribution functions in. Passed to [distributional::dist_wrap()]. One of:
#'   - a string: use the environment for the package with the given name
#'   - an [environment]: use the given environment
#'   - `NULL` (default): use the calling environment
#' @param lb <[string][character]> The name of an input column (for `data.frame` and `brms::prior` objects)
#' that contains the lower bound of the distribution, which if present will produce a truncated distribution
#' using [`dist_truncated()`][distributional::dist_truncated]. Ignored if `object[[lb]]` is `NULL` or if
#' it is `NA` for the corresponding input row.
#' @param ub <[string][character]> The name of an input column (for `data.frame` and `brms::prior` objects)
#' that contains the upper bound of the distribution, which if present will produce a truncated distribution
#' using [`dist_truncated()`][distributional::dist_truncated]. Ignored if `object[[ub]]` is `NULL` or if
#' it is `NA` for the corresponding input row.
#' @param to_r_names <scalar [logical]> If `TRUE` (the default), certain common aliases for distribution
#' names are automatically translated into names that R can recognize (i.e., names which have functions
#' starting with `r`, `p`, `q`, and `d` representing random number generators, distribution functions,
#' etc. for that distribution), using the `r_dist_name` function. For example, `"normal"` is translated
#' into `"norm"` and `"lognormal"` is translated into `"lnorm"`.
#' @param dist_name <[character]> For `r_dist_name()`, a character vector of distribution names to be
#' translated into distribution names R recognizes. Unrecognized names are left as-is.
#' @return
#'
#' - `parse_dist` returns a data frame containing at least two columns named after the `dist` and `args`
#' parameters. If the input is a data frame, the output is a data frame of the same length with those
#' two columns added. If the input is a character vector or factor, the output is a two-column data frame
#' with the same number of rows as the length of the input.
#'
#' - `r_dist_name` returns a character vector the same length as the input containing translations of the
#' input names into distribution names R can recognize.
#'
#' @seealso See [stat_slabinterval()] and its shortcut stats, which can easily make use of
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
#' # parse_dist is particularly useful with the output of brms::prior(),
#' # which follows the same format as above
#'
#' @importFrom tibble tibble
#' @importFrom distributional dist_wrap dist_truncated
#' @export
parse_dist = function(
  object,
  ...,
  dist = ".dist",
  args = ".args",
  dist_obj = ".dist_obj",
  package = NULL,
  to_r_names = TRUE
) {
  UseMethod("parse_dist")
}

#' @rdname parse_dist
#' @export
parse_dist.default = function(object, ...) {
  cli_abort(
    c(
      "{.arg object} must be a character vector, factor, data frame, or {.help brms::prior} object.",
      "x" = "{.arg object} was {.type {object}}"
    ),
    class = "ggdist_unsupported_type"
  )
}

#' @rdname parse_dist
#' @export
parse_dist.data.frame = function(
  object,
  dist_col,
  ...,
  dist = ".dist",
  args = ".args",
  dist_obj = ".dist_obj",
  package = NULL,
  lb = "lb",
  ub = "ub",
  to_r_names = TRUE
) {
  package = package %||% parent.frame()

  dists = eval_tidy(enquo(dist_col), object)
  parsed_dists = parse_dist(dists, ..., package = package, to_r_names = to_r_names)

  object[[dist]] = parsed_dists$.dist
  object[[args]] = parsed_dists$.args
  object[[dist_obj]] = parsed_dists$.dist_obj

  # add upper/lower bounds using truncation
  lbs = as.numeric(object[[lb]])
  ubs = as.numeric(object[[ub]])
  should_truncate = !is.na(lbs) | !is.na(ubs)
  if (length(should_truncate) > 0) {
    lbs = lbs[should_truncate]
    ubs = ubs[should_truncate]
    # need to replace NAs with Infs in bounds since NA for brms means no truncation (i.e. Inf)
    lbs[is.na(lbs)] = -Inf
    ubs[is.na(ubs)] = Inf
    object[[dist_obj]][should_truncate] = dist_truncated(object[[dist_obj]][should_truncate], lower = lbs, upper = ubs)
  }

  object
}

#' @rdname parse_dist
#' @export
parse_dist.character = function(
  object,
  ...,
  dist = ".dist",
  args = ".args",
  dist_obj = ".dist_obj",
  package = NULL,
  to_r_names = TRUE
) {
  package = package %||% parent.frame()

  na_spec = tibble( # for unparseable specs
    dist = NA,
    args = list(NA)
  )

  result = map_dfr_(object, function(dist_spec) {
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
    args_list = tryCatch(eval(parse(text = args_spec)), error = function(...) NULL)

    if (is.null(args_list)) {
      na_spec
    } else {
      tibble(
        dist = dist_name,
        args = list(args_list),
        dist_obj = do.call(dist_wrap, c(list(dist = dist_name, package = package), args_list))
      )
    }
  })

  names(result) = c(dist, args, dist_obj)
  result
}

#' @rdname parse_dist
#' @export
parse_dist.factor = function(
  object,
  ...,
  dist = ".dist",
  args = ".args",
  dist_obj = ".dist_obj",
  package = NULL,
  to_r_names = TRUE
) {
  package = package %||% parent.frame()
  parse_dist(
    as.character(object), ...,
    dist = dist, args = args, dist_obj = dist_obj, package = package, to_r_names = to_r_names
  )
}

#' @rdname parse_dist
#' @export
parse_dist.brmsprior = function(
  object,
  dist_col = prior,
  ...,
  dist = ".dist",
  args = ".args",
  dist_obj = ".dist_obj",
  package = NULL,
  to_r_names = TRUE
) {
  package = package %||% parent.frame()
  parse_dist.data.frame(
    as.data.frame(object), {{ dist_col }}, ...,
    dist = dist, args = args, dist_obj = dist_obj, package = package, to_r_names = to_r_names
  )
}


# r_dist_name -------------------------------------------------------------

r_dist_lookup = c(
  normal = "norm",
  lognormal = "lnorm",
  exponential = "exp",
  chisquare = "chisq",
  uniform = "unif",
  studentt = "student_t",
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
  dist = as.character(dist)
  invalid =
    is.na(mget(paste0("d", dist), inherits = TRUE, ifnotfound = NA)) |
    is.na(mget(paste0("p", dist), inherits = TRUE, ifnotfound = NA)) |
    is.na(mget(paste0("q", dist), inherits = TRUE, ifnotfound = NA))
  failed_names = dist[invalid & !is.na(dist)]
  if (length(failed_names) > 0) {
    cli_warn(
      c(
        "The following distribution names were not recognized and were ignored: {failed_names}",
        "i" = "See {.emph Details} in the {.help ggdist::stat_slabinterval} documentation for information
          on specifying distribution names.",
        if ("lkjcorr" %in% failed_names) c(
          "i" = "See the {.help ggdist::marginalize_lkjcorr} documentation for help visualizing LKJ
            distributions."
        )
      ),
      class = "ggdist_unsupported_distribution_name"
    )
  }
  dist[invalid] = NA
  dist
}
