# spread_draws (used to be "extract_samples" / "tidy_samples" / "spread_samples")
#
# Author: mjskay
###############################################################################

# Names that should be suppressed from global variable check by codetools
# Names used broadly should be put in _global_variables.R
globalVariables(c(".."))


# deprecated names for spread_draws --------------------------------------

#' @rdname tidybayes-deprecated
#' @format NULL
#' @usage NULL
#' @export
extract_samples = function(...) {
  .Deprecated("spread_draws", package = "tidybayes") # nocov
  spread_draws(...)               # nocov
}

#' @rdname tidybayes-deprecated
#' @format NULL
#' @usage NULL
#' @export
tidy_samples = function(...) {
  .Deprecated("spread_draws", package = "tidybayes") # nocov
  spread_draws(...)              # nocov
}

#' @rdname tidybayes-deprecated
#' @format NULL
#' @usage NULL
#' @export
spread_samples = function(...) {
  .Deprecated("spread_draws", package = "tidybayes") # nocov
  spread_draws(...)              # nocov
}


# spread_draws ----------------------------------------------------------

#' Extract draws of variables in a Bayesian model fit into a tidy data format
#'
#' Extract draws from a Bayesian model for one or more variables (possibly with named
#' dimensions) into one of two types of long-format data frames.
#'
#' Imagine a JAGS or Stan fit named \code{fit}. The model may contain a variable named
#' \code{b[i,v]} (in the JAGS or Stan language) with dimension \code{i} in \code{1:100} and
#' dimension \code{v} in \code{1:3}. However, the default format for draws returned from
#' JAGS or Stan in R will not reflect this indexing structure, instead
#' they will have multiple columns with names like \code{"b[1,1]"}, \code{"b[2,1]"}, etc.
#'
#' \code{spread_draws} and \code{gather_draws} provide a straightforward
#' syntax to translate these columns back into properly-indexed variables in two different
#' tidy data frame formats, optionally recovering dimension types (e.g. factor levels) as it does so.
#'
#' \code{spread_draws} and \code{gather_draws} return data frames already grouped by
#' all dimensions used on the variables you specify.
#'
#' The difference between \code{spread_draws} is that names of variables in the model will
#' be spread across the data frame as column names, whereas \code{gather_draws} will
#' gather variables into a single column named \code{".variable"} and place values of variables into a
#' column named \code{".value"}. To use naming schemes from other packages (such as \code{broom}), consider passing
#' results through functions like \code{\link{to_broom_names}} or \code{\link{to_ggmcmc_names}}.
#'
#' For example, \code{spread_draws(fit, a[i], b[i,v])} might return a grouped
#' data frame (grouped by \code{i} and \code{v}), with:
#' \itemize{
#'    \item column \code{".chain"}: the chain number. \code{NA} if not applicable to the model
#'      type; this is typically only applicable to MCMC algorithms.
#'    \item column \code{".iteration"}: the iteration number. Guaranteed to be unique within-chain only.
#'      \code{NA} if not applicable to the model type; this is typically only applicable to MCMC algorithms.
#'    \item column \code{".draw"}: a unique number for each draw from the posterior. Order is not
#'      guaranteed to be meaningful.
#'    \item column \code{"i"}: value in \code{1:5}
#'    \item column \code{"v"}: value in \code{1:10}
#'    \item column \code{"a"}: value of \code{"a[i]"} for draw \code{".draw"}
#'    \item column \code{"b"}: value of \code{"b[i,v]"} for draw \code{".draw"}
#'  }
#'
#' \code{gather_draws(fit, a[i], b[i,v])} on the same fit would return a grouped
#' data frame (grouped by \code{i} and \code{v}), with:
#' \itemize{
#'    \item column \code{".chain"}: the chain number
#'    \item column \code{".iteration"}: the iteration number
#'    \item column \code{".draw"}: the draw number
#'    \item column \code{"i"}: value in \code{1:5}
#'    \item column \code{"v"}: value in \code{1:10}, or \code{NA}
#'      if \code{".variable"} is \code{"a"}.
#'    \item column \code{".variable"}: value in \code{c("a", "b")}.
#'    \item column \code{".value"}: value of \code{"a[i]"} (when \code{".variable"} is \code{"a"})
#'      or \code{"b[i,v]"} (when \code{".variable"} is \code{"b"}) for draw \code{".draw"}
#'  }
#'
#' \code{spread_draws} and \code{gather_draws} can use type information
#' applied to the \code{fit} object by \code{\link{recover_types}} to convert columns
#' back into their original types. This is particularly helpful if some of the dimensions in
#' your model were originally factors. For example, if the \code{v} dimension
#' in the original data frame \code{data} was a factor with levels \code{c("a","b","c")},
#' then we could use \code{recover_types} before \code{spread_draws}:
#'
#' \preformatted{fit \%>\%
#'  recover_types(data) %\>\%
#'  spread_draws(fit, b[i,v])
#' }
#'
#' Which would return the same data frame as above, except the \code{"v"} column
#' would be a value in \code{c("a","b","c")} instead of \code{1:3}.
#'
#' For variables that do not share the same subscripts (or share
#' some but not all subscripts), we can supply their specifications separately.
#' For example, if we have a variable d[i] with the same i subscript
#' as b[i,v], and a variable x with no subscripts, we could do this:
#'
#' \preformatted{spread_draws(fit, x, d[i], b[i,v])}
#'
#' Which is roughly equivalent to this:
#'
#' \preformatted{spread_draws(fit, x) \%>\%
#'  inner_join(spread_draws(fit, d[i])) \%>\%
#'  inner_join(spread_draws(fit, b[i,v])) \%>\%
#'  group_by(i,v)
#' }
#'
#' Similarly, this:
#'
#' \preformatted{gather_draws(fit, x, d[i], b[i,v])}
#'
#' Is roughly equivalent to this:
#'
#' \preformatted{bind_rows(
#'  gather_draws(fit, x),
#'  gather_draws(fit, d[i]),
#'  gather_draws(fit, b[i,v])
#' )}
#'
#'
#' The \code{c} and \code{cbind} functions can be used to combine multiple variable names that have
#' the same dimensions. For example, if we have several variables with the same
#' subscripts \code{i} and \code{v}, we could do either of these:
#'
#' \preformatted{spread_draws(fit, c(w, x, y, z)[i,v])}
#' \preformatted{spread_draws(fit, cbind(w, x, y, z)[i,v])}  # equivalent
#'
#' Each of which is roughly equivalent to this:
#'
#' \preformatted{spread_draws(fit, w[i,v], x[i,v], y[i,v], z[i,v])}
#'
#' Besides being more compact, the \code{c()}-style syntax is currently also
#' faster (though that may change).
#'
#' Dimensions can be omitted from the resulting data frame by leaving their names
#' blank; e.g. \code{spread_draws(fit, b[,v])} will omit the first dimension of
#' \code{b} from the output. This is useful if a dimension is known to contain all
#' the same value in a given model.
#'
#' The shorthand \code{..} can be used to specify one column that should be put
#' into a wide format and whose names will be the base variable name, plus a dot
#' ("."), plus the value of the dimension at \code{..}. For example:
#'
#' \code{spread_draws(fit, b[i,..])} would return a grouped data frame
#' (grouped by \code{i}), with:
#' \itemize{
#'  \item column \code{".chain"}: the chain number
#'  \item column \code{".iteration"}: the iteration number
#'  \item column \code{".draw"}: the draw number
#'  \item column \code{"i"}: value in \code{1:20}
#'  \item column \code{"b.1"}: value of \code{"b[i,1]"} for draw \code{".draw"}
#'  \item column \code{"b.2"}: value of \code{"b[i,2]"} for draw \code{".draw"}
#'  \item column \code{"b.3"}: value of \code{"b[i,3]"} for draw \code{".draw"}
#' }
#'
#' An optional clause in the form \code{| wide_dimension} can also be used to put
#' the data frame into a wide format based on \code{wide_dimension}. For example, this:
#'
#' \preformatted{spread_draws(fit, b[i,v] | v)}
#'
#' is roughly equivalent to this:
#'
#' \preformatted{spread_draws(fit, b[i,v]) \%>\% spread(v,b)}
#'
#' The main difference between using the \code{|} syntax instead of the
#' \code{..} syntax is that the \code{|} syntax respects prototypes applied to
#' dimensions with \code{\link{recover_types}}, and thus can be used to get
#' columns with nicer names. For example:
#'
#' \code{fit \%>\% recover_types(data) \%>\% spread_draws(b[i,v] | v)} would return a grouped data frame
#' (grouped by \code{i}), with:
#' \itemize{
#'  \item column \code{".chain"}: the chain number
#'  \item column \code{".iteration"}: the iteration number
#'  \item column \code{".draw"}: the draw number
#'  \item column \code{"i"}: value in \code{1:20}
#'  \item column \code{"a"}: value of \code{"b[i,1]"} for draw \code{".draw"}
#'  \item column \code{"b"}: value of \code{"b[i,2]"} for draw \code{".draw"}
#'  \item column \code{"c"}: value of \code{"b[i,3]"} for draw \code{".draw"}
#' }
#'
#' Finally, variable names can be regular expressions by setting \code{regex = TRUE}; e.g.:
#'
#' \code{spread_draws(fit, `b_.*`[i], regex = TRUE)}
#'
#' Would return a tidy data frame with variables starting with `b_` and having one dimension.
#'
#' @param model A supported Bayesian model fit. Tidybayes supports a variety of model objects;
#' for a full list of supported models, see \link{tidybayes-models}.
#' @param ... Expressions in the form of
#' \code{variable_name[dimension_1, dimension_2, ...] | wide_dimension}. See `Details`.
#' @param regex If \code{TRUE}, variable names are treated as regular expressions and all column matching the
#' regular expression and number of dimensions are included in the output. Default \code{FALSE}.
#' @param sep Separator used to separate dimensions in variable names, as a regular expression.
#' @return A data frame.
#' @author Matthew Kay
#' @seealso \code{\link{recover_types}}, \code{\link{compose_data}}.
#' @keywords manip
#' @examples
#'
#' library(dplyr)
#' library(ggplot2)
#'
#' data(RankCorr, package = "tidybayes")
#'
#' RankCorr %>%
#'   spread_draws(b[i, j])
#'
#' RankCorr %>%
#'   spread_draws(b[i, j], tau[i], u_tau[i])
#'
#'
#' RankCorr %>%
#'   gather_draws(b[i, j], tau[i], u_tau[i])
#'
#' RankCorr %>%
#'   gather_draws(tau[i], typical_r) %>%
#'   median_qi()
#'
#' @importFrom rlang enquos
#' @importFrom purrr reduce
#' @importFrom dplyr inner_join group_by_at
#' @rdname spread_draws
#' @export
spread_draws = function(model, ..., regex = FALSE, sep = "[, ]") {
  tidysamples = lapply(enquos(...), function(variable_spec) {
    spread_draws_(model, variable_spec, regex = regex, sep = sep)
  })

  #get the groups from all the samples --- when we join them together,
  #the grouping information is lost (actually, only the groups on the
  #first data frame in a join is retained), so we'll have to recreate
  #the full set of groups from all the data frames after we join them
  groups_ = tidysamples %>%
    map(group_vars) %>%
    reduce(union)

  tidysamples %>%
    reduce(function(tidysample1, tidysample2) {
      by_ = intersect(names(tidysample1), names(tidysample2))
      inner_join(tidysample1, tidysample2, by = by_)
    }) %>%
    group_by_at(groups_)
}

#' @import dplyr
#' @importFrom tidyr spread_
#' @importFrom tibble has_name
spread_draws_ = function(model, variable_spec, regex = FALSE, sep = "[, ]") {
  #parse a variable spec in the form variable_name[dimension_name_1, dimension_name_2, ..] | wide_dimension
  spec = parse_variable_spec(variable_spec)
  variable_names = spec[[1]]
  dimension_names = spec[[2]]
  wide_dimension_name = spec[[3]]

  #extract the draws into a long data frame
  draws = spread_draws_long_(model, variable_names, dimension_names, regex = regex, sep = sep)

  #convert variable and/or dimensions back into usable data types
  constructors = attr(model, "constructors")
  if (is.null(constructors)) constructors = list()
  for (column_name in c(variable_names, dimension_names)) {
    if (column_name %in% names(constructors)) {
      #we have a data type constructor for this dimension, convert it
      draws[[column_name]] = constructors[[column_name]](draws[[column_name]])
    }
  }

  #spread a column into wide format if requested (only if one variable, because
  #we can't spread multiple keys simultaneously for the same value)
  if (!is.null(wide_dimension_name)) {
    #wide dimension requested by name
    if (length(variable_names) != 1) {
      stop("Cannot extract draws from multiple variables in wide format.")
    }
    draws %>%
      spread_(wide_dimension_name, variable_names)
  }
  else if (has_name(draws, "..")) {
    #a column named ".." is present, use it to form a wide version of the data
    #with numbered names based on the variable name
    if (length(variable_names) != 1) {
      stop("Cannot extract draws from multiple variables in wide format.")
    }
    draws %>%
      #the ".." column will have been set as a grouping column because it was
      #specified as a dimension; therefore before we can modify it we have to
      #remove it from the grouping columns on this table (mutate does not
      #allow you to modify grouping columns)
      group_by_at(setdiff(group_vars(.), "..")) %>%
      mutate(.. = paste0(variable_names, ".", ..)) %>%
      spread_("..", variable_names)
  }
  else {
    #no wide column => just return long version
    draws
  }
}

#' @importFrom tidyr spread_ separate_ gather_
#' @import stringi
#' @import dplyr
spread_draws_long_ = function(model, variable_names, dimension_names, regex = FALSE, sep = "[, ]") {
  draws = tidy_draws(model)
  if (!regex) {
    variable_names = escape_regex(variable_names)
  }

  if (is.null(dimension_names)) {
    #no dimensions, just find the colnames matching the regex(es)
    variable_regex = paste0("^(", paste(variable_names, collapse = "|"), ")$")
    variable_names_index = stri_detect_regex(colnames(draws), variable_regex)

    if (!any(variable_names_index)) {
      stop(paste0("No variables found matching spec: ",
        "c(", paste0(variable_names, collapse = ","), ")"
      ))
    }

    variable_names = colnames(draws)[variable_names_index]
    draws[, c(".chain", ".iteration", ".draw", variable_names)]
  }
  else {
    dimension_sep_regex = sep
    dimension_regex = "(.+)"

    #find the variables to extract matching the given names and number of dimensions
    variable_regex = paste0("^",
      #variable name
      "(", paste(variable_names, collapse = "|"), ")\\[",
      #dimensions
      paste0(rep(dimension_regex, length(dimension_names)), collapse = dimension_sep_regex),
      "\\]$"
    )
    variable_names_index = stri_detect_regex(colnames(draws), variable_regex)
    if (!any(variable_names_index)) {
      stop(paste0("No variables found matching spec: ",
        "c(", paste0(variable_names, collapse = ","), ")",
        "[", paste0(dimension_names, collapse = ","), "]"
      ))
    }
    variable_names = colnames(draws)[variable_names_index]

    #rename columns to drop trailing "]" to eliminate extraneous last column
    #when we do separate(), below. e.g. "x[1,2]" becomes "x[1,2". Do the same
    #with variable_names so we can select the columns
    colnames(draws)[variable_names_index] = stri_sub(colnames(draws)[variable_names_index], to = -2)
    variable_names = stri_sub(variable_names, to = -2)

    #specs containing empty dimensions (e.g. mu[] or mu[,k]) will produce
    #some dimension_names == ""; we can't use empty variable names below, so we
    #replace them with the ".drop" placeholder and then drop those columns later.
    #TODO: probably a better way to do this.
    temp_dimension_names = dimension_names %>%
      #must give each blank dimension column a unique name, otherwise spread_() won't work below
      ifelse(. == "", paste0(".drop", seq_along(.)), .)
    dimension_names = dimension_names[dimension_names != ""]

    # Make long format data frame of the variables we want to split.
    # The following code chunk is approximately equivalent to this:
    #
    #   long_draws = draws[, c(".chain", ".iteration", ".draw", variable_names)] %>%
    #     gather_(".variable", ".value", variable_names)
    #
    # but takes half as long to run (makes a difference with large samples):
    long_draws = draws[,c(".chain",".iteration",".draw")] %>%
      cbind(map_dfr(variable_names, function(variable_name) data.frame(
        .variable = variable_name,
        .value = draws[[variable_name]],

        stringsAsFactors = FALSE
      )))

    long_draws %>%
      #next, split dimensions in variable names into columns
      separate_(".variable", c(".variable", ".dimensions"), sep = "\\[|\\]") %>%
      separate_(".dimensions", temp_dimension_names, sep = dimension_sep_regex,
        convert = TRUE #converts dimensions to numerics if applicable
      ) %>%
      #now, make the value of each variable a column
      spread_(".variable", ".value") %>%
      #drop the columns that correpond to blank dimensions in the original spec
      select(-starts_with(".drop")) %>%
      #group by the desired dimensions so that we return a pre-grouped data frame to the user
      group_by_at(dimension_names)
  }
}


# helpers -----------------------------------------------------------------

# get all variable names from an expression
# based on http://adv-r.had.co.nz/dsl.html
all_names <- function(x) {
  if (is.atomic(x)) {
    NULL
  } else if (is.name(x)) {
    name = as.character(x)
    if (name == "") {
      NULL
    }
    else {
      name
    }
  } else if (is.call(x) || is.pairlist(x)) {
    children <- lapply(x[-1], all_names)
    unique(unlist(children))
  } else {
    stop("Don't know how to handle type `", typeof(x), "`",
      call. = FALSE)
  }
}

#parse a variable spec in the form variable_name[dimension_name_1, dimension_name_2, ..] | wide_dimension
#into a list with three elements:
# 1. A vector of variable names
# 2. A vector of dimension names (or NULL if none)
# 3. The name of the wide dimension (or NULL if none)
#' @importFrom purrr reduce map map2
#' @importFrom rlang set_names
parse_variable_spec = function(variable_spec) {
  names = all_names(variable_spec[[2]])
  #specs for each bare variable name in the spec expression
  names_spec = names %>%
    set_names() %>%
    map(function(name) list(name, NULL, NULL))


  c_function = function(...) {
    reduce(list(...), function(spec1, spec2) map2(spec1, spec2, base::c))
  }

  spec_env = as.environment(c(
    list(
      c = c_function,
      cbind = c_function,

      `[` = function(spec, ...) {
        dimension_names = as.character(substitute(list(...))[-1])

        list(
          spec[[1]],
          c(spec[[2]], dimension_names),
          spec[[3]]
        )
      },

      `|` = function(spec, by) {
        wide_dimension_name = by[[1]]
        if (!is.null(spec[[3]])) {
          stop("Left-hand side of `|` cannot contain `|`")
        }
        if (length(wide_dimension_name) != 1) {
          stop("Right-hand side of `|` must be exactly one name")
        }

        list(
          spec[[1]],
          spec[[2]],
          wide_dimension_name
        )
      }
    ),
    names_spec
  ))

  eval_tidy(variable_spec, spec_env)
}
