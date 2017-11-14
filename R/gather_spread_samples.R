# spread_samples and gather_samples (used to be "extract_samples" or "tidy_samples")
#
# Author: mjskay
###############################################################################

# Names that should be suppressed from global variable check by codetools
# Names used broadly should be put in _global_variables.R
globalVariables(c(".."))


# DEPRECATED NAMES FOR spread_samples
#' @export
extract_samples = function(...) {
  .Deprecated("spread_samples")
  spread_samples(...)
}
#' @export
tidy_samples = function(...) {
  .Deprecated("spread_samples")
  spread_samples(...)
}


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
    stop("Don't know how to handle type ", typeof(x),
      call. = FALSE)
  }
}

#parse a variable spec in the form variable_name[index_name_1, index_name_2, ..] | wide_index
#into a list with three elements:
# 1. A vector of variable names
# 2. A vector of index names (or NULL if none)
# 3. The name of the wide index (or NULL if none)
#' @importFrom stats setNames
#' @importFrom purrr reduce map map2
#' @importFrom rlang set_names
parse_variable_spec = function(variable_spec) {
  names = all_names(variable_spec$expr)
  #specs for each bare variable name in the spec expression
  names_spec = names %>%
    set_names() %>%
    map(function(name) list(name, NULL, NULL))


  c_function = function(...) {
    reduce(list(...), function(spec1, spec2) map2(spec1, spec2, base::c))
  }

  spec_env = c(
    names_spec,
    list(
      c = c_function,
      cbind = c_function,

      `[` = function(spec, ...) {
        index_names = as.character(substitute(list(...))[-1])

        list(
          spec[[1]],
          c(spec[[2]], index_names),
          spec[[3]]
        )
      },

      `|` = function(spec, by) {
        wide_index_name = by[[1]]
        if (!is.null(spec[[3]])) {
          stop("Left-hand side of `|` cannot contain `|`")
        }
        if (length(wide_index_name) != 1) {
          stop("Right-hand side of `|` must be exactly one name")
        }

        list(
          spec[[1]],
          spec[[2]],
          wide_index_name
        )
      }
    )
  )

  lazy_eval(variable_spec, spec_env)
}

#' Extract samples of parameters in a Bayesian model fit into a tidy data format
#'
#' Extract samples from a Bayesian/MCMC sampler for a variable with the given named
#' indices into one of two types of long-format data frames.
#'
#' Imagine a JAGS or Stan fit named \code{fit}. The model may contain a parameter named
#' \code{b[i,v]} (in the JAGS or Stan language) with \code{i} in \code{1:100} and \code{v} in \code{1:3}.
#' However, samples returned from JAGS or Stan in R will not reflect this indexing structure, instead
#' they will have multiple columns with names like \code{"b[1,1]"}, \code{"b[2,1]"}, etc.
#'
#' \code{spread_samples} and \code{gather_samples} provide a straightforward
#' syntax to translate these columns back into properly-indexed variables in two different
#' tidy data frame formats, optionally recovering index types (e.g. factor levels) as it does so.
#'
#' \code{spread_samples} and \code{gather_samples} return data frames already grouped by
#' all indices used on the variables you specify.
#'
#' The difference between \code{spread_samples} is that names of parameters in the model will
#' be spread across the data frame as column names, whereas \code{gather_samples} will
#' gather terms into a single column named \code{"term"} and place estimates of terms into a
#' column names \code{"estimate"}. The \code{"term"} and \code{"estimate"} naming scheme
#' is used in order to be consistent with output from the \code{\link[broom]{tidy}} function
#' in the broom package, to make it easier to use tidybayes with broom for model comparison.
#'
#' For example, \code{spread_samples(fit, a[i], b[i,v])} might return a grouped
#' data frame (grouped by \code{i} and \code{v}), with:
#' \itemize{
#'    \item column \code{".chain"}: the chain number
#'    \item column \code{".iteration"}: the interation number
#'    \item column \code{"i"}: value in \code{1:5}
#'    \item column \code{"v"}: value in \code{1:10}
#'    \item column \code{"a"}: value of \code{"a[i]"} for iteration number
#'      \code{".iteration"} on chain number \code{".chain"}
#'    \item column \code{"b"}: value of \code{"b[i,v]"} for iteration number
#'      \code{".iteration"} on chain number \code{".chain"}
#'  }
#'
#' \code{gather_samples(fit, a[i], b[i,v])} on the same fit would return a grouped
#' data frame (grouped by \code{i} and \code{v}), with:
#' \itemize{
#'    \item column \code{".chain"}: the chain number
#'    \item column \code{".iteration"}: the interation number
#'    \item column \code{"i"}: value in \code{1:5}
#'    \item column \code{"v"}: value in \code{1:10}, or \code{NA}
#'      if \code{"term"} is \code{"a"}.
#'    \item column \code{"term"}: value in \code{c("a", "b")}.
#'    \item column \code{"estimate"}: value of \code{"a[i]"} (when \code{"term"} is \code{"a"})
#'      or \code{"b[i,v]"} (when \code{"term"} is \code{"b"}) for iteration number
#'      \code{".iteration"} on chain number \code{".chain"}
#'  }
#'
#' \code{spread_samples} and \code{gather_samples} can use type information
#' applied to the \code{fit} object by \code{\link{recover_types}} to convert columns
#' back into their original types. This is particularly helpful if some of the indices in
#' your model were originally factors. For example, if the \code{v} index
#' in the original data frame \code{data} was a factor with levels \code{c("a","b","c")},
#' then we could use \code{recover_types} before \code{spread_samples}:
#'
#' \preformatted{fit \%>\%
#'  recover_types(data) %\>\%
#'  spread_samples(fit, b[i,v])
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
#' \preformatted{spread_samples(fit, x, d[i], b[i,v])}
#'
#' Which is roughly equivalent to this:
#'
#' \preformatted{spread_samples(fit, x) \%>\%
#'  inner_join(spread_samples(fit, d[i])) \%>\%
#'  inner_join(spread_samples(fit, b[i,v])) \%>\%
#'  group_by(i,v)
#' }
#'
#' Similarly, this:
#'
#' \preformatted{gather_samples(fit, x, d[i], b[i,v])}
#'
#' Is roughly equivalent to this:
#'
#' \preformatted{bind_rows(
#'  gather_samples(fit, x),
#'  gather_samples(fit, d[i]),
#'  gather_samples(fit, b[i,v])
#' )}
#'
#'
#' The \code{c} and \code{cbind} functions can be used to combine multiple variable names that have
#' the same indices. For example, if we have several variables with the same
#' subscripts \code{i} and \code{v}, we could do either of these:
#'
#' \preformatted{spread_samples(fit, c(w, x, y, z)[i,v])}
#' \preformatted{spread_samples(fit, cbind(w, x, y, z)[i,v])}  # equivalent
#'
#' Each of which is roughly equivalent to this:
#'
#' \preformatted{spread_samples(fit, w[i,v], x[i,v], y[i,v], z[i,v])}
#'
#' Besides being more compact, the \code{c()}-style syntax is currently also
#' faster (though that may change).
#'
#' Indices can be omitted from the resulting data frame by leaving their names
#' blank; e.g. \code{spread_samples(fit, b[,v])} will omit the first index of
#' \code{b} from the output. This is useful if an index is known to contain all
#' the same value in a given model.
#'
#' The shorthand \code{..} can be used to specify one column that should be put
#' into a wide format and whose names will be the base variable name, plus a dot
#' ("."), plus the value of the index at \code{..}. For example:
#'
#' \code{spread_samples(fit, b[i,..])} would return a grouped data frame
#' (grouped by \code{i}), with:
#' \itemize{
#'  \item column \code{".chain"}: the chain number
#'  \item column \code{".iteration"}: the interation number
#'  \item column \code{"i"}: value in \code{1:20}
#'  \item column \code{"b.1"}: value of \code{"b[i,1]"} for iteration number
#'    \code{".iteration"} on chain number \code{".chain"}
#'  \item column \code{"b.2"}: value of \code{"b[i,2]"} for iteration number
#'    \code{".iteration"} on chain number \code{".chain"}
#'  \item column \code{"b.3"}: value of \code{"b[i,3]"} for iteration number
#'    \code{".iteration"} on chain number \code{".chain"}
#' }
#'
#' An optional clause in the form \code{| wide_index} can also be used to put
#' the data frame into a wide format based on \code{wide_index}. For example, this:
#'
#' \preformatted{spread_samples(fit, b[i,v] | v)}
#'
#' is roughly equivalent to this:
#'
#' \preformatted{spread_samples(fit, b[i,v]) \%>\% spread(v,b)}
#'
#' The main difference between using the \code{|} syntax instead of the
#' \code{..} syntax is that the \code{|} syntax respects prototypes applied to
#' indices with \code{\link{recover_types}}, and thus can be used to get
#' columns with nicer names. For example:
#'
#' \code{fit \%>\% recover_types(data) \%>\% spread_samples(b[i,v] | v)} would return a grouped data frame
#' (grouped by \code{i}), with:
#' \itemize{
#'  \item column \code{".chain"}: the chain number
#'  \item column \code{".iteration"}: the interation number
#'  \item column \code{"i"}: value in \code{1:20}
#'  \item column \code{"a"}: value of \code{"b[i,1]"} for iteration number
#'    \code{".iteration"} on chain number \code{".chain"}
#'  \item column \code{"b"}: value of \code{"b[i,2]"} for iteration number
#'    \code{".iteration"} on chain number \code{".chain"}
#'  \item column \code{"c"}: value of \code{"b[i,3]"} for iteration number
#'    \code{".iteration"} on chain number \code{".chain"}
#' }
#'
#' Finally, parameter names can be regular expressions by setting \code{regex = TRUE}; e.g.:
#'
#' \code{spread_samples(fit, `b_.*`[i], regex = TRUE)}
#'
#' Would return a tidy data frame with parameters starting with `b_` and having one index.
#'
#' @param model A supported Bayesian model fit / MCMC object. Currently
#' supported models include \code{\link[coda]{mcmc}}, \code{\link[coda]{mcmc.list}},
#' \code{\link[runjags]{runjags}}, \code{\link[rstan]{stanfit}}, \code{\link[rstanarm]{stanreg-objects}},
#' \code{\link[brms]{brm}}, and anything with its own \code{\link[coda]{as.mcmc.list}} implementation.
#' If you install the \code{tidybayes.rethinking} package (available at
#' \url{https://github.com/mjskay/tidybayes.rethinking}), \code{map} and
#' \code{map2stan} models from the \code{rethinking} package are also supported.
#' @param ... Expressions in the form of
#' \code{variable_name[index_1, index_2, ...] | wide_index}. See `Details`.
#' @param regex If \code{TRUE}, parameter names are treated as regular expressions and all column matching the
#' regular expression and number of indices are included in the output. Default \code{FALSE}.
#' @param sep Separator used to separate indices in parameter names, as a regular expression.
#' @return A data frame.
#' @author Matthew Kay
#' @seealso \code{\link{recover_types}}, \code{\link{compose_data}}.
#' @keywords manip
#' @examples
#'
#' ##TODO
#'
#' @aliases extract_samples tidy_samples
#' @importFrom lazyeval lazy_dots
#' @importFrom purrr reduce
#' @importFrom dplyr inner_join
#' @rdname spread_samples
#' @export
spread_samples = function(model, ..., regex = FALSE, sep = "[, ]") {
  tidysamples = lapply(lazy_dots(...), function(variable_spec) {
    spread_samples_(model, variable_spec, regex = regex, sep = sep)
  })

  #get the groups from all the samples --- when we join them together,
  #the grouping information is lost (actually, only the groups on the
  #first data frame in a join is retained), so we'll have to recreate
  #the full set of groups from all the data frames after we join them
  groups_ = tidysamples %>%
    map(groups) %>%
    reduce(union)

  tidysamples %>%
    reduce(function(tidysamples1, tidysamples2) {
      by_ = intersect(names(tidysamples1), names(tidysamples2))
      inner_join(tidysamples1, tidysamples2, by = by_)
    }) %>%
    group_by_(.dots = groups_)
}
#' @import dplyr
#' @importFrom tidyr spread_
#' @importFrom lazyeval lazy_eval
#' @importFrom tibble has_name
spread_samples_ = function(model, variable_spec, regex = FALSE, sep = "[, ]") {
  #parse a variable spec in the form variable_name[index_name_1, index_name_2, ..] | wide_index
  spec = parse_variable_spec(variable_spec)
  variable_names = spec[[1]]
  index_names = spec[[2]]
  wide_index_name = spec[[3]]

  #extract the samples into a long data frame
  samples = spread_samples_long_(model, variable_names, index_names, regex = regex, sep = sep)

  #convert variable and/or indices back into usable data types
  constructors = attr(model, "constructors")
  if (is.null(constructors)) constructors = list()
  for (column_name in c(variable_names, index_names)) {
    if (column_name %in% names(constructors)) {
      #we have a data type constructor for this index, convert it
      samples[[column_name]] = constructors[[column_name]](samples[[column_name]])
    }
  }

  #spread a column into wide format if requested (only if one variable, because
  #we can't spread multiple keys simultaneously for the same value)
  if (!is.null(wide_index_name)) {
    #wide index requested by name
    if (length(variable_names) != 1) {
      stop("Cannot extract samples of multiple variables in wide format.")
    }
    samples %>%
      spread_(wide_index_name, variable_names)
  }
  else if (has_name(samples, "..")) {
    #a column named ".." is present, use it to form a wide version of the data
    #with numbered names based on the variable name
    if (length(variable_names) != 1) {
      stop("Cannot extract samples of multiple variables in wide format.")
    }
    samples %>%
      #the ".." column will have been set as a grouping column because it was
      #specified as an index; therefore before we can modify it we have to
      #remove it from the grouping columns on this table (mutate does not
      #allow you to modify grouping columns)
      group_by_(.dots = setdiff(groups(.), "..")) %>%
      mutate(.. = paste0(variable_names, ".", ..)) %>%
      spread_("..", variable_names)
  }
  else {
    #no wide column => just return long version
    samples
  }
}


#' @importFrom tidyr spread_ separate_ gather_
#' @import stringi
#' @import dplyr
spread_samples_long_ = function(model, variable_names, index_names, regex = FALSE, sep = "[, ]") {
  samples = as_sample_tibble(model)
  if (!regex) {
    variable_names = escape_regex(variable_names)
  }

  if (is.null(index_names)) {
    #no indices, just find the colnames matching the regex(es)
    variable_regex = paste0("^(", paste(variable_names, collapse = "|"), ")$")
    variable_names_index = stri_detect_regex(colnames(samples), variable_regex)

    if (!any(variable_names_index)) {
      stop(paste0("No parameters found matching spec: ",
        "c(", paste0(variable_names, collapse = ","), ")"
      ))
    }

    variable_names = colnames(samples)[variable_names_index]
    samples[, c(".chain", ".iteration", variable_names)]
  }
  else {
    index_sep_regex = sep
    index_regex = "(.+)"

    #find the variables to extract matching the given names and number of indices
    variable_regex = paste0("^",
      #variable name
      "(", paste(variable_names, collapse = "|"), ")\\[",
      #indices
      paste0(rep(index_regex, length(index_names)), collapse = index_sep_regex),
      "\\]$"
    )
    variable_names_index = stri_detect_regex(colnames(samples), variable_regex)
    if (!any(variable_names_index)) {
      stop(paste0("No parameters found matching spec: ",
        "c(", paste0(variable_names, collapse = ","), ")",
        "[", paste0(index_names, collapse = ","), "]"
      ))
    }
    variable_names = colnames(samples)[variable_names_index]

    #rename columns to drop trailing "]" to eliminate extraneous last column
    #when we do separate(), below. e.g. "x[1,2]" becomes "x[1,2". Do the same
    #with variable_names so we can select the columns
    colnames(samples)[c(-1, -2)] = stri_sub(colnames(samples)[c(-1, -2)], to = -2)
    variable_names = stri_sub(variable_names, to = -2)

    #specs containing empty indices (e.g. mu[] or mu[,k]) will produce
    #some index_names == ""; we can't use empty variable names below, so we
    #replace them with the ".drop" placeholder and then drop those columns later.
    #TODO: probably a better way to do this.
    temp_index_names = index_names %>%
      #must give each blank index column a unique name, otherwise spread_() won't work below
      ifelse(. == "", paste0(".drop", seq_along(.)), .)
    index_names = index_names[index_names != ""]

    samples[, c(".chain", ".iteration", variable_names)] %>%
      #make long format for the variables we want to split
      gather_(".variable", ".value", variable_names) %>%
      #next, split indices in variable names into columns
      separate_(".variable", c(".variable", ".indices"), sep = "\\[|\\]") %>%
      separate_(".indices", temp_index_names, sep = index_sep_regex,
        convert = TRUE #converts indices to numerics if applicable
      ) %>%
      #now, make the value of each variable a column
      spread_(".variable", ".value") %>%
      #drop the columns that correpond to blank indices in the original spec
      select(-starts_with(".drop")) %>%
      #group by the desired indices so that we return a pre-grouped data frame to the user
      group_by_(.dots = index_names)
  }
}


#' @rdname spread_samples
#' @importFrom dplyr bind_rows
#' @export
gather_samples = function(model, ..., regex = FALSE, sep = "[, ]") {
  tidysamples = lapply(lazy_dots(...), function(variable_spec) {
    model %>%
      spread_samples_(variable_spec, regex = regex, sep = sep) %>%
      gather_terms()
  })

  #get the groups from all the samples --- when we bind them together,
  #the grouping information is not always retained, so we'll have to recreate
  #the full set of groups from all the data frames after we bind them
  groups_ = tidysamples %>%
    map(groups) %>%
    reduce(union)

  bind_rows(tidysamples) %>%
    group_by_(.dots = groups_)
}
