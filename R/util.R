# Utility functions for ggdist
#
# Author: mjskay
###############################################################################


# deparse that is guaranteed to return a single string (instead of
# a list of strings if the expression goes to multiple lines)
deparse0 = function(expr, width.cutoff = 500, ...) {
  paste0(deparse(expr, width.cutoff = width.cutoff, ...), collapse = "")
}

stop0 = function(...) {
  stop(..., call. = FALSE)
}

warning0 = function(...) {
  warning(..., call. = FALSE)
}

#' Raise an error if packages are not installed
#' @param packages character vector of packages to check for
#' @param context beginning of error message giving the context (e.g. a
#' package name and/or argument combination) that requires the package.
#' @param ... additional messages placed into the `cli_abort()` message.
#' @importFrom rlang caller_env
#' @noRd
stop_if_not_installed = function(packages, context = "This functionality", ..., call = caller_env()) {
  for (package in packages) {
    if (!requireNamespace(package, quietly = TRUE)) {
      stop_not_installed(package, context = context, ..., call = call)
    }
  }
}
stop_not_installed = function(package, context = "This functionality", ..., call = caller_env()) {
  cli_abort(
    c(
      paste0(context, ' requires the {.pkg {package}} package.'),
      ">" = 'Install the {.pkg {package}} package with {.run install.packages("{package}")}',
      ...
    ),
    class = "ggdist_missing_package",
    ggdist_package = package,
    call = call
  )
}

# get all variable names from an expression
# based on http://adv-r.had.co.nz/dsl.html
all_names = function(x) {
  if (is.atomic(x)) {
    NULL
  } else if (is.name(x)) {
    as.character(x)
  } else if (is.call(x) || is.pairlist(x)) {
    children = lapply(x[-1], all_names)
    unique(unlist(children))
  } else {
    stop0("Don't know how to handle type ", deparse0(typeof(x)))
  }
}

# set missing values from x to provided default values
defaults = function(x, defaults) {
  c(x, defaults[setdiff(names(defaults), names(x))])
}

#' A variation of match.fun() that allows prefixes to be supplied to strings
#' to determine the function to be used, and which ensures we always fall back
#' to searching the ggdist namespace for the function
#' in case ggdist is not in the caller's search path.
#' @noRd
match_function = function(f, prefix = "", env = globalenv()) {
  if (is.function(f)) return(f)

  f = paste0(prefix, f)
  get0(f, mode = "function", envir = env) %||%
    get(f, mode = "function", envir = getNamespace("ggdist"))
}

#' Check for NAs and remove them as needed
#' @noRd
check_na = function(x, na.rm) {
  if (anyNA(x)) {
    if (isTRUE(na.rm)) {
      x = x[!is.na(x)]
    } else {
      stop0("`x` must not contain missing (NA) values")
    }
  }
  x
}

# simple version of destructuring assignment
`%<-%` = function(vars, values, envir = parent.frame()) {
  vars = as.character(substitute(vars)[-1])
  for (i in seq_along(vars)) {
    assign(vars[[i]], values[[i]], envir = envir)
  }
  invisible(NULL)
}

# deprecations and warnings -----------------------------------------------

#' @importFrom rlang enexprs
.Deprecated_arguments = function(old_names, ..., message = "", which = -1, fun = as.character(sys.call(which))[[1]]) {
  deprecated_args = intersect(old_names, names(enexprs(...)))

  if (length(deprecated_args) > 0) {
    stop0(
      "\nIn ", fun, "(): The `", deprecated_args[[1]], "` argument is deprecated.\n",
      message,
      "See help(\"ggdist-deprecated\").\n"
    )
  }
}

.Deprecated_argument_alias = function(new_arg, old_arg, which = -1, fun = as.character(sys.call(which))[[1]]) {
  old_name = quo_name(enquo(old_arg))
  if (rlang::is_missing(old_arg)) {
    new_arg
  } else {
    new_name = quo_name(enquo(new_arg))

    warning0(
      "\nIn ", fun, "(): The `", old_name, "` argument is a deprecated alias for `",
      new_name, "`.\n",
      "Use the `", new_name, "` argument instead.\n",
      "See help(\"tidybayes-deprecated\") or help(\"ggdist-deprecated\").\n"
    )

    old_arg
  }
}


# data frames -------------------------------------------------------------

#' fast data frame creation
#' @noRd
data_frame0 = function(...) {
  vctrs::data_frame(..., .name_repair = "minimal")
}

#' rename columns using a lookup table
#' @param data data frame
#' @param new_names lookup table of new column names, where names are
#' old column names
#' @noRd
rename_cols = function(data, new_names) {
  to_rename = names(data) %in% names(new_names)
  names(data)[to_rename] = new_names[names(data)[to_rename]]
  data
}

#' makes the specified columns into list columns. All other
#' columns (except grouping columns) are dropped.
#' @param data a data frame (possibly grouped)
#' @param list_cols character vector of names of columns to turn into
#'   list columns
#' @noRd
make_list_cols = function(data, list_cols) {
  group_cols = group_vars_(data)
  ddply_(data, group_cols, function(d) {
    new_data_frame(c(
      d[1, group_cols, drop = FALSE],
      lapply(d[, list_cols, drop = FALSE], list)
    ))
  })
}


# workarounds -------------------------------------------------------------

# workarounds / replacements for common patterns

map_dfr_ = function(data, fun, ...) {
  # drop-in replacement for purrr::map_dfr
  vec_rbind(!!!lapply(data, fun, ...))
}

#' faster version of a map over rows of a data frame, like:
#' map_dfr(seq_len(nrow(data)), function(i) {
#'   row = data[i, , drop = FALSE]
#'   ...
#' })
#' @noRd
row_map_dfr_ = function(data, fun, ...) {
  map_dfr_(seq_len(nrow(data)), function(row_i) {
    # faster version of row_df = data[row_i, , drop = FALSE]
    row_df = new_data_frame(lapply(data, vctrs::vec_slice, row_i), n = 1L)
    fun(row_df, ...)
  })
}

pmap_ = function(.l, .f, ...) {
  .mapply(.f, .l, list(...))
}

ddply_ = function(data, groups, fun, ...) {
  vec_rbind(!!!dlply_(data, groups, fun, ...))
}

fct_explicit_na_ = function(x) {
  x = as.factor(x)
  if (anyNA(x) || anyNA(levels(x))) {
    na_name = "(Missing)"
    levels_x = levels(x)
    while (na_name %in% levels_x) {
      na_name = paste0(na_name, "+")
    }
    levels(x) = c(levels_x, na_name)
    x[is.na(x)] = na_name
  }
  x
}

dlply_ = function(data, groups, fun, ...) {
  # must make NAs explicit or they will be dropped by split()
  group_factors = lapply(data[, groups, drop = FALSE], fct_explicit_na_)

  if (length(group_factors) >= 1) {
    # group_is = a list where each element is a numeric vector of indices
    # corresponding to one group
    group_is = unname(split(seq_len(nrow(data)), group_factors, drop = TRUE, lex.order = TRUE))

    lapply(group_is, function(group_i) {
      # faster version of row_df = data[group_i, , drop = FALSE]
      row_df = new_data_frame(lapply(data, vctrs::vec_slice, group_i), n = length(group_i))
      fun(row_df, ...)
    })
  } else {
    list(fun(data, ...))
  }
}

split_df = function(data, groups) {
  dlply_(data, groups, identity)
}

map_dbl_ = function(.x, .f, ...) {
  vapply(.x, .f, FUN.VALUE = NA_real_, ...)
}

map_lgl_ = function(.x, .f, ...) {
  vapply(.x, .f, FUN.VALUE = NA, ...)
}

map2_ = function(.x, .y, .f, ...) {
  .mapply(.f, list(.x, .y), list(...))
}

map2_chr_ = function(.x, .y, .f) {
  vctrs::list_unchop(map2_(.x, .y, .f), ptype = character())
}

fct_rev_ = function(x) {
  if (is.character(x)) {
    x = factor(x)
  } else if (!is.factor(x)) {
    stop0("`x` must be a factor (or character vector).")
  }
  factor(x, levels = rev(levels(x)), ordered = is.ordered(x))
}


# array manipulation ------------------------------------------------------

# flatten dimensions of an array
flatten_array = function(x) {
  # determine new dimension names in the form x,y,z
  # start with numeric names
  .dim = dim(x) %||% length(x)
  dimname_lists = lapply(.dim, seq_len)
  .dimnames = dimnames(x)
  if (!is.null(.dimnames)) {
    # where character names are provided, use those instead of the numeric names
    dimname_lists = lapply(seq_along(dimname_lists), function(i) .dimnames[[i]] %||% dimname_lists[[i]])
  }
  # if this has more than one dimension and the first dim is length 1 and is unnamed, drop it
  # basically: we don't want row vectors to have a bunch of "1,"s in front of their indices.
  if (length(.dim) > 1 && .dim[[1]] == 1 && length(.dimnames[[1]]) == 0) {
    dimname_lists = dimname_lists[-1]
  }

  # flatten array
  dim(x) = length(x)

  if (length(dimname_lists) == 1) {
    index_names = dimname_lists[[1]]
  } else {
    # expand out the dimname lists into the appropriate combinations and assemble into new names
    dimname_grid = expand.grid(dimname_lists, KEEP.OUT.ATTRS = FALSE)
    index_names = do.call(paste, c(list(sep = ","), dimname_grid))
    # make it a factor to preserve original order
    index_names = ordered(index_names, levels = index_names)
  }

  list(x = x, index_names = index_names)
}


# sequences ---------------------------------------------------------------

#' Create sequences of length n interleaved with its own reverse sequence.
#' e.g.
#' seq_interleaved(6) = c(1, 6, 2, 5, 3, 4)
#' seq_interleaved(5) = c(1, 5, 2, 4, 3)
#' @noRd
seq_interleaved = function(n) {
  if (n <= 2) {
    i = seq_len(n)
  } else {
    i = numeric(n)
    i[c(TRUE,FALSE)] = seq_len(ceiling(n/2))
    i[c(FALSE,TRUE)] = seq(n, ceiling(n/2) + 1)
  }

  i
}

#' seq_interleaved that starts at n instead of 1
#' @noRd
seq_interleaved_rev = function(n) {
  n + 1 - seq_interleaved(n)
}

#' a variant of seq_interleaved that proceeds outwards from the middle,
#' for use with layout = "weave" when side = "both" in dots geoms
#' @noRd
seq_interleaved_centered = function(n) {
  half_n = floor(n/2)
  if (n %% 2 == 1) {
    # odd n should have 1 in the middle
    head = rev(seq_interleaved_rev(half_n)) * 2
    tail = seq_interleaved_rev(half_n) * 2 + 1
    c(head, 1, tail)
  } else if (half_n %% 2 == 0) {
    # even n with even halves should have 1 just above half way
    head = rev(seq_interleaved_rev(half_n)) * 2
    tail = seq_interleaved(half_n) * 2 - 1
    c(head, tail)
  } else {
    # even n with odd halves should have 1 just below half way
    head = rev(seq_interleaved(half_n)) * 2 - 1
    tail = seq_interleaved_rev(half_n) * 2
    c(head, tail)
  }
}
