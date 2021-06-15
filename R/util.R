# Utility functions for ggdist
#
# Author: mjskay
###############################################################################


# deparse that is guaranteed to return a single string (instead of
# a list of strings if the expression goes to multiple lines)
deparse0 = function(expr, width.cutoff = 500, ...) {
  paste0(deparse(expr, width.cutoff = width.cutoff, ...), collapse = "")
}

# get all variable names from an expression
# based on http://adv-r.had.co.nz/dsl.html
all_names = function(x) {
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
    children = lapply(x[-1], all_names)
    unique(unlist(children))
  } else {
    stop("Don't know how to handle type `", typeof(x), "`",
      call. = FALSE)
  }
}

# set missing values from x to provided default values
defaults = function(x, defaults) {
  c(x, defaults[setdiff(names(defaults), names(x))])
}


# deprecations and warnings -----------------------------------------------

#' @importFrom rlang enexprs
.Deprecated_arguments = function(old_names, ..., message = "", which = -1, fun = as.character(sys.call(which))[[1]]) {
  deprecated_args = intersect(old_names, names(enexprs(...)))

  if (length(deprecated_args) > 0) {
    stop(
      "\nIn ", fun, "(): The `", deprecated_args[[1]], "` argument is deprecated.\n",
      message,

      call. = FALSE
    )
  }
}

.Deprecated_argument_alias = function(new_arg, old_arg, which = -1, fun = as.character(sys.call(which))[[1]]) {
  if (missing(old_arg)) {
    new_arg
  } else {
    new_name = quo_name(enquo(new_arg))
    old_name = quo_name(enquo(old_arg))

    warning(
      "\nIn ", fun, "(): The `", old_name, "` argument is a deprecated alias for `",
      new_name, "`.\n",
      "Use the `", new_name, "` argument instead.\n",
      "See help(\"tidybayes-deprecated\") or help(\"ggdist-deprecated\").\n",

      call. = FALSE
    )

    old_arg
  }
}



# workarounds -------------------------------------------------------------

# workaround replacements for other patterns that don't quite do what we need them to
# (especially when it comes to rvars...)

pmap_dfr_ = function(data, fun) {
  # this is roughly equivalent to
  # pmap_dfr(df, function(...) { ... })
  # but works properly with vctrs (pmap_dfr seems broken on rvars?)
  purrr::map_dfr(vctrs::vec_chop(data), function(row) do.call(fun, lapply(row, `[[`, 1)))
}

ddply_ = function(data, groups, fun, ...) {
  purrr::map_dfr(dplyr::group_split(data, dplyr::across(groups)), fun, ...)
}

dlply_ = function(data, groups, fun, ...) {
  lapply(dplyr::group_split(data, dplyr::across(groups)), fun, ...)
}

vapply_dbl = function(X, FUN, ...) {
  vapply(X, FUN, FUN.VALUE = numeric(1), ...)
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

#' a variant of seq_interleaved that proceeds outwards from the middle,
#' for use with side = "both" in dots geoms
#' @noRd
seq_interleaved_centered = function(n) {
  is_odd = n %% 2
  head = rev(seq_interleaved(floor(n/2))) * 2 - 1 + is_odd
  center = if (is_odd) 1
  tail = seq_interleaved(floor(n/2)) * 2 + is_odd
  c(head, center, tail)
}
