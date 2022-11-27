# probability expressions within after_stat() for stat_slabinterval
#
# Author: mjskay
###############################################################################


Pr_ = function(x) {
  expr = substitute(x)
  label = paste0("Pr_(", as_label(expr), ")")

  if (is.call(expr)) {
    f = as_name(expr[[1]])
    e1 = parse_Pr_element(expr[[2]], label)
    e2 = parse_Pr_element(expr[[3]], label)
    switch(f,
      ">" =, ">=" =, "<" =, "<=" = {
        check_Pr_cdf_element_combination(e1, e2, label)
        e1 = standardize_Pr_element(e1)
        e2 = standardize_Pr_element(e2)
        if (f %in% c(">", ">=")) e1 = e2
        if (e1 == "dist") {
          return(quote(after_stat(cdf)))
        } else {
          return(quote(after_stat(1 - cdf)))
        }
      },
      "%in%" = {
        e1 = standardize_Pr_element(e1)
        e2 = standardize_Pr_element(e2)
        if (e1 %in% c("value", "dist") && e2 == "interval") {
          return(quote(after_stat(.width)))
        }
      }
    )
  }

  stop0("Unrecognized probability expression: `", label, "`")
}

p_ = function(x) {
  expr = substitute(x)
  label = paste0("p_(", as_label(expr), ")")

  if (is.name(expr)) {
    e = standardize_Pr_element(parse_Pr_element(expr, label))
    if (e == "value") {
      return(quote(after_stat(pdf)))
    }
  }

  stop0("Unrecognized probability expression: `", label, "`")
}


# helpers -----------------------------------------------------------------

parse_Pr_element = function(e, label = NULL) {
  e = deparse0(e)
  switch(e,
    X =, xdist = "xdist",
    Y =, ydist = "ydist",
    x = "x",
    y = "y",
    value = "value",
    dist = "dist",
    interval = "interval",
    stop0("Unknown probability expression element in `", label, "`: ", e)
  )
}

standardize_Pr_element = function(e) {
  switch(e,
    x =, y =, value = "value",
    xdist =, ydist =, dist = "dist",
    e
  )
}

check_Pr_cdf_element_combination = function(e1, e2, label = NULL) {
  if (!list(sort(c(e1, e2))) %in% list(
      c("x", "xdist"),
      c("y", "ydist"),
      c("dist", "value")
    )
  ) {
    stop0("Invalid combination of expression elements in `", label, "`: ", e1, " and ", e2)
  }
}

check_Pr_in_element_combination = function(e1, e2, label = NULL) {
  if (!list(c(e1, e2)) %in% list(
    c("", "xdist"),
    c("y", "ydist"),
    c("dist", "value")
  )
  ) {
    stop0("Invalid combination of expression elements in `", label, "`: ", e1, " and ", e2)
  }
}
