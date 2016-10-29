# tidybayes: R Package for composing data for and extracting samples from Bayesian samplers in a tidy data format 

[![Build Status](https://travis-ci.org/mjskay/tidybayes.png?branch=master)](https://travis-ci.org/mjskay/tidybayes)

_Matthew Kay, University of Michigan <mjskay@umich.edu>_

[Tidy](http://cran.r-project.org/web/packages/tidyr/vignettes/tidy-data.html)
data frames (one observation per row) are particularly convenient for use
in a variety of R data manipulation and visualization packages. However,
when using MCMC / Bayesian samplers like JAGS or Stan in R, we often have
to translate this data into a form the sampler understands, and then after
running the model, translate the resulting sample into a more tidy
format for use with other R functions.  `tidybayes` aims to simplify these 
two common (often tedious) operations:

* __Composing data__ for use with the sampler. This often means translating
  data from a `data.frame` into a `list` , making sure `factors` are encoded as
  numerical data, adding variables to store the length of indices, etc. This
  package helps automate these operations using the `compose_data` function, which
  automatically handles data types like `numeric` , `logical` , `factor` , and `ordinal`, 
  and allows easy extensions for converting other datatypes into a format the
  sampler understands by providing your own implementation of the generic `as.data_list`.

* __Gathering samples__ from the sampler. This often means extracting indices
  from parameters with names like `"b[1,1]"` , `"b[1,2]"` into separate columns
  of a data frame, like `i = c(1,1,..)` and `j = c(1,2,...)`. More tediously,
  sometimes these indices actually correspond to levels of a factor in the original
  data; e.g. `"x[1]"` might correspond to a value of `x` for the first level of
  some factor. We provide a straightforward way to convert samples of a
  variable with indices into a long-format ("[tidy]
  (http://cran.r-project.org/web/packages/tidyr/vignettes/tidy-data.html)") data
  frame, with automatic back-conversion of common data types (factors, logicals)
  using the `gather_samples` function, including automatic recovery of factor levels
  corresponding to variable indices. In most cases this kind of long-format
  data is much easier to use with other data-manipulation and plotting packages
  (e.g., `dplyr` , `tidyr` , `ggplot2` ) than the format provided by default from
  the sampler.
  
`tidybayes` also provides some additional functionality for data manipulation
and visualization tasks common to many models:

* __Visualizing posterior estimates__, which when many estimates are involved
  can be done succinctly using eye plots (aka raindrop plots or violin
  plots). Eye plots are a compact representation of posterior densities that 
  combines with credible intervals and point estimates with a symmetric visualization 
  of density, making for straightforward and compact comparison of many data points. 
  The `geom_eye` function provides a convenient way to generate eye plots using 
  `ggplot2`, and `ggeye` offers a shortcut combining `ggplot`, `geom_eye`, and
  `coord_flip`.
  
  Posterior fit lines (with densities around the fit) can also be visualized
  using a combination of `predict_curve` and `predict_curve_density` plus
  `geom_rect`. More succinct shortcuts for this are coming.

* __Comparing a variable across levels of a factor__, which often means first
  generating pairs of levels of a factor (according to some desired set of 
  comparisons) and then computing a function over the value of the comparison
  variable for those pairs of levels. Assuming your data is in the long-format
  returned by `gather_samples` (i.e. the `..` and `|` syntax for that 
  function was not used), the `compare_levels` function allows comparison
  across levels to be made easily.

## Installation

You can install the latest development version from GitHub with these R
commands:

```r
install.packages("devtools")
devtools::install_github("mjskay/tidybayes")
```

## Examples

TBD.

## Problems

Should you encounter any issues with this package, contact Matthew Kay
(<mjskay@umich.edu>). If you have found a bug, please file it [here]
(https://github.com/mjskay/tidybayes/issues/new) with minimal code to reproduce
the issue.

