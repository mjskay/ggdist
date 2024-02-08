
# ggdist: Visualizations of distributions and uncertainty

[![R CMD check
status](https://github.com/mjskay/ggdist/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/mjskay/ggdist/actions/workflows/R-CMD-check.yaml)
[![Coverage
status](https://codecov.io/gh/mjskay/ggdist/branch/master/graph/badge.svg)](https://app.codecov.io/github/mjskay/ggdist?branch=master)
[![CRAN
status](https://www.r-pkg.org/badges/version/ggdist)](https://cran.r-project.org/package=ggdist)
![Download count](https://cranlogs.r-pkg.org/badges/last-month/ggdist)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.3879620.svg)](https://doi.org/10.5281/zenodo.3879620)

<figure>
<img src="man/figures/preview.png" alt="Preview of ggdist plots" />
<figcaption aria-hidden="true">Preview of ggdist plots</figcaption>
</figure>

[ggdist](https://mjskay.github.io/ggdist/) is an R package that provides
a flexible set of `ggplot2` geoms and stats designed especially for
visualizing distributions and uncertainty. It is designed for both
frequentist and Bayesian uncertainty visualization, taking the view that
uncertainty visualization can be unified through the perspective of
distribution visualization: for frequentist models, one visualizes
confidence distributions or bootstrap distributions (see
`vignette("freq-uncertainty-vis")`); for Bayesian models, one visualizes
probability distributions (see the
[tidybayes](https://mjskay.github.io/tidybayes/) package, which builds
on top of `ggdist`).

The `geom_slabinterval()` / `stat_slabinterval()` family (see
`vignette("slabinterval")`) makes it easy to visualize point summaries
and intervals, eye plots, half-eye plots, ridge plots, CCDF bar plots,
gradient plots, histograms, and more:

<figure>
<img src="man/figures/slabinterval_family.png"
alt="The slabinterval family of geoms and stats" />
<figcaption aria-hidden="true">The slabinterval family of geoms and
stats</figcaption>
</figure>

The `geom_dotsinterval()` / `stat_dotsinterval()` family (see
`vignette("dotsinterval")`) makes it easy to visualize dot+interval
plots, Wilkinson dotplots, beeswarm plots, and quantile dotplots (and
combined with half-eyes, composite plots like rain cloud plots):

<img src="man/figures/README/halfeye_dotplot-1.png" width="672" />

The `geom_lineribbon()` / `stat_lineribbon()` family (see
`vignette("lineribbon")`) makes it easy to visualize fit lines with an
arbitrary number of uncertainty bands:

<img src="man/figures/README/lineribbon-1.png" width="672" />

All stat in `ggdist` also support visualizing analytical distributions
and vectorized distribution data types like
[distributional](https://pkg.mitchelloharawild.com/distributional/)
objects or `posterior::rvar()` objects. This is particularly useful when
visualizing uncertainty in frequentist models (see
`vignette("freq-uncertainty-vis")`) or when visualizing priors in a
Bayesian analysis.

The `ggdist` geoms and stats also form a core part of the
[tidybayes](https://mjskay.github.io/tidybayes/) package (in fact, they
originally were part of `tidybayes`). For examples of the use of
`ggdist` geoms and stats for visualizing uncertainty in Bayesian models,
see the vignettes in tidybayes, such as
`vignette("tidybayes", package = "tidybayes")` or
`vignette("tidy-brms", package = "tidybayes")`.

## Cheat sheets

<a href="https://github.com/mjskay/ggdist/blob/master/figures-source/cheat_sheet-slabinterval.pdf"><img src="https://raw.githubusercontent.com/mjskay/ggdist/master/figures-source/cheat_sheet-slabinterval.png" width="630" /></a>

## Installation

You can install the currently-released version from CRAN with this R
command:

``` r
install.packages("ggdist")
```

Alternatively, you can install the latest development version from
GitHub with these R commands:

``` r
install.packages("devtools")
devtools::install_github("mjskay/ggdist")
```

## Feedback, issues, and contributions

I welcome feedback, suggestions, issues, and contributions! I am not
particularly reliable over email, though you can contact me at
<mjskay@northwestern.edu>. On [Twitter](https://twitter.com/mjskay) I am
more reliable. If you have found a bug, please file it
[here](https://github.com/mjskay/ggdist/issues/new) with minimal code to
reproduce the issue. Pull requests should be filed against the
[`dev`](https://github.com/mjskay/ggdist/tree/dev) branch.

## Citing `ggdist`

Matthew Kay (2024). ggdist: Visualizations of Distributions and
Uncertainty in the Grammar of Graphics. *IEEE Transactions on
Visualization and Computer Graphics*. DOI:
[10.1109/TVCG.2023.3327195](https://doi.org/10.1109/TVCG.2023.3327195).

Matthew Kay (2024). ggdist: Visualizations of Distributions and
Uncertainty. R package version 3.3.1.9000,
<https://mjskay.github.io/ggdist/>. DOI:
[10.5281/zenodo.3879620](https://doi.org/10.5281/zenodo.3879620).
