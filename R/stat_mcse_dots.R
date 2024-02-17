# stats for blurry mcse dotplots
#
# Author: mjskay
###############################################################################


# stat_mcse_dots ------------------------------------------------

StatMcseDots = ggproto("StatMcseDots", StatDots,
  default_aes = defaults(aes(
    blur = after_stat(se)
  ), StatDots$default_aes),

  compute_slab = function(self, ...) compute_slab_dots(self, ..., compute_mcse = TRUE)
)

#' @title Blurry MCSE dot plot (stat)
#' @description
#' Variant of [stat_dots()] for creating blurry dotplots of quantiles. Uses
#' [posterior::mcse_quantile()] to calculate the Monte Carlo Standard Error
#' of each quantile computed for the dotplot, yielding an `se` computed variable
#' that is by default mapped onto the `blur` aesthetic of [geom_blur_dots()].
#' @eval rd_dotsinterval_shortcut_stat(
#'   "mcse_dots", "blurry MCSE dot", geom_name = "blur_dots",
#'   title = FALSE, describe = FALSE, examples = FALSE
#' )
#' @examplesIf getRversion() >= "4.1" && requireNamespace("posterior", quietly = TRUE)
#' library(dplyr)
#' library(ggplot2)
#'
#' theme_set(theme_ggdist())
#'
#' set.seed(1234)
#' data.frame(x = rnorm(1000)) %>%
#'   ggplot(aes(x = x)) +
#'   stat_mcse_dots(quantiles = 100, layout = "weave")
#' @export
stat_mcse_dots = make_stat(StatMcseDots, geom = "blur_dots")
