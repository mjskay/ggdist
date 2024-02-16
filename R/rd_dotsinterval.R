# Documentation methods for the dotsinterval family
#
# Author: mjskay
###############################################################################



# shortcut stats/geoms ----------------------------------------------------

rd_dotsinterval_shortcut_geom = function(
  geom_name,
  chart_type,
  from_name = "dotsinterval",
  title = TRUE,
  describe = TRUE
) {
  geom = get(paste0("Geom", title_case(geom_name)))

  c(
    if (title) glue_doc('@title <<title_case(chart_type)>> plot (shortcut geom)'),
    if (describe) glue_doc('
      @description
      Shortcut version of [geom_dotsinterval()] for creating <<chart_type>> plots.
      Geoms based on [geom_dotsinterval()] create dotplots that automatically
      ensure the plot fits within the available space.

      Roughly equivalent to:
      '),
    if (describe) rd_shortcut_geom(geom_name, from_name),
    '@inheritParams geom_dotsinterval',
    glue_doc('
      @return A [ggplot2::Geom] representing a <<chart_type>> geometry which can
      be added to a [ggplot()] object.
      '),
    '@template details-dotsinterval-family',
    '@template references-quantile-dotplots',
    rd_dotsinterval_aesthetics(geom_name),
    if (exists(paste0("stat_", geom_name))) glue_doc('
      @seealso See [stat_<<geom_name>>()] for the stat version, intended for
      use on sample data or analytical distributions.
      '),
    '@seealso See [geom_dotsinterval()] for the geometry this shortcut is based on.',
    '@seealso See `vignette("dotsinterval")` for a variety of examples of use.',
    '@family dotsinterval geoms',
    glue_doc('@examples
      library(dplyr)
      library(ggplot2)

      data(RankCorr_u_tau, package = "ggdist")

      # orientation is detected automatically based on
      # which axis is discrete

      RankCorr_u_tau %>%
        ggplot(aes(x = u_tau)) +
        geom_<<geom_name>>()

      RankCorr_u_tau %>%
        ggplot(aes(y = u_tau)) +
        geom_<<geom_name>>()
      ')
  )
}

rd_dotsinterval_shortcut_stat = function(
  stat_name, chart_type,
  geom_name = stat_name
) {
  stat = get(paste0("Stat", title_case(stat_name)))
  geom = get(paste0("Geom", title_case(geom_name)))

  c(
    glue_doc('@title <<title_case(chart_type)>> plot (shortcut stat)'),
    glue_doc('
      @description
      A combination of [stat_slabinterval()] and [geom_dotsinterval()] with sensible defaults
      for making <<chart_type>> plots. While [geom_dotsinterval()] is intended for use on data
      frames that have already been summarized using a [point_interval()] function,
      [stat_<<stat_name>>()] is intended for use directly on data frames of draws or of
      analytical distributions, and will perform the summarization using a [point_interval()]
      function. Geoms based on [geom_dotsinterval()] create dotplots that automatically determine a bin width that
      ensures the plot fits within the available space. They can also ensure dots do not overlap.
      '),
    if (stat_name != "dotsinterval") c(
      '@description\n Roughly equivalent to:',
      rd_shortcut_stat(stat_name, geom_name, from_name = "dotsinterval")
    ),
    if (stat_name != "dotsinterval") '@inheritParams stat_dotsinterval',
    '@inheritParams geom_dotsinterval',
    rd_layer_params(geom_name, stat, as_dots = TRUE),
    glue_doc('
      @param geom Use to override the default connection between
      [stat_<<stat_name>>()] and [geom_<<geom_name>>()]'),
    '@template details-dotsinterval-family',
    '@template references-quantile-dotplots',
    '@template details-x-y-xdist-ydist',
    glue_doc('
      @return A [ggplot2::Stat] representing a <<chart_type>> geometry which can
      be added to a [ggplot()] object.'),
    rd_slabinterval_computed_variables(stat),
    rd_dotsinterval_aesthetics(geom_name, stat),
    glue_doc('
      @seealso
      See [geom_<<geom_name>>()] for the geom underlying this stat.
      See `vignette("dotsinterval")` for a variety of examples of use.
      '),
    '@family dotsinterval stats',
    glue_doc('
      @examples
      library(dplyr)
      library(ggplot2)
      library(distributional)

      theme_set(theme_ggdist())

      # ON SAMPLE DATA
      tibble(x = 1:10) %>%
        group_by_all() %>%
        do(tibble(y = rnorm(100, .$x))) %>%
        ggplot(aes(x = x, y = y)) +
        stat_<<stat_name>>()

      # ON ANALYTICAL DISTRIBUTIONS
      # Vectorized distribution types, like distributional::dist_normal()
      # and posterior::rvar(), can be used with the `xdist` / `ydist` aesthetics
      tibble(
        x = 1:10,
        sd = seq(1, 3, length.out = 10)
      ) %>%
        ggplot(aes(x = x, ydist = dist_normal(x, sd))) +
        stat_<<stat_name>>(quantiles = 50)
      ')
  )
}


# aesthetics --------------------------------------------------------------

#' Provides documentation of aesthetics for dotsintervals
#' @noRd
rd_dotsinterval_aesthetics = function(
  geom_name = "dotsinterval",
  stat = NULL,
  vignette = "dotsinterval"
) {

  out = glue_doc('
    @section Aesthetics:
    The dots+interval `stat`s and `geom`s have a wide variety of aesthetics that control
    the appearance of their three sub-geometries: the **dots** (aka the **slab**), the
    **point**, and the **interval**.

    ')

  out = c(out, rd_aesthetics_sections(geom_name, stat, vignette = vignette))

  glue_collapse(out, "\n")
}


# shared parameter docs ---------------------------------------------------

rd_param_dots_layout = function() {
  paste0("@param layout ", GeomDotsinterval$get_param_docs()$layout)
}

rd_param_dots_overlaps = function() {
  paste0("@param overlaps ", GeomDotsinterval$get_param_docs()$overlaps)
}
