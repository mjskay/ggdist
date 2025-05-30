# Documentation methods for the lineribbon family
#
# Author: mjskay
###############################################################################



# lineribbon shortcut stats/geoms ---------------------------------------------------

rd_lineribbon_shortcut_stat = function(
  stat_name, chart_type,
  geom_name = stat_name,
  from_name = "lineribbon"
) {
  stat = get(paste0("Stat", camel_case(stat_name)))
  geom = get(paste0("Geom", camel_case(geom_name)))

  c(
    glue_doc('@title <<title_case(chart_type)>> plot (shortcut stat)'),
    glue_doc('
      @description
      A combination of [stat_slabinterval()] and [geom_lineribbon()] with sensible defaults
      for making <<chart_type>> plots. While [geom_lineribbon()] is intended for use on data
      frames that have already been summarized using a [point_interval()] function,
      [stat_<<stat_name>>()] is intended for use directly on data frames of draws or of
      analytical distributions, and will perform the summarization using a [point_interval()]
      function.
      '),
    '@description\n **Roughly equivalent to:**',
    rd_shortcut_stat(stat_name, geom_name, from_name = from_name),
    '@inheritParams stat_pointinterval',
    '@inheritParams geom_lineribbon',
    rd_layer_params(geom_name, stat, as_dots = TRUE),
    glue_doc('
      @param geom <[Geom][ggplot2::Geom] | [string][character]> Use to override
      the default connection between [stat_<<stat_name>>()] and [geom_<<geom_name>>()]'),
    glue_doc('
      @param show.legend <[logical]> Should this layer be included in the legends?
      `NA`, the default, includes if any aesthetics are mapped. `FALSE` never includes,
      and `TRUE` always includes. It can also be a named logical vector to finely select
      the aesthetics to display.'),
    '@template details-x-y-xdist-ydist',
    glue_doc('
      @return A [ggplot2::Stat] representing a <<chart_type>> geometry which can
      be added to a [`ggplot()`][ggplot2::ggplot] object.'),
    rd_slabinterval_computed_variables(stat),
    rd_lineribbon_aesthetics(geom_name, stat),
    glue_doc('
      @seealso
      See [geom_<<geom_name>>()] for the geom underlying this stat.
      '),
    '@family lineribbon stats',
    glue_doc('
      @examples
      library(dplyr)
      library(ggplot2)
      library(distributional)

      theme_set(theme_ggdist())

      # ON SAMPLE DATA
      set.seed(12345)
      tibble(
        x = rep(1:10, 100),
        y = rnorm(1000, x)
      ) %>%
        ggplot(aes(x = x, y = y)) +
        stat_<<stat_name>>() +
        scale_fill_brewer()

      # ON ANALYTICAL DISTRIBUTIONS
      # Vectorized distribution types, like distributional::dist_normal()
      # and posterior::rvar(), can be used with the `xdist` / `ydist` aesthetics
      tibble(
        x = 1:10,
        sd = seq(1, 3, length.out = 10)
      ) %>%
        ggplot(aes(x = x, ydist = dist_normal(x, sd))) +
        stat_<<stat_name>>() +
        scale_fill_brewer()
      ')
  )
}


# aesthetics --------------------------------------------------------------

#' Provides documentation of aesthetics for lineribbons
#' @noRd
rd_lineribbon_aesthetics = function(
  geom_name = "lineribbon",
  stat = NULL,
  vignette = "lineribbon"
) {

  out = glue_doc('
    @section Aesthetics:
    The line+ribbon `stat`s and `geom`s have a wide variety of aesthetics that control
    the appearance of their two sub-geometries: the **line** and the **ribbon**.

    ')

  out = c(out, rd_aesthetics_sections(geom_name, stat, vignette = vignette))

  glue_collapse(out, "\n")
}
