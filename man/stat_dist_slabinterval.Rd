% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stat_dist_slabinterval.R
\name{stat_dist_slabinterval}
\alias{stat_dist_slabinterval}
\alias{stat_dist_halfeye}
\alias{stat_dist_eye}
\alias{stat_dist_ccdfinterval}
\alias{stat_dist_cdfinterval}
\alias{stat_dist_gradientinterval}
\alias{stat_dist_pointinterval}
\alias{stat_dist_interval}
\alias{stat_dist_slab}
\title{Distribution + interval plots (eye plots, half-eye plots, CCDF barplots, etc) for analytical distributions (ggplot stat)}
\usage{
stat_dist_slabinterval(
  mapping = NULL,
  data = NULL,
  geom = "slabinterval",
  position = "identity",
  ...,
  slab_type = c("pdf", "cdf", "ccdf"),
  p_limits = c(NA, NA),
  outline_bars = FALSE,
  orientation = NA,
  limits = NULL,
  n = 501,
  .width = c(0.66, 0.95),
  show_slab = TRUE,
  show_interval = TRUE,
  na.rm = FALSE,
  show.legend = c(size = FALSE),
  inherit.aes = TRUE
)

stat_dist_halfeye(...)

stat_dist_eye(
  mapping = NULL,
  data = NULL,
  geom = "slabinterval",
  position = "identity",
  ...,
  show.legend = c(size = FALSE),
  inherit.aes = TRUE
)

stat_dist_ccdfinterval(
  mapping = NULL,
  data = NULL,
  geom = "slabinterval",
  position = "identity",
  ...,
  slab_type = "ccdf",
  normalize = "none",
  show.legend = c(size = FALSE),
  inherit.aes = TRUE
)

stat_dist_cdfinterval(..., slab_type = "cdf", normalize = "none")

stat_dist_gradientinterval(
  mapping = NULL,
  data = NULL,
  geom = "slabinterval",
  position = "identity",
  ...,
  show.legend = c(size = FALSE, slab_alpha = FALSE),
  inherit.aes = TRUE
)

stat_dist_pointinterval(..., show_slab = FALSE)

stat_dist_interval(
  mapping = NULL,
  data = NULL,
  geom = "interval",
  position = "identity",
  ...,
  show_slab = FALSE,
  show_point = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

stat_dist_slab(
  mapping = NULL,
  data = NULL,
  geom = "slab",
  position = "identity",
  ...,
  show.legend = NA,
  inherit.aes = TRUE
)
}
\arguments{
\item{mapping}{Set of aesthetic mappings created by \code{\link[ggplot2:aes]{aes()}} or
\code{\link[ggplot2:aes_]{aes_()}}. If specified and \code{inherit.aes = TRUE} (the
default), it is combined with the default mapping at the top level of the
plot. You must supply \code{mapping} if there is no plot mapping.}

\item{data}{The data to be displayed in this layer. There are three
options:

If \code{NULL}, the default, the data is inherited from the plot
data as specified in the call to \code{\link[ggplot2:ggplot]{ggplot()}}.

A \code{data.frame}, or other object, will override the plot
data. All objects will be fortified to produce a data frame. See
\code{\link[ggplot2:fortify]{fortify()}} for which variables will be created.

A \code{function} will be called with a single argument,
the plot data. The return value must be a \code{data.frame}, and
will be used as the layer data. A \code{function} can be created
from a \code{formula} (e.g. \code{~ head(.x, 10)}).}

\item{geom}{Use to override the default connection between
\code{stat_slabinterval} and \code{\link[=geom_slabinterval]{geom_slabinterval()}}}

\item{position}{Position adjustment, either as a string, or the result of
a call to a position adjustment function.}

\item{...}{Other arguments passed to \code{\link[=layer]{layer()}}. They may also be arguments to the paired geom
(e.g., \code{\link[=geom_pointinterval]{geom_pointinterval()}})}

\item{slab_type}{The type of slab function to calculate: probability density (or mass) function (\code{"pdf"}),
cumulative distribution function (\code{"cdf"}), or complementary CDF (\code{"ccdf"}).}

\item{p_limits}{Probability limits (as a vector of size 2) used to determine the lower and upper
limits of the slab. E.g., if this is \code{c(.001, .999)}, then a slab is drawn
for the distribution from the quantile at \code{p = .001} to the quantile at \code{p = .999}. If the lower
(respectively upper) limit is \code{NA}, then the lower (upper) limit will be the minimum (maximum) of the
distribution's support if it is finite, and \code{0.001} (\code{0.999}) if it is not finite. E.g., if
\code{p_limits} is \code{c(NA, NA)} on a gamma distribution the effective value of \code{p_limits} would be
\code{c(0, .999)} since the gamma distribution is defined on \verb{(0, Inf)}; whereas on a normal distribution
it would be equivalent to \code{c(.001, .999)} since the normal distribution is defined on \verb{(-Inf, Inf)}.}

\item{outline_bars}{For discrete distributions (whose slabs are drawn as histograms), determines
if outlines in between the bars are drawn when the \code{slab_color} aesthetic is used. If \code{FALSE}
(the default), the outline is drawn only along the tops of the bars; if \code{TRUE}, outlines in between
bars are also drawn.}

\item{orientation}{Whether this geom is drawn horizontally (\code{"horizontal"}) or
vertically (\code{"vertical"}). The default, \code{NA}, automatically detects the orientation based on how the
aesthetics are assigned, and should generally do an okay job at this. When horizontal (resp. vertical),
the geom uses the \code{y} (resp. \code{x}) aesthetic to identify different groups, then for each group uses
the \code{x} (resp. \code{y}) aesthetic and the \code{thickness} aesthetic to draw a function as an slab, and draws
points and intervals horizontally (resp. vertically) using the \code{xmin}, \code{x}, and \code{xmax} (resp.
\code{ymin}, \code{y}, and \code{ymax}) aesthetics. For compatibility with the base
ggplot naming scheme for \code{orientation}, \code{"x"} can be used as an alias for \code{"vertical"} and \code{"y"} as an alias for
\code{"horizontal"} (tidybayes had an \code{orientation} parameter before ggplot did, and I think the tidybayes naming
scheme is more intuitive: \code{"x"} and \code{"y"} are not orientations and their mapping to orientations is, in my
opinion, backwards; but the base ggplot naming scheme is allowed for compatibility).}

\item{limits}{Manually-specified limits for the slab, as a vector of length two. These limits are combined with those
computed based on \code{p_limits} as well as the limits defined by the scales of the plot to determine the
limits used to draw the slab functions: these limits specify the maximal limits; i.e., if specified, the limits
will not be wider than these (but may be narrower).Use \code{NA} to leave a limit alone; e.g.
\code{limits = c(0, NA)} will ensure that the lower limit does not go below 0, but let the upper limit
be determined by either \code{p_limits} or the scale settings.}

\item{n}{Number of points at which to evaluate \code{slab_function}}

\item{.width}{The \code{.width} argument passed to \code{interval_function} or \code{point_interval}.}

\item{show_slab}{Should the slab portion of the geom be drawn? Default \code{TRUE}.}

\item{show_interval}{Should the interval portion of the geom be drawn? Default \code{TRUE}.}

\item{na.rm}{If \code{FALSE}, the default, missing values are removed with a warning. If \code{TRUE}, missing
values are silently removed.}

\item{show.legend}{Should this layer be included in the legends? Default is \code{c(size = FALSE)}, unlike most geoms,
to match its common use cases. \code{FALSE} hides all legends, \code{TRUE} shows all legends, and \code{NA} shows only
those that are mapped (the default for most geoms).}

\item{inherit.aes}{If \code{FALSE}, overrides the default aesthetics,
rather than combining with them. This is most useful for helper functions
that define both data and aesthetics and shouldn't inherit behaviour from
the default plot specification, e.g. \code{\link[ggplot2:borders]{borders()}}.}

\item{normalize}{How to normalize heights of functions input to the \code{thickness} aesthetic. If \code{"all"}
(the default), normalize so that the maximum height across all data is \code{1}; if \code{"panels"}, normalize within
panels so that the maximum height in each panel is \code{1}; if \code{"xy"}, normalize within
the x/y axis opposite the \code{orientation} of this geom so that the maximum height at each value of the
opposite axis is \code{1}; if \code{"groups"}, normalize within values of the opposite axis and within
groups so that the maximum height in each group is \code{1}; if \code{"none"}, values are taken as is with no
normalization (this should probably only be used with functions whose values are in [0,1], such as CDFs).}

\item{show_point}{Should the point portion of the geom be drawn? Default \code{TRUE}.}
}
\value{
A \link[ggplot2:ggplot2-ggproto]{ggplot2::Stat} representing a slab or combined slab+interval geometry which can
be added to a \code{\link[=ggplot]{ggplot()}} object.
}
\description{
Stats for computing distribution functions (densities or CDFs) + intervals for use with
\code{\link[=geom_slabinterval]{geom_slabinterval()}}. Uses the \code{dist} aesthetic to specify a distribution using
objects from the \href{https://pkg.mitchelloharawild.com/distributional/}{distributional} package,
or using distribution names and \code{arg1}, ... \code{arg9} aesthetics (or \code{args} as a list column)
to specify distribution arguments. See \emph{Details}.
}
\details{
A highly configurable stat for generating a variety of plots that combine a "slab"
that describes a distribution plus an interval. Several "shortcut" stats are provided
which combine multiple options to create useful geoms, particularly \emph{eye plots}
(a combination of a violin plot and interval), \emph{half-eye plots} (a density plus interval),
and \emph{CCDF bar plots} (a complementary CDF plus interval).

The shortcut stat names follow the pattern \code{stat_dist_[name]}.

Stats include:

\itemize{
\item \code{stat_dist_eye}: Eye plots (violin + interval)
\item \code{stat_dist_halfeye}: Half-eye plots (density + interval)
\item \code{stat_dist_ccdfinterval}: CCDF bar plots (CCDF + interval)
\item \code{stat_dist_cdfinterval}: CDF bar plots (CDF + interval)
\item \code{stat_dist_gradientinterval}: Density gradient + interval plots
\item \code{stat_dist_pointinterval}: Point + interval plots
\item \code{stat_dist_interval}: Interval plots
}

These stats expect a \code{dist} aesthetic to specify a distribution. This aesthetic
can be used in one of two ways:
\itemize{
\item \code{dist} can be any distribution object from the \href{https://pkg.mitchelloharawild.com/distributional/}{distributional}
package, such as \code{\link[=dist_normal]{dist_normal()}}, \code{\link[=dist_beta]{dist_beta()}}, etc. Since these functions are vectorized,
other columns can be passed directly to them in an \code{\link[=aes]{aes()}} specification; e.g.
\code{aes(dist = dist_normal(mu, sigma))} will work if \code{mu} and \code{sigma} are columns in the
input data frame.
\item \code{dist} can be a character vector giving the distribution name. Then the  \code{arg1}, ... \code{arg9}
aesthetics (or \code{args} as a list column) specify distribution arguments. Distribution names
should correspond to R functions that have \code{"p"}, \code{"q"}, and \code{"d"} functions; e.g. \code{"norm"}
is a valid distribution name because R defines the \code{\link[=pnorm]{pnorm()}}, \code{\link[=qnorm]{qnorm()}}, and \code{\link[=dnorm]{dnorm()}}
functions for Normal distributions.

See the \code{\link[=parse_dist]{parse_dist()}} function for a useful way to generate \code{dist} and \code{args}
values from human-readable distribution specs (like \code{"normal(0,1)"}). Such specs are also
produced by other packages (like the \code{brms::get_prior} function in brms); thus,
\code{\link[=parse_dist]{parse_dist()}} combined with the stats described here can help you visualize the output
of those functions.
}
}
\section{Computed Variables}{

The following variables are computed by this stat and made available for
use in aesthetic specifications (\code{aes()}) using the \code{stat()} or \code{after_stat()}
functions:
\itemize{
\item \code{x} or \code{y}: For slabs, the input values to the slab function.
For intervals, the point summary from the interval function. Whether it is \code{x} or \code{y} depends on \code{orientation}
\item \code{xmin} or \code{ymin}: For intervals, the lower end of the interval from the interval function.
\item \code{xmax} or \code{ymax}: For intervals, the upper end of the interval from the interval function.
\item \code{.width}: For intervals, the interval width as a numeric value in \verb{[0, 1]}.
\item \code{level}: For intervals, the interval width as an ordered factor.
\item \code{f}: For slabs, the output values from the slab function (such as the PDF, CDF, or CCDF),
determined by \code{slab_type}.
\item \code{pdf}: For slabs, the probability density function.
\item \code{cdf}: For slabs, the cumulative distribution function.
}
}

\section{Aesthetics}{

The slab+interval \code{stat}s and \code{geom}s have a wide variety of aesthetics that control
the appearance of their three sub-geometries: the \strong{slab}, the \strong{point}, and
the \strong{interval}.

These \code{stat}s support the following aesthetics:
\itemize{
\item \code{x}: x position of the geometry (when orientation = \code{"vertical"}); or sample data to be summarized
(when \code{orientation = "horizontal"}) except for \code{stat_dist_} geometries (which use only one of \code{x} or \code{y}
at a time along with the \code{dist} aesthetic).
\item \code{y}: y position of the geometry (when orientation = \code{"horizontal"}); or sample data to be summarized
(when \code{orientation = "vertical"}) except for \code{stat_dist_} geometries (which use only one of \code{x} or \code{y}
at a time along with the \code{dist} aesthetic).
\item \code{dist}: A name of a distribution (e.g. \code{"norm"}) or a \pkg{distributional} object (e.g. \code{\link[=dist_normal]{dist_normal()}}).
See \strong{Details}.
\item \code{args}: Distribution arguments (\code{args} or \code{arg1}, ... \code{arg9}). See \strong{Details}.
}
In addition, in their default configuration (paired with \code{\link[=geom_slabinterval]{geom_slabinterval()}}) the following aesthetics are supported by the underlying geom:

\strong{Slab-specific aesthetics}
\itemize{
\item \code{thickness}: The thickness of the slab at each \code{x} value (if \code{orientation = "horizontal"}) or
\code{y} value (if \code{orientation = "vertical"}) of the slab.
\item \code{side}: Which side to place the slab on. \code{"topright"}, \code{"top"}, and \code{"right"} are synonyms
which cause the slab to be drawn on the top or the right depending on if \code{orientation} is \code{"horizontal"}
or \code{"vertical"}. \code{"bottomleft"}, \code{"bottom"}, and \code{"left"} are synonyms which cause the slab
to be drawn on the bottom or the left depending on if \code{orientation} is \code{"horizontal"} or
\code{"vertical"}. \code{"topleft"} causes the slab to be drawn on the top or the left, and \code{"bottomright"}
causes the slab to be drawn on the bottom or the right. \code{"both"} draws the slab mirrored on both
sides (as in a violin plot).
\item \code{scale}: What proportion of the region allocated to this geom to use to draw the slab. If \code{scale = 1},
slabs that use the maximum range will just touch each other. Default is \code{0.9} to leave some space.
\item \code{justification}: Justification of the interval relative to the slab, where \code{0} indicates bottom/left
justification and \code{1} indicates top/right justification (depending on \code{orientation}). If \code{justification}
is \code{NULL} (the default), then it is set automatically based on the value of \code{side}: when \code{side} is
\code{"top"}/\code{"right"} \code{justification} is set to \code{0}, when \code{side} is \code{"bottom"}/\code{"left"}
\code{justification} is set to \code{1}, and when \code{side} is \code{"both"} \code{justification} is set to 0.5.
\item \code{datatype}: When using composite geoms directly without a \code{stat} (e.g. \code{\link[=geom_slabinterval]{geom_slabinterval()}}), \code{datatype} is used to
indicate which part of the geom a row in the data targets: rows with \code{datatype = "slab"} target the
slab portion of the geometry and rows with \code{datatype = "interval"} target the interval portion of
the geometry. This is set automatically when using ggdist \code{stat}s.
}
\strong{Interval-specific aesthetics}
\itemize{
\item \code{xmin}: Left end of the interval sub-geometry (if \code{orientation = "horizontal"}).
\item \code{xmax}: Right end of the interval sub-geometry (if \code{orientation = "horizontal"}).
\item \code{ymin}: Lower end of the interval sub-geometry (if \code{orientation = "vertical"}).
\item \code{ymax}: Upper end of the interval sub-geometry (if \code{orientation = "vertical"}).
}
\strong{Point-specific aesthetics}
\itemize{
\item \code{shape}: Shape type used to draw the \strong{point} sub-geometry.
}
\strong{Color aesthetics}
\itemize{
\item \code{colour}: (or \code{color}) The color of the \strong{interval} and \strong{point} sub-geometries.
Use the \code{slab_color}, \code{interval_color}, or \code{point_color} aesthetics (below) to
set sub-geometry colors separately.
\item \code{fill}: The fill color of the \strong{slab} and \strong{point} sub-geometries. Use the \code{slab_fill}
or \code{point_fill} aesthetics (below) to set sub-geometry colors separately.
\item \code{alpha}: The opacity of the \strong{slab}, \strong{interval}, and \strong{point} sub-geometries. Use the \code{slab_alpha},
\code{interval_alpha}, or \code{point_alpha} aesthetics (below) to set sub-geometry colors separately.
\item \code{colour_ramp}: (or \code{color_ramp}) A secondary scale that modifies the \code{color}
scale to "ramp" to another color. See \code{scale_colour_ramp()} for examples.
\item \code{fill_ramp}: (or \code{fill_ramp}) A secondary scale that modifies the \code{fill}
scale to "ramp" to another color. See \code{scale_fill_ramp()} for examples.
}
\strong{Line aesthetics}
\itemize{
\item \code{size}: Width of the outline around the \strong{slab} (if visible). Also determines the width of
the line used to draw the \strong{interval} and the size of the \strong{point}, but raw
\code{size} values are transformed according to the \code{interval_size_domain}, \code{interval_size_range},
and \code{fatten_point} parameters of the \code{geom} (see above). Use the \code{slab_size},
\code{interval_size}, or \code{point_size} aesthetics (below) to set sub-geometry line widths separately
(note that when size is set directly using the override aesthetics, interval and point
sizes are not affected by \code{interval_size_domain}, \code{interval_size_range}, and \code{fatten_point}).
\item \code{stroke}: Width of the outline around the \strong{point} sub-geometry.
\item \code{linetype}: Type of line (e.g., \code{"solid"}, \code{"dashed"}, etc) used to draw the \strong{interval}
and the outline of the \strong{slab} (if it is visible). Use the \code{slab_linetype} or
\code{interval_linetype} aesthetics (below) to set sub-geometry line types separately.
}
\strong{Slab-specific color/line override aesthetics}
\itemize{
\item \code{slab_fill}: Override for \code{fill}: the fill color of the slab.
\item \code{slab_colour}: (or \code{slab_color}) Override for \code{colour}/\code{color}: the outline color of the slab.
\item \code{slab_alpha}: Override for \code{alpha}: the opacity of the slab.
\item \code{slab_size}: Override for \code{size}: the width of the outline of the slab.
\item \code{slab_linetype}: Override for \code{linetype}: the line type of the outline of the slab.
}
\strong{Interval-specific color/line override aesthetics}
\itemize{
\item \code{interval_colour}: (or \code{interval_color}) Override for \code{colour}/\code{color}: the color of the interval.
\item \code{interval_alpha}: Override for \code{alpha}: the opacity of the interval.
\item \code{interval_size}: Override for \code{size}: the line width of the interval.
\item \code{interval_linetype}: Override for \code{linetype}: the line type of the interval.
}
\strong{Point-specific color/line override aesthetics}
\itemize{
\item \code{point_fill}: Override for \code{fill}: the fill color of the point.
\item \code{point_colour}: (or \code{point_color}) Override for \code{colour}/\code{color}: the outline color of the point.
\item \code{point_alpha}: Override for \code{alpha}: the opacity of the point.
\item \code{point_size}: Override for \code{size}: the size of the point.
}
\strong{Other aesthetics} (these work as in standard \code{geom}s)
\itemize{
\item \code{width}
\item \code{height}
\item \code{group}
}
See examples of some of these aesthetics in action in \code{vignette("slabinterval")}.
Learn more about the sub-geom override aesthetics (like \code{interval_color}) in the \link[ggdist]{scales} documentation.
Learn more about basic ggplot aesthetics in \code{vignette("ggplot2-specs")}.
}

\examples{

library(dplyr)
library(ggplot2)
library(distributional)

theme_set(theme_ggdist())

dist_df = tribble(
  ~group, ~subgroup, ~mean, ~sd,
  "a",          "h",     5,   1,
  "b",          "h",     7,   1.5,
  "c",          "h",     8,   1,
  "c",          "i",     9,   1,
  "c",          "j",     7,   1
)

dist_df \%>\%
  ggplot(aes(x = group, dist = "norm", arg1 = mean, arg2 = sd, fill = subgroup)) +
  stat_dist_eye(position = "dodge")

# Using functions from the distributional package (like dist_normal()) with the
# dist aesthetic can lead to more compact/expressive specifications

dist_df \%>\%
  ggplot(aes(x = group, dist = dist_normal(mean, sd), fill = subgroup)) +
  stat_dist_eye(position = "dodge")

# the stat_dist_... family applies a Jacobian adjustment to densities
# when plotting on transformed scales in order to plot them correctly.
# It determines the Jacobian using symbolic differentiation if possible,
# using stats::D(). If symbolic differentation fails, it falls back
# to numericDeriv(), which is less reliable; therefore, it is
# advisable to use scale transformation functions that are defined in
# terms of basic math functions so that their derivatives can be
# determined analytically (most of the transformation functions in the
# scales package currently have this property).
# For example, here is a log-Normal distribution plotted on the log
# scale, where it will appear Normal:
data.frame(dist = "lnorm", logmean = log(10), logsd = 2*log(10)) \%>\%
  ggplot(aes(y = 1, dist = dist, arg1 = logmean, arg2 = logsd)) +
  stat_dist_halfeye() +
  scale_x_log10(breaks = 10^seq(-5,7, by = 2))

# see vignette("slabinterval") for many more examples.

}
\seealso{
See \code{\link[=geom_slabinterval]{geom_slabinterval()}} for more information on the geom these stats
use by default and some of the options they have. See \code{\link[=stat_sample_slabinterval]{stat_sample_slabinterval()}}
for the versions of these stats that can be used on samples.
See \code{vignette("slabinterval")} for a variety of examples of use.
}
