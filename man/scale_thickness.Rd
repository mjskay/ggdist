% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scale_thickness.R
\name{scale_thickness}
\alias{scale_thickness}
\alias{scale_thickness_shared}
\alias{scale_thickness_identity}
\title{Slab thickness scale (ggplot2 scale)}
\usage{
scale_thickness_shared(
  name = waiver(),
  breaks = waiver(),
  labels = waiver(),
  limits = function(l) c(min(0, l[[1]]), l[[2]]),
  renormalize = FALSE,
  oob = scales::oob_keep,
  guide = "none",
  expand = c(0, 0),
  ...
)

scale_thickness_identity(..., guide = "none")
}
\arguments{
\item{name}{The name of the scale. Used as the axis or legend title. If
\code{waiver()}, the default, the name of the scale is taken from the first
mapping used for that aesthetic. If \code{NULL}, the legend title will be
omitted.}

\item{breaks}{One of:
\itemize{
\item \code{NULL} for no breaks
\item \code{waiver()} for the default breaks computed by the
\link[scales:new_transform]{transformation object}
\item A numeric vector of positions
\item A function that takes the limits as input and returns breaks
as output (e.g., a function returned by \code{\link[scales:breaks_extended]{scales::extended_breaks()}}).
Note that for position scales, limits are provided after scale expansion.
Also accepts rlang \link[rlang:as_function]{lambda} function notation.
}}

\item{labels}{One of:
\itemize{
\item \code{NULL} for no labels
\item \code{waiver()} for the default labels computed by the
transformation object
\item A character vector giving labels (must be same length as \code{breaks})
\item An expression vector (must be the same length as breaks). See ?plotmath for details.
\item A function that takes the breaks as input and returns labels
as output. Also accepts rlang \link[rlang:as_function]{lambda} function
notation.
}}

\item{limits}{One of:
\itemize{
\item \code{NULL} to use the default scale range
\item A numeric vector of length two providing limits of the scale.
Use \code{NA} to refer to the existing minimum or maximum
\item A function that accepts the existing (automatic) limits and returns
new limits. Also accepts rlang \link[rlang:as_function]{lambda} function
notation.
Note that setting limits on positional scales will \strong{remove} data outside of the limits.
If the purpose is to zoom, use the limit argument in the coordinate system
(see \code{\link[ggplot2:coord_cartesian]{coord_cartesian()}}).
}}

\item{renormalize}{<scalar \link{logical}> When mapping values to the \code{thickness} scale, should those
values be allowed to be renormalized by geoms (e.g. via the \code{normalize} parameter
to \code{\link[=geom_slabinterval]{geom_slabinterval()}})? The default is \code{FALSE}: if \code{scale_thickness_shared()}
is in use, the geom-specific \code{normalize} parameter is ignored (this is achieved
by flagging values as already normalized by wrapping them in \code{thickness()}).
Set this to \code{TRUE} to allow geoms to also apply their own normalization.
Note that if you set renormalize to \code{TRUE}, subguides created via the
\code{subguide} parameter to \code{\link[=geom_slabinterval]{geom_slabinterval()}} will display the scaled values
output by this scale, not the original data values.}

\item{oob}{One of:
\itemize{
\item Function that handles limits outside of the scale limits
(out of bounds). Also accepts rlang \link[rlang:as_function]{lambda}
function notation.
\item The default (\code{\link[scales:oob]{scales::censor()}}) replaces out of
bounds values with \code{NA}.
\item \code{\link[scales:oob]{scales::squish()}} for squishing out of bounds values into range.
\item \code{\link[scales:oob]{scales::squish_infinite()}} for squishing infinite values into range.
}}

\item{guide}{A function used to create a guide or its name. See
\code{\link[ggplot2:guides]{guides()}} for more information.}

\item{expand}{<\link{numeric}> Vector of limit expansion constants of length
2 or 4, following the same format used by the \code{expand} argument of
\code{\link[ggplot2:continuous_scale]{continuous_scale()}}. The default is not to expand the limits.
You can use the convenience function \code{\link[ggplot2:expansion]{expansion()}} to generate the
expansion values; expanding the lower limit is usually not recommended
(because with most \code{thickness} scales the lower limit is the baseline
and represents \code{0}), so a typical usage might be something like
\code{expand = expansion(c(0, 0.05))} to expand the top end of the scale
by 5\%.}

\item{...}{
  Arguments passed on to \code{\link[ggplot2:continuous_scale]{ggplot2::continuous_scale}}
  \describe{
    \item{\code{aesthetics}}{The names of the aesthetics that this scale works with.}
    \item{\code{scale_name}}{\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#deprecated}{\figure{lifecycle-deprecated.svg}{options: alt='[Deprecated]'}}}{\strong{[Deprecated]}} The name of the scale
that should be used for error messages associated with this scale.}
    \item{\code{palette}}{A palette function that when called with a numeric vector with
values between 0 and 1 returns the corresponding output values
(e.g., \code{\link[scales:pal_area]{scales::pal_area()}}).}
    \item{\code{minor_breaks}}{One of:
\itemize{
\item \code{NULL} for no minor breaks
\item \code{waiver()} for the default breaks (one minor break between
each major break)
\item A numeric vector of positions
\item A function that given the limits returns a vector of minor breaks. Also
accepts rlang \link[rlang:as_function]{lambda} function notation. When
the function has two arguments, it will be given the limits and major
breaks.
}}
    \item{\code{n.breaks}}{An integer guiding the number of major breaks. The algorithm
may choose a slightly different number to ensure nice break labels. Will
only have an effect if \code{breaks = waiver()}. Use \code{NULL} to use the default
number of breaks given by the transformation.}
    \item{\code{rescaler}}{A function used to scale the input values to the
range [0, 1]. This is always \code{\link[scales:rescale]{scales::rescale()}}, except for
diverging and n colour gradients (i.e., \code{\link[ggplot2:scale_colour_gradient2]{scale_colour_gradient2()}},
\code{\link[ggplot2:scale_colour_gradientn]{scale_colour_gradientn()}}). The \code{rescaler} is ignored by position
scales, which always use \code{\link[scales:rescale]{scales::rescale()}}. Also accepts rlang
\link[rlang:as_function]{lambda} function notation.}
    \item{\code{na.value}}{Missing values will be replaced with this value.}
    \item{\code{transform}}{For continuous scales, the name of a transformation object
or the object itself. Built-in transformations include "asn", "atanh",
"boxcox", "date", "exp", "hms", "identity", "log", "log10", "log1p", "log2",
"logit", "modulus", "probability", "probit", "pseudo_log", "reciprocal",
"reverse", "sqrt" and "time".

A transformation object bundles together a transform, its inverse,
and methods for generating breaks and labels. Transformation objects
are defined in the scales package, and are called \verb{transform_<name>}. If
transformations require arguments, you can call them from the scales
package, e.g. \code{\link[scales:transform_boxcox]{scales::transform_boxcox(p = 2)}}.
You can create your own transformation with \code{\link[scales:new_transform]{scales::new_transform()}}.}
    \item{\code{trans}}{\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#deprecated}{\figure{lifecycle-deprecated.svg}{options: alt='[Deprecated]'}}}{\strong{[Deprecated]}} Deprecated in favour of
\code{transform}.}
    \item{\code{position}}{For position scales, The position of the axis.
\code{left} or \code{right} for y axes, \code{top} or \code{bottom} for x axes.}
    \item{\code{call}}{The \code{call} used to construct the scale for reporting messages.}
    \item{\code{super}}{The super class to use for the constructed scale}
  }}
}
\value{
A \link[ggplot2:ggplot2-ggproto]{ggplot2::Scale} representing a scale for the \code{thickness}
aesthetic for \code{ggdist} geoms. Can be added to a \code{\link[ggplot2:ggplot]{ggplot()}} object.
}
\description{
This \pkg{ggplot2} scale linearly scales all \code{thickness} values of geoms
that support the \code{thickness} aesthetic (such as \code{\link[=geom_slabinterval]{geom_slabinterval()}}). It
can be used to align the \code{thickness} scales across multiple geoms (by default,
\code{thickness} is normalized on a per-geom level instead of as a global scale).
For a comprehensive discussion and examples of slab scaling and normalization,
see the \href{https://mjskay.github.io/ggdist/articles/thickness.html}{\code{thickness} scale article}.
}
\details{
By default, normalization/scaling of slab thicknesses is controlled by geometries,
not by a \pkg{ggplot2} scale function. This allows various functionality not
otherwise possible, such as (1) allowing different geometries to have different
thickness scales and (2) allowing the user to control at what level of aggregation
(panels, groups, the entire plot, etc) thickness scaling is done via the \code{normalize}
parameter to \code{\link[=geom_slabinterval]{geom_slabinterval()}}.

However, this default approach has one drawback: two different geoms will always
have their own scaling of \code{thickness}. \code{\link[=scale_thickness_shared]{scale_thickness_shared()}} offers an
alternative approach: when added to a chart, all geoms will use the same
\code{thickness} scale, and geom-level normalization (via their \code{normalize} parameters)
is ignored. This is achieved by "marking" thickness values as already
normalized by wrapping them in the \code{\link[=thickness]{thickness()}} data type (this can be
disabled by setting \code{renormalize = TRUE}).

Note: while a slightly more typical name for \code{scale_thickness_shared()} might
be \code{scale_thickness_continuous()}, the latter name would cause this scale
to be applied to all \code{thickness} aesthetics by default according to the rules
\pkg{ggplot2} uses to find default scales. Thus, to retain the usual behavior
of \code{\link[=stat_slabinterval]{stat_slabinterval()}} (per-geom normalization of \code{thickness}), this scale
is called \code{scale_thickness_shared()}.
}
\examples{
library(distributional)
library(ggplot2)
library(dplyr)

prior_post = data.frame(
  prior = dist_normal(0, 1),
  posterior = dist_normal(0.1, 0.5)
)

# By default, separate geoms have their own thickness scales, which means
# distributions plotted using two separate geoms will not have their slab
# functions drawn on the same scale (thus here, the two distributions have
# different areas under their density curves):
prior_post \%>\%
  ggplot() +
  stat_halfeye(aes(xdist = posterior)) +
  stat_slab(aes(xdist = prior), fill = NA, color = "red")

# For this kind of prior/posterior chart, it makes more sense to have the
# densities on the same scale; thus, the areas under both would be the same.
# We can do that using scale_thickness_shared():
prior_post \%>\%
  ggplot() +
  stat_halfeye(aes(xdist = posterior)) +
  stat_slab(aes(xdist = prior), fill = NA, color = "#e41a1c") +
  scale_thickness_shared()

}
\seealso{
The \link{thickness} datatype.

The \code{thickness} aesthetic of \code{\link[=geom_slabinterval]{geom_slabinterval()}}.

\code{\link[=subscale_thickness]{subscale_thickness()}}, for setting a \code{thickness} sub-scale within
a single \code{\link[=geom_slabinterval]{geom_slabinterval()}}.

Other ggdist scales: 
\code{\link{scale_colour_ramp}},
\code{\link{scale_side_mirrored}()},
\code{\link{sub-geometry-scales}}
}
\author{
Matthew Kay
}
\concept{ggdist scales}
