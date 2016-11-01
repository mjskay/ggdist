# flatworm
# 
# Author: mjskay
###############################################################################

# Names that should be suppressed from global variable check by codetools
# Names used broadly should be put in _global_variables.R
globalVariables(c("expected_p","expected_z",".cuts",".worm_y","worm"))


#' Flatworm plot for normalized quantile residuals
#' 
#' Generates a flattened version of a detrended normal Q-Q plot (a worm plot)
#' for regression diagnostics.
#' 
#' @param object The object to generate diagnostics from, such as a model (for
#' \code{flatworm.lm}, \code{flatworm.map}, ...) or a data frame of residuals
#' (for \code{flatworm.data.frame}).
#' @param cols Bare (unquoted) name of a column to facet over for generating 
#' multiple flatworm plots (i.e. small multiples). If cols is numeric, it
#' will be split into quartiles; if is it a factor, into levels.
#' @param y \code{map2stan} and \code{map} objects may contain multiple dependent
#' variables; use \code{y} (as an unquoted expression) to specify the desired
#' response to calculate residuals from (or \code{NULL} to use the first
#' link function in the model).
#' @param residual_z Bare name of the normalized residuals in \code{object}.
#' @param ylim Limits of the y axis to plot. If a numeric vector of length 2, specifies
#' min and max. If numeric vector of length 1, specifies abs(min) and max. If
#' \code{NA} or \code{NULL}, the y limits are determined from the data.
#' @param points Show residuals as points
#' @param lines Show residuals as lines
#' @param loess Show a \code{\link{loess}} fit to detrended residuals.
#' @param cubic Show a cubic fit to detrended residuals.
#' @param z_cubic Show a cubic fit to detrended residuals in z-space, translated onto the
#' SE scale used by flatworm. This is the same fit used by \code{\link[gamlss]{wp}}
#' displayed on the transformed y scale used by \code{flatworm}.
#' @param ... Additional arguments passed onto \code{flatworm.data.frame} by
#' model-specific versions of \code{flatworm}.
#' 
#' Flatworm implements a variant of a worm plot (See e.g. \code{\link[gamlss]{wp}}). 
#' Unlike the traditional worm plot, its x-axis is scaled in units of standard
#' errors. Thus the arcs that define ~ +-2 SE in a worm plot (the region most
#' residuals should be within) instead define a band, making it a more straightforward
#' task to judge whether the residuals are contained within +- 2 SE.
#' 
#' Model-specific versions of \code{flatworm} simply extract data and normalized
#' residuals from a model and then pass them to \code{flatworm.data.frame}.
#' 
#' @seealso \code{\link[gamlss]{wp}}
#' 
#' @import dplyr
#' @import ggplot2
#' @importFrom lsmeans recover.data
#' @importFrom magrittr %<>%
#' @importFrom stats dnorm pnorm lm predict quantile rstandard
#' @importFrom modelr seq_range
#' @export
flatworm = function(object, ...) UseMethod("flatworm", object)
#' @rdname flatworm
#' @export
flatworm.lm = function(object, cols = NULL, ...) {
    .cols = substitute(cols)

    #get data and standardized residuals
    data = recover.data(object)
    # r = residuals(object) %>% {./sd(.)}
    r = rstandard(object)
    if (is.character(data) && length(data) == 1) {
        #recover data will fail if there are no predictors and return a character
        data = data.frame(
            residual_z = r
        )
    }
    else {
        data = cbind(
            recover.data(object),
            residual_z = r
        )
    }

    eval(bquote(flatworm(data, residual_z, cols = .(.cols), ...)))
}
#' @rdname flatworm
#' @export
flatworm.map = function(object, cols = NULL, y = NULL, ...) {
    .cols = substitute(cols)
    .y = substitute(y)
    
    if (is.null(.y)) {
        .y = first_y_expr(object)
    }
    
    eval(bquote(
        tidy_sim(, object, .predicted) %>%
            summarise(
                residual_z = qnorm(mean(.predicted < .(.y)))
            ) %>%
            ungroup() %>%
            flatworm(residual_z, cols = .(.cols), ...)
    ))
}
#' @rdname flatworm
#' @export
flatworm.map2stan = flatworm.map
#' @rdname flatworm
#' @export
flatworm.data.frame = function(object, residual_z, cols = NULL, 
    ylim = 6, points = TRUE, lines = FALSE,
    loess = TRUE, cubic = FALSE, z_cubic = FALSE,
    ...
) {
    data = object
    .residual_z = substitute(residual_z)
    .cols = substitute(cols)
    ylim = if (is.numeric(ylim) && length(ylim) == 1) c(-ylim, ylim) else ylim
    
    add_cols = !is.null(.cols)
    if (add_cols) {
        #when cols is supplied, create a plot with worms split by `cols`. To do that
        #we'll need a factor in the data frame representing that split. This could be
        #an existing variable (if x is a factor or a logical) or could be cuts based on
        #quantiles of cols (if it is numeric)
        eval(bquote({
            cols_value = data %$% .(.cols)
            if (is.factor(cols_value) || is.logical(cols_value) || length(unique(cols_value)) < 4) {
                data %<>% mutate(.cuts = .(.cols))
            }
            else {
                data %<>% mutate(.cuts = cut(.(.cols), quantile(.(.cols)), include.lowest=TRUE))
            }
        }))
    }
    else {
        data %<>% mutate(.cuts = "")
    }
    
    eval(bquote({
        data %<>%
            arrange(.(.residual_z)) %>%
            group_by(.cuts) %>%
            mutate(
                expected_p = ppoints(n()),
                expected_z = qnorm(expected_p),
                se = (1/dnorm(expected_z)) * (sqrt(expected_p * (1 - expected_p)/n()))
            )
        p = ggplot(data, aes(x = expected_z, y = (.(.residual_z) - expected_z)/se) )
    }))
    
    if (points) {
        p = p + geom_point()
    }
    if (lines) {
        p = p + geom_line()
    }
    if (loess) {
        p = p + stat_smooth(se=FALSE, method="loess", color="red", span=0.25)
    }
    if (cubic) {
        p = p + stat_smooth(method=lm, se=FALSE, formula = y ~ poly(x, 3), color="red")
    }
    if (z_cubic) {
        #TODO: de-uglify this. Can't just do poly regression on original scale because 
        #the shape of the fit curve near the se boundaries can be quite different
        eval(bquote(data %<>% mutate(
            .worm_y = .(.residual_z) - expected_z
        )))
        m.worm = if (add_cols) {
            lm(.worm_y ~ poly(expected_z, 3)*.cuts, data = filter(data, is.finite(.worm_y)))
        }
        else {
            lm(.worm_y ~ poly(expected_z, 3), data = filter(data, is.finite(.worm_y)))
        }
        predictions = 
            expand.grid(
                expected_z = seq_range(data$expected_z, n = 100),
                .cuts = unique(data$.cuts)
            ) %>%
            group_by(.cuts) %>%
            mutate(
                expected_p = pnorm(expected_z),
                se = (1/dnorm(expected_z)) * (sqrt(expected_p * (1 - expected_p)/sum(data$.cuts == .cuts[[1]])))
            ) %>%
            ungroup %>%
            do(cbind(., worm = predict(m.worm, .)/.$se))
        #p = p + stat_smooth(method = lm, formula = y ~ poly(x, 3), se=FALSE, color="red", size=1.5)
        p = p + geom_line(aes(y=worm), data=predictions, color="red", size=1.5)
    }
    if (add_cols) {
        p = p + facet_grid(. ~ .cuts)
    }
    if (is.numeric(ylim)) {
        p = p + coord_cartesian(ylim = ylim)
    }
    
    p = p +
        geom_vline(xintercept = 0, linetype="dashed") +
        geom_hline(yintercept = 0, linetype="dashed") +
        geom_hline(yintercept = c(-2,2))
    
    p
}
