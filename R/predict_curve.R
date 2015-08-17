# fit_curve and for generate fit curves and densities from posteriors
# 
# Author: mjskay
###############################################################################

predict_curve = function(data, formula, summary = median, ...) {
    #get response name and formula to generate response
    response_name = formula[[2]]
    prediction_formula = formula[[3]]

    #get the predictors we will vary over the curve
    varied_predictors = expand.grid(..., KEEP.OUT.ATTRS=FALSE)
    if (nrow(varied_predictors) == 0) {
        #no predictors provided => use a data frame with one row
        #and no columns so that the predictors are evaluated once
        #per group in data
        varied_predictors = data.frame(row.names=1)
    }
    
    eval(bquote(
        do(data, {
            #for every group defined in data ...
            ldply(1:nrow(varied_predictors), function(r) { 
                #and for every prediction point on the curve
                #defined by the values in (...)
                predictor_row = as.list(varied_predictors[r,,drop=FALSE])
                # N.B. we convert predictor_row to a list first (then the final result back 
                # to a data.frame) because if summary returns more than one row 
                # (as in density prediction, for example) we can't just do the calculation 
                # within a data.frame: predictor_row is always exactly one row, so there 
                # will be a row count mismatch if summary returns > 1 row.
                # By doing the calculation within a list, the single predictor_row
                # will automatically be repeated to match the number of rows returned
                # when we convert back to a data.frame afterwards.
                within(predictor_row, 
                    #get the response value summarized by the summary function
                    .(response_name) <- summary(with(., .(prediction_formula)))
                ) %>% as.data.frame(optional = TRUE)
            })
        })
    ))
}

predict_curve_density = function(data, formula, summary = function(...) density_bins(..., n = n), n = 50, ...) {
    predict_curve(data, formula, summary = summary, ...)
}

histogram_bins = function(x, ...) {
    h = hist(x, plot=FALSE)

    data.frame(
        mid = rowMeans(embed(h$breaks, 2)),
        lower = h$breaks[-length(h$breaks)],
        upper = h$breaks[-1],
        density = h$density
    )
}

density_bins = function(x, ...) {
    d = density(x, ...)
    
    mid = d$x
    last_mid = length(mid)
    x_diffs = mid[-1] - mid[-last_mid]
    
    data.frame(
        mid = mid,
        lower = c(mid[[1]] - x_diffs[[1]]/2, mid[-1] - x_diffs/2),
        upper = c(mid[-last_mid] + x_diffs/2, mid[[last_mid]] + x_diffs[[last_mid - 1]]/2),
        density = d$y
    )
}


#df_density = df %>%
#    group_by(g) %>%
#    predict_curve_density(y ~ x)
#    predict_curve(y ~ x, summary=density_bins)
#
#df_curve = df %>%
#    group_by(g) %>%
#    predict_curve(y ~ x)
#
#
#ggplot(df_density) + 
#    geom_rect(aes(xmin=y.lower, xmax=y.upper, ymin=as.numeric(g)-0.5, ymax=as.numeric(g)+0.5, fill=y.density), color=NA) +
#    geom_line(aes(x=y, y=as.numeric(g)), df_curve, color="blue", size=2)
#    scale_alpha_continuous
#
#ggplot(data.frame(x1=d$x - d$bw/2, x2=d$x + d$bw/2, d = d$y), aes(xmin=x1, xmax=x2, ymin=-1, ymax=1, fill=d)) + geom_rect()
#
#
#
#
#min_x = min_x
#binmin = floor(min_x)        weight_breaks = seq(floor(min(.$weight)), ceiling(max(.$weight)), by=weight_bin_size)
#
#df = data.frame(
#    x=c(rnorm(1000), rnorm(1000,1)), 
#    g=c(rep("a",1000), rep("b", 1000))
#)
#
#x = rnorm(1000)
#
#
