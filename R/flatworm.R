# flatworm
# 
# Author: mjskay
###############################################################################

flatworm = function(object, ...) UseMethod("flatworm", object)
flatworm.lm = function(object, x = NULL, ...) {
    .x = substitute(x)
    
    r = residuals(object)
    data = cbind(
        recover.data(object), 
        residual_z = r / sd(r)
    )
    
    eval(bquote(flatworm(data, residual_z, x = .(.x), ...)))
}
flatworm.map = function(object, x = NULL, y = NULL, ...) {
    .x = substitute(x)
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
            flatworm(residual_z, x = .(.x), ...)
    ))
}
flatworm.map2stan = flatworm.map
flatworm.data.frame = function(object, residual_z, x = NULL, 
    ylim = 6, points = TRUE, cubic = TRUE, loess = FALSE
) {
    data = object
    .residual_z = substitute(residual_z)
    .x = substitute(x)
    ylim = if (is.numeric(ylim) && length(ylim) == 1) c(-ylim, ylim) else ylim
    
    add_x = !is.null(.x)
    if (add_x) {
        #when x is supplied, create a plot with worms split by x. To do that
        #we'll need a factor in the data frame representing that split. This could be
        #an existing variable (if x is a factor or a logical) or could be cuts based on
        #quantiles of x (if it is numeric)
        eval(bquote({
            x_value = data %$% .(.x)
            if (is.factor(x_value) || is.logical(x_value) || length(unique(x_value)) < 4) {
                data %<>% mutate(.cuts = .(.x))
            }
            else {
                data %<>% mutate(.cuts = cut(.(.x), quantile(.(.x)), include.lowest=TRUE))
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
        p = ggplot(data, aes(x = expected_z, y = (.(.residual_z) - expected_z)/se))
    }))
    
    if (points) {
        p = p + geom_point()
    }
    if (loess) {
        p = p + stat_smooth(linetype="dashed", se=FALSE, color="red")
    }
    if (cubic) {
        #TODO: de-uglify this. Can't just do poly regression on original scale because 
        #the shape of the fit curve near the se boundaries can be quite different
        eval(bquote(data %<>% mutate(
            .worm_y = .(.residual_z) - expected_z
        )))
        m.worm = if (add_x) {
            lm(.worm_y ~ poly(expected_z, 3)*.cuts, data = filter(data, is.finite(.worm_y)))
        }
        else {
            lm(.worm_y ~ poly(expected_z, 3), data = filter(data, is.finite(.worm_y)))
        }
        predictions = 
            expand.grid(
                expected_z = seq_range(data$expected_z, length.out = 100),
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
    if (add_x) {
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
