as.constructor = function(x) UseMethod("as.constructor")

as.constructor.default = function(x) identity

as.constructor.factor = function(x) {
    x_levels = levels(x)
    x_is_ordered = is.ordered(x)
    function(x) factor(x, labels=x_levels, ordered=x_is_ordered)
}

as.constructor.logical = function(x) as.logical

apply_prototypes = function(model, ...) { 
    if (!is.list(attr(model, "constructors"))) {
        attr(model, "constructors") = list()
    }
    for (prototypes in list(...)) {
        #we iterate this way instead of building a list directly 
        #so that existing names are overwritten
        for (variable_name in names(prototypes)) {
            attr(model, "constructors")[[variable_name]] = as.constructor(prototypes[[variable_name]])
        }
    }
    model
}
