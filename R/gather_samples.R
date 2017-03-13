# gather_samples (used to be "extract_samples" or "tidy_samples")
# 
# Author: mjskay
###############################################################################

# Names that should be suppressed from global variable check by codetools
# Names used broadly should be put in _global_variables.R
globalVariables(c(".."))


# DEPRECATED NAMES FOR gather_samples
#' @export
extract_samples = function(...) {
    .Deprecated("gather_samples")
    gather_samples(...)
}
#' @export
tidy_samples = function(...) {
    .Deprecated("gather_samples")
    gather_samples(...)
}


# get all variable names from an expression
# based on http://adv-r.had.co.nz/dsl.html
all_names <- function(x) {
    if (is.atomic(x)) {
        NULL
    } else if (is.name(x)) {
        name = as.character(x)
        if (name == "") {
            NULL
        }
        else {
            name
        }
    } else if (is.call(x) || is.pairlist(x)) {
        children <- lapply(x[-1], all_names)
        unique(unlist(children))
    } else {
        stop("Don't know how to handle type ", typeof(x), 
            call. = FALSE)
    }
}

#parse a variable spec in the form variable_name[index_name_1, index_name_2, ..] | wide_index
#into a list with three elements:
# 1. A vector of variable names
# 2. A vector of index names (or NULL if none)
# 3. The name of the wide index (or NULL if none)
#' @importFrom stats setNames
#' @importFrom purrr reduce map2
parse_variable_spec = function(variable_spec) {
    names = all_names(variable_spec$expr)
    #specs for each bare variable name in the spec expression
    names_spec = 
        lapply(names, function(name) list(name, NULL, NULL)) %>%
        setNames(names)
    
    spec_env = c(
        names_spec,
        list(
            c = function(...) {
                reduce(list(...), function(spec1, spec2) map2(spec1, spec2, base::c))
            },
            
            `[` = function(spec, ...) {
                index_names = as.character(substitute(list(...))[-1])

                list(
                    spec[[1]],
                    c(spec[[2]], index_names),
                    spec[[3]]
                )
            },
            
            `|` = function(spec, by) {
                wide_index_name = by[[1]]
                if (!is.null(spec[[3]])) {
                    stop("Left-hand side of `|` cannot contain `|`")
                }
                if (length(wide_index_name) != 1) {
                    stop("Right-hand side of `|` must be exactly one name")
                }
                
                list(
                    spec[[1]],
                    spec[[2]],
                    wide_index_name
                )
            }
        )
    )

    lazy_eval(variable_spec, spec_env)
}

#' Gather samples from a Bayesian sampler in a tidy data format
#' 
#' Extract samples from a Bayesian/MCMC sampler for a variable with the given named
#' indices into a long-format data frame.
#' 
#' Imagine a JAGS or Stan fit named \code{fit}. The model may contain a variable named 
#' \code{b[i,v]} (in the JAGS or Stan language) with \code{i} in \code{1:100} and \code{v} in \code{1:3}.
#' However, samples returned from JAGS or Stan in R will not reflect this indexing structure, instead
#' they will have multiple columns with names like \code{"b[1,1]"}, \code{"b[2,1]"}, etc.
#' 
#' \code{gather_samples} provides a straightforward syntax to translate these columns back into
#' properly-indexed variables in a tidy (long-format) data frame, optionally recovering
#' index types (e.g. factor levels) as it does so.
#' 
#' \code{gather_samples} returns data frames already grouped by all indices used on the variables you specify.
#' 
#' For example, \code{gather_samples(fit, b[i,v])} would return a grouped
#' data frame (grouped by \code{i} and \code{v}), with:
#' \itemize{
#'      \item column \code{".chain"}: the chain number 
#'      \item column \code{".iteration"}: the interation number
#'      \item column \code{"i"}: value in \code{1:20}
#'      \item column \code{"v"}: value in \code{1:3}
#'      \item column \code{"b"}: value of \code{"b[i,v]"} for iteration number
#'          \code{".iteration"} on chain number \code{".chain"}
#'  }
#' 
#' \code{gather_samples} can use type information applied to the \code{fit}
#' object by \code{\link{recover_types}} to convert columns back into their
#' original types. This is particularly helpful if some of the indices in
#' your model were originally factors. For example, if the \code{v} index
#' in the original data frame \code{data} was a factor with levels \code{c("a","b","c")},
#' then we could use \code{recover_types} before \code{gather_samples}:
#' 
#' \preformatted{fit \%>\%
#'     recover_types(data) %\>\%
#'     gather_samples(fit, b[i,v])
#'  }
#'  
#' Which would return the same data frame as above, except the \code{"v"} column
#' would be a value in \code{c("a","b","c")} instead of \code{1:3}.
#' 
#' For variables that do not share the same subscripts (or share
#' some but not all subscripts), we can supply their specifications separately. 
#' For example, if we have a variable d[i] with the same i subscript 
#' as b[i,v], and a variable x with no subscripts, we could do this:
#' 
#' \preformatted{gather_samples(fit, x, d[i], b[i,v])}
#' 
#' Which is roughly equivalent to this:
#'
#' \preformatted{gather_samples(fit, x) \%>\%
#'     inner_join(gather_samples(fit, d[i])) \%>\%
#'     inner_join(gather_samples(fit, b[i,v])) \%>\%
#'     group_by(i,v)
#' }
#' 
#' The \code{c} function can be used to combine multiple variable names that have 
#' the same indices. For example, if we have several variables with the same
#' subscripts \code{i} and \code{v}, we could do this:
#' 
#' \preformatted{gather_samples(fit, c(w, x, y, z)[i,v])}
#' 
#' Which is roughly equivalent to this:
#' 
#' \preformatted{gather_samples(fit, w[i,v], x[i,v], y[i,v], z[i,v])}
#' 
#' Besides being more compact, the \code{c}-style syntax is currently also
#' faster (though that may change).
#' 
#' The shorthand \code{..} can be used to specify one column that should be put
#' into a wide format and whose names will be the base variable name plus the
#' value of the index at \code{..}. For example:
#' 
#' \code{gather_samples(fit, b[i,..])} would return a grouped data frame
#' (grouped by \code{i}), with:
#' \itemize{
#'      \item column \code{".chain"}: the chain number 
#'      \item column \code{".iteration"}: the interation number
#'      \item column \code{"i"}: value in \code{1:20}
#'      \item column \code{"b1"}: value of \code{"b[i,1]"} for iteration number
#'          \code{".iteration"} on chain number \code{".chain"}
#'      \item column \code{"b2"}: value of \code{"b[i,2]"} for iteration number
#'          \code{".iteration"} on chain number \code{".chain"}
#'      \item column \code{"b3"}: value of \code{"b[i,3]"} for iteration number
#'          \code{".iteration"} on chain number \code{".chain"}
#'  }
#' 
#' An optional clause in the form \code{| wide_index} can also be used to put
#' the data frame into a wide format based on \code{wide_index}. For example, this:
#' 
#' \preformatted{gather_samples(fit, b[i,v] | v)}
#' 
#' is roughly equivalent to this:
#' 
#' \preformatted{gather_samples(fit, b[i,v]) \%>\% spread(v,b)}
#' 
#' The main difference between using the \code{|} syntax instead of the
#' \code{..} syntax is that the \code{|} syntax respects prototypes applied to
#' indices with \code{\link{recover_types}}, and thus can be used to get
#' columns with nicer names. For example:
#' 
#' \code{fit \%>\% recover_types(data) \%>\% gather_samples(fit, b[i,v] | v)} would return a grouped data frame
#' (grouped by \code{i}), with:
#' \itemize{
#'      \item column \code{".chain"}: the chain number 
#'      \item column \code{".iteration"}: the interation number
#'      \item column \code{"i"}: value in \code{1:20}
#'      \item column \code{"a"}: value of \code{"b[i,1]"} for iteration number
#'          \code{".iteration"} on chain number \code{".chain"}
#'      \item column \code{"b"}: value of \code{"b[i,2]"} for iteration number
#'          \code{".iteration"} on chain number \code{".chain"}
#'      \item column \code{"c"}: value of \code{"b[i,3]"} for iteration number
#'          \code{".iteration"} on chain number \code{".chain"}
#'  }
#'
#' @param model A supported Bayesian model fit / MCMC object. Currently
#' supported models include \code{\link[coda]{mcmc}}.
#' @param ... Expressions in the form of
#' \code{variable_name[index_1, index_2, ...] | wide_index}. See `Details`.
#' @return A data frame.
#' @author Matthew Kay
#' @seealso \code{\link{recover_types}}, \code{\link{compose_data}}.
#' @keywords manip
#' @examples
#' 
#' ##TODO
#' 
#' @aliases extract_samples tidy_samples
#' @importFrom lazyeval lazy_dots
#' @importFrom purrr reduce
#' @importFrom dplyr inner_join
#' @export
gather_samples = function(model, ...) {
    tidysamples = lapply(lazy_dots(...), function(variable_spec) {
        gather_samples_(model, variable_spec) 
    })
    
    #get the groups from all the samples --- when we join them together,
    #the grouping information is lost (actually, only the groups on the
    #first data frame in a join is retained), so we'll have to recreate
    #the full set of groups from all the data frames after we join them
    groups_ = tidysamples %>%
        map(groups) %>%
        reduce(union)
    
    tidysamples %>%
        reduce(function(tidysamples1, tidysamples2) {
            by_ = intersect(names(tidysamples1), names(tidysamples2))
            inner_join(tidysamples1, tidysamples2, by = by_)
        }) %>%
        group_by_(.dots = groups_)
}
#' @import dplyr
#' @importFrom tidyr spread_
#' @importFrom lazyeval lazy_eval
#' @importFrom tibble has_name
gather_samples_ = function(model, variable_spec) {
    #parse a variable spec in the form variable_name[index_name_1, index_name_2, ..] | wide_index
    spec = parse_variable_spec(variable_spec)
    variable_names = spec[[1]]
    index_names = spec[[2]]
    wide_index_name = spec[[3]]
    
    #extract the samples into a long data frame
    samples = gather_samples_long_(model, variable_names, index_names)
    
    #convert variable and/or indices back into usable data types
    constructors = attr(model, "constructors")
    if (is.null(constructors)) constructors = list()
    for (column_name in c(variable_names, index_names)) {
        if (column_name %in% names(constructors)) {
            #we have a data type constructor for this index, convert it
            samples[[column_name]] = constructors[[column_name]](samples[[column_name]])
        }
    }
    
    #spread a column into wide format if requested (only if one variable, because
    #we can't spread multiple keys simultaneously for the same value)
    if (!is.null(wide_index_name)) {
        #wide index requested by name
        if (length(variable_names) != 1) {
            stop("Cannot extract samples of multiple variables in wide format.")
        }
        samples %>%
            spread_(wide_index_name, variable_names)
    }
    else if (has_name(samples, "..")) {
        #a column named ".." is present, use it to form a wide version of the data
        #with numbered names based on the variable name
        if (length(variable_names) != 1) {
            stop("Cannot extract samples of multiple variables in wide format.")
        }
        samples %>%
            #the ".." column will have been set as a grouping column because it was
            #specified as an index; therefore before we can modify it we have to
            #remove it from the grouping columns on this table (mutate does not
            #allow you to modify grouping columns)
            group_by_(.dots = groups(.) %>% setdiff("..")) %>%
            mutate(.. = factor(.., labels=variable_names)) %>%
            spread_("..", variable_names)
    }
    else {
        #no wide column => just return long version
        samples
    }
}


## Extract a sample from an MCMC chain for a variable with the given named indices into a long-format data frame.
## For example, imagine a variable b[i,v] with i in [1..100] and v in [1..3]. An MCMC sample returned from JAGS 
## (for example) would have columns with names like "b[1,1]", "b[2,1]", etc. 
##
## gather_samples_long_(mcmcChain, "b", c("i", "v")) would return a data frame with:
##		column ".sample": values in [1..nrow(mcmcChain)]
## 		column "i":		  values in [1..100]
##		column "v":		  values in [1..3]
##      column "b":       sample number ".sample" of "b[i,v]" in mcmcChain 
##
#' @importFrom tidyr spread_ separate_ gather_
#' @import stringi
#' @import dplyr
gather_samples_long_ = function(model, variable_names, index_names) {
    samples = as_sample_tibble(model)
    if (is.null(index_names)) {
        #no indices, just return the samples with a sample index added
        samples[,c(".chain", ".iteration", variable_names)]
    }
    else {
        #determine what variables to extract: find variable names with trailing square brackets
        variable_regex = paste0("^(", paste(variable_names, collapse="|"), ")\\[")
        variable_names_index = stri_detect_regex(colnames(samples), variable_regex)
        variable_names = colnames(samples)[variable_names_index]

        #rename columns to drop trailing "]" to eliminate extraneous last column
        #when we do separate(), below. e.g. "x[1,2]" becomes "x[1,2". Do the same
        #with variable_names so we can select the columns
        colnames(samples)[c(-1,-2)] = stri_sub(colnames(samples)[c(-1,-2)], to=-2)
        variable_names = stri_sub(variable_names, to=-2)
        
        #specs containing empty indices (e.g. mu[] or mu[,k]) will produce
        #some index_names == ""; we can't use empty variable names below, so we
        #replace them with the ".drop" placeholder and then drop those columns later.
        #TODO: probably a better way to do this.
        temp_index_names = index_names %>% 
            #must give each blank index column a unique name, otherwise spread_() won't work below
            ifelse(. == "", paste0(".drop", seq_along(.)), .)
        index_names = index_names[index_names != ""]

        samples[,c(".chain", ".iteration", variable_names)] %>%
            #make long format for the variables we want to split
            gather_(".variable", ".value", variable_names) %>%
            #next, split indices in variable names into columns
            separate_(".variable", c(".variable", temp_index_names), sep="[ :,]|\\[|\\]",
                convert=TRUE #converts indices to numerics
            ) %>%
            #now, make the value of each variable a column
            spread_(".variable", ".value") %>%
            #drop the columns that correpond to blank indices in the original spec
            select(-starts_with(".drop")) %>%
            #group by the desired indices so that we return a pre-grouped data frame to the user
            group_by_(.dots = index_names)
    }
}
