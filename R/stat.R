# Helper methods for creating stats
#
# Author: mjskay
###############################################################################

# Summarise a data frame using the given function within the specified
# groups, but keep any columns in the groups that have only one value in them
# (i.e. columns where every value in the column is the same)
summarise_by = function(data, by = "group", fun) {
  plyr::ddply(data, by, function(d) {
    new_d = fun(d)
    missing_names = setdiff(names(d), names(new_d))

    # add back in columns with only one value in them
    for (col in missing_names) {
      if (length(unique(d[[col]])) == 1) {
        if (is.list(d[[col]])) {
          # list columns must be wrapped
          new_d[[col]] = list(d[[col]][[1]])
        } else{
          new_d[[col]] = d[[col]][[1]]
        }
      }
    }

    new_d
  })
}
