# Documentation methods
#
# Author: mjskay
###############################################################################



# stat/geom shortcuts -----------------------------------------------------

rd_shortcut_geom = function(geom_name, from_name = "slabinterval") {
  from = get(paste0("Geom", title_case(from_name)))
  geom = get(paste0("Geom", title_case(geom_name)))

  changed = changed_geom_values(from = from, to = geom)

  geom_args = glue_collapse(c(
    if (length(changed$aes)) glue_doc('aes(<<changed$aes>>)'),
    if (length(changed$params)) changed$params,
    if (length(changed$args)) changed$args
  ), ",\n  ")

  glue_doc('
    @description
    ```
    geom_<<from_name>>(
      <<geom_args>>
    )
    ```
    '
  )
}

rd_shortcut_stat = function(stat_name, geom_name = stat_name, from_name = "slabinterval") {
  from = get(paste0("Stat", title_case(from_name)))
  stat = get(paste0("Stat", title_case(stat_name)))
  geom = get(paste0("Geom", title_case(geom_name)))

  changed = changed_geom_values(
    from = from, to = stat,
    exclude_aes = "datatype",
    exclude_params = c("show_point", "show_interval")
  )

  stat_args = glue_collapse(c(
    if (length(changed$aes)) glue_doc('aes(<<changed$aes>>)'),
    if (geom_name != from_name) glue_doc('geom = "<<geom_name>>"'),
    if (length(changed$params)) changed$params,
    if (length(changed$args)) changed$args
  ), ",\n  ")

  glue_doc('
    @description
    ```
    stat_<<from_name>>(
      <<stat_args>>
    )
    ```
    '
  )
}


# lists -------------------------------------------------------------------

#' Given a named list, format it as a `\describe` documentation block
#' @noRd
rd_describe_list = function(x) {
  list_items = glue_collapse(glue_doc('\\item{`<<names(x)>>`}{<<x>>}'), "\n")
  glue_doc('
    \\describe{
      <<list_items>>
    }')
}

#' Given an unnamed list, format it as a bulleted list (`\itemize` documentation block)
#' @noRd
rd_bulleted_list = function(x) {
  list_items = glue_collapse(glue_doc("\\item{<<x>>}"), "\n")
  glue_doc('
    \\itemize{
      <<list_items>>
    }')
}

#' Given a named list, format it as a bulleted list (`\itemize` documentation block)
#' @noRd
rd_named_list = function(x) {
  list_items = glue_collapse(glue_doc("\\item{`<<names(x)>>`: <<x>>}"), "\n")
  glue_doc('
    \\itemize{
      <<list_items>>
    }')
}

#' Given a named list of aesthetic / aesthetic doc pairs, output a list of them
#' for use in docs. Used by rd_slabinterval_aesthetics
#' @noRd
rd_aesthetics = function(aes_docs, include_only) {
  aes_docs = aes_docs[intersect(names(aes_docs), include_only)]
  rd_named_list(aes_docs)
}


# helpers -----------------------------------------------------------------

#' @importFrom glue glue glue_collapse
glue_doc = function(...) {
  glue(..., .null = NULL, .open = "<<", .close = ">>", .envir = parent.frame())
}

title_case = function(x) {
  substring(x, 1, 1) = toupper(substring(x, 1, 1))
  x
}

#' get the values of aesthetics, parameters, and layer arguments
#' that have changed from one stat/geom (`from`) to another (`to`)
#' @param from a Stat or Geom
#' @param to a Stat or Geom that is a child of `from`
#' @param exclude_aes,exclude_params,exclude_args names of elements to
#' ignore when looking for changes
#' @return a list with `"aes"`, `"params"`, and `"args"`, each of which is
#' a string with comma-separated `name = value` pairs giving the changed
#' values of aesthetics, parameters, and layer arguments
#' @noRd
changed_geom_values = function(
  from, to,
  exclude_aes = character(),
  exclude_params = character(),
  exclude_args = character()
) {
  # find the changed aesthetics and params for this stat
  changed_values = function(list, exclude) {
    # find values changes in the child stat
    values = to[[list]][
      map_lgl_(names(to[[list]]), function(name)
        !identical(to[[list]][[name]], from[[list]][[name]])
      )
    ]
    # find deleted values
    deleted_names = setdiff(names(from[[list]]), names(to[[list]]))
    deleted_values = rep(list(NULL), length(deleted_names))
    names(deleted_values) = deleted_names
    values = c(values, deleted_values)

    # turn values into strings like x = "foo", y = "bar"
    value_text = lapply(values, function(x) deparse0(get_expr(x)))
    value_text = value_text[!names(value_text) %in% exclude]
    if (length(value_text)) paste(names(value_text), "=", value_text, collapse = ", ")
  }

  list(
    aes = changed_values("default_aes", exclude = exclude_aes),
    params = changed_values("default_params", exclude = exclude_params),
    args = changed_values("layer_args", exclude = exclude_args)
  )
}
