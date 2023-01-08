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
    exclude_aes = "datatype"
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


# aesthetics -----------------------------------------------------

#' Provides documentation aesthetics sections of a stat/geom.
#' @param geom_name lowercase base name of geom
#' @param stat `ggplot2::Stat` object
#' @param vignette vignette to refer readers to for more information on this family
#' @noRd
rd_aesthetics_sections = function(
  geom_name = "slabinterval", stat = NULL, vignette = geom_name
) {
  geom = get(paste0("Geom", title_case(geom_name)))

  hidden_aes = union(geom$hidden_aes, stat$hidden_aes)

  filter_aes = function(aes_list, include, exclude = hidden_aes) {
    names_to_keep = setdiff(intersect(names(aes_list), include), exclude)
    aes_list[names_to_keep]
  }

  # stat aesthetics
  stat_aes = (stat$get_aes_docs %||% list)()
  pos_aes = list(
    x = 'x position of the geometry',
    y = 'y position of the geometry'
  )
  if (!is.null(stat)) {
    out = glue_doc('
      These `stat`s support the following aesthetics:

      <<rd_named_list(filter_aes(stat_aes, stat$aesthetics()))>>

      In addition, in their default configuration (paired with [geom_<<geom_name>>()])
      the following aesthetics are supported by the underlying geom:

      ')
  } else {
    # for geoms we only need positional aesthetics
    out = glue_doc('
      **Positional aesthetics**

      <<rd_named_list(filter_aes(pos_aes, geom$aesthetics()))>>

      ')
  }

  # geom aesthetics
  geom_aes_sections = (geom$get_aes_docs %||% list)()
  for (i in seq_along(geom_aes_sections)) {
    section = names(geom_aes_sections)[[i]]
    geom_aes = filter_aes(geom_aes_sections[[i]], geom$aesthetics())
    if (length(geom_aes) == 0) next;
    out = c(out, glue_doc('
      **<<section>>**

      <<rd_named_list(geom_aes)>>

      '))
  }

  # undocumented aesthetics
  documented_aes = c(unlist(lapply(geom_aes_sections, names)), names(pos_aes))
  undocumented_aes = setdiff(geom$aesthetics(), c(documented_aes, hidden_aes))
  if (length(undocumented_aes) > 0) {
    out = c(out, glue_doc('
      **Other aesthetics** (these work as in standard `geom`s)

      <<rd_bulleted_list(glue::backtick(undocumented_aes))>>

      '))
  }

  out = c(out, glue_doc('
    See examples of some of these aesthetics in action in `vignette("<<vignette>>")`.
    Learn more about the sub-geom override aesthetics (like `interval_color`) in the
    \\link[ggdist]{scales} documentation. Learn more about basic ggplot aesthetics in
    `vignette(\"ggplot2-specs\")`.
    '))

  glue_collapse(out, "\n")
}


# params ------------------------------------------------------------------

#' Provides documentation of params for layers containing AbstractGeoms
#' @noRd
rd_layer_params = function(geom_name = "slabinterval", stat = NULL, as_dots = FALSE) {
  geom = get(paste0("Geom", title_case(geom_name)))

  params = geom$get_param_docs()

  # filter out hidden params or ones defined in the stat
  param_names = setdiff(
    names(geom$default_params),
    c(names(stat$default_params), geom$hidden_params, stat$hidden_params)
  )
  params = params[param_names]

  missing_docs = sapply(params, is.null)
  if (any(missing_docs)) {
    stop("Missing docs for params: ", paste0(param_names[missing_docs], collapse = ", "))
  }

  if (length(params)) {
    if (as_dots) {
      glue_doc('
        @param ...  Other arguments passed to [layer()]. These are often aesthetics, used to set an aesthetic
          to a fixed value, like `colour = "red"` or `linewidth = 3` (see **Aesthetics**, below). They may also be
          parameters to the paired geom/stat. When paired with the default geom, [geom_<<geom_name>>()],
          these include:
          <<rd_describe_list(params)>>
        ')
    } else {
      glue_doc('@param <<names(params)>> <<params>>')
    }
  }
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


# helpers -----------------------------------------------------------------

#' @importFrom glue glue glue_collapse
glue_doc = function(...) {
  glue(..., .null = NULL, .open = "<<", .close = ">>", .envir = parent.frame())
}

title_case = function(x) {
  substring(x, 1, 1) = toupper(substring(x, 1, 1))
  x
}

snake_case = function(x) {
  tolower(gsub("([a-z])([A-Z])", "\\1_\\2", x))
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
