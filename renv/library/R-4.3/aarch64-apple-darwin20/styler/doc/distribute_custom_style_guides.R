## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
styler::cache_deactivate()
options(styler.colored_print.vertical = FALSE)

## -----------------------------------------------------------------------------
styler::specify_transformers_drop(
  spaces = list(style_space_around_tilde = "'~'"),
  tokens = list(resolve_semicolon = "';'")
)

