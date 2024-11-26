## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
styler::cache_deactivate()

## ----echo = FALSE, include = FALSE--------------------------------------------
options(styler.colored_print.vertical = FALSE)

## ----comment = ""-------------------------------------------------------------
library(styler)
style_text("string = 'hi there'")

## -----------------------------------------------------------------------------
transformers <- tidyverse_style()
names(transformers)

## -----------------------------------------------------------------------------
library(magrittr)
levels <- c("space", "line_break", "indention", "token")
purrr::map(
  levels,
  ~ names(transformers[[.x]])
) %>%
  purrr::set_names(levels)

## -----------------------------------------------------------------------------
styler:::force_assignment_op

## -----------------------------------------------------------------------------
transformers$token$force_assignment_op <- NULL

## -----------------------------------------------------------------------------
style_text("string = 'hi there'", transformers = transformers)

## -----------------------------------------------------------------------------
eq_assign_style <- function(...) {
  transformers <- tidyverse_style(...)
  transformers$token$force_assignment_op <- NULL
  transformers
}

style_text("string = 'hi there'", style = eq_assign_style)

## -----------------------------------------------------------------------------
style_text("string <- 'hi there'", style = eq_assign_style)

