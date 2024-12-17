## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options(styler.colored_print.vertical = FALSE)
styler::cache_deactivate()

## ----setup--------------------------------------------------------------------
library(styler)

## ----eval = FALSE-------------------------------------------------------------
#  function() {
#    # a comment
#    x <- 2 # <- change this line
#  }
#  
#  another(call)

