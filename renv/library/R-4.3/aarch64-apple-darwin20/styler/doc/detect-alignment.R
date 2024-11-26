## ----include=FALSE------------------------------------------------------------
knitr::opts_chunk$set(
  eval = FALSE,
  collapse = TRUE,
  comment = "#>"
)
styler::cache_deactivate()

## -----------------------------------------------------------------------------
#  call(
#    a   =       3,
#    bre = 3213232
#  )

## -----------------------------------------------------------------------------
#  call(
#    a = 3,
#    bre = 3213232
#  )

## -----------------------------------------------------------------------------
#  tibble::tribble(
#    ~key_here,  ~right_aligned,
#    "left",            "right", # comments are allowed
#    "long string",      "shrt" # columns can overlap ('~' above ',')
#  )
#  
#  tibble::tribble(
#    ~key_here,     ~left_aligned,
#    "left",        "right", # comments are allowed
#    "long string", "shrt" # columns can overlap ('~' above ',')
#  )
#  
#  # right-aligned after =
#  purrr::map(x, fun, # arguments on same line as opening brace are not considered
#    arg2 =       2,
#    ar   = f(k, x)
#  )
#  
#  # left aligned after =
#  purrr::map(x, fun, # arguments on same line as opening brace are not considered
#    arg2 = 2,
#    ar   = f(k, x)
#  )

## -----------------------------------------------------------------------------
#  call(
#    # column 1  | column 2 |
#    abkj = f(2), 7,        # | row 1
#    more_ = "a", 2         # | row 2
#  )

## -----------------------------------------------------------------------------
#  # all arguments of first column named -> must right align values after `=`,
#  # one or more spaces around `=`, none before and at least one after the comma.
#  # aligned if the (imaginary) comma on the last line is in line with the commas
#  fell(
#    x  =    1,
#    y  =   23,
#    zz = NULL
#  )
#  
#  # this works also with more than one column
#  fell(
#    x  =    1, annoying =       3,
#    y  =   23, # nothing in column 2 for row 2
#    zz = NULL, finally  = "stuff"
#  )
#  
#  # or if not all arguments of the first column are named
#  gell(
#    p = 2,   g = gg(x),  n = 3 * 3, #
#    31,    fds =    -1, gz = f / 3,
#  )

## -----------------------------------------------------------------------------
#  # all arguments of first column named -> must left align values after `=`,
#  # at least one space before `=`, exactly one after, none before and at least one
#  # after the comma.
#  # aligned if the first values after `=` are aligned (and exactly one space after
#  # `=`)
#  fell(
#    x = 1,
#    y = 23,
#    zz = NULL
#  )
#  
#  # this works also with more than one column
#  fell(
#    x  = 1,   annoying = 3,
#    y  = 23, # nothing in column 2 for row 2
#    zz = NULL, finally = "stuff"
#  )
#  
#  # or if not all arguments of the first column are named
#  gell(
#    p = 2, g = gg(x), n = 3 * 3, #
#    31, fds = -1, gz = f / 3 + 1,
#  )

## -----------------------------------------------------------------------------
#  call(
#    x = 2,           p = "another",
#    y = "hhjkjkbew", x = 3
#  )
#  
#  tibble::tribble(
#    ~x,        ~y,
#    "another", 1:3,
#    "b",       1211234
#  )

