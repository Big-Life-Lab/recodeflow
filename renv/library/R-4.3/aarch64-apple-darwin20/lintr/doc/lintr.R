## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----show_default_settings, echo = FALSE--------------------------------------
default_settings <- lintr::default_settings
default_settings$linters <- "`lintr::default_linters`"
default_settings$comment_token <- "(lintr-bot comment token for automatic GitHub comments)"
default_settings$exclusions <- "(empty)"

make_string <- function(x) {
  if (inherits(x, "regex")) {
    paste0("regex: `", x, "`")
  } else {
    as.character(x)
  }
}

defaults_table <- data.frame(
  default = vapply(default_settings, make_string, character(1L)),
  stringsAsFactors = FALSE
)

# avoid conflict when loading lintr in echo=TRUE cell below
rm(default_settings)

knitr::kable(defaults_table)

## ----show_linter_defaults, echo = FALSE---------------------------------------
library(lintr) # needed here for formalArgs

default_linters <- lintr::default_linters
linters_with_args <- lapply(
  setNames(nm = intersect(names(default_linters), lintr::available_linters(tags = "configurable")$linter)),
  formalArgs
)

make_setting_string <- function(linter_name) {
  args <- linters_with_args[[linter_name]]
  if (is.null(args)) {
    return("")
  }

  arglist <- vapply(args, function(arg) {
    env <- environment(default_linters[[linter_name]])
    deparse(env[[arg]])
  }, character(1L))

  paste0(args, " = ", arglist, collapse = ", ")
}

defaults_table <- data.frame(
  row.names = names(default_linters),
  settings = vapply(names(default_linters), make_setting_string, character(1L)),
  stringsAsFactors = FALSE
)

knitr::kable(defaults_table)

## ----show_tags----------------------------------------------------------------
lintr::available_tags(packages = "lintr")

## ----show_tag_linters---------------------------------------------------------
linters <- lintr::linters_with_tags(tags = c("package_development", "readability"))
names(linters)

## ----show_all_linter_names----------------------------------------------------
names(lintr::all_linters())

## ----programmatic_lintr-------------------------------------------------------
library(lintr)
linter_name <- "assignment_linter"

show_lint <- function(l) {
  lint_df <- as.data.frame(l)
  print(lint_df[, c("line_number", "message", "linter")])
}
hline <- function() cat(strrep("-", getOption("width") - 5L), "\n", sep = "")

withr::with_tempfile("tmp", {
  writeLines("a = 1", tmp)

  # linter column is just 'get'
  show_lint(lint(tmp, linters = get(linter_name)()))
  hline()

  this_linter <- get(linter_name)()
  attr(this_linter, "name") <- linter_name
  # linter column is 'assignment_linter'
  show_lint(lint(tmp, linters = this_linter))
  hline()

  # more concise alternative for this case: use eval(call(.))
  show_lint(lint(tmp, linters = eval(call(linter_name))))
})

## ----show_long_line_lint, echo = FALSE----------------------------------------
lint("X = 42L # -------------- this comment overflows the default 80 chars line length.\n",
  parse_settings = FALSE
)

## ----show_nolint, echo = FALSE------------------------------------------------
lint("X = 42L # nolint ------ this comment overflows the default 80 chars line length.\n",
  parse_settings = FALSE
)

## ----show_long_line_lint_not_skipped, echo = FALSE----------------------------
lint("X = 42L # nolint: object_name_linter. this comment overflows the default 80 chars line length.\n",
  parse_settings = FALSE
)

## ----show_nolint_multiple, echo = FALSE---------------------------------------
lint(
  paste(
    "X = 42L",
    "# nolint: object_name_linter, line_length_linter. this comment overflows the default 80 chars line length.\n"
  ),
  parse_settings = FALSE
)

## ----show_nolint_abbrev, echo = FALSE-----------------------------------------
lint(
  paste(
    "X = 42L",
    "# nolint: object_name, line_len. this comment still overflows the default 80 chars line length.\n"
  ),
  parse_settings = FALSE
)

## ----show_comment_code_lint, echo = FALSE-------------------------------------
lint("# x <- 42L\n# print(x)\n", parse_settings = FALSE)

## ----show_comment_code_nolint, echo = FALSE-----------------------------------
lint("# nolint start: commented_code_linter.\n# x <- 42L\n# print(x)\n# nolint end\n",
  parse_settings = FALSE
)

