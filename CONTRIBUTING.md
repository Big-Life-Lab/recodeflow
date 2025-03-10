# Contributing to recodeflow

## Installation

1. [Install R](https://www.r-project.org/) 

   This package uses R version 4.3.2. If you have another version of R installed
   on your system, we recommend using [rig](https://github.com/r-lib/rig) which
   allows you to install and manage multiple versions of R. Use the commands
   `rig add 4.3.2` and `rig default 4.3.2` to install and use the required R
   version.
2. Install dependencies

   This package uses [renv](https://rstudio.github.io/renv/articles/renv.html)
   to manage its dependencies. Start an R session within the project root and
   renv should bootstrap itself. You can then run the command `renv::restore()`
   to install the dependencies.

You should now be ready to start working on the package

## Running tests

[Install the dependencies](#Installation)

Run the command `devtools::test()` within the R session to run the tests.

## Filing an issue

The easiest way to propose a change or new feature is to file an issue. If you've found a bug, you may also create an associated issue. If possible, try to illustrate your proposal or the bug with a minimal [reproducible example](https://www.tidyverse.org/help/#reprex). When filing an issue, please use the issues template available on the GitHub [repository](https://github.com/Big-Life-Lab/recodeflow/issues).

## Pull request

*  Please create a Git branch for each pull request (PR). [Click here](https://help.github.com/en/articles/creating-a-pull-request-from-a-fork) for information on how to create a PR.
*  Your contributed code should roughly follow the tidyverse [style guide](http://style.tidyverse.org).
*  recodeflow uses [roxygen2](https://cran.r-project.org/package=roxygen2), with
[Markdown syntax](https://bookdown.org/yihui/rmarkdown/markdown-syntax.html),
for documentation.
*  recodeflow uses [testthat](https://cran.r-project.org/package=testthat). Adding tests to the PR makes merging the PR into the code base less prone to bugs.
*  If your PR is a user-visible change, you may add a bullet to the top of `NEWS.md` describing the changes made. You may optionally add your GitHub username, and links to relevant issue(s)/PR(s).

## Code of Conduct

Please note that this project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree to
abide by its terms.
