---
editor_options: 
  markdown: 
    wrap: 72
---

# recodeflow ![](man/figures/logo.svg){align="right" width="180"}

<!-- badges: start -->

[![Lifecycle:
development](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![](https://img.shields.io/cran/v/cchsflow?color=green)](https://CRAN.R-project.org/package=TBA)
![](https://img.shields.io/github/v/release/big-life-lab/recodeflow?color=green&label=GitHub)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

<!-- badges: end -->

Intro....

## Usage

.... update this example....

*Calculate a harmonized BMI variable for CCHS 2001 cycle*

        # load test cchs data - included in cchsflow

        cchs2001_BMI <- rec_with_table(cchs2001_p, "HWTGBMI")
        

Notes printed to console indicate issues that may affect BMI
classification for your study.

    Loading cchsflow variable_details
    Using the passed data variable name as database_name
    NOTE for HWTGBMI : CCHS 2001 restricts BMI to ages 20-64
    NOTE for HWTGBMI : CCHS 2001 and 2003 codes not applicable and missing 
    variables as 999.6 and 999.7-999.9 respectively, while CCHS 2005 onwards codes 
    not applicable and missing variables as 999.96 and 999.7-999.99 respectively
    NOTE for HWTGBMI : Don't know (999.7) and refusal (999.8) not included
    in 2001 CCHS"

## Installation

        # Install release version from CRAN
        install.packages("cchsflow")

        # Install the most recent version from GitHub
        devtools::install_github("Big-Life-Lab/cchsflow")

Do you just want new variables not yet added to the CRAN version?

You can download and use the latest version of
[`variables.csv`](https://github.com/Big-Life-Lab/cchsflow/blob/master/inst/extdata/variables.csv)
and
[`variable_details.csv`](https://github.com/Big-Life-Lab/cchsflow/blob/master/inst/extdata/variable_details.csv)
from GitHub.

## What is in the `cchsflow` package?

*cchsflow* package includes:

1.  `variables.csv` - a list of variables that can be transformed across
    CCHS surveys.\
2.  `variable_details.csv` - information that describes how the
    variables are recoded.
3.  Vignettes - that describe how to use R to transform or generate new
    derived variables that are listed in `variables.csv`.
    Transformations are performed using `rec_with_table()`.
    `variables.csv` and `variable_details.csv` can be used with other
    statistics programs (see
    [issue](https://github.com/Big-Life-Lab/cchsflow/issues)).
4.  Demonstration CCHS data - `cchsflow` includes a random sample of 200
    respondents from each CCHS PUMF file from 2001 to 2014. These data
    are used for the vignettes. The CCHS test data is stored in /data as
    .RData files. They can be read as a package database.

<!-- -->

    # read the CCHS 2014 PUMF test data

    test_data <- cchs2014_p

This repository does not include the full CCHS data. Information on how
to access the CCHS data can is
[here](https://www150.statcan.gc.ca/n1/pub/82-620-m/2005001/4144189-eng.htm).
The Canadian university community can also access the CCHS through
[ODESI](http://odesi2.scholarsportal.info/webview/) (see
health/Canada/Canadian Community Health Survey).

### Roadmap

Project on the roadmap can be found on
[here](https://github.com/Big-Life-Lab/cchsflow/projects).

## Contributing

Please follow [this
guide](https://big-life-lab.github.io/cchsflow/CONTRIBUTING.html) if you
would like to contribute to the *cchsflow* package.
