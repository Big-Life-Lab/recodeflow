# recodeflow <img src="man/figures/logo.svg" align="right" width="180"/>

<!-- badges: start -->

[![Lifecycle: development](https://img.shields.io/badge/lifecycle-experimental-blue.svg)](https://www.tidyverse.org/lifecycle/#experimental) <!--
[![](https://img.shields.io/cran/v/cchsflow?color=green)](https://CRAN.R-project.org/package=TBA)
![](https://img.shields.io/github/v/release/big-life-lab/recodeflow?color=green&label=GitHub)
--> [![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

<!-- badges: end -->

# Introduction

## What is `recodeflow`?

`recodeflow` recodes variables from multiple data sets into harmonized variables.

`recodeflow` has basic functions and templates required to define, recode, and harmonize variables for any dataset.

## Why should I use `recodeflow`?

Recoding and cleaning your data is typically the most time consuming step of your project. Existing functions such as `sjmisc::rec()` and `dplyr:recode()` work well but they are **limited to recoding one variable at a time**. 

The `recodeflow` package takes data cleaning and recoding one step further. `recodeflow` allows you to  recode **multiple variables at the same time**, and **harmonize variables** across similar databases even when the variables and variables' categories change.

`recodeflow` also helps to **reduce errors**, **document the recode process**, and ensures your new variables have labels and other **metadata**.

Even if your project has few variables,`recodeflow` can save you time.


## How does `recodeflow` work?

Use the worksheets `variables` and `variable_details` to list your variables and state how to recode the each variable.

Once your variables are defined, use `recodeflow` functions to clean and recode your data. The main `recodeflow` function is `rec_with_table` which recodes variables within you dataset(s) based on how you've defined the variable in the worksheets `variables` and `variable_details`.


## What's included in `recodeflow`?

The `recodeflow` package includes:

-   **functions** required to clean and recode variables.

-   **worksheets:**

    -   **`variables`** a list of variables to recode and
    -   **`variable_details`** mapping of variables across datasets and a list of instructions for recoding variables.

We've also created the following documentation to help you understand `recodeflow`:

-   **how to guides** examples of how to use `recodeflow` and adapt `recodeflow` for your dataset,
-   **articles** that describe package elements (e.g., variables) in detail,
-   **references** that describe all `recodeflow` functions, and
-   **example data** to demonstrate `recodeflow` functions and templates.

## Where is `recodeflow` used?

Currently `recodeflow` is used in packages that harmonize health surveys and health administrative databases.

- `cchsflow` is a package that harmonizes variables across cycles of the Canadian Community Health Survey (CCHS). cchsflow is [published](https://big-life-lab.github.io/cchsflow/index.html). 

- `raiflow` is a package that will harmonize variables within the Resident Assessments Instruments (RAI) from various sources: Canada's Continuing Care Reporting System (CCRS) and Ontario's Resident Assessment Instrutment for Home Care (RAI-HC). `raiflow` is currently underdevelopment.

# Requirements

* If you plan on using the `recode_to_pmml` function's `custom_function_files` argument, you need access to the `local-transformations-generator` repo

# Roadmap

Projects on the roadmap are at the Github repository recodeflow under the [projects tab](https://github.com/Big-Life-Lab/recodeflow/projects).

# Contributing

Please follow the [**recodeflow contribution guide**](https://big-life-lab.github.io/recodeflow/CONTRIBUTING.html) if you would like to contribute to the `recodeflow` package.
