# recodeflow <img src="man/figures/logo.svg" align="right" width="180"/>

<!-- badges: start -->

[![Lifecycle:
development](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![](https://img.shields.io/cran/v/cchsflow?color=green)](https://CRAN.R-project.org/package=TBA)
![](https://img.shields.io/github/v/release/big-life-lab/recodeflow?color=green&label=GitHub)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

<!-- badges: end -->

# Introduction

## What is `recodeflow`?

`recodeflow` transforms variables from multiple data sets into harmonize variables.

`recodeflow` has the basic functions and templates required to define, transform, and harmonize variables for any dataset.


## Why should I use `recodeflow`? 

Let's walk through an example. 

Each year, Canadians complete a survey, and the data is collected. The data is coded.  You, a scientist, want to use data from the 10 surveys to answer two research questions **but there's a catch:**

At the survey level: 

- Not every survey question was asked each year.
- The survey answers changed from year to year. 

At the data level:

- Questions are not coded the same from year to year (e.g., variable names differ)
- Answers are not coded the same from year to year (e.g., height is coded in meters in some years and feet in other years)

The data is messy! You start the timely and detailed process of cleaning and transforming the data. You think "there must be a better way."

That's where `recodeflow` comes to the rescue. 

With `recodeflow` you can clean and transform your entire dataset with a few lines of code. Even better, once you have defined your variables you have variable sheets you can reuse or share with colleagues - saving you even more time down the line.


## How does `recodeflow` work?

Use templates `variable_sheet` and `variable_details` to list your variables, and state how to transform the each variable.

Once your variables are defined, use `recodeflow` functions to clean and transform your data. The main `recodeflow` function is `rec_with_table` which transforms variables within you dataset(s) based on how you've defined the variable in `variable_sheet` and `variable_details`.


## What's does the `recodeflow` package include?

The `recodeflow` package includes: 

- **functions** required to clean and transform variables.
- **templates:**

  - **`variable_sheet`** a list of variables to transform, and
  - **`variable_details`** mapping of variables across datasets and a list of instructions for transforming variables.


We've also created the following documentation, to help you understand `recodeflow`:

- **how to guides** examples of how to use `recodeflow` and adapt `recodeflow` for your dataset
- **articles** that describe package elements (e.g., variable_sheet) in detail 
- **references** that describe all `recodeflow` functions 
- **example data** to demonstrate `recodeflow` functions and templates.


## Where is `recodeflow` used?

Currently `recodeflow` is used in packages that harmonize health surveys and health administrative databases. 

- `cchsflow` is a package that harmonizes variables across cycles of the Canadian Community Health Survey (CCHS). cchsflow is (published)[https://big-life-lab.github.io/cchsflow/index.html].
-`raiflow` is a package that will harmonize variables within the Resident Assessments Instruments (RAI) from various sources: Canada's Continuing Care Reporting System (CCRS), and Ontario's Resident Assessment Instrutment for Home Care (RAI-HC). `raiflow` is currently underdevelopment. 


# Roadmap

Projects on the roadmap are at the Github repository recodeflow under the [projects tab](https://github.com/Big-Life-Lab/recodeflow/projects).

# Contributing

Please follow the [**recodeflow contribution guide**](https://big-life-lab.github.io/recodeflow/CONTRIBUTING.html) if you
would like to contribute to the `recodeflow` package.


