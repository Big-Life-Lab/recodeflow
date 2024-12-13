# Building the documentation

To build the documentation run the following commands in the R console from
the repo root:

1. `source('data-raw/prep_pbc_data.R')`. This will fill the data directory
   with all the files needed by the documentation.
2. `devtools::document()`. This will update the `man` folder.
3. `pkgdown::build_site()`. This will build the documentation website and put
   it in the docs folder.
