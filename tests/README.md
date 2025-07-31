This folder contains all the tests for the package.

## Commands

### Running the tests

Use the command `devtools::test()` within the R console to run the tests.

If using [rig](https://github.com/r-lib/rig) to manage your R versions you
can use the command `rig run -r 4.3.2 -e "devtools::test()"`

### Custom functions

When writing a `rec_with_table` test that requires a custom function for
example to test a derived variable, use `setup_custom_function` to handle
the setup and cleanup of the custom function.
