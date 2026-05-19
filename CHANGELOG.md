# Changelog

All notable changes to this project will be documented in this file.

The format is based on
[Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to
[Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased] 

### Added

* Added a new utility function `get_start_variables` 
* Added an `invalid_database_reference` validation to `parse_variables_sheet()`
  that checks every database referenced in `variableStart` is declared in that
  row's `databaseStart`.

### Changed

* `databaseStart` is now a required column on the variables sheet passed to
  `parse_variables_sheet()`. Previously only `variable` and `variableStart`
  were required; callers passing a sheet without `databaseStart` will now
  receive a `missing_required_columns` error.
