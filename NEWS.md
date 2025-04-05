# osdatahub 0.3.0

Breaking changes and deprecated functions

* Removed `extent_from_ons_code()` and `get_ons_geom()` after ONS decommissioned the service.

Other improvements and minor fixes

* Added documentation website. 
* Added support for Names API.
* Added vignette on common CRS pitfalls.
* Added multiple pages of results from `query_places()`.
* Fixed bug in specifying output CRS from `query_places()`.
* Reduced number of segments per quadrant used in `extent_from_radius()`.
* Updated vignette index entry titles.
* Expanded test coverage.

# osdatahub 0.2.0

* Updated documentation on Maps API.
* Updated README with contribution information.
* Updated license file to MIT.
* Updated function examples.

# osdatahub 0.1.2

* Removed default output directories in all download functions.
* Specified `tempdir()` for vignettes and examples where needed.
* Exposed parameter `dataset` for Places API queries.
* Unwrapped examples from `\dontrun{}` where API key is not used.
* Proofreading and copy editing of all text.

# osdatahub 0.1.1

* Expanded test coverage for APIs.
* Preparation for initial CRAN submission.

# osdatahub 0.1.0

* Initial internal release of `osdatahub` package.
* Supports NGD - Features, Downloads, Maps and Places APIs.
* Added `extent_from_*` functions to define query bounds.
* Added a `NEWS.md` file to track changes to the package.
