
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `osdatahub` (R package):

<!-- badges: start -->
<!-- badges: end -->

> Coding in Python?  
> `osdatahub` has a sibling package for Python developers with similar
> functionality, [check it out
> here](https://github.com/OrdnanceSurvey/osdatahub).

`osdatahub` is an `R` package from Ordnance Survey (OS) that makes it
easier to interact with OS data via the [OS Data Hub
APIs](https://osdatahub.os.uk/).

OS is the national mapping agency for Great Britain and produces a large
variety of mapping and geospatial products. Much of OS’s data is
available via the [OS Data Hub](https://osdatahub.os.uk/), a platform
that hosts both free and premium data products.

<img src="man/figures/os-logo.png">

## Features

-   Get access to Ordnance Survey data in as few as 2-3 lines of code
-   Easily query geographic extents using bounding boxes, radii, British
    National Grid references, and ONS geographies
-   Request as much data as you need with automatic API paging
-   Supports the OS National Geographic Database (NGD) - Features
    Places, Maps and Downloads APIs

**Note**: This package is under active development.

# Setup

## Installation

`osdatahub` is available on CRAN.

``` r
install.packages('osdatahub')
```

Load the library to begin using it.

``` r
library(osdatahub)
```

You’ll also need to sign-up for an account on the [OS Data
Hub](https://osdatahub.os.uk/) and get an API key to access certain
features. If you’ve setup you’re account and need help getting a key,
try the following steps:

1.  Navigate to the **API Dashboard** located on the top navigation bar
2.  Go to **My Projects**
3.  Click **Create a new project**, give your project a name, then click
    **Create project**
4.  Select **Add an API to this project**
5.  Choose the APIs you would like to use and click **Done**

# Quick Start

## NGD Features API

We can use the NGD Features API by loading the library:

``` r
library(osdatahub)
```

Then we need to get our [OS API key](https://osdatahub.os.uk/) and store
it as a variable ([find out how to do this securely with environment
variables](https://github.com/OrdnanceSurvey/osdatahub/blob/master/Examples/Setting%20up%20an%20API%20key.ipynb)):

``` r
key <- "[YOUR KEY GOES HERE]"
```

Next, we must decide which NGD Collection we are interested in. We can
discover the available collection ids in 2 ways:

1.  Browse the [OS Data Hub Technical
    Documentation](https://osdatahub.os.uk/docs/ofa/technicalSpecification)
2.  Run the `list_ngd_collections()` function:

``` r
list_ngd_collections(simple = TRUE)
```

Then we can create a query to the NGD API:

``` r
results = query_ngd(collection = 'bld-fts-buildingline-1',
                    max_results = 50,
                    key = key)
```

The `query_ngd()` function supports many different options and filters,
such as various output CRS’, CQL filters, and start and end times for
temporal features.

The data stored in the results variable will be in geojson format,
limited to 50 features (based on the `max_results` parameter). To save
the query results as a geojson file, it’s useful to the use the
[`sf`package](https://cran.r-project.org/package=sf). If the `sf`
package is available, a spatial data frame can be returned directly from
the API by specifying `returnType = 'sf'`.

If you have the ID of a specific feature you would like to query, you
can use the `query_ngd()` function to return just that record from a
collection:

``` r
feature_id <- "0000013e-5fed-447d-a627-dae6fb215138"

feature <- query_ngd(feature_id, 
                     collection = 'bld-fts-buildingline-1', 
                     key = key)
```

## Downloads API

If you’d like to download an entire dataset instead of querying the API
on demand, the OS Data Hub has the [Downloads
API](https://osdatahub.os.uk/docs/downloads/technicalSpecification).
This API allows you to search, explore, and download both [Open Data
Products](https://osdatahub.os.uk/downloads/open) (e.g. OS Open Rivers,
Boundary-Line, and a 1:250,000 scale colour raster of Great Britain) and
Premium Data Packages using `R`.

It is possible to download Open Data products without an API key, but
the Premium Data Packages require you to have a premium API key and
order the package you want to download on the [OS Data Hub
website](https://osdatahub.os.uk/downloads/).

The first step to download data is to discover which products are
available. You can see the available datasets on the [OS Data Hub
website](https://osdatahub.os.uk/downloads/) or using the following
snippet of code:

``` r
library(osdatahub)

list_os_opendata()
```

You can also see all Premium Data Packages available to download using
your premium API key:

``` r
key <- '[YOUR KEY GOES HERE]'
list_os_datapackages(key = key)
```

Note: For Premium Data Packages, this query will only return datasets if
you have previously *ordered* the dataset on the OS Data Hub Website.

Once you have found a package you’d like to download, you can get a list
of the different products you can download:

``` r
greenspace = list_os_opendata('OpenGreenSpace')
```

Once you know the dataset and specific product you’d like to download,
you can download the dataset locally:

``` r
download_os_opendata(greenspace, 
                     file_name = 'opgrsp_essh_nj.zip', 
                     output_dir = tempdir())
```

## Maps API

The [OS Maps
API](https://osdatahub.os.uk/docs/wmts/technicalSpecification) provides
access to pre-rendered raster tiles. Choose the map style that suites
your use case best, whether you need the detail of OS MasterMap or our
iconic Leisure maps. There are four styles available: Road, Outdoor,
Light and Leisure. Each map style contains OS OpenData and Premium data
layers. These are available in British National Grid for GB data and Web
Mercator projections. The API in the `osdatahub` R package provides a
convenience function to download the tiles to your local machine for
small, static maps and graphics. For more details on the Maps API, see
the [technical
documentation](https://osdatahub.os.uk/docs/wmts/technicalSpecification).

The first step to download data is to define a local extent to query the
map:

``` r
library(osdatahub)
key <- '[YOUR KEY GOES HERE]'

# Find the bounds of a British National Grid square.
OS_ext <- extent_from_bng('SU3715')
```

You can download the tiles which cover the extent to your local machine
as .png files. Then these tiles can be stitched together and
georeferenced for spatial applications.

``` r
imgTile <- query_maps(OS_ext, 
                      layer = 'Light_27700', 
                      key = key,
                      output_dir = tempdir())
```

## Places API

The [OS Places
API](https://osdatahub.os.uk/docs/places/technicalSpecification)
provides a detailed view of an address and its life cycle. It contains
all the records of AddressBase® Premium and AddressBase® Premium –
Islands and so provides all the information relating to an address or
property from creation to retirement.

The Places API is always going to return us addresses. It allows
geographic (extent-based) and non-geographic queries.

``` r
library(osdatahub)
key <- '[YOUR KEY GOES HERE]'

# Create a polygon extent.
extent <- extent_from_bbox(c(600000, 310200, 600900, 310900), 'EPSG:27700')

# Extract addresses within the extent polygon.
results <- places_query(extent, limit = 50, key = key)
```

Note: the Places API requires a *premium* API key.

------------------------------------------------------------------------

# Contribute

This package is still under active development and we welcome
contributions from the community. Please contact us via Email.
