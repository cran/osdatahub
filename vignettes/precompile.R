# Vignettes that depend on internet access and API key have been pre-compiled.
# Remember to add the *.Rmd.orig to the buildignore file.

# create vignette templates
# usethis::use_vignette('interactive-plotting')

## NOTE -- make sure the the OS API key is set in memory before compiling.
osdatahub::set_os_key(key)
# delete the created *.Rmd files
# optionally, rename the .Rmd.orig files and edit, then proceed
# file.rename("vignettes/ngd-api.Rmd.orig", "vignettes/ngd-api.Rmd")
# file.rename("vignettes/extents.Rmd.orig", "vignettes/extents.Rmd")
# file.rename("vignettes/plotting-results.Rmd.orig", "vignettes/plotting-results.Rmd")
# file.rename("vignettes/interactive-plotting.Rmd.orig", "vignettes/interactive-plotting.Rmd")
# file.rename("vignettes/crs-pitfalls.Rmd.orig", "vignettes/crs-pitfalls.Rmd")
#

# quick renaming of files

file.rename("vignettes/ngd-api.Rmd", "vignettes/ngd-api.Rmd.orig")
file.rename("vignettes/extents.Rmd", "vignettes/extents.Rmd.orig")
file.rename("vignettes/plotting-results.Rmd", "vignettes/plotting-results.Rmd.orig")
file.rename("vignettes/interactive-plotting.Rmd", "vignettes/interactive-plotting.Rmd.orig")
file.rename("vignettes/crs-pitfalls.Rmd", "vignettes/crs-pitfalls.Rmd.orig")

# set API key

library(knitr)

knit("vignettes/ngd-api.Rmd.orig", "vignettes/ngd-api.Rmd")
knit("vignettes/extents.Rmd.orig", "vignettes/extents.Rmd")
knit("vignettes/plotting-results.Rmd.orig", "vignettes/plotting-results.Rmd")
knit("vignettes/interactive-plotting.Rmd.orig", "vignettes/interactive-plotting.Rmd")
knit("vignettes/crs-pitfalls.Rmd.orig", "vignettes/crs-pitfalls.Rmd")
# knit("vignettes/json-paging.Rmd.orig", "vignettes/json-paging.Rmd")

# Move the figures to the vignettes folder.
figs <- list.files(pattern = "fig-")
fs::file_move(figs, fs::path("vignettes/", figs))

library(devtools)
build_vignettes(quiet = F)

## Note: Remember to Knit the readme.rmd

library(pkgdown)
pkgdown::build_site()

