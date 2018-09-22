
<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Build Status](https://travis-ci.org/oharar/rBBS.svg?branch=master)](https://travis-ci.org/oharar/rBBS)

rBBS
====

An R package to work with data from the North American Breeding Bird Survey (BBS).

The BBS is a cooperative effort between the United States Geological Survey (USGS) and Environment and Climate Change Canada's Canadian Wildlife Service (CWS). See more information at <https://www.pwrc.usgs.gov/bbs/>, and see also the terms of use for BBS data at <https://www.pwrc.usgs.gov/BBS/RawData/>.

Installation
------------

Install the development version from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("patrickbarks/rBBS")
```

Usage
-----

To work with BBS data we first need to download it from the USGS ftp server. This can be done manually following the links at <https://www.pwrc.usgs.gov/bbs/>, or by using the function `bbs_download`:

``` r
library("rBBS")

# download to local directory (e.g. '.' gives the current working directory)
bbs_download(dest = '.', countries = 'United States')
```

With the data downloaded, we can build metadata tables using the `bbs_meta_*` functions

``` r
bcr <- bbs_meta_bcr(bbs_dir = '.')
strata <- bbs_meta_strata(bbs_dir = '.')
regions <- bbs_meta_regions(bbs_dir = '.')
routes <- bbs_meta_routes(bbs_dir = '.')
species <- bbs_meta_species(bbs_dir = '.')
weather <- bbs_meta_weather(bbs_dir = '.')
```

or build tables with bird-count data using the `bbs_build_*` functions, e.g.

``` r
bbs_10 <- bbs_build_10(bbs_dir = '.', countries = 'United States')
```

The original BBS data does not include counts of zero (i.e. instances where a species was not observed on a given route), but these can be built in by setting `zeros = TRUE` in the `bbs_build_*` functions.

However, note that the full BBS dataset *with* zeros includes more than 92 million rows, which requires some time to build and a few GB of memory. It's therefore a good idea to subset to the species, years, or locations of interest within the call to `bbs_build_*`, e.g.

``` r
bbs_10 <- bbs_build_10(bbs_dir = '.', zeros = TRUE,
                       states = c('Washington', 'Oregon', 'California'))
```

Example
-------

Let's say we're interested in the distribution of the Western Meadowlark within the United States. We can find its American Ornithological Union species code (aou) in the metadata table `species` using

``` r
species[grep('Meadowlark', species$english_common_name),]
# aou for the Western Meadowlark is 5011
```

Now we can build a table with the 10-stop count data (including zeros) for the Western Meadowlark

``` r
bbs_wm <- bbs_build_10(bbs_dir = '.', zeros = TRUE,
                       countries = 'United States', aou = 5011)
```

Next, we'll summarize the occurrence of the Western Meadowlark by route (unique combinations of `country_num` x `state_num` x `route`). We'll calculate 'occurrence' simply as any instance of `species_total > 0` on a given route.

``` r
library(dplyr)

bbs_wm_occur <- bbs_wm %>% 
  filter(state_num != 3) %>% # remove Alaska
  group_by(country_num, state_num, route) %>% 
  summarize(occur = ifelse(any(species_total > 0), TRUE, FALSE)) %>% 
  ungroup() %>% 
  left_join(routes) # join to routes metadata which includes route coordinates
```

And finally, we'll use `ggplot2` to overlay the occurrence data on a map of the United States.

``` r
library(ggplot2)
states <- map_data("state")

ggplot(states) + 
  geom_polygon(aes(long, lat, group = group), fill = 'grey80') +
  geom_point(data = bbs_wm_occur, aes(longitude, latitude, col = occur), size = 2) +
  scale_color_brewer(palette = 'Set1', name = 'Observed')
```

![](man/img/map.png)

Contributions
-------------

All contributions are welcome. Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.
