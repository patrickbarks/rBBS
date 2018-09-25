
<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Build Status](https://travis-ci.org/oharar/rBBS.svg?branch=master)](https://travis-ci.org/oharar/rBBS)

rBBS
====

An R package to work with data from the North American Breeding Bird Survey (BBS).

The BBS is a large-scale bird monitoring program that was initiated in 1966 and currently encompasses over 4,600 active survey routes in North America. Each survey is conducted by a skilled volunteer during the height of the breeding season, and entails 50 three-minute point counts conducted at half-mile intervals along a 24.5-mile route.

The BBS is a cooperative effort between the United States Geological Survey (USGS) and Environment and Climate Change Canada's Canadian Wildlife Service (CWS).

See more information at <https://www.pwrc.usgs.gov/bbs/>.

See also the terms of use for BBS data at <https://www.pwrc.usgs.gov/BBS/RawData/>.

BBS datasets
------------

The BBS provides two main datasets:

*50-stop data*

-   includes bird counts for each of the 50 stops on a given survey
-   only consistently available from 1997 onward

*10-stop data*

-   bird counts binned into groups of 10 stops (i.e. stops 1-10, 11-20, ..., 41-50)
-   consistently available for all survey years

Installation
------------

Install the development version from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("patrickbarks/rBBS")
```

Usage
-----

#### Download data

To work with BBS data we first need to download it from the USGS ftp server. This can be done manually following the links at <https://www.pwrc.usgs.gov/bbs/>, or by using the function `bbs_download`.

``` r
library("rBBS")

# "." gives the working directory; you may prefer to specify a different one
bbs_download(dest = ".")
```

The default options download all metadata files and all 10-stop data. The 50-stop data can be downloaded by adding the argument `fifty_stop = TRUE`.

#### Build metadata tables

Metadata tables can be built using the `bbs_meta_*` functions.

``` r
bcr <- bbs_meta_bcr(bbs_dir = ".")
strata <- bbs_meta_strata(bbs_dir = ".")
regions <- bbs_meta_regions(bbs_dir = ".")
routes <- bbs_meta_routes(bbs_dir = ".")
species <- bbs_meta_species(bbs_dir = ".")
weather <- bbs_meta_weather(bbs_dir = ".")
```

#### Build bird count tables

Tables with bird-count data can be built using the `bbs_build_*` functions:

``` r
bbs_10 <- bbs_build_10(bbs_dir = ".") # 10-stop data
bbs_50 <- bbs_build_50(bbs_dir = ".") # 50-stop data
```

##### Build in counts of zero

The original BBS data does not include counts of zero (i.e. instances where a species was not observed on a given route), but these can be built in by setting `zeros = TRUE` in the `bbs_build_*` functions.

However, note that the full dataset *with* zeros includes more than 92 million rows, which requires some time to build and a few gigabytes of memory. It's therefore a good idea to subset to the species, years, or locations of interest within the call to `bbs_build_*`, e.g.

``` r
bbs_10 <- bbs_build_10(bbs_dir = ".", zeros = TRUE,
                       states = c('Washington', 'Oregon', 'California'))
```

Example
-------

Let's say we're interested in the distribution of the Western Meadowlark within the United States. We can find its American Ornithological Union species code (aou) in the metadata table `species`.

``` r
species$aou[species$english_common_name == 'Western Meadowlark']
#> [1] 5011
```

Now we can build a table with the 10-stop count data (including zeros) for the Western Meadowlark.

``` r
bbs_wm <- bbs_build_10(bbs_dir = ".", zeros = TRUE,
                       countries = 'United States', aou = 5011)
```

Next, we'll summarize the occurrence of the Western Meadowlark by route (unique combinations of `country_num` x `state_num` x `route`). We'll calculate occurrence simply as any instance of `species_total > 0` on a given route.

``` r
library(dplyr)

bbs_wm_occur <- bbs_wm %>% 
  filter(state_num != 3) %>% # remove Alaska
  group_by(country_num, state_num, route) %>% 
  summarize(occur = ifelse(any(species_total > 0), TRUE, FALSE)) %>% 
  ungroup() %>% 
  left_join(routes) # join to routes metadata which includes route coordinates
```

And finally, we'll plot the occurrence data on a map of the United States.

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
