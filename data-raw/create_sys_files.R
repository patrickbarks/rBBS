
## stratum areas
library(rvest)
library(dplyr)

st_tab <- read_html("https://www.pwrc.usgs.gov/bbs/stratanames/index.html") %>% 
  html_node("#Table2") %>% 
  html_table(header = TRUE)

strat_areas <- rbind.data.frame(st_tab[,1:3], st_tab[,5:7]) %>% 
  dplyr::filter(!is.na(Stratum)) %>% 
  setNames(c("stratum", "stratum_name", "stratum_area")) %>% 
  dplyr::select(-stratum_name)


## filenames and common names for stateprovs in 10-stop data (States/)

# 10-stop files (by stateprov)
ts <- 'ftp://ftpext.usgs.gov/pub/er/md/laurel/BBS/DataFiles/States/'
ts_lines <- readLines(ts)
ts_lines_split <- strsplit(ts_lines, '[[:space:]]+')
ts_files <- vapply(ts_lines_split, function(x) x[length(x)], character(1))
ts_files <- ts_files[grep('\\.txt$|\\.pdf$|\\.zip$', ts_files)]

# write csv with file names and manually add in common names, abbreviations,
#  and 50-state zip files
df <- data.frame(file = ts_files, stringsAsFactors = F)
# write.csv(df, 'data-raw/ten-stop-files.csv', row.names = F)

# write to sysdata
library(readr)
df_zip <- read_csv('data-raw/zip-files.csv')
usethis::use_data(strat_areas, df_zip, overwrite = TRUE, internal = TRUE)



## subset weather data for tests
bbs_dir <- '~/bbs_data/'
wx <- bbs_meta_weather(bbs_dir = bbs_dir)
regions <- bbs_meta_regions(bbs_dir = bbs_dir)
regions[regions$country_name == 'Canada',]

# subset to Nunavut and NWT
wx <- wx[wx$state_num %in% c(43, 62),]

setwd('inst/testdata/')
write.csv(wx, 'weather.csv', row.names = FALSE)

zip('Weather.zip', 'weather.csv')
file.remove('weather.csv')
setwd('../../')




## subset 50-stop zip file to Nunavut for tests
nunavut_zip <- '~/bbs_data/50-StopData/1997ToPresent_SurveyWide/Fifty7.zip'

temp <- tempdir()
unzip(nunavut_zip, exdir = temp)

bbs_50 <- read.csv(paste(temp, 'fifty7.csv', sep = '/'))
bbs_50 <- subset(bbs_50, statenum == 62) # subset to Nunavut

dir.create('inst/testdata/50-StopData/1997ToPresent_SurveyWide/', recursive = TRUE)

setwd('inst/testdata/50-StopData/1997ToPresent_SurveyWide/')
write.csv(bbs_50, 'fifty7.csv', row.names = FALSE)

zip('Fifty7.zip', 'fifty7.csv')
file.remove('fifty7.csv')
setwd('~/rBBS')

