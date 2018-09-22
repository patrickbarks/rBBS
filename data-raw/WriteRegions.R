RegionsForZipFiles <- GetRegions(ZipFiles = TRUE)
devtools::use_data(RegionsForZipFiles, overwrite=TRUE, internal = TRUE)

# filenames and common names for stateprovs in 10-stop data (States/)

# 10-stop files (by stateprov)
ts <- 'ftp://ftpext.usgs.gov/pub/er/md/laurel/BBS/DataFiles/States/'
ts_lines <- readLines(ts)
ts_lines_split <- strsplit(ts_lines, '[[:space:]]+')
ts_files <- vapply(ts_lines_split, function(x) x[length(x)], character(1))
ts_files <- ts_files[grep('\\.txt$|\\.pdf$|\\.zip$', ts_files)]

# write csv with file names and manually add in common names and abbreviations
df <- data.frame(file = ts_files, stringsAsFactors = F)
# write.csv(df, 'data-raw/ten-stop-files.csv', row.names = F)

# write to sysdata
ts_df <- read.csv('data-raw/ten-stop-files.csv', stringsAsFactors = FALSE)
devtools::use_data(ts_df, overwrite = TRUE, internal = TRUE)




# subset weather data for tests
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


