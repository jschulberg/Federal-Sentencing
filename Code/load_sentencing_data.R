###########################################################################
### Set Up -----------------------------------------------------------------
###########################################################################
# Bring in packages
suppressMessages(library("tidyverse")) # Used for data wrangling
suppressMessages(library("tidyr")) # Used for data cleaning
suppressMessages(library("ggplot2")) # Used for visualizations
suppressMessages(library("readxl")) # Used for loading excel files
suppressMessages(library("readr")) # Used for working with files
suppressMessages(library("pander")) # Used for pretty tables
suppressMessages(library("lubridate")) # Used for fixing dates
suppressMessages(library("praise")) # Used for positive reinforcement
suppressMessages(library("stringr")) # Used for positive reinforcement
suppressMessages(library('vroom')) # for fast reading of large dataframes
source('Code/utils.R')

start_time <- Sys.time()

# Expand Vroom connection size to accomodate bigger datasets
Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 10)

# list all csv's in data/procssed (unzipped Archive.zip first)
f <- fs::dir_ls(path = 'Data/Processed/', glob = "*csv")

# map_df vroom function to read in each csv as a dataframe then stack them
## if all csv's have the same columns then we can just use read_all_zip below
sentencing_df_temp = map_df(f, ~vroom(.x, 
                                      .name_repair = ~ janitor::make_clean_names(., 
                                                                                 case = "lower_camel")))
                            
# filter out null columns
sentencing_df <- filter_null_columns(sentencing_df_temp, .75)

end_time <- Sys.time()
end_time - start_time

#
#list_of_dfs <- map(f, ~vroom(.x))

##
##read_all_zip <- function(file, ...) {
#  filenames <- unzip(file, list = TRUE)$Name
##  vroom(purrr::map(filenames, ~ unz(file, .x)), ...)
#}

#df_list <- read_all_zip('Data/Processed/Archive.zip')

#temp <- vroom('Data/Processed/Archive.zip')

#sentencing_df <- filter_null_columns(temp, .75)