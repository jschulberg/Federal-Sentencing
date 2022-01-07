###########################################################################
###########################################################################

#######               Federal Sentencing Analysis               #######

###########################################################################
###########################################################################
# In this script, I will analyze the Federal Sentencing dataset included
# in the data folder of this repository. I will use a variety of exploratory
# and modeling techniques to do answer the following questions:

# How 


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


### Bring in the data, taking advantage of the project structure
# Note: There are 11 CSVs, one for each year between 2010-2020,
#       so we'll bring them all in programmatically into one df
start_time <- Sys.time()

# Initialize an empty tibble to hold all of our data
sentencing_data <- dplyr::tibble()

for (csv in unzip('Data/Processed/Archive.zip', list = TRUE)$Name) {
  # Where we at
  print(paste0("Reading in ", csv, "..."))
  
  # Read it in as a temporary tibble
  temp_data <- read_csv(unzip('Data/Processed/Archive.zip', csv),
    col_names = TRUE,
    # Only pull in columns we care about to speed up the process
    # Definitions and descriptions of all variables are available in the codebook here:
    # https://www.ussc.gov/sites/default/files/pdf/research-and-publications/datafiles/USSC_Public_Release_Codebook_FY99_FY20.pdf
    col_types = cols_only(
      AGE = col_double(), # Actual Age
      YEARS = col_double(), # Age Category buckets
      ALTDUM = col_logical(), # Dummy indicator of alternative sentence
      ALTMO = col_double(), # Total months of alternative incarceration
      AMENDYR = col_character(), # The guideline manual used to calculate the guideline calculations
      AMTFINEC = col_double(), # Total dollar amount of both fine and cost of supervision
      AMTREST = col_double(), # Dollar amount of restitution
      FINE = col_double(), # Dollar amount of fine ordered (including cost of supervision)
      FINEWAIV = col_logical(), # Indicates if fine was waived because of defendant's inability to pay
      AMTTOTAL = col_double(), # Sum of the imposed dollar amounts of fine, cost of supervision, and restitution
      ARMCRIM = col_factor(), # # of lvls to be subtracted due to defendant's acceptance of responsibility
      CAROFFAP = col_logical(), # Indicator to whether career offender status was applied
      CAROFFEN = col_factor(), # # of lvls to subtract from the Career Offender Offense Level due to defendant's Acceptance of Responsibility
      CARROFFLV = col_double(), # Offense level due to the application of career offender status
      CASETYPE = col_factor(), # 1 = Felony; 2 = Misdemenaor A; 3 = Misdemeanor B/C (2018-present)
      CIRCDIST = col_double(), # Districts in the order in which they appear in Sourcebook
      CITIZEN = col_factor(), # 1 = US Citizen; 2 = Resident; 3 = Undocumented Immigrant; 4 = Unknown; 5 = Extradited Alien
      NEWCIT = col_logical(), # 0 = U.S. Citizen; 1 = Non-U.S. Citizen
      CITWHERE = col_character(), # Defendant's country of citizenship
      COMDUM = col_logical(), # Indicator whether defendant received community confinement
      MOCOMCON = col_double(), # Total months of community confinement
      COSTSUP = col_double(), # Cost of supervision, only if ordered separately from fine
      CRIMHIST = col_logical(), # Whether defendant has ANY criminal history or LE contacts
      CRIMLIV = col_double(),
      CRIMPTS = col_double(),
      CRMLIVAP = col_factor(),
      CRPTS = col_logical(),
      DAYSDUM = col_logical(), # Indicator whether offender received a sentence of any days imposed in prison
      DEFCONSL = col_factor(), # Type of defense counsel used in the instant offense
      DISPOSIT = col_factor(), # Disposition of the defendant's case
      DOB = col_date(), # Date of Birth
      DOBMON = col_factor(), # Month of Birth
      DOBYR = col_double(), # Year of Birth
      DRUGMIN = col_double(), # Mandatory min sentence (in months) associated with drug statutes
      'DRUGTYP1' = col_character(), # Drug types involved in a case
      'DRUGTYPX' = col_character(), # Drug types involved in a case
      DTGDL = col_logical(), # Identifies cases with one of the "Big 7" drug guidelines as the primary guideline
      ECONDUM = col_logical(), # Indicator of whether a fine/cost of supervision or restitution amount was given
      EDUCATN = col_factor(), # Highest lvl of education completed by defendant
      HISPORIG = col_factor(), # 0 = No Info available; 1 = Non-Hispanic; 2 = Hispanic
      HOMDUM = col_logical(), # Indicates whether defendant received home detention
      MOHOMDET = col_double(), # Total months of home detention ordered
      HRCOMSRV = col_double(), # Total # of comm service hours ordered
      INOUT = col_logical(), # 0 = Received Prison Sentence; 1 = Received Non-Prison Sentence
      SENSPCAP = col_double(), # Months of sentence. 0 = probation; >470 = life
      INTDUM = col_logical(), # Indicates whether a defendant received intermittent confinement
      MOINTCON = col_double(), # Total months of intermittent confinement ordered
      'LOSS1' = col_double(), # Dollar amount of loss for which the offender is responsible
      'LOSSX' = col_double(), # Same as above
      'LOSSHI' = col_double(), # Same as above
      MAND1 = col_factor(), # Status of any mandatory minimums at sentencing
      MANDX = col_factor(), # Status of any mandatory minimums at sentencing
      MARRIED = col_factor(), # Marital status
      MNTHDEPT = col_double(), # Diff in months between guideline minimum (GLMIN) and sentence length (SENSPCAP)
      MONCIRC = col_factor(), # Judicial circuit in which the defendant was sentenced
      MONRACE = col_factor(), # Offender's race
      NEWRACE = col_factor(), # Recode of MONRACE
      MONSEX = col_logical(), # FALSE/0 = Male; TRUE/1 = Female
      NEWCNVTN = col_logical(), # 0 = Plea; 1 = Trial
      NEWEDUC = col_factor(), # 1 = Less than HS; 3 = HS; 5 = Some College; 6 = College Grad
      NOCOUNTS = col_double(), # The # of counts of conviction
      NODRUG = col_double(), # The # of drugs involved in the case
      NOREAVAR = col_double(), # The # of reasons why a sentence outside the range was imposed
      REAS1 = col_character(), # The first reason for why a sentence was imposed outside the range
      REAS2 = col_character(), # Same as above, second one
      REAS3 = col_character(), # Same as above
      REASON1 = col_character(), # The reason given by the court for a departure sentence
      REASON2 = col_character(), # The reason given by the court for a departure sentence
      REASON3 = col_character(), # The reason given by the court for a departure sentence
      NOUSTAT = col_double(), # The # of unique statutes in a case
      NUMDEPEN = col_double(), # The # of dependents the offender supports
      OFFGUIDE = col_character(), # Primary type of crime for case, starting in 2018
      OFFTYPSB = col_character(), # Primary type of crime for case, 2010-2017
      OTHRMIN = col_double(), # The # of months for mandatory min for other reasons
      PCNTDEPT = col_double(), # % diff between guideline minimum and the sentence length
      PRISDUM = col_logical(), # Indicates if defendant received a prison sentence
      PROBATN = col_double(), # Total probation ordered, in months
      PROBDUM = col_logical(), # Indicates if defendant received probation
      RANGEPT = col_factor(), # Denotes where within the range a case was sentenced.
      SENTDATE = col_date(), # Date of sentencing
      SENTIMP = col_factor(), # Type of sentence given
      SENTMON = col_character(), # Month of sentencing
      SENTYR = col_integer() # Year of sentencing
    ),
    # n_max = 100, # Uncomment this line to make the code run much much faster, but only if you want to see a sample of the data
    progress = show_progress()
    ) %>% 
    janitor::remove_empty(which = c("rows", "cols")) %>% 
    as_tibble()
  
  # Concatenate the new dataset to our master
  sentencing_data <- bind_rows(temp_data, sentencing_data)
  
  # Unzipping the file created a large CSV version of it. Let's delete this
  unlink(csv)
}

end_time <- Sys.time()
end_time - start_time

readr::write_csv(sentencing_data, "Data/sentencing_data.csv")



### Alternate Method of Reading in Data (Backward Selection)
# Another way to read in the data is to read in one dataset at a 
# time, but include all columns available in said dataset. Afterwards,
# we standardize the column names (which might not necessarily be all
# caps in every dataset) and remove any columns which are >75% null

# Create a function to figure out which columns are even mostly filled
# to begin with
filter_null_columns <- function(df, perc_null = .75) {
  
  # Figure out how many nulls we have in each column
  print('Calculating number of nulls in each column...')
  num_nulls <- df %>% 
    head(100) %>% 
    summarise_all(list(~sum(is.na(.))/length(.)))
  
  # Create a new tibble with two columns:
  #   1. The name of a variable (upper case)
  #   2. The % of values that are null in the given column
  num_nulls_pivoted <- tibble(
    # Upper case variable names
    variable = names(num_nulls) %>% stringr::str_to_upper(),
    percent_null = num_nulls[1, ] %>% t()
  )
  
  print(paste0("Retaining ", 
               sum(num_nulls_pivoted$percent_null < perc_null), 
               " columns | Removing ", 
               sum(num_nulls_pivoted$percent_null > perc_null),
               " mostly null columns.")
        )
        
  # Now filter out any variables that are more than 75% null
  print(paste0("Removing columns that are more than ", 
               perc_null, 
               "% null..."))
  num_nulls_reduced <- num_nulls_pivoted %>% 
    filter(percent_null < perc_null)
  
  # Now filter the original tibble to match the columns available
  # in the num_nulls_reduced tibble
  df_filtered <- df %>% 
    select(., which(stringr::str_to_upper(names(df)) %in% num_nulls_reduced$variable))
    
  # Return the final tibble
  return(df_filtered)
}

# Now let's loop through all of the CSVs in the zip folder, filter out
# columns that are mostly null, and concatenate it to a master tibble
start_time <- Sys.time()

# Initialize an empty tibble to hold all of our data
sentencing_data <- dplyr::tibble()

for (csv in unzip('Data/Processed/Archive.zip', list = TRUE)$Name) {
  # Where we at
  print(paste0("Reading in ", csv, "..."))
  
  # Read it in as a temporary tibble
  temp_df <- read_csv(unzip('Data/Processed/Archive.zip', csv),
                      col_names = TRUE,
                      n_max = 100, # Uncomment this line to make the code run much much faster, but only if you want to see a sample of the data
                      progress = show_progress())
  
  # Remove columns that are mostly null in this
  temp_filtered <- filter_null_columns(temp_df, perc_null = .75)
  
  # Concatenate the new dataset to our master
  sentencing_data <- bind_rows(temp_filtered, sentencing_data)
  
  # Unzipping the file created a large CSV version of it. Let's delete this
  unlink(csv)
}

end_time <- Sys.time()
end_time - start_time