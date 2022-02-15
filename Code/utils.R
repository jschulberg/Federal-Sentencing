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