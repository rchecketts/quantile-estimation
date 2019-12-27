
quantile_estimation <- function (actual_quantiles, actual_values, estimate_quantiles){
  ##Overview
  #This will return the estimates for the given estimate quantile.  If there are estimates
  #that are above or below the actual data range than the last standard deviation is used
  #for the calculation.  Currently the only family of estimation supported is gaussian.
  
  ##inputs
  #actual_quantiles: vector of the quantiles for actual data
  #actual_values: the values associated with the actual_quantiles
  #estimate_quantiles: the quantiles to be estimated (0 < estimate_quantiles < 1)
  
  ##outputs
  #returns a tibble (dplyr dataframe) with estimate being the quantile estimates
  
  ##dependencies
  #dplyr
  #tidyr
  
  ##examples
  #tst <- quantile_estimation(actual_quantiles = seq(.05,.95,.05), actual_values = c(253,287,306,320,343,362,377,395,406,419,430,445,462,479,512,546,583,634,771), estimate_quantiles = seq(.01,.99, .01))
  
  
  ##load dependencies if not already loaded
  if("dplyr" %in% (.packages())){
    #already loaded
  } else{
    library(dplyr)
  }

  if("tidyr" %in% (.packages())){
    #already loaded
  } else{
    library(tidyr)
  }
  
  
  ##Gather data around actual information and information to be estimated
  actual_quantiles <- round(actual_quantiles, 5)
  estimate_quantiles <- round(estimate_quantiles, 5)
  
  all_quantiles <- sort(unique(c(actual_quantiles, estimate_quantiles)))
  
  all_sd <- qnorm(p = all_quantiles)
  all_sd_diff <- all_sd[2:length(all_sd)] - all_sd[1:(length(all_sd)-1)]
  

  ##tibble with only the estimate information
  df_all_estimates <- tibble(
    quantile = all_quantiles
    , sd_diff = lead(qnorm(quantile)) - qnorm(quantile)
  )

  
  ##tibble with only the actual information
  df_actual <- tibble(
    quantile = actual_quantiles[1:(length(actual_quantiles))]
    , start_quantile = quantile
    , end_quantile = c(actual_quantiles[2:length(actual_quantiles)],NA)
    , val = actual_values[1:(length(actual_values))]
    , end_val = c(actual_values[2:length(actual_values)],NA)
  ) %>% mutate(
    val_diff = end_val - val
    , val_sd = val_diff / (qnorm(end_quantile) - qnorm(start_quantile))
  )
  
  
  ##tibble with the actual and estimate information combined
  df_all_estimates <- df_all_estimates %>% left_join(
    df_actual
    , by = 'quantile'
  ) %>% mutate(
    source = case_when(is.na(val) ~ 'ESTIMATE', TRUE ~ 'ACTUAL')
    , group = case_when(
      !is.na(val) ~ paste0(start_quantile," - ", end_quantile)
    )
  ) %>% fill(
    val_sd
    , .direction = 'down'
  ) %>% fill(
    group
    , .direction = 'down'
  ) %>% fill(
    val_sd 
    , .direction = 'up'
  ) %>% mutate(
    group = case_when(
      is.na(group) ~ paste0('0 - ', min(actual_quantiles))
      , TRUE ~ group
    )
    , sd_diff = case_when(
      quantile < min(actual_quantiles) ~ -sd_diff
      , TRUE ~ sd_diff
    )
  )
  
  
  ##rearrange the estimates that are below the actual range
  df_low_estimates <- df_all_estimates %>% filter(
    quantile < min(actual_quantiles)
  ) %>% arrange(
    desc(quantile)
  ) %>% mutate(
    sd_diff_rt = cumsum(sd_diff)
  ) %>% arrange(
    quantile
  ) 
    
    
  ##Combine the low estimates to the other estimates
  df_all_estimates <- bind_rows(
    df_low_estimates
    , df_all_estimates %>% filter(
      quantile >= min(actual_quantiles)
    ) %>% group_by(
      group
    ) %>% mutate(
      sd_diff_rt = cumsum(
        sd_diff
      )
    )
    )
    
  
  ##calculate the estimates
  df_all_estimates <- df_all_estimates %>% fill(
    val
    , .direction = 'down'
  ) %>% fill(
    val
    , .direction = 'up'
  ) %>% mutate(
    sd_diff_rt = case_when(
      quantile < min(actual_quantiles) ~ sd_diff_rt
      , quantile == min(actual_quantiles) ~ 0
      , TRUE ~ lag(sd_diff_rt)
    )
    #, estimate = val + sd_diff_rt * val_sd
    , estimate = case_when(
      source == 'ACTUAL' ~ val
      , TRUE ~ val + sd_diff_rt * val_sd
    )
  )

  
  return(df_all_estimates)
}

