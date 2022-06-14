
cleanedDF <- joinBuffer %>% 
    mutate(post = sale_date >= 2017) %>%
    select(sale_price , flagshipParkDist,
           parkDist, subwayDist, libDistance, totPermits,
           HispanicLatinoPercent, whitePercent,blackPercent,
           BAPlusPercent, ownerPercent, median_gross_rent,
           median_household_income_in_2019_inflation_adjusted_dollars,
           total_units,beds, baths, sqft , buffer_1320, buffer_1980, buffer_2640,
           buffer_3300, buffer_3960, sale_date, post) %>%
    
    filter(complete.cases(.))

# incremental treatments

incrementalYearsDF <- function(df, year){
    cleanedDF <- df %>% 
        mutate(post = sale_date >= year) %>%
        select(sale_price , flagshipParkDist,
               parkDist, subwayDist, libDistance, totPermits,
               HispanicLatinoPercent, whitePercent,blackPercent,
               BAPlusPercent, ownerPercent, median_gross_rent,
               median_household_income_in_2019_inflation_adjusted_dollars,
               total_units,beds, baths, sqft , buffer_1320, buffer_1980, buffer_2640,
               buffer_3300, buffer_3960, sale_date, post) %>%
        filter(complete.cases(.))
}

incrementalMods <- function(df){
    dndMod <- lm(log(sale_price)~ flagshipParkDist+
                     parkDist+ subwayDist+ libDistance+ totPermits+
                     HispanicLatinoPercent+ whitePercent+blackPercent+
                     BAPlusPercent+ ownerPercent+ median_gross_rent+ 
                     median_household_income_in_2019_inflation_adjusted_dollars+ 
                     total_units+beds+ baths+ sqft + (buffer_1320 + buffer_1980  + buffer_2640 +
                                                          buffer_3300 + buffer_3960) * post, data = df)
}
treatYear <- seq(2012, 2018)

incremeentalDF <- treatYear %>% map(~incrementalYearsDF(joinBuffer, .x))
incrementalOut <- incremeentalDF %>% map(~incrementalMods(.x))

# select treatments
selectTreatmentDF <- function(df, buffer1, buffer2){
    sumDF <- df %>% 
        mutate(sum_col = buffer_1320+buffer_1980+buffer_2640+
                   buffer_3300 + buffer_3960) %>% 
        mutate(treatment = if_else(sum_col >= buffer1 &  sum_col <= buffer2, 1, 0)) 
}
selectYearsDF <- function(df, year){
    cleanedDF <- df %>% 
        mutate(post = sale_date >= year) %>%
        select(sale_price , flagshipParkDist,
               parkDist, subwayDist, libDistance, totPermits,
               HispanicLatinoPercent, whitePercent,blackPercent,
               BAPlusPercent, ownerPercent, median_gross_rent,
               median_household_income_in_2019_inflation_adjusted_dollars,
               total_units,beds, baths, sqft , treatment, sale_date, post) %>%
        filter(complete.cases(.))
}
subsetMods <- function(df){
    dndMod <- lm(log(sale_price)~ flagshipParkDist+
                     parkDist+ subwayDist+ libDistance+ totPermits+
                     HispanicLatinoPercent+ whitePercent+blackPercent+
                     BAPlusPercent+ ownerPercent+ median_gross_rent+ 
                     median_household_income_in_2019_inflation_adjusted_dollars+ 
                     total_units+beds+ baths+ sqft + treatment * post, data = df)
}


# 0.25-0.5 Treatment
treatDF1 <- selectTreatmentDF(cleanedDF, buffer1 = 3, buffer2 = 5)

subsetDF1 <- treatYear %>% purrr::map(~selectYearsDF(df  = treatDF1, year = .x))
subsetOut1 <- subsetDF1 %>% purrr::map(~subsetMods(.x))

subsetOut1

# 0.375-0.5 treatment
treatDF2 <- selectTreatmentDF(cleanedDF, buffer1 = 3, buffer2 = 4)

subsetDF2 <- treatYear %>% purrr::map(~selectYearsDF(df  = treatDF2, year = .x))
subsetOut2 <- subsetDF2 %>% purrr::map(~subsetMods(.x))

subsetOut2

# 0.25-0.75 treatment
treatDF3 <- selectTreatmentDF(cleanedDF, buffer1 = 2, buffer2 = 5)

subsetDF3 <- treatYear %>% purrr::map(~selectYearsDF(df  = treatDF3, year = .x))
subsetOut3 <- subsetDF3 %>% purrr::map(~subsetMods(.x))

subsetOut3


## how to combine all of the data.


collectSubset <- function(results, year, type){
    df <- results %>% summary()
    df <- df$coefficients %>% as.data.frame()
    out <- df %>% slice(df%>% dim() %>% "["(1)) %>% mutate(year = year, type = type)
}

out1 <- map2_dfr(subsetOut1, treatYear, ~collectSubset(.x,.y, "quarter_HalfTreat"))
out2 <- map2_dfr(subsetOut2, treatYear, ~collectSubset(.x,.y, "threeEigth_HalfTreat"))
out3 <- map2_dfr(subsetOut3, treatYear, ~collectSubset(.x,.y, "quarter_threeQuarterTreat"))

subsetsOutcome <- bind_rows(out1, out2, out3) %>% 
    select(Estimate, "Pr(>|t|)", year, type) %>% 
    rename(pval = "Pr(>|t|)") %>% 
    pivot_longer(-c(year, type)) %>% 
    pivot_wider(names_from = year, values_from = value)

collectIncremental <- function(results, year){
    df <- results %>% summary()
    df <- df$coefficients %>% as.data.frame()
    out <- df %>% slice(24:28) %>% mutate(year = year)
}

incrementalOutcome <- map2_dfr(incrementalOut, treatYear, ~collectIncremental(.x,.y)) %>% 
    tibble::rownames_to_column() %>%
    rowwise() %>% 
    mutate(type = str_split(rowname, ":") %>% unlist() %>% "["(1)) %>% 
    select(Estimate, "Pr(>|t|)", year, type) %>% 
    rename(pval = "Pr(>|t|)") %>% 
    pivot_longer(-c(year, type)) %>% 
    pivot_wider(names_from = year, values_from = value) 


totalOutcome <- subsetsOutcome %>% bind_rows(incrementalOutcome)

write_csv(totalOutcome, "data/totalModelOutput.csv")
