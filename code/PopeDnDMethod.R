root <- rprojroot::find_rstudio_root_file()
dataDir <- file.path(root, 'data')
shapeDir <- file.path(root, 'shape')

library(dplyr)
library(readxl)
library(purrr)
library(readr)
library(stringr)
library(lubridate)
library(ggplot2)
library(googlesheets4)
library(hpiR)
library(lfe)
library(tmap)
library(tidyr)


gs4_auth(
    email = "ksakamoto09@gmail.com",
    path = NULL,
    scopes = "https://www.googleapis.com/auth/spreadsheets",
    cache = gargle::gargle_oauth_cache(),
    use_oob = gargle::gargle_oob_default(),
    token = NULL
)

## whole foods
wf <- sf::st_read(file.path(shapeDir, "wholeFoods"))
pluto <- sf::st_read(file.path(shapeDir, "PLUTO_Dissolve")) %>% filter(Borough == "MN")

wfHarlem <- wf %>% filter(store == "Whole Foods Harlem") %>% sf::st_transform(2263)
wfHarlemBufferHalf <- wfHarlem %>% sf::st_buffer(2640)

wfHarlemBufferHalf <- wfHarlem %>% sf::st_buffer(2640*1.2) # testing quarter
wfHarlemBufferOne <- wfHarlem %>% sf::st_buffer(2640*2)

plutoWF <- pluto %>% filter(sf::st_intersects(pluto, wfHarlemBufferOne,sparse = FALSE)) %>% 
    select(BoroName, Block) %>% 
    group_by(Block) %>% 
    summarize(BoroName = first(BoroName)) %>% 
    sf::st_cast() %>% 
    ungroup() %>% 
    filter(!Block %in% c(1111, 1897)) %>% 
    sf::st_join(wfHarlemBufferHalf %>% mutate(buffer = "Half"),join =sf::st_within) %>%  
    dplyr::mutate(buffer = if_else(is.na(buffer), "Mile", buffer),
                  Block = as.character(Block))
mnSalesJoin <- readr::read_csv(file.path(dataDir, "modelling/allSalesForModel.csv")) %>% 
    mutate(rowid = row_number(),
           block = as.character(block)) %>% 
    filter(sale_price > 50000, sale_price < 10000000,
           tax_class_at_time_of_sale %in% c(1,2),
           str_starts(building_class_at_present, "A|B|C|D|R|S")) %>% 
    left_join(plutoWF %>% sf::st_drop_geometry() %>% 
                  select(Block, buffer) %>% 
                  rename(block = Block))

streeteasy <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1j9HIF-DY08jo0tyi_Rl6AQ3vkTcwlq9DvSo5znpgXyc",sheet = 1)

newDF <- tibble(block = streeteasy$block,
                lot = streeteasy$lot,
                sale_price = streeteasy$sale_price,
                sale_date = streeteasy$sale_date,
                beds = streeteasy$beds,
                baths = streeteasy$baths,
                sqft = streeteasy$sqft)%>% 
    mutate(sale_date = lubridate::as_datetime(sale_date)) 

imputeDF <- newDF %>% 
    group_by(block, lot) %>% 
    #tidyr::fill(beds, baths, sqft) %>%  
    tidyr::fill(beds, baths, sqft, .direction = "up") %>% 
    ungroup()

library(caret)

preProcValues <- preProcess(imputeDF %>% 
                                #dplyr::select(beds, baths, sqft) %>%
                                as.data.frame(),
                            method = c("knnImpute"),
                            k = 5,
                            knnSummary = mean)
imputeDF2 <- predict(object = preProcValues, newdata = imputeDF %>% as.data.frame(), na.action = na.pass)

procNames <- data.frame(col = names(preProcValues$mean), mean = preProcValues$mean, sd = preProcValues$std)

for(i in procNames$col){
    imputeDF2[i] <- imputeDF2[i]*preProcValues$std[i]+preProcValues$mean[i] 
}

newDF2 <- imputeDF2 %>% 
    mutate_at(vars(block, lot, sale_date), as.character) %>% 
    mutate(baths = ceiling(baths / 0.5) * 0.5,
           beds = round(beds))

mnSalesFinal <- mnSalesJoin %>% 
    mutate_at(vars(block, lot, sale_date), as.character) %>% 
    left_join(newDF2, by = c("block" = "block", 
                             "lot" = "lot",
                             "sale_date" = "sale_date",
                             "sale_price" = "sale_price"))
## Different Distances
#####
distanceList <- as.list(seq(1320,4620, 660))

bufferFunction <- function(buffer, data, wf){
    varName  <- paste0("buffer_", buffer)
    wfBuffer <- wf %>% sf::st_buffer(buffer)
    
    output <- data %>% 
        filter(!Block %in% c(1111, 1897)) %>% 
        sf::st_join(wfBuffer %>% mutate(!!varName := 1),join =sf::st_within) %>%  
        sf::st_drop_geometry() %>% 
        select(!!varName)
}

distanceBuffers <- map_dfc(distanceList, ~bufferFunction(.x, plutoWF, wfHarlem))
distanceBuffers <- distanceBuffers %>% replace(is.na(.), 0)

harlemDFScale <- mnSalesFinal  %>% 
    select(geoid, sale_date, sale_price, bbl,block,rowid,
           'land_square_feet', 'gross_square_feet', 'flagshipParkDist',
           'parkDist', 'subwayDist', 'libDistance', 'totPermits',
           'HispanicLatinoPercent', 'whitePercent','blackPercent',
           'BAPlusPercent', 'ownerPercent', 'median_gross_rent', 
           'median_household_income_in_2019_inflation_adjusted_dollars',
           'total_units','beds', 'baths', 'sqft') %>% 
    mutate_at(vars(
        'land_square_feet', 'gross_square_feet', 'flagshipParkDist',
        'parkDist', 'subwayDist', 'libDistance', 'totPermits','median_gross_rent', 
        'median_household_income_in_2019_inflation_adjusted_dollars',
        'total_units','beds', 'baths', 'sqft'), ~(scale(.) %>% as.vector)) 
# %>% filter(complete.cases(.))

joinBuffer <- harlemDFScale  %>% 
    left_join(plutoWF %>% bind_cols(distanceBuffers) %>% sf::st_drop_geometry() %>% distinct(.), by = c("block"="Block")) %>% 
    group_by(block) %>% 
    filter(complete.cases(sale_price, beds, baths)) %>% 
    tidyr::fill(flagshipParkDist, parkDist, subwayDist, libDistance,
                totPermits, median_gross_rent,total_units,
                median_household_income_in_2019_inflation_adjusted_dollars,
                .direction = "downup") %>% 
    ungroup()


### actual part changed

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


hedonicMod <- lm(log(sale_price)~ flagshipParkDist+
   parkDist+ subwayDist+ libDistance+ totPermits+
   HispanicLatinoPercent+ whitePercent+blackPercent+
   BAPlusPercent+ ownerPercent+ median_gross_rent+ 
   median_household_income_in_2019_inflation_adjusted_dollars+ 
   total_units+beds+ baths+ sqft + buffer_1320 + buffer_1980  + buffer_2640 +
       buffer_3300 + buffer_3960, data = cleanedDF)

dndMod <- lm(log(sale_price)~ flagshipParkDist+
                     parkDist+ subwayDist+ libDistance+ totPermits+
                     HispanicLatinoPercent+ whitePercent+blackPercent+
                     BAPlusPercent+ ownerPercent+ median_gross_rent+ 
                     median_household_income_in_2019_inflation_adjusted_dollars+ 
                     total_units+beds+ baths+ sqft + buffer_1320 + buffer_1980  + buffer_2640 +
                 buffer_3300 + buffer_3960 + 
                 (buffer_1320 + buffer_1980  + buffer_2640 +
                     buffer_3300 + buffer_3960) : post, data = cleanedDF)

dndMod <- model.matrix(log(sale_price)~  sqft + 
                 (buffer_1320 + buffer_1980  + buffer_2640 +
                      buffer_3300 + buffer_3960) * post, data = cleanedDF)


dndMod <- lm(log(sale_price)~ flagshipParkDist+
                 parkDist+ subwayDist+ libDistance+ totPermits+
                 HispanicLatinoPercent+ whitePercent+blackPercent+
                 BAPlusPercent+ ownerPercent+ median_gross_rent+ 
                 median_household_income_in_2019_inflation_adjusted_dollars+ 
                 total_units+beds+ baths+ sqft + (buffer_1320 + buffer_1980  + buffer_2640 +
                                                      buffer_3300 + buffer_3960) * post, data = cleanedDF)
summary(dndMod)

summary(hedonicMod)

dndRes <- resid(dndMod)

plot(fitted(dndMod), dndRes)

qqnorm(dndRes)
qqline(dndRes)



### comparison table

compdf <- mnSalesJoin %>% 
    mutate_at(vars(block, lot, sale_date), as.character) %>% 
    left_join(imputeDF%>% 
                  mutate_at(vars(block, lot, sale_date), as.character), by = c("block" = "block", 
                             "lot" = "lot",
                             "sale_date" = "sale_date",
                             "sale_price" = "sale_price"))  %>% 
    select(geoid, sale_date, sale_price, bbl,block,rowid,
           'land_square_feet', 'gross_square_feet', 'flagshipParkDist',
           'parkDist', 'subwayDist', 'libDistance', 'totPermits',
           'HispanicLatinoPercent', 'whitePercent','blackPercent',
           'BAPlusPercent', 'ownerPercent', 'median_gross_rent', 
           'median_household_income_in_2019_inflation_adjusted_dollars',
           'total_units','beds', 'baths', 'sqft')  %>% 
    left_join(plutoWF %>% bind_cols(distanceBuffers) %>% sf::st_drop_geometry() %>% distinct(.), by = c("block"="Block")) %>% 
    group_by(block) %>% 
    filter(complete.cases(sale_price, beds, baths)) %>% 
    tidyr::fill(flagshipParkDist, parkDist, subwayDist, libDistance,
                totPermits, median_gross_rent,total_units,
                median_household_income_in_2019_inflation_adjusted_dollars,
                .direction = "downup") %>% 
    ungroup() %>% 
    mutate(sum_col = buffer_1320+buffer_1980+buffer_2640+
               buffer_3300 + buffer_3960+buffer_4620) %>%
    mutate(buffer = case_when(
        sum_col == 6 ~ "1320",
        sum_col == 5 ~ "1980",
        sum_col == 4 ~ "2640",
        sum_col == 3 ~ "3300",
        sum_col == 2 ~ "3960",
        sum_col == 1 ~ "4620",
        sum_col == 0 ~ "0"
    )) %>% 
    mutate(post = sale_date >= 2017)


compdf %>% group_by(buffer, post) %>% 
    summarize(medSalePrice = median(sale_price),
              medsqft = median(sqft, na.rm = TRUE))

## removing the 0-1/4 mile
sumDF <- cleanedDF %>% 
    mutate(sum_col = buffer_1320+buffer_1980+buffer_2640+
               buffer_3300 + buffer_3960) %>% 
    filter(sum_col >0)
dndMod_subset <- lm(log(sale_price)~ flagshipParkDist+
                 parkDist+ subwayDist+ libDistance+ totPermits+
                 HispanicLatinoPercent+ whitePercent+blackPercent+
                 BAPlusPercent+ ownerPercent+ median_gross_rent+ 
                 median_household_income_in_2019_inflation_adjusted_dollars+ 
                 total_units+beds+ baths+ sqft + (buffer_1320 + buffer_1980  + buffer_2640 +
                                                      buffer_3300 + buffer_3960) * post, data = sumDF)

summary(dndMod_subset)

## half mile or not

# 2016
simpleDF2015 <- joinBuffer %>% 
    mutate(post = sale_date >= 2015) %>%
    select(sale_price , flagshipParkDist,
           parkDist, subwayDist, libDistance, totPermits,
           HispanicLatinoPercent, whitePercent,blackPercent,
           BAPlusPercent, ownerPercent, median_gross_rent,
           median_household_income_in_2019_inflation_adjusted_dollars,
           total_units,beds, baths, sqft , buffer_1320, buffer_1980, buffer_2640,
           buffer_3300, buffer_3960, sale_date, post) %>%
    filter(complete.cases(.)) %>% 
    mutate(halfMile = if_else(buffer_2640 == 1, 0,1))

## Permits

permits <- read_csv(file.path(dataDir, "DOB_Permit_Issuance2010_2020.csv"))

permits <- permits %>% 
    janitor::clean_names()%>% 
    select(borough, job_type,
           block, lot, permit_type,
           filing_date, latitude, longitude) %>% 
    mutate(filing_date = lubridate::mdy_hms(filing_date),
           year = lubridate::year(filing_date),
           month = lubridate::month(filing_date))

permitsMN <- permits %>% filter(borough == "MANHATTAN")
rm(permits)

permitsWF <- permitsMN %>% mutate(block = as.numeric(block),
                                  block = as.character(block)) %>%
    rename(Block = block) %>% 
    left_join(plutoWF %>% sf::st_drop_geometry()) %>% 
    filter(!is.na(buffer))

permitsSum <- permitsWF %>% group_by()
    

## Figure 1
## Log counts of housing transactions


countsDF <- joinBuffer %>% 
    mutate(sum_col = buffer_1320+buffer_1980+buffer_2640+
               buffer_3300 + buffer_3960+buffer_4620) %>%
    mutate(buffer_1320 = if_else(sum_col == 6, 1, 0),
           buffer_1980 = if_else(sum_col == 5, 1, 0),
           buffer_2640 = if_else(sum_col == 4, 1, 0),
           buffer_3300 = if_else(sum_col == 3, 1, 0),
           buffer_3960 = if_else(sum_col == 2, 1, 0),
           post = sale_date >= 2014
           ) %>%
    mutate(post = sale_date >= 2017) %>%
    select(sale_price , flagshipParkDist,
           parkDist, subwayDist, libDistance, totPermits,
           HispanicLatinoPercent, whitePercent,blackPercent,
           BAPlusPercent, ownerPercent, median_gross_rent,
           median_household_income_in_2019_inflation_adjusted_dollars,
           total_units,beds, baths, sqft , buffer_1320, buffer_1980, buffer_2640,
           buffer_3300, buffer_3960, sale_date, post,sum_col) %>%
    filter(complete.cases(.))


combCounts <- countsDF %>% 
    select(c(buffer_1320:sale_date, sum_col)) %>% 
    mutate(Date = ymd(sale_date),
           Year = year(Date),
           Month = month(Date),
           Quarter = quarter(Date)) %>% 
    pivot_longer(-c(sale_date, Date, Year, Month, Quarter, sum_col)) %>% 
    filter(value == 1) %>% bind_rows(
countsDF %>% 
    select(c(buffer_1320:sale_date, sum_col)) %>% 
    mutate(Date = ymd(sale_date),
           Year = year(Date),
           Month = month(Date),
           Quarter = quarter(Date)) %>% 
    pivot_longer(-c(sale_date, Date, Year, Month, Quarter, sum_col)) %>% 
    filter(sum_col == 1))

# by quarter
combCounts %>% group_by(Year, Quarter, name) %>% 
    summarize(Date = min(Date),
              count = n(),
              logCount = log(count)) %>% 
    ggplot(aes(x = Date, y = logCount, group = name)) +
    geom_line(aes(color = name)) +
    geom_point()

# by year
combCounts %>% group_by(Year, name) %>% 
    summarize(Date = min(Date),
              count = n(),
              logCount = log(count)) %>% 
    ggplot(aes(x = Date, y = logCount, group = name)) +
    geom_line(aes(color = name)) +
    geom_point(shape = 1) + theme_minimal()


## Figure 2
## residual plot of log price regression 
## residuals aggreagation

cleanedDF$res1 <- dndMod$residuals

# by quarter
cleanedDF %>% 
    select(buffer_1320,buffer_1980,buffer_2640, buffer_3300, buffer_3960,
           sale_price, sale_date, res1) %>% 
    tidyr::pivot_longer(-c(sale_price, sale_date, res1), names_to = "buffer", values_to = "val") %>% 
    filter(val ==1) %>% 
    mutate(quarter = quarter(sale_date),
           year = year(sale_date),
           year_quarter = paste0(year,"_", quarter)) %>% 
    group_by(year_quarter, buffer) %>% 
    summarize(avgRes = mean(res1),
              medRes = median(res1),
              date = first(sale_date)) %>% 
    select(date, medRes, avgRes, buffer) %>% 
    ggplot(aes(x = date, y = medRes, group = buffer, color = buffer)) +
    geom_point() +
    geom_line()

## by year
cleanedDF %>% 
    select(buffer_1320,buffer_1980,buffer_2640, buffer_3300, buffer_3960,
           sale_price, sale_date, res1) %>% 
    tidyr::pivot_longer(-c(sale_price, sale_date, res1), names_to = "buffer", values_to = "val") %>% 
    filter(val ==1) %>% 
    mutate(
        year = year(sale_date)) %>% 
    group_by(year, buffer) %>% 
    summarize(avgRes = mean(res1),
              medRes = median(res1)) %>% 
    select(year, medRes, avgRes, buffer) %>% 
    ggplot(aes(x = year, y = medRes, group = buffer)) +
    geom_line( aes(color = buffer)) + 
    geom_point(shape = 1) +
    theme_minimal()
