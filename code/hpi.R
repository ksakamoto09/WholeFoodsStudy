library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(sf)
library(leaflet)
library(hpiR)

harlemlots <- st_read("shape/HarlemSales/HarlemSales.gpkg")
harlemlots <- harlemlots %>% filter(borough_name == "Manhattan")
mnSales <- resSales %>% filter(borough == 1)


joinDF <- mnSales %>% left_join(harlemlots) 

joinFilter <- joinDF %>% filter(!is.na(subwayDist))
joinFilter <- joinFilter %>% mutate(qrtr = lubridate::quarter(sale_date),
                      rowid = 1:nrow(joinFilter)) %>% 
    filter(sale_price > 10000)

joinFilter %>%mutate(HalfMile = as.factor(HalfMile)) %>% 
    filter(sale_price > 10000,residential_units < 3, residential_units > 0) %>% 
    group_by(month,year,residential_units,HalfMile) %>% summarize(priceMean = mean(sale_price),
                                                            priceMed = median(sale_price),
                                                            date = min(sale_date)) %>% 
    ggplot(aes(x = date, y = priceMed, color =HalfMile)) + 
    geom_point() +
    geom_line() + 
    facet_grid(HalfMile~residential_units, scales = "free_y") + 
    theme_minimal()

joinFilter %>%mutate(HalfMile = as.factor(HalfMile),
                     qrtr = lubridate::quarter(sale_date)) %>% 
    filter(sale_price > 10000,residential_units < 3, residential_units > 0) %>% 
    group_by(qrtr, year,residential_units,HalfMile) %>% summarize(priceMean = mean(sale_price),
                                                            priceMed = median(sale_price),
                                                            date = min(sale_date)) %>% 
    ggplot(aes(x = date, y = priceMed, color =HalfMile)) + 
    geom_point() +
    geom_line() + 
    facet_grid(HalfMile~residential_units) + 
    theme_minimal()

joinFilter %>%mutate(HalfMile = as.factor(HalfMile)) %>% 
    filter(sale_price > 10000,residential_units < 3, residential_units > 0) %>% 
    group_by(year,residential_units,HalfMile) %>% summarize(priceMean = mean(sale_price),
                                                          priceMed = median(sale_price),
                                                          date = min(sale_date)) %>% 
    ggplot(aes(x = date, y = priceMed, color =HalfMile)) + 
    geom_point() +
    geom_line() + 
    facet_grid(HalfMile~residential_units, scales = "free_y") + 
    theme_minimal()


sales_hhdf <- hedCreateTrans(trans_df = joinFilter,
                             prop_id = 'ObjID',
                             trans_id = 'rowid',
                             price = 'sale_price',
                             date= 'sale_date',
                             periodicity = 'quarterly')


model_spec <- as.formula('log(price) ~ as.factor(HalfMile) + flagshipParkDist + parkDist + subwayDist + 
                         libDistance + year_built + residential_units + gross_square_feet')

hed_model <- hpiModel(model_type = 'hed',
                      hpi_df = sales_hhdf,
                      estimator = 'base',
                      mod_spec = model_spec,
                      log_dep = TRUE)
hed_index <- modelToIndex(model_obj = hed_model)
plot(hed_index)

hed_3 <- hedIndex(trans_df = sales_hhdf,
                  estimator = 'base',
                  log_dep = TRUE,
                  trim_model = FALSE,
                  dep_var = 'price',
                  ind_var = c('as.factor(HalfMile)', 'flagshipParkDist', 'parkDist', 'subwayDist', 
                                  'libDistance', 'year_built', 'residential_units', 'gross_square_feet'),
                  smooth = TRUE,
                  smooth_order = 5)
plot(hed_3, smooth = TRUE)


# Returns an accuracy object in place
hed_hpi <- calcAccuracy(hpi_obj = hed_3,
                        test_type = 'rt',
                        test_method = 'insample',
                        pred_df = joinFilter,
                        in_place = TRUE,
                        in_place_name = 'is_accuracy')
names(hed_hpi)
## [1] "data"  "model" "index"

# Returns a smooth accuracy object in place
hed_hpi <- calcAccuracy(hpi_obj = hed_3,
                        test_type = 'rt',
                        test_method = 'insample',
                        smooth = TRUE,
                        pred_df = rt_hpi$data,
                        in_place = TRUE,
                        in_place_name = 'smooth_is_accuracy')