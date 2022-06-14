library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(sf)
library(leaflet)
library(hpiR)
library(purrr)
library(googlesheets4)


root <- rprojroot::find_rstudio_root_file()
dataDir <- file.path(root, 'data')
shapeDir <- file.path(root, 'shape')

## whole foods
wf <- st_read(file.path(shapeDir, "wholeFoods"))
pluto <- st_read(file.path(shapeDir, "PLUTO_Dissolve")) %>% filter(Borough == "MN")

wfHarlem <- wf %>% filter(store == "Whole Foods Harlem") %>% st_transform(2263)
wfHarlemBufferHalf <- wfHarlem %>% st_buffer(2640)

wfHarlemBufferHalf <- wfHarlem %>% st_buffer(2640*1.2) # testing quarter
wfHarlemBufferOne <- wfHarlem %>% st_buffer(2640*2)

plutoWF <- pluto %>% filter(st_intersects(pluto, wfHarlemBufferOne,sparse = FALSE)) %>% 
  select(BoroName, Block) %>% 
  group_by(Block) %>% 
  summarize(BoroName = first(BoroName)) %>% 
  st_cast() %>% 
  ungroup() %>% 
  filter(!Block %in% c(1111, 1897)) %>% 
  sf::st_join(wfHarlemBufferHalf %>%mutate(buffer = "Half"),join =st_within) %>%  
  dplyr::mutate(buffer = if_else(is.na(buffer), "Mile", buffer))

## sales
# for building classes https://www.propertyshark.com/mason/text/nyc_building_class.html
mnSalesJoin <- readr::read_csv(file.path(dataDir, "modelling/allSalesForModel.csv")) %>% 
    mutate(rowid = row_number()) %>% 
    filter(sale_price > 50000, sale_price < 10000000,
           tax_class_at_time_of_sale %in% c(1,2),
           str_starts(building_class_at_present, "A|B|C|D|R|S")) %>% 
  left_join(plutoWF %>% st_drop_geometry() %>% 
              select(Block, buffer) %>% 
              rename(block = Block))

streeteasy <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1ZA8rXr-K5LukwLhQB2t4pQ-RzUOJWm9_4eazumDAfqk/edit?usp=sharing",sheet = 1)



newDF <- tibble(block = streeteasy$block,
                lot = streeteasy$lot,
                sale_price = streeteasy$sale_price,
                sale_date = streeteasy$sale_date,
                beds = streeteasy$beds,
                baths = streeteasy$baths,
                sqft = streeteasy$sqft)%>% 
  mutate(sale_date = lubridate::as_datetime(sale_date))


mnSalesJoin <- mnSalesJoin %>% left_join(newDF, by = c("block" = "block", "lot" = "lot", "sale_price" = "sale_price",
                                        "sale_date" = "sale_date"))

wfDF <- mnSalesJoin %>% filter(buffer == "Half")
notwfDF <- mnSalesJoin %>% filter(buffer == "Mile")

sales_hhdfwf <- hedCreateTrans(trans_df = wfDF,
                             prop_id = 'bbl',
                             trans_id = 'rowid',
                             price = 'sale_price',
                             date= 'sale_date',
                             periodicity = 'quarterly')
sales_hhdfnotwf <- hedCreateTrans(trans_df = notwfDF,
                               prop_id = 'bbl',
                               trans_id = 'rowid',
                               price = 'sale_price',
                               date= 'sale_date',
                               periodicity = 'quarterly')


# sale_price ~ 
#     land_square_feet + gross_square_feet + year_built  + flagshipParkDist + parkDist + subwayDist +
#     libDistance + popBlack/TotPop + popHisp/TotPop + avgHHsize + MedHHI + OwnerOccUnits/TotOccUnits + 
#     OO_20k + OO20_49k + OO50_99k + OO50_99k + OO100_149k + OO150_299k + OO300_499k + OO_500_749k + 
#     OO750_999k + OO1mil_ + permits 

hed_modelwf <- hpiModel(model_type = 'hed',
                      hpi_df = sales_hhdfwf,
                      estimator = 'base',
                      dep_var = 'price',
                      ind_var = c('land_square_feet', 'gross_square_feet', 'flagshipParkDist',
                                  'parkDist', 'subwayDist', 'libDistance', 'totPermits',
                                  'HispanicLatinoPercent', 'whitePercent','blackPercent',
                                  'BAPlusPercent', 'ownerPercent', 'median_gross_rent', 
                                  'median_household_income_in_2019_inflation_adjusted_dollars', 
                                  'total_units', 'beds', 'baths', 'sqft'),
                      log_dep = TRUE)

hed_modelnotwf <- hpiModel(model_type = 'hed',
                      hpi_df = sales_hhdfnotwf,
                      estimator = 'base',
                      dep_var = 'price',
                      ind_var =c('land_square_feet', 'gross_square_feet', 'flagshipParkDist',
                                 'parkDist', 'subwayDist', 'libDistance', 'totPermits',
                                 'HispanicLatinoPercent', 'whitePercent','blackPercent',
                                 'BAPlusPercent', 'ownerPercent', 'median_gross_rent', 
                                 'median_household_income_in_2019_inflation_adjusted_dollars',
                                 'total_units', 'beds', 'baths', 'sqft'),
                      log_dep = TRUE)
hed_indexwf <- modelToIndex(model_obj = hed_modelwf)
hed_indexnnonwf <- modelToIndex(model_obj = hed_modelnotwf)

plot(hed_indexwf)
plot(hed_indexnnonwf)

bind_rows(tibble(val = hed_indexnnonwf$value, type = "not",
       time = hed_indexnnonwf$numeric),
      tibble(val = hed_indexwf$value, type = "wf",
             time = hed_indexwf$numeric)) %>% 
    ggplot(aes(time, val, group = type, color = type)) +
    geom_line()+
    geom_smooth() + 
  geom_vline(xintercept = 2017)

## for year
bind_rows(tibble(val = hed_indexnnonwf$value, type = "not",
                 time = as.numeric(hed_indexnnonwf$name)),
          tibble(val = hed_indexwf$value, type = "wf",
                 time = as.numeric(hed_indexwf$name))) %>% 
  ggplot(aes(time, val, group = type, color = type)) +
  geom_line()+
  geom_smooth() + 
  geom_vline(xintercept = 2017)

hed_3 <- hedIndex(trans_df = sales_hhdfwf,
                  estimator = 'base',
                  log_dep = TRUE,
                  trim_model = FALSE,
                  dep_var = 'price',
                  ind_var = c( 'flagshipParkDist', 'parkDist', 'subwayDist', 
                              'libDistance', 'year_built', 'residential_units', 'gross_square_feet',
                              'beds', 'baths', 'sqft'),
                  smooth = TRUE,
                  smooth_order = 5)
plot(hed_3, smooth = TRUE)


## Different Distances
#####
distanceList <- as.list(seq(1320,4620, 660))

bufferFunction <- function(buffer, data, wf){
  varName  <- paste0("buffer_", buffer)
  wfBuffer <- wf %>% st_buffer(buffer)
  
  output <- data %>% 
    filter(!Block %in% c(1111, 1897)) %>% 
    sf::st_join(wfBuffer %>%mutate(!!varName := 1),join =st_within) %>%  
    st_drop_geometry() %>% 
    select(!!varName)
}

distanceBuffers <- map_dfc(distanceList, ~bufferFunction(.x, plutoWF, wfHarlem))
distanceBuffers <- distanceBuffers %>% replace(is.na(.), 0)

harlemDFScale <- mnSalesJoin  %>% 
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
    'total_units','beds', 'baths', 'sqft'), ~(scale(.) %>% as.vector)) %>% 
  filter(complete.cases(.))

joinBuffer <- harlemDFScale  %>% 
  left_join(plutoWF %>% bind_cols(distanceBuffers) %>% st_drop_geometry %>% distinct(.), by = c("block"="Block"))


bufferHPI <- function(buffer, df,period = "yearly"){
  bufferCol = paste0("buffer_", buffer)
  sales_hhdfwf <- hedCreateTrans(trans_df = df %>% filter(get(bufferCol)==1),
                                 prop_id = 'bbl',
                                 trans_id = 'rowid',
                                 price = 'sale_price',
                                 date= 'sale_date',
                                 periodicity = period)
  sales_hhdfnotwf <- hedCreateTrans(trans_df = df %>% filter(get(bufferCol)==0),
                                    prop_id = 'bbl',
                                    trans_id = 'rowid',
                                    price = 'sale_price',
                                    date= 'sale_date',
                                    periodicity = period)
  hed_modelwf <- hpiModel(model_type = 'hed',
                          hpi_df = sales_hhdfwf,
                          estimator = 'base',
                          dep_var = 'price',
                          ind_var = c('land_square_feet', 'gross_square_feet', 'flagshipParkDist',
                                      'parkDist', 'subwayDist', 'libDistance', 'totPermits',
                                      'HispanicLatinoPercent', 'whitePercent','blackPercent',
                                      'BAPlusPercent', 'ownerPercent', 'median_gross_rent', 
                                      'median_household_income_in_2019_inflation_adjusted_dollars', 
                                      'total_units','beds', 'baths', 'sqft'),
                          log_dep = TRUE)
  
  hed_modelnotwf <- hpiModel(model_type = 'hed',
                             hpi_df = sales_hhdfnotwf,
                             estimator = 'base',
                             dep_var = 'price',
                             ind_var =c('land_square_feet', 'gross_square_feet', 'flagshipParkDist',
                                        'parkDist', 'subwayDist', 'libDistance', 'totPermits',
                                        'HispanicLatinoPercent', 'whitePercent','blackPercent',
                                        'BAPlusPercent', 'ownerPercent', 'median_gross_rent', 
                                        'median_household_income_in_2019_inflation_adjusted_dollars',
                                        'total_units','beds', 'baths', 'sqft'),
                             log_dep = TRUE)
  hed_indexwf <- modelToIndex(model_obj = hed_modelwf)
  hed_indexnnonwf <- modelToIndex(model_obj = hed_modelnotwf)
list("bufferCol" = bufferCol,
     "hed_indexwf" = hed_indexwf,
     "hed_indexnnonwf" = hed_indexnnonwf)
}

dataFunc <- function(data, period = "yearly"){
  if(period == "yearly"){
    ## for year
    bind_rows(tibble(val = data$hed_indexnnonwf$value, type = "not",
                     time = as.numeric(data$hed_indexnnonwf$name)),
              tibble(val = data$hed_indexwf$value, type = "wf",
                     time = as.numeric(data$hed_indexwf$name))) 
  }else {
    bind_rows(tibble(val = data$hed_indexnnonwf$value, type = "not",
                     time = data$hed_indexnnonwf$numeric),
              tibble(val = data$hed_indexwf$value, type = "wf",
                     time = data$hed_indexwf$numeric)) 
  }
}
plotFunc <- function(data, treatYear, period = "yearly"){
  
data%>% 
      ggplot(aes(time, val, group = type, color = type)) +
      geom_line()+
      geom_smooth() + 
      geom_vline(xintercept = treatYear)+
      ggtitle(paste(data$bufferCol, period))
    
}

library(lfe)

dndFunc <- function(data, treatyear){
  data %>% 
    mutate(treat = if_else(type =="wf", 1, 0),
           year = if_else(time >=treatyear, 1, 0)) %>% 
    felm(formula = val~time*treat | year,  data = .) %>%
    summary(robust = TRUE)
}

hpiCheck <- tidyr::crossing(treatbuffer = seq(1320,4620, 660), treatYear = seq(2015, 2018))

testhpi <- hpiCheck %>% 
  mutate(data1 = purrr::map(treatbuffer, ~bufferHPI(.x, joinBuffer))) %>% 
  mutate(
  dataClean = purrr::map(data1, ~dataFunc(.x)),
         plot = purrr::map2(dataClean,treatYear, ~plotFunc(.x,.y)),
  mod = purrr::map2(dataClean, treatYear, ~dndFunc(.x, .y)),
  coef = purrr::map_dbl(mod, ~.x$coefficients[3,1]),
  pval = purrr::map_dbl(mod, ~.x$coefficients[3,4]))

print(testhpi, n = 24)

testhpi %>% 
  ggplot(aes(x = pval, y = coef, color = as.factor(treatYear))) +
  geom_point() +
  facet_wrap(~treatbuffer)

plots[[2]]$data %>% 
  mutate(treat = if_else(type =="wf", 1, 0),
         year = if_else(time >2014, 1, 0)) %>% 
felm(formula = val~time*treat | year,  data = .) %>%
  summary(robust = TRUE)


plots[[3]]$data %>% 
  mutate(treat = if_else(type =="wf", 1, 0),
         year = if_else(time >2016, 1, 0)) %>% 
  group_by(treat, year) %>% 
  summarize(val = mean(val))

# control
101-97.5
# treatment
99.8-75.6
# control
(101-97.5) - (99.8-75.6)

plots[[1]]$data %>% 
  mutate(treat = if_else(type =="wf", 1, 0),
         year = if_else(time >2016, 1, 0)) %>% 
  felm(formula = val~time*treat | year,  data = .) %>%
  summary(robust = TRUE)


plots[[1]]$data %>% 
  mutate(treat = if_else(type =="wf", 1, 0),
         year = if_else(time >2016, 1, 0)) %>% 
  group_by(treat, year) %>% 
  summarize(val = mean(val))

# control
101-97.9
# treatment
100-95

(100-95)-(101-97.9)


## cutting off at 2011 gives better parallel trend
plots[[2]]$data %>% 
  mutate(treat = if_else(type =="wf", 1, 0),
         year = if_else(time >2016, 1, 0)) %>% 
  filter(time > 2011) %>% 
  felm(formula = val~time*treat | year,  data = .) %>%
  summary(robust = TRUE)

plots[[3]]$data %>% 
  mutate(treat = if_else(type =="wf", 1, 0),
         year = if_else(time >2016, 1, 0)) %>% 
  filter(time > 2011) %>% 
  felm(formula = val~time*treat | year,  data = .) %>%
  summary(robust = TRUE)

plots[[4]]$data %>% 
  mutate(treat = if_else(type =="wf", 1, 0),
         year = if_else(time >2016, 1, 0)) %>% 
  filter(time > 2012) %>% 
  felm(formula = val~time*treat | year,  data = .) %>%
  summary(robust = TRUE)
