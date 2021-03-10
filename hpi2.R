library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(sf)
library(leaflet)
library(hpiR)

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
           #str_starts(building_class_category, "0|1"),
           str_starts(building_class_at_present, "A|B|C|D|R|S")) %>% 
  left_join(plutoWF %>% st_drop_geometry() %>% 
              select(Block, buffer) %>% 
              rename(block = Block))

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
                                  'median_household_income_in_2019_inflation_adjusted_dollars', 'total_units'),
                      log_dep = TRUE)

hed_modelnotwf <- hpiModel(model_type = 'hed',
                      hpi_df = sales_hhdfnotwf,
                      estimator = 'base',
                      dep_var = 'price',
                      ind_var =c('land_square_feet', 'gross_square_feet', 'flagshipParkDist',
                                 'parkDist', 'subwayDist', 'libDistance', 'totPermits',
                                 'HispanicLatinoPercent', 'whitePercent','blackPercent',
                                 'BAPlusPercent', 'ownerPercent', 'median_gross_rent', 
                                 'median_household_income_in_2019_inflation_adjusted_dollars','total_units'),
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