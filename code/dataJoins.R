library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(sf)

nyct <- st_read("shape/nyct2010/") %>% st_drop_geometry() %>% 
    mutate_if(is.factor, as.character)

plutoDissolve <- st_read("shape/PLUTO_Dissolve/") %>% 
    mutate_if(is.factor, as.character)

plutoJoin <- left_join(plutoDissolve, nyct %>% select(CT2010, Borough, Geo_FIPS), by = c("CT2010_y" = "CT2010",
                                                                                         "Borough" = "Borough"))
# census demographic data
ct18 <- readxl::read_xls("data/census/nyct_2014_2018.xls") %>% 
    mutate(Geo_FIPS = as.character(Geo_FIPS))
ct13 <- readxl::read_xls("data/census/nyct_2009_2013.xls") %>% 
    mutate(Geo_FIPS = as.character(Geo_FIPS))

plutoJoin18  <- plutoJoin %>% left_join(ct18)
plutoJoin13  <- plutoJoin %>% left_join(ct13)


# harlem lots with physical features
harlemlots <- st_read("shape/HarlemSales/HarlemSales.gpkg") %>% 
    filter(borough_name == "Manhattan") %>% 
    rename(BoroName = borough_name,
           Block = block) %>% 
    mutate(BoroName = as.character(BoroName)) %>% 
    select(-ObjID)

mnResSales <- readr::read_csv("data/rollingSales/nycSales_2010_2019") %>% 
    filter(str_detect(`BUILDING CLASS AT TIME OF SALE`,pattern = "[A,B,C,D]")) %>% 
    mutate(`SALE DATE` = as_date(`SALE DATE`),
           borough_name = case_when(BOROUGH == 1 ~ "Manhattan",
                                    BOROUGH == 2 ~ "Bronx",
                                    BOROUGH == 3 ~ "Brooklyn",
                                    BOROUGH == 4 ~ "Queens",
                                    BOROUGH == 5 ~ "Staten Island")) %>% janitor::clean_names() %>% 
    mutate(year = year(sale_date),
           month = month(sale_date),
           borough = as.factor(borough)) %>% 
    filter(borough == 1) %>% 
    rename(Block = block,
           BoroName = borough_name)


harlemSales <- mnResSales %>% left_join(harlemlots, by = c("BoroName" = "BoroName",
                                                           "Block" = "Block",
                                                           "lot" = "lot")) %>% filter(!is.na(lat))

harlemSales13 <- harlemSales %>% filter(year < 2014) %>% left_join(plutoJoin13 %>% st_drop_geometry())
harlemSales18 <- harlemSales %>% filter(year > 2013) %>% left_join(plutoJoin18 %>% st_drop_geometry())
harlemSales <- harlemSales13 %>% bind_rows(harlemSales18)


harlemSalesClean <- harlemSales %>% select(-c(CT2010_y,borough,geom,Borough, apartment_number, ease_ment))


readr::write_csv(harlemSalesClean, "data/harlemSales_Attributes.csv")
