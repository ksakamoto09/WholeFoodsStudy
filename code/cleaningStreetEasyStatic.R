library(dplyr)
library(readr)
library(fuzzyjoin)
library(sf)
library(stringr)
library(tidyr)
createURL <- function(address, aptNum){
    ## check if apt number is there
    if(!is.na(aptNum)){
        url <- paste0("http://streeteasy.com/building/", address, "-new_york-", aptNum, ".html") 
    }else{ 
        url <- paste0("http://streeteasy.com/building/", address, "-new_york", ".html") # just use address
    }
    url
}


se1 <- read_csv("~/Downloads/Street Easy Data/sales_listings.1.csv")
se2 <- read_csv("~/Downloads/Street Easy Data/sales_listings.2.csv")
se3 <- read_csv("~/Downloads/Street Easy Data/sales_listings.3.csv")
se4 <- read_csv("~/Downloads/Street Easy Data/sales_listings.4.csv")

seAll <- se1 %>% filter(stringr::str_detect(neighborhood, "harlem")) %>% 
    bind_rows(se2 %>% filter(stringr::str_detect(neighborhood, "harlem"))) %>% 
    bind_rows(se3 %>% filter(stringr::str_detect(neighborhood, "harlem"))) %>% 
    bind_rows(se4 %>% filter(stringr::str_detect(neighborhood, "harlem")))
rm(se1)
rm(se2)
rm(se3)
rm(se4)

root <- rprojroot::find_rstudio_root_file()
dataDir <- file.path(root, 'data')
shapeDir <- file.path(root, 'shape')

mnSales <- read_csv(file.path(dataDir, "rollingSales","nycSales_2010_2019")) %>% 
    filter(BOROUGH == 1) %>% janitor:::clean_names()

wf <- st_read(file.path(shapeDir, "wholeFoods")) %>% 
    filter(store == "Whole Foods Harlem") %>% st_transform(2263)
wfHarlemBufferOne <- wf %>% st_buffer(2640*2)

pluto <- st_read(file.path(shapeDir, "PLUTO_Dissolve")) %>% filter(Borough == "MN")

plutoWF <- pluto %>% filter(st_intersects(pluto, wfHarlemBufferOne,sparse = FALSE)) %>% 
    select(BoroName, Block) %>% 
    group_by(Block) %>% 
    summarize(BoroName = first(BoroName)) %>% 
    st_cast() %>% 
    ungroup()

mnSalesJoin <- mnSales %>%
    left_join(plutoWF %>% janitor:::clean_names() %>% st_drop_geometry(), by = c("block" = "block")) %>% 
    filter(!is.na(boro_name)) %>% 
    filter(sale_price > 50000, sale_price < 10000000,
           #str_starts(building_class_category, "0|1"),
           str_starts(building_class_at_present, "A|B|C|D|R|S")) %>% 
    tidyr::separate(address,  c("address","aptNum"), ",") %>%
    mutate(aptNum = if_else(is.na(apartment_number), aptNum, apartment_number))

# original join
joinTest <- mnSalesJoin %>% slice() %>% rowwise() %>% 
    mutate(url = createURL(address, aptNum, apartment_number)) %>% 
    ungroup() %>% 
    left_join(seAll, by = c("url"="url"))

seAll$url[1]

joinTest %>% filter(!is.na(gps_coordinates)) %>% View()

## cleaning

seClean <- seAll %>%
    mutate(address = str_remove_all(string = address, '[#,.]'),
                 address  = str_replace(address,"_e_", "_east_"),
                 address  = str_replace(address,"_w_", "_west_"),
                 address  = str_replace(address,"mount", "mt"),
                 address  = str_replace(address,"saint", "st"),
                 address = str_replace(address, "_5_", "_fifth_"),
                 address = str_replace(address, "_5th_", "_fifth_"),
                 address = str_replace(address, "boulevard", "blvd"),
                 address = str_replace(address, "_bl_", "_blvd_"),
                 address = str_replace(address, "adam_c_", "adam_clayton_"),
                 address = str_replace(address, "_fred_", "_frederick_"),
                 address = str_replace(address, "_8_", "_frederick_douglass_blvd_"),
                 address = str_replace(address, "_avenue_", "_ave_"),
                 address = str_replace(address, "_av_", "_ave_"),
                 address = str_replace(address, "_3_", "_third_"),
                 address = str_replace(address, "_3rd_", "_third_")) %>% 
    separate(address, into = c("address", "aptNum"), "_#") %>% 
    mutate(url = if_else(is.na(aptNum),
                         paste0("http://streeteasy.com/building/", address, "-new_york.html"),
                         paste0("http://streeteasy.com/building/", address, "-new_york-", aptNum, ".html") 
    )) %>% 
    separate(address, c("stNum", "stRest"), sep = "_", extra = "merge") %>%
    mutate(stRest = paste0("_",stRest)) %>%
    mutate(stRestNum = str_extract(stRest, "\\d+"),
           stLastNum = sapply(str_split(stRest, "_"), function(x)x%>% rev() %>% "["(1)))

rsClean <- mnSalesJoin %>% 
    mutate(
        address= address %>% 
            # str_replace_all(" ", "_") %>%
            #             str_replace("___", "_") %>%
                            str_to_lower(),
           aptNum = aptNum %>% str_to_lower() %>% str_trim(),
                apartment_number = apartment_number %>% str_to_lower() %>% str_trim(),
                #address = if_else(!is.na(apartment_number), paste0(address, "_", apartment_number), address),
                address = str_remove_all(string = address, '[#,.]'),
                address  = str_replace(address,"_e_", "_east_"),
                address  = str_replace(address,"_w_", "_west_"),
                address  = str_replace(address,"mount", "mt"),
                address  = str_replace(address,"saint", "st"),
                address = str_replace(address, "_5_", "_fifth_"),
                address = str_replace(address, "_5th_", "_fifth_"),
                address = str_replace(address, "boulevard", "blvd"),
                address = str_replace(address, "_bl_", "_blvd_"),
                address = str_replace(address, "adam_c_", "adam_clayton_"),
                address = str_replace(address, "_fred_", "_frederick_"),
                address = str_replace(address, "_8_", "_frederick_douglass_blvd_"),
                address = str_replace(address, "_avenue_", "_ave_"),
                address = str_replace(address, "_av_", "_ave_"),
                address = str_replace(address, "_3_", "_third_"),
                address = str_replace(address, "_3rd_", "_third_"))  %>% 
    rowwise() %>% 
    mutate( url = createURL(address, aptNum)) %>% 
    ungroup() %>% 
    separate(address, c("stNum", "stRest"), sep = "_", extra = "merge",remove = FALSE) %>%
    mutate(stRest = paste0("_",stRest))%>%
    mutate(stRestNum = str_extract(stRest, "\\d+"),
           stLastNum = sapply(str_split(stRest, "_"), function(x)x %>% rev() %>% "["(1)))

# get matching URLS
rsJoin1 <- rsClean %>% left_join(seClean, by = c("url" = "url")) %>% 
    filter(!is.na(saleid))

# get non matching URLS
rsClean2 <-   rsClean %>% left_join(seClean, by = c("url" = "url")) %>% 
    filter(is.na(saleid)) %>% 
    distinct(stNum.x, stRest.x, aptNum.x, url)

rsClean2 %>% group_by(url) %>% count() %>% filter(n>1)

rsClean %>% filter( url == "http://streeteasy.com/building/100_lasalle_street-new_york.html") %>% View()

rsClean %>% 
    filter(residential_units<=1 | !is.na(aptNum)) %>% View()

match_fun_distance <- function(left, right) {
    stringdist::stringdist(left, right,method = "lv") <= 2
}


fuzzyJoinOut <- rsClean2 %>%
    distinct(stNum,stRest, stRestNum,stLastNum, .keep_all = TRUE) %>%
    fuzzy_left_join(seClean %>% distinct(stNum,stRest,stRestNum,stLastNum), 
                         by = list( x = c("stNum","stRest","stRestNum","stLastNum"),
                                    y=c("stNum", "stRest","stRestNum","stLastNum")),
                    match_fun = list(`==`, match_fun_distance, `==`, `==`))


fuzzyJoinOut  %>% View()

fuzzyJoinOut %>% filter(is.na(stRest.y)) %>% View()
