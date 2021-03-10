library(dplyr)

## here we are going to find the root_file 
root <- rprojroot::find_rstudio_root_file()
## we are going to set the data file path here
dataDir <- file.path(root, 'data')

permits <- readr::read_csv(file.path(dataDir, "DOB_Permit_Issuance2010_2020.csv")) %>% 
    janitor::clean_names()
permits <- permits %>% 
    select(borough, job_type,
           block, lot, permit_type,
           filing_date, latitude, longitude)

permits <- permits %>% 
    mutate(filing_date = lubridate::mdy_hms(filing_date),
           year = lubridate::year(filing_date),
           month = lubridate::month(filing_date))

permitBlocks <- permits %>% group_by(borough, block, month, year) %>% 
    summarize(date = min(filing_date),
              count = n()) 

library(sf)
harlemlots <- st_read("shape/HarlemSales/HarlemSales.gpkg") %>% 
    filter(borough_name == "Manhattan") %>% 
    st_drop_geometry() %>% 
    select(borough_name, block, HalfMile) %>% 
    rename(borough = borough_name) %>% 
    mutate(HalfMile = if_else(HalfMile == 0, "within .5mile", "outside .5mile"),
           block = as.character(block),
           borough = "MANHATTAN",
           block = stringr::str_pad(block, 5, side = "left", pad = "0")) %>% 
    distinct()

permitBlockJoin <- permitBlocks %>% left_join(harlemlots) %>% 
    filter(!is.na(HalfMile))

library(ggplot2)
permitBlockJoin %>% group_by(month, year, HalfMile) %>% 
    summarize(count = sum(count, na.rm = TRUE),
              date = min(date)) %>% 
    ggplot(aes(date, count, color = HalfMile)) +
    geom_line()+ 
    geom_vline(xintercept = as.POSIXct("2017-07-21"),
               linetype="dotted", 
               color = "blue", size=0.5) + 
    geom_smooth(size = 0.5) +
    theme_minimal() + 
    ggtitle("Building Permits near Whole Foods Harlem")

permitBlockJoin %>% group_by(year, HalfMile) %>% 
    summarize(count = sum(count, na.rm = TRUE),
              date = min(date)) %>% 
    ggplot(aes(date, count, color = HalfMile)) +
    geom_line() + 
    geom_vline(xintercept = as.POSIXct("2017-07-21"),
               linetype="dotted", 
               color = "blue", size=0.5) + 
    geom_smooth(size = 0.5) +
    theme_minimal() + 
    ggtitle("Building Permits near Whole Foods Harlem")
