# harlem lots with physical features
harlemlots <- st_read("../shape/HarlemSales/HarlemSales.gpkg") %>% 
    filter(borough_name == "Manhattan") %>% 
    rename(BoroName = borough_name,
           Block = block) %>% 
    mutate(BoroName = as.character(BoroName)) %>% 
    select(-ObjID)

mnResSales <- readr::read_csv("../data/rollingSales/nycSales_2010_2019") %>% 
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
harlemSales %>% group_by(year, month, HalfMile) %>% 
    count() %>% 
    ungroup() %>% 
    mutate(calDate = ymd(paste(year,month,"01",sep = "-"))) %>% 
    ggplot(aes(x = calDate, y = n)) + 
    geom_point(aes(color = HalfMile)) 



## number of sales

harlemSales %>% group_by(year, HalfMile) %>% 
    count() %>% 
    ungroup() %>% 
    mutate(calDate = ymd(paste(year,"01","01",sep = "-")),
           treatment = if_else(HalfMile < 1, "Outside Buffer", "Inside Buffer"),
           time = factor(if_else(calDate < ymd("2017-01-01"), "Before WF", "After WF"), levels = c("Before WF", "After WF"))) %>% 
    ggplot(aes(x = calDate, y = n, color = treatment)) + 
    geom_point() +
    facet_wrap(~time)+
    geom_smooth(method = "lm",se = FALSE, aes(group = treatment)) 

# by tax class
harlemSales %>% group_by(year, HalfMile,tax_class_at_present) %>% 
    count() %>% 
    ungroup() %>% 
    mutate(calDate = ymd(paste(year,"01","01",sep = "-")),
           treatment = if_else(HalfMile < 1, "Outside Buffer", "Inside Buffer"),
           time = factor(if_else(calDate < ymd("2017-01-01"), "Before WF", "After WF"), levels = c("Before WF", "After WF"))) %>% 
    ggplot(aes(x = calDate, y = n, color = treatment)) + 
    geom_point() +
    facet_grid(tax_class_at_present~time)+
    geom_smooth(method = "lm",se = FALSE, aes(group = treatment)) 

### By Price

### avg price
harlemSales %>% group_by(year, HalfMile) %>% 
    summarize(avg = mean(sale_price, na.rm=TRUE),
              med = median(sale_price, na.rm=TRUE)) %>% 
    ungroup() %>% 
    mutate(calDate = ymd(paste(year,"01","01",sep = "-")),
           treatment = if_else(HalfMile < 1, "Outside Buffer", "Inside Buffer"),
           time = factor(if_else(calDate < ymd("2017-01-01"), "Before WF", "After WF"), levels = c("Before WF", "After WF")),
           med = if_else(med == 0, NA_real_, med)) %>% 
    ggplot(aes(x = calDate, y = avg, color = treatment)) + 
    geom_point() +
    facet_wrap(~time)+
    geom_smooth(method = "lm",se = FALSE, aes(group = treatment)) 

### avg price
harlemSales %>% group_by(year, HalfMile) %>% 
    summarize(avg = mean(sale_price, na.rm=TRUE),
              med = median(sale_price, na.rm=TRUE)) %>% 
    ungroup() %>% 
    mutate(calDate = ymd(paste(year,"01","01",sep = "-")),
           treatment = if_else(HalfMile < 1, "Outside Buffer", "Inside Buffer"),
           time = factor(if_else(calDate < ymd("2017-01-01"), "Before WF", "After WF"), levels = c("Before WF", "After WF")),
           med = if_else(med == 0, NA_real_, med)) %>% 
    ggplot(aes(x = calDate, y = med, color = treatment)) + 
    geom_point() +
    facet_wrap(~time)+
    geom_smooth(method = "lm",se = FALSE, aes(group = treatment)) 

### by tax class avg price
harlemSales %>% group_by(year, HalfMile,tax_class_at_present) %>% 
    summarize(avg = mean(sale_price, na.rm=TRUE),
              med = median(sale_price, na.rm=TRUE)) %>% 
    ungroup() %>% 
    mutate(calDate = ymd(paste(year,"01","01",sep = "-")),
           treatment = if_else(HalfMile < 1, "Outside Buffer", "Inside Buffer"),
           time = factor(if_else(calDate < ymd("2017-01-01"), "Before WF", "After WF"), levels = c("Before WF", "After WF")),
           med = if_else(med == 0, NA_real_, med)) %>% 
    ggplot(aes(x = calDate, y = avg, color = treatment)) + 
    geom_point() +
    facet_grid(tax_class_at_present~time)+
    geom_smooth(method = "lm",se = FALSE, aes(group = treatment)) 

### by tax class med price
harlemSales %>% group_by(year, HalfMile,tax_class_at_present) %>% 
    summarize(avg = mean(sale_price, na.rm=TRUE),
              med = median(sale_price, na.rm=TRUE)) %>% 
    ungroup() %>% 
    mutate(calDate = ymd(paste(year,"01","01",sep = "-")),
           treatment = if_else(HalfMile < 1, "Outside Buffer", "Inside Buffer"),
           time = factor(if_else(calDate < ymd("2017-01-01"), "Before WF", "After WF"), levels = c("Before WF", "After WF")),
           med = if_else(med == 0, NA_real_, med)) %>% 
    ggplot(aes(x = calDate, y = med, color = treatment)) + 
    geom_point() +
    facet_grid(tax_class_at_present~time)+
    geom_smooth(method = "lm",se = FALSE, aes(group = treatment)) 


        
library(tmap)
tm_shape(sf::st_as_sf( harlemSales %>% 
                           mutate(calDate = ymd(paste(year,"01","01",sep = "-")),
                                  HalfMile = as.character(HalfMile),
                                  time = factor(if_else(calDate < ymd("2017-01-01"), "Before WF", "After WF"), levels = c("Before WF", "After WF"))))) + 
    tm_dots(col = "HalfMile")


### Create hedonic pricing model for each year, separate inside and outside buffer for each tax class
### create value for each year, each lot. 

## add in spatial lagged values.
