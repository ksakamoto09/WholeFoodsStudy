library(dplyr)
library(sf)

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

MNpermits <- permits %>% filter(!is.na(latitude), borough == "MANHATTAN") %>% 
    st_as_sf(coords = c("longitude", "latitude")) %>% 
    st_set_crs(4326) %>% 
    st_transform(2263)


shapeDir <- file.path(root, 'shape')

## whole foods
wf <- st_read(file.path(shapeDir, "wholeFoods"))

wfHarlem <- wf %>% filter(store == "Whole Foods Harlem") %>% st_transform(2263)
wfHarlemBufferOne <- wfHarlem %>% st_buffer(2640*2) 
wfHarlemBufferHalf <- wfHarlem %>% st_buffer(2640) 


permitJoin <- MNpermits  %>% st_join(wfHarlemBufferOne) %>% 
    filter(!is.na(store))

library(ggplot2)
permitBuffer <- permitJoin %>% st_join(wfHarlemBufferHalf %>% mutate(buffer = "half") %>% select(buffer)) %>% 
    mutate(buffer = if_else(is.na(buffer), "mile", buffer))

permitBuffer %>% 
    st_drop_geometry() %>% 
    group_by(month, year, buffer) %>% 
    rename(date = filing_date) %>% 
    summarize(count = n(),
              date = min(date)) %>% 
    ggplot(aes(date, count, group = buffer)) +
    geom_line(aes(color = buffer))+ 
    geom_vline(xintercept = as.POSIXct("2017-07-21"),
               linetype="dotted", 
               color = "blue", size=0.5) + 
    geom_smooth(size = 0.5) +
    theme_minimal() + 
    ggtitle("Building Permits near Whole Foods Harlem")

permitBuffer %>% 
    st_drop_geometry() %>% 
    group_by(year, buffer) %>% 
    rename(date = filing_date) %>% 
    summarize(count = n(),
              date = min(date)) %>% 
    ggplot(aes(date, count, group = buffer)) +
    geom_line(aes(color=buffer)) + 
    geom_vline(xintercept = as.POSIXct("2017-07-21"),
               linetype="dotted", 
               color = "blue", size=0.5) + 
    geom_smooth(size = 0.5) +
    theme_minimal() + 
    ggtitle("Building Permits near Whole Foods Harlem")


modDF <- permitBuffer %>% 
    st_drop_geometry() %>% 
    group_by(month, year, buffer) %>% 
    rename(date = filing_date) %>% 
    summarize(count = n(),
              date = min(date)) %>% 
    mutate(time = if_else(year <=2018, 0,1),
           treatment = if_else(buffer == "mile", 0,1))


mod1 <- lm(count~time*treatment, modDF)

summary(mod1)

plots[[3]]$data %>% 
    mutate(date = lubridate::date_decimal(time))

bufferData <- function(treatBuffer, treatYear){
    wfHarlemBuffer <- wfHarlem %>% st_buffer(treatBuffer) 
    permitBuffer <- permitJoin %>% st_join(wfHarlemBuffer %>% mutate(buffer = 1) %>% select(buffer)) %>% 
        mutate(buffer = if_else(is.na(buffer),  0, buffer))
    
    modDF <- permitBuffer %>% 
        st_drop_geometry() %>% 
        group_by(month, year, buffer) %>% 
        rename(date = filing_date) %>% 
        summarize(count = n(),
                  date = min(date)) %>% 
        mutate(time = if_else(year <=treatYear, 0,1)) %>% 
        rename(treatment = buffer) %>% 
        mutate(time = as.factor(time),
               treatment = as.factor(treatment))
}

plotBuffer <- function(bufferData, year){
    bufferData %>% 
        ggplot(aes(date, count, group = treatment)) +
        geom_line(aes(color = treatment))+ 
        geom_vline(xintercept = as.POSIXct(paste0(year,"-07-21")),
                   linetype="dotted", 
                   color = "blue", size=0.5) + 
        geom_smooth(size = 0.5) +
        theme_minimal() + 
        ggtitle("Building Permits near Whole Foods Harlem")
}
    
modFunc <- function(modDF){
    mod1 <- lm(count~time*treatment, modDF)
}

coefFunc <- function(mod){
    mod$coefficients[4]
}

pvalFunc <- function(mod){
    summary(mod)$coefficients[4,4]
}


permitCheck <- tidyr::crossing(treatbuffer = seq(1320,4620, 660), treatYear = seq(2015, 2018))

test <- permitCheck %>% 
    mutate(modDF = purrr::map2(treatbuffer, treatYear, ~bufferData(.x,.y)),
           plots = purrr::map2(modDF, treatYear, ~plotBuffer(.x,.y)),
           mods = purrr::map(modDF, ~modFunc(.x)),
           coef = purrr::map_dbl(mods, ~coefFunc(.x)),
           pval = purrr::map_dbl(mods, ~pvalFunc(.x)))


test$plots[[1]]

test %>% 
    ggplot(aes(x = pval, y = coef, color = as.factor(treatYear))) +
    geom_point() +
    facet_wrap(~treatbuffer)


## animation

urb_anim = 
    tm_shape(wfHarlemBufferOne) + tm_borders() + 
    tm_shape(permitJoin) + tm_dots() +
    tm_facets(along = "year", free.coords = FALSE)

tmap_animation(urb_anim, filename = "urb_anim.gif", delay = 55)
