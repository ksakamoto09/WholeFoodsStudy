library(jsonlite)
library(utils)


cityurl <- "https://api.cityofnewyork.us/geoclient/v1/bbl.json?"

appid <-"&app_id=8ba7b31f&app_key=2980cbc5ea7459f560747bcaebbd4a44" 

placeQuery <- function(row){
    Sys.sleep(0.1)
    block <- paste0("&block=", row$block)
    lot <- paste0("&lot=", row$lot)
    borough <- paste0("borough=", row$borough_name)    
    queryurl <- paste0(cityurl,borough,block,lot,appid)
   # print(queryurl)
    rd <- fromJSON(URLencode(queryurl))
    if(rd$bbl$geosupportReturnCode == "00"){
        if(!is.null(rd$bbl$latitudeInternalLabel)){
            lat <- rd$bbl$latitudeInternalLabel
            long <- rd$bbl$longitudeInternalLabel
            outpuDF <- row %>% bind_cols(tibble::tibble(lat, long) %>% head(1)) 
        }
    }
}

bblGroup <- resSales %>% group_by(borough_name, block, lot) %>% count()

bblGroupList <- bblGroup %>% ungroup() %>% mutate(id = seq(nrow(bblGroup))) %>% 
    select(-n) %>% group_by(id)  %>% tidyr::nest()

x <-seq(1,nrow(bblGroup),by = 2500)
y <- seq(2500,nrow(bblGroup),by = 2500)
y <- c(y, nrow(bblGroup))
sequences <- tibble(x,y)

runeachFunc <- function(seqs, bblGroupList){
    df <- bblGroupList$data[seqs$x:seqs$y] %>% purrr::map_df(~placeQuery(.x))
    df
}

seqlist <- split(sequences, seq(nrow(sequences)))
# placesDF <-seqlist[1:2] %>%  purrr::map_df(~runeachFunc(.x, bblGroupList))
# placesDF2 <-seqlist[3:10] %>%  purrr::map_df(~runeachFunc(.x, bblGroupList))
# placesDF3 <-seqlist[11:15] %>%  purrr::map_df(~runeachFunc(.x, bblGroupList))
# placesDF4 <-seqlist[16:17] %>%  purrr::map_df(~runeachFunc(.x, bblGroupList))
# placesDF5 <-seqlist[18] %>%  purrr::map_df(~runeachFunc(.x, bblGroupList))
# placesDF6 <-seqlist[19] %>%  purrr::map_df(~runeachFunc(.x, bblGroupList))
# placesDF7 <-seqlist[20] %>%  purrr::map_df(~runeachFunc(.x, bblGroupList))
# placesDF8 <-seqlist[21:34] %>%  purrr::map_df(~runeachFunc(.x, bblGroupList))
# placesDF9 <-seqlist[35:40] %>%  purrr::map_df(~runeachFunc(.x, bblGroupList))
# placesDF10 <-seqlist[41:50] %>%  purrr::map_df(~runeachFunc(.x, bblGroupList))
# placesDF11 <-seqlist[51:60] %>%  purrr::map_df(~runeachFunc(.x, bblGroupList))
# placesDF12 <-seqlist[61:70] %>%  purrr::map_df(~runeachFunc(.x, bblGroupList))
placesDF13 <-seqlist[71:85] %>%  purrr::map_df(~runeachFunc(.x, bblGroupList))
placesDF14 <-seqlist[86:95] %>%  purrr::map_df(~runeachFunc(.x, bblGroupList))
placesDF15 <-seqlist[96:100] %>%  purrr::map_df(~runeachFunc(.x, bblGroupList))
placesDF16 <-seqlist[101:105] %>%  purrr::map_df(~runeachFunc(.x, bblGroupList))
placesDF17 <-seqlist[106:110] %>%  purrr::map_df(~runeachFunc(.x, bblGroupList))
placesDF18 <-seqlist[111:115] %>%  purrr::map_df(~runeachFunc(.x, bblGroupList))

# tests
placesTest <- bblGroupList$data[47501:48000] %>% purrr::map_df(~placeQuery(.x))
placesTest <- bblGroupList$data[49001:49500ß] %>% purrr::map_df(~placeQuery(.x))

length(seqlist)

placeDF <- readr::read_csv("placesTotal.csv")
placeTotal <- placeDF %>% bind_rows(placesDF13) %>% bind_rows(placesDF14) %>%
    bind_rows(placesDF15) %>% bind_rows(placesDF16) %>%
    bind_rows(placesDF17) %>%
    bind_rows(placesDF18) 
readr::write_csv(placeTotal,"placesTotal.csv")


