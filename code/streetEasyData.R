library(rvest)
library(tibble)
library(readr)
library(dplyr)
library(stringr)

root <- rprojroot::find_rstudio_root_file()
dataDir <- file.path(root, 'data')
mnSales <- readr::read_csv(file.path(dataDir, "mnSalesJoin.csv"))


## rcrawler

library(Rcrawler)

dataTest <- ContentScraper(Url =  "https://streeteasy.com/building/madison-house/16d",
                           CssPatterns = c(".detail_cell",".price"),
                           ExcludeCSSPat = (".secondary_text"),
                           ManyPerPattern = TRUE, asDataFrame = TRUE)

createURL <- function(address, aptNum, aptNum2){
    newAddress <- address %>% str_replace_all(" ", "-") %>% 
        str_replace("---", "-") %>% 
        str_to_lower()
    ## check if apt number is there
    if(!is.na(aptNum)){
        newApt <- paste0("-", aptNum %>% str_to_lower())
        url <- paste0("http://streeteasy.com/building/", newAddress, "-new_york", newApt, ".html") 
    } else if(!is.na(aptNum2)){ # check if there is another apt number
        newApt <- paste0("/", aptNum2 %>% str_to_lower())
        url <- paste0("http://streeteasy.com/building/", newAddress, "-new_york", newApt, ".html") 
    }else{ 
        url <- paste0("http://streeteasy.com/building/", newAddress, "-new_york", ".html") # just use address
    }
}

mnSales %>% distinct(address, aptNum, apartment_number)

test <- mnSales %>% slice() %>% rowwise() %>% 
    mutate(url = createURL(address, aptNum, apartment_number))

scrapeDetail <- function(url){
    Sys.sleep(20)
    dataOut <- ContentScraper(Url =  url,
                              CssPatterns = c(".detail_cell",".price"),
                              ExcludeCSSPat = (".secondary_text"),
                              ManyPerPattern = TRUE, 
                              asDataFrame = TRUE)
    if(dataOut[1,1] == "HTTP error code:404"){
        dataOut <- NA
    }
}

urls <- test  %>% distinct(url) 

urlsOut <- urls %>% ungroup() %>% 
    mutate(id = row_number())

subVec <- seq(1, nrow(urlsOut), 100)
counter <- 0
for(i in subVec){
    if(i>1){
        counter <- counter+1
        output <- urlsOut %>% slice(i-100:i)
        urlPath <- paste0("data/urlList/streetEasyUrl", counter, ".csv")
        write_csv(x = output, path = urlPath)
    }
}
scrapeDetail <- function(url){
    Sys.sleep(rnorm(1,15,5))
    dataOut <- ContentScraper(Url =  url,
                              CssPatterns = c(".detail_cell",".price"),
                              ExcludeCSSPat = (".secondary_text"),
                              ManyPerPattern = TRUE, 
                              asDataFrame = TRUE)
    # if(dataOut[1,1] == "HTTP error code:404"){
    #   dataOut <- NA
    # }
}

seqVec <- seq(11,20)

for(i in seqVec){
    pathOpen <- file.path(root, paste0("data/urlList/streetEasyUrl",i, ".csv"))
    df <- read.csv(pathOpen, stringsAsFactors = FALSE)
    output <- df %>% 
        rowwise() %>%
        mutate(results = list(scrapeDetail(url)))
    write.csv(output, file.path(root, paste0("data/scrapedData/streetEasyUrl",i, ".csv")))
}

pathOpen <- file.path(root, paste0("data/urlList/streetEasyUrl",3, ".csv"))
df <- read.csv(pathOpen,stringsAsFactors = FALSE)
output <- df %>% 
    rowwise() %>%
    mutate(results = list(scrapeDetail(url)))
write_csv(output, file.path(root, paste0("data/scrapedData/streetEasyUrl",i,".csv")))

test <- scrapeDetail(df$url[1])
test
