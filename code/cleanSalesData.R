library(dplyr)
library(readxl)
library(purrr)

root <- rprojroot::find_rstudio_root_file()
dataDir <- file.path(root, 'data')
salesDir <- file.path(dataDir, 'rollingSales')
salesFiles <- list.files(salesDir, full.names = TRUE)

## Combine Annualized sales data

combinexlsFunc <- function(file){
    #print(file)
    output <- readxl::read_xls(file, skip = 4, sheet = 1, col_names = names(test)) %>% 
        mutate(BOROUGH = as.character(BOROUGH),
               `TAX CLASS AT TIME OF SALE` = as.character(`TAX CLASS AT TIME OF SALE`),
               `APARTMENT NUMBER` = as.character(`APARTMENT NUMBER`))
    if(output[1,1,] %in% c("BOROUGH", "BOROUGH\n")) {
        output <- readxl::read_xls(file, skip = 5,col_names =  names(test))%>% 
            mutate(BOROUGH = as.character(BOROUGH),
                   `TAX CLASS AT TIME OF SALE` = as.character(`TAX CLASS AT TIME OF SALE`),
                   `APARTMENT NUMBER` = as.character(`APARTMENT NUMBER`))
    }
    output
}

combinexlsxFunc <- function(file){
    output <- readxl::read_xlsx(file, ,skip = 5,col_names = names(test)) %>% 
        mutate(BOROUGH = as.character(BOROUGH),
               `TAX CLASS AT TIME OF SALE` = as.character(`TAX CLASS AT TIME OF SALE`),
               `APARTMENT NUMBER` = as.character(`APARTMENT NUMBER`))
    if(output[1,1,] %in% c("BOROUGH", "BOROUGH\n")) {
        output <- readxl::read_xls(file, skip = 6,col_names =  names(test))%>% 
            mutate(BOROUGH = as.character(BOROUGH),
                   `TAX CLASS AT TIME OF SALE` = as.character(`TAX CLASS AT TIME OF SALE`),
                   `APARTMENT NUMBER` = as.character(`APARTMENT NUMBER`))
    }
    output
}


df1 <- salesFiles[stringr::str_detect(salesFiles, "xls$")] %>% map_df(~combinexlsFunc(.x))
df2 <- salesFiles[stringr::str_detect(salesFiles, "xlsx$")] %>% map_df(~combinexlsxFunc(.x))

annualizedSales <- df1 %>% bind_rows(df2)
