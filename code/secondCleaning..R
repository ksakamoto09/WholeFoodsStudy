harlemSales_att <- read_csv("data/modelling/harlemSales_Attributes.csv")

# whole foods
wf <- st_read(file.path(shapeDir, "wholeFoods"))
wfHarlem <- wf %>% filter(store == "Whole Foods Harlem") %>% st_transform(2263)
wfHarlemBufferHalf <- wfHarlem %>% st_buffer(2640)
wfHarlemBufferOne <- wfHarlem %>% st_buffer(2640*2)
## sales
mnSalesJoin <- readr::read_csv(file.path(dataDir, "mnSalesJoin.csv")) %>% 
    mutate(rowid = row_number()) %>% 
    filter(sale_price > 50000, sale_price < 10000000)

wfDF <- mnSalesJoin %>% filter(buffer == "Half")
notwfDF <- mnSalesJoin %>% filter(buffer == "Mile")


plutoBlocks <- pluto %>% st_join(wfHarlemBufferOne) %>% 
    filter(!is.na(store)) %>% st_drop_geometry() %>% 
    select(Block, CT2010_y)

allWFHarlemSales <- mnSalesJoin %>% filter(block %in%plutoBlocks$Block) %>% 
    select(-c(borough.y, res_area, res_area_tot, res_percent, store, buffer, time))

allWFHarlemSalesDist <- allWFHarlemSales %>% left_join(
    harlemlots %>% filter(BoroName == "Manhattan") %>% 
        select(-HalfMile) %>% 
        st_drop_geometry(),
    by = c("block" = "Block", "lot" = "lot")
)


acs <- read_csv("data/modelling/ACS2019_CT_NY_NYC.csv") %>% 
    janitor::clean_names() %>% 
    mutate(geoid = as.character(geoid))
names(acs)

acspredictors <- acs %>% 
    mutate(whitePercent = total_population_white_alone/total_population,
           blackPercent = total_population_black_or_african_american_alone/total_population,
           HispanicLatinoPercent = total_population_hispanic_or_latino/total_population,
           bachelorsPlus = 
               population_25_years_and_over_bachelors_degree+
               population_25_years_and_over_masters_degree+
               population_25_years_and_over_professional_school_degree+
               population_25_years_and_over_doctorate_degree,
           BAPlusPercent = bachelorsPlus/ population_25_years_and_over,
           ownerPercent = occupied_housing_units_owner_occupied/occupied_housing_units) %>% 
    select(geoid, total_population,whitePercent,blackPercent, HispanicLatinoPercent, 
           bachelorsPlus, BAPlusPercent,ownerPercent,
           median_gross_rent,
           median_household_income_in_2019_inflation_adjusted_dollars,
           average_household_size)

allWFHarlemSalesDistACS <- allWFHarlemSalesDist %>% mutate(geoid = paste0("36061", ct2010_y)) %>% 
    left_join(acspredictors)
    

permits <- read_csv(file.path(dataDir, "DOB_Permit_Issuance2010_2020.csv"))

permits <- permits %>% 
    janitor::clean_names()%>% 
    select(borough, job_type,
           block, lot, permit_type,
           filing_date, latitude, longitude)

permits <- permits %>% 
    mutate(filing_date = lubridate::mdy_hms(filing_date),
           year = lubridate::year(filing_date),
           month = lubridate::month(filing_date))

permitsGroup<- permitsMN %>% filter(borough == "MANHATTAN") %>% 
    group_by(block, permit_type) %>% count() %>% tidyr::pivot_wider(names_from = permit_type, values_from = n, values_fill = 0) %>% 
    ungroup() %>% 
    mutate(totPermits = AL + EW + PL + DM + EQ + FO + NB + SG,
           block = as.numeric(block))

finalData <- allWFHarlemSalesDistACS %>% left_join(permitsGroup)
