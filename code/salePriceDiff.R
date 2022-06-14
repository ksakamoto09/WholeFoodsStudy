
repeats <- mnSalesJoin %>% 
    group_by(block, lot) %>% 
    count() %>% 
    filter(n >2)

salesNewOld <- mnSalesJoin %>% left_join(repeats) %>% 
    filter(!is.na(n)) %>% group_by(block, lot) %>% 
    summarize(newDate = max(sale_date),
              oldDate = min(sale_date)) %>% 
    tidyr::pivot_longer(names_to = "dateType", values_to = "Date", -c(block, lot)) %>% 
    ungroup()


salesDiff <- salesNewOld %>% left_join(mnSalesJoin %>% select(block, lot, sale_date, sale_price),
                          by = c("block" = "block",
                                 "lot" = "lot",
                                 "Date" = "sale_date")) %>% 
    select(-Date) %>% 
    group_by(block, lot, dateType) %>%
    summarize(sale_price= max(sale_price)) %>%
    tidyr::pivot_wider(names_from = dateType, values_from = sale_price) %>% 
    mutate(diff = newDate - oldDate,
           diffPercent = (newDate-oldDate)/oldDate) %>% 
    group_by(block) %>% 
    summarize(diff = mean(diff),
              diffPercent = mean(diffPercent))


    tm_shape(plutoWF %>% inner_join(salesDiff %>% rename(Block = block))) + 
    tm_polygons(col = "diffPercent")
    tm_shape(plutoWF %>% inner_join(salesDiff %>% rename(Block = block))) + 
        tm_polygons(col = "diff")
    