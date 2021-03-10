library(glmnet)
library(coefplot)
library(readr)
library(dplyr)
library(useful)
library(caret)

harlemlots <- read_csv("data/modelling/harlemSales_Attributes.csv") 
permits <- read_csv("data/modelling/PermitsBlocks.csv") %>% 
    rename(Block = block)  %>% 
    filter(borough == "MANHATTAN") %>% 
    mutate(Block = as.numeric(Block)) %>% 
    group_by(borough, Block, year) %>% 
    summarize(permits = sum(count)) 

harlemDF <- harlemlots%>% 
    filter(BoroName == "Manhattan") %>% 
    left_join(permits %>% select(-borough))

harlemDF <- harlemDF %>% filter(sale_price > 10000, tax_class_at_present != "4") %>% 
    mutate(Year = year(sale_date))

harlemDFScale <- harlemDF  %>% 
    mutate_at(vars(residential_units ,
                     land_square_feet , gross_square_feet , year_built , HalfMile , flagshipParkDist , parkDist , subwayDist ,
                     libDistance , popBlack , popHisp , avgHHsize , MedHHI , OwnerOccUnits , 
                     OO750_999k , OO1mil_, permits), ~(scale(.) %>% as.vector)) %>% 
    mutate(sale_price = harlemDF$sale_price) %>% 
    filter(complete.cases(.))

harlemDFModel <- harlemDFScale %>% tidyr::nest(-year)


valueFormula <- sale_price ~ tax_class_at_present + building_class_at_present + residential_units +
    land_square_feet + gross_square_feet + year_built + HalfMile + flagshipParkDist + parkDist + subwayDist +
    libDistance + popBlack/TotPop + popHisp/TotPop + avgHHsize + MedHHI + OwnerOccUnits/TotOccUnits + 
    OO_20k + OO20_49k + OO50_99k + OO50_99k + OO100_149k + OO150_299k + OO300_499k + OO_500_749k + 
    OO750_999k + OO1mil_ + permits - 1

valueFormula2 <- log(sale_price) ~ residential_units +
    land_square_feet + gross_square_feet + year_built + HalfMile + flagshipParkDist + parkDist + subwayDist +
    libDistance + popBlack/TotPop + popHisp/TotPop + avgHHsize + MedHHI + OwnerOccUnits/TotOccUnits + 
    OO_20k + OO20_49k + OO50_99k + OO50_99k + OO100_149k + OO150_299k + OO300_499k + OO_500_749k + 
    OO750_999k + OO1mil_ + permits - 1
formulaList <- list(mod1  =valueFormula,
                    modlog = valueFormula2)

library(purrr)
formulaDF <- map2_dfr(formulaList, formulaList %>% names(), function(x,y)tibble(name = y,formula = list(x)))%>%
    tidyr::pivot_wider(names_from = name, values_from = formula)

## Functions for analysis
xFunc <- function(formula,data) build.x(formula, data, contrasts=FALSE, sparse=TRUE)
yFunc <- function(formula,data) build.y(formula, data) %>% as.numeric()

### Elastic Net
myControl <- trainControl(method = "cv", number = 5)
myGrid <- expand.grid(
    alpha = seq(0.1,.9,.1), lambda = seq(0,0.2,.05)
)
alphaFunc <- function(formula,modelDF){
    set.seed(990)
    out <- train(formula, data = modelDF,
                 method = "glmnet",tuneGrid = myGrid,
                 tfControl=myControl,na.action=na.exclude)
    out$bestTune$alpha
}
modelFunc <- function(x, y, HP){
    set.seed(990)
    mod <- glmnet::cv.glmnet(x=x,
                     y=y,alpha = HP,
                     family='gaussian', nfolds=5, standardize = FALSE)
}

coefFunc <- function(mod, name){
    coefplot::coefplot(mod, sort='magnitude',lambda='lambda.1se', intercept = FALSE) + 
        ggtitle(name) +
        theme_minimal()
}

predictFunc <- function(mod, x)tibble(predict = as.vector(predict(mod,x)))
residFunc <- function(data, y)data %>% mutate(residual = predict - y) %>% select(residual)
MSEFunc <- function(mod)mod$cvm[mod$lambda==mod$lambda.1se]

harlemDFModel <- harlemDFModel %>% rowwise() %>% 
    bind_cols(formulaDF) %>% 
    tidyr::pivot_longer(-c(year,data), names_to = "name", values_to = "formula")
## Create table
modList <- harlemDFModel %>% mutate(x = map2(formula,data,~xFunc(.x,.y)),
                                y = map2(formula,data,~yFunc(.x,.y)),
                                alpha = map2_dbl(formula,data,~alphaFunc(.x,.y)),
                                mods = map2(x,y, ~modelFunc(.x,.y,alpha)),
                                #coef = map2(mods, name, ~coefFunc(.x,.y)),
                                prediction = map2(mods, x,~predictFunc(.x,.y)),
                                residual = map2(prediction,y, ~residFunc(.x,.y)),
                                MSE = map_dbl(mods, ~MSEFunc(.x)),
                                lambda = map_dbl(mods, function(x)x$lambda.1se))
