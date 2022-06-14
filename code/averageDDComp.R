library(tibble)
library(dplyr)

ddDFc <- tibble(time = c(rep_len(0,5), rep_len(1,5)),
               control = 0,
               val = 1:10)
ddDFt <- tibble(time = c(rep_len(0,5), rep_len(1,5)),
                control = 1,
                val = c(10:14, 30:34))

ddDFALL <- ddDFc %>% bind_rows(ddDFt)

cTable <- ddDFc %>% group_by(time) %>% summarize(mean = mean(val))
tTable <- ddDFt %>% group_by(time) %>% summarize(mean = mean(val))

(tTable$mean[2] - tTable$mean[1]) - (cTable$mean[2] - cTable$mean[1])

mod <- lm(val~time*control, ddDFALL)

summary(mod)

