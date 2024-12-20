library(plm)       
library(AER)       
library(stargazer) 
library(haven)

data <- readRDS("exam_TWEA1.R")
pdata <- pdata.frame(data, index = c("uspto_class","grntyr"))

IV_fs <- plm(count_cl ~ count_cl_itt, 
              data = pdata, model = "within", effect = "twoways")

IV_2sls <- plm(count_usa ~count_cl | count_cl_itt, data = pdata, model = "within", effect = "twoways")

robust_se1 <- coeftest(IV_fs, vcov = vcovHC(IV_fs, type = "HC3", cluster = "group"))

stargazer(IV_fs, IV_2sls, type = "latex", 
          se = list(robust_se1[,2], summary(IV_2sls)$coefficients[,2]),
          out = "iv_table.tex")
