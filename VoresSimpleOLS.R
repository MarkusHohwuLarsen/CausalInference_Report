library(plm)
library(lmtest)
library(sandwich)
library(stargazer)

# Load your dataset
data <- readRDS("exam_TWEA1.R")# Adjust with your file path
data <- pdata.frame(data, index = c("uspto_class","grntyr")) # Define panel structure


# 3. Third Regression
model3 <- plm(count_usa ~ anypatent, 
              data = data, 
              model = "within", effect = "twoways")
cluster_se3 <- coeftest(model3, vcov = vcovHC(model3, type = "HC3", cluster = "group"))


# 6. Sixth Regression
model6 <- plm(count_usa ~ count_cl, 
              data = data, 
              model = "within", effect = "twoways")
cluster_se6 <- coeftest(model6, vcov = vcovHC(model6, type = "HC3", cluster = "group"))


stargazer(model3, model6, 
          type = "html", 
          title = "Regression Results", 
          se = list(cluster_se3[,2],cluster_se6[,2]), 
          out = "ols_table.html")
