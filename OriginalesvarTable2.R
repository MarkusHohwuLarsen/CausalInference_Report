# Load required packages
library(plm)       # For fixed-effects panel regression
library(lmtest)    # For robust standard errors
library(sandwich)  # For clustered standard errors
library(haven)
library(stargazer)

# Load your dataset
data <- read_dta("chem_patents_maindataset.dta")# Adjust with your file path
data <- pdata.frame(data, index = c("uspto_class","grntyr")) # Define panel structure



# 2. Second Regression
model2 <- plm(count_usa ~ treat + count_for, 
              data = data, 
              model = "within", effect = "twoways")
cluster_se2 <- coeftest(model2, vcov = vcovHC(model2, type = "HC3", cluster = "group"))

# 3. Third Regression
model3 <- plm(count_usa ~ treat, 
              data = data, 
              model = "within", effect = "twoways")
cluster_se3 <- coeftest(model3, vcov = vcovHC(model3, type = "HC3", cluster = "group"))

# 4. Fourth Regression
model4 <- plm(count_usa ~ count_cl + count_for, 
              data = data, 
              model = "within", effect = "twoways")
cluster_se4 <- coeftest(model4, vcov = vcovHC(model4, type = "HC3", cluster = "group"))

# 5. Fifth Regression
model5 <- plm(count_usa ~ count_cl + count_cl_2 + count_for, 
              data = data, 
              model = "within", effect = "twoways")
cluster_se5 <- coeftest(model5, vcov = vcovHC(model5, type = "HC3", cluster = "group"))

# 6. Sixth Regression
model6 <- plm(count_usa ~ count_cl, 
              data = data, 
              model = "within", effect = "twoways")
cluster_se6 <- coeftest(model6, vcov = vcovHC(model6, type = "HC3", cluster = "group"))

# 7. Seventh Regression
model7 <- plm(count_usa ~ year_conf + year_conf_2 + count_for, 
              data = data, 
              model = "within", effect = "twoways")
cluster_se7 <- coeftest(model7, vcov = vcovHC(model7, type = "HC3", cluster = "group"))

# 8. Eighth Regression
model8 <- plm(count_usa ~ year_conf + count_for, 
              data = data, 
              model = "within", effect = "twoways")
cluster_se8 <- coeftest(model8, vcov = vcovHC(model8, type = "HC3", cluster = "group"))

# 9. Ninth Regression
model9 <- plm(count_usa ~ year_conf, 
              data = data, 
              model = "within", effect = "twoways")
cluster_se9 <- coeftest(model9, vcov = vcovHC(model9, type = "HC3", cluster = "group"))

# stargazer(model1, model2, model3, model4,  model5, model6, model7,
#           type = "html",
#           title = "Regression Results",
#           out = "regression_results.html")
# 
# stargazer(model1, model2, model3, model4, model5, model6, model7, 
#           type = "html", 
#           title = "Regression Results", 
#           apply.se = coeftest(., vcov=vcovHC(., type = "HC1", cluster = "group")), 
#           out = "regression_results.html")

stargazer(model2, model3, model5,model4, model6, model7, 
          type = "html", 
          title = "Regression Results", 
          se = list(cluster_se2[,2], cluster_se3[,2], cluster_se5[,2], 
                    cluster_se4[,2], cluster_se6[,2], cluster_se7[,2]), 
          out = "regression_results.html")
