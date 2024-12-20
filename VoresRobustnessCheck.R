library(plm)
library(ggplot2)
library(tibble)
library(haven)
library(broom)
# #A. Controlling for Preexisting Time Trends
data <- readRDS("exam_TWEA1.R") 
licensed_classes <- c()
for(i in 1:NROW(data)){
  if(data$anypatent[i]==1){
    licensed_classes <- c(licensed_classes, data$uspto_class[i])
  }
}
licensed_classes <- unique(licensed_classes)
data <- data %>% mutate(
  licensed = ifelse(uspto_class %in% licensed_classes, 1, 0),
  trend = licensed * grntyr
)

# Generate the necessary variables
for (year in 1878:1939) {
  data[[paste0("td_", year)]] <- ifelse(data$grntyr == year, 1, 0)
}
  for (x in 1919:1939) {
    data[[paste0("treat_", x)]] <- ifelse(data$grntyr == x, data$anypatent, 0)
  }
treat_vars <- grep("^treat_", names(data), value = TRUE)
td_vars <- grep("^td_", names(data), value = TRUE)

model_licensed <- plm(count_usa ~ td_1878 + td_1879 + td_1880 + td_1881 + 
                        td_1882 + td_1883 + td_1884 + td_1885 + td_1886 + td_1887 + 
                        td_1888 + td_1889 + td_1890 + td_1891 + td_1892 + td_1893 + 
                        td_1894 + td_1895 + td_1896 + td_1897 + td_1898 + td_1899 + 
                        td_1900 + td_1901 + td_1902 + td_1903 + td_1904 + td_1905 + 
                        td_1906 + td_1907 + td_1908 + td_1909 + td_1910 + td_1911 + 
                        td_1912 + td_1913 + td_1914 + td_1915 + td_1916 + td_1917 + 
                        td_1918 + td_1919 + td_1920 + td_1921 + td_1922 + td_1923 + 
                        td_1924 + td_1925 + td_1926 + td_1927 + td_1928 + td_1929 + 
                        td_1930 + td_1931 + td_1932 + td_1933 + td_1934 + td_1935 + 
                        td_1936 + td_1937 + td_1938 + td_1939 + treat_1919 + treat_1920 + 
                        treat_1921 + treat_1922 + treat_1923 + treat_1924 + treat_1925 + 
                        treat_1926 + treat_1927 + treat_1928 + treat_1929 + treat_1930 + 
                        treat_1931 + treat_1932 + treat_1933 + treat_1934 + treat_1935 + 
                        treat_1936 + treat_1937 + treat_1938 + treat_1939 + trend, data=data)

td_results <- tidy(model_licensed) %>%
  filter(grepl("^td_", term)) %>%
  mutate(year = as.numeric(gsub("td_", "", term)))

treat_results <- tidy(model_licensed) %>%
  filter(grepl("^treat_", term)) %>%
  mutate(year = as.numeric(gsub("treat_", "", term))) %>% mutate(estimate2 = estimate) %>% select(year, estimate2) # Extract year from term

combined_results <- left_join(td_results, treat_results, by =c("year"="year") )
combined_results <- combined_results %>%
  mutate(
    Untreated_subclasses = estimate,
    estimate2 = ifelse(is.na(estimate2), 0, estimate2),
    Treated_subclasses = estimate+estimate2) %>% filter(year >=1915)

data_long <- combined_results %>% 
  pivot_longer(cols = c(Untreated_subclasses, Treated_subclasses), 
               names_to = "variable", 
               values_to = "value")

ggplot(data_long, aes(x = year, y = value, color = variable)) +
  geom_line(stat = "summary", fun = "sum") +
  labs(title = "Controlling for linear time trends", 
       x = "Grant Year", y = "Coefficient for year dummies") +
  theme_minimal()



#B) Interactions between Main Classes and Year Fixed Effect
