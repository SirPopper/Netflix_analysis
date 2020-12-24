library(tidyverse)


##### load industry data ####
industry <- read_csv2("./Data/IndustryData.csv")

#create regions from industry data
industry <- industry %>% mutate(ucan = UCAN, 
              emea = Europe+Africa+West_Asia, 
              latam = South_America+Central_America, 
              apac = Asia_sum- West_Asia + Oceania_Australia)
industry

industry_selected <- industry %>%
  select(ucan:apac) * 1000
industry_selected <- industry_selected %>% mutate(year = industry$Year)
industry_selected <- industry_selected %>% select(year, everything(), -APAC)
industry_selected <- as_tibble(industry_selected)

#tidying industry data
industry_tidy <- pivot_longer(industry_selected, cols=("ucan":"apac"), names_to ="region")

#descriptive industry data
industry_tidy %>% ggplot(aes(x = year, y = value, col=region)) + 
  geom_line()


#### load netflix data ####
netflix <- read_csv2("./Data/NetflixData.csv")

#tidying netflix data
netflix
netflix_tidy <- pivot_longer(netflix, cols = ("ucan":"apac"), names_to = "region")
netflix_tidy <- netflix_tidy %>%  mutate(year = Year)



#### joining data set ####

df <- full_join(industry_tidy, netflix_tidy, by = c("year", "region"))
df_tidy <- df %>% mutate(value_industry = value.x/1000, value_netflix = value.y/1000) %>% select(-value.x, -value.y, -Year)
df_tidy

#### Calculating the factors ####

#ucan

ucan <- df_tidy %>% filter(region == "ucan", year < 2020)
ucan_fit <- lm(value_netflix~value_industry, data= ucan)
summary(ucan_fit)

#emea

emea <- df_tidy %>% filter(region == "emea", year < 2020)
emea_fit <- lm(value_netflix~value_industry, data= emea)
summary(emea_fit)

#latam

latam <- df_tidy %>% filter(region == "latam", year < 2020)
latam_fit <- lm(value_netflix~value_industry, data= latam)
summary(latam_fit)

#apac

apac <- df_tidy %>% filter(region == "apac", year < 2020)
apac_fit <- lm(value_netflix~value_industry, data= apac)
summary(apac_fit)


#### forecast subscribers till 2025 #####

years <- c(2017:2025)

#ucan

ucan_predict <- df_tidy %>% filter(region =="ucan", year >= 2020) %>% 
  mutate(value_netflix = coefficients(ucan_fit)[1] + coefficients(ucan_fit)[2]*value_industry)

#emea

emea_predict <- df_tidy %>% filter(region =="emea", year >= 2020) %>% 
  mutate(value_netflix = coefficients(emea_fit)[1] + coefficients(emea_fit)[2]*value_industry)

#latam

latam_predict <- df_tidy %>% filter(region =="latam", year >= 2020) %>% 
  mutate(value_netflix = coefficients(latam_fit)[1] + coefficients(latam_fit)[2]*value_industry)

#apac

apac_predict <- df_tidy %>% filter(region =="apac", year >= 2020) %>% 
  mutate(value_netflix = coefficients(apac_fit)[1] + coefficients(apac_fit)[2]*value_industry)


#merge dataframes
total_predicted <- rbind(ucan_predict, emea_predict, latam_predict, apac_predict) %>% arrange(year)
df_2019 <- df_tidy %>% filter(year < 2020)
total <- rbind(df_2019, total_predicted)

#plot netflix growth
total %>% ggplot(aes(year, value_netflix, col=region)) + 
  geom_line()


#wide data format
netflix_forecast <- total %>% select(-value_industry) %>% 
  pivot_wider(names_from =region, values_from = value_netflix)

netflix_forecast
        

