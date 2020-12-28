library(tidyverse)
library(stringr)
library(lubridate)
library(ggthemes)


####Preparation####
# load 10 years S&P500 Price History
SnP <- read_csv2("./Data/SnP500_monthly.csv")

# load 10 years Netflix Price History
Netflix <- read_csv2("./Data/NetflixStockData.csv")

# rename column names
SnP <- SnP %>% rename(Date = `Exchange Date`, Chg = `%Chg`)
Netflix <- Netflix %>% rename(Date = `Exchange Date`, Chg = `%Chg`, Turnover = `Turnover - USD`)

# change format of percent returns of SnP
SnP$Chg <- str_replace(SnP$Chg, "%", "")
SnP$Chg <- str_replace(SnP$Chg, ",", ".")
SnP$Chg <- SnP$Chg %>% as.numeric(SnP$Chg)
SnP$Chg <- SnP$Chg/100

# change format of percent returns of Netflix
Netflix$Chg <- str_replace(Netflix$Chg, "%", "")
Netflix$Chg <- str_replace(Netflix$Chg, ",", ".")
Netflix$Chg <- Netflix$Chg %>% as.numeric(Netflix$Chg)
Netflix$Chg <- Netflix$Chg/100

# merging into one data frame
Netflix_Return <- Netflix %>% select(Date, Chg)
SnP_Return <- SnP %>% select(Date, Chg)
df <- inner_join(Netflix_Return, SnP_Return, by = "Date")
df <- df %>% mutate(Chg_Netflix = Chg.x, Chg_SnP = Chg.y) %>% select(Date, Chg_Netflix, Chg_SnP)
df

#### 10 year beta ####
fit <- lm(df$Chg_Netflix ~ df$Chg_SnP)
summary(fit)

df %>% ggplot(aes(Chg_Netflix, Chg_SnP)) + 
  geom_point() + 
  geom_smooth(method="lm") + 
  theme_tufte()

#### 5 year beta ####
Netflix_5 <- Netflix %>% filter(Date > "2015-01-01")
SnP_5 <- SnP %>% filter(Date > "2015-01-01")

fit <- lm(Netflix_5$Chg ~ SnP_5$Chg)
summary(fit)

#### 3 year beta ####
Netflix_3 <- Netflix %>% filter(Date > "2017-01-01")
SnP_3 <- SnP %>% filter(Date > "2017-01-01")

fit <- lm(Netflix_3$Chg ~ SnP_3$Chg)
summary(fit)



