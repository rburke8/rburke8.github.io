#money supply A2.2.1 - Components of M4 https://www.bankofengland.co.uk/statistics/Tables
library(plm)
library(lmtest)
library(dynlm)
library(dplyr)
library(vars)
library(sandwich)
library(tidyr)
library(stargazer)
library(ggplot2)
library(lubridate)
library(quantmod)
library(tidyverse)
library(stringr)
library(scales)
library(ggthemes)
##Retail deposits and cash, M4 - excluding wholsesale 

df <- read.csv("data/Bank of England  Database (3).csv") %>% 
  rename(M4 = 2)%>%
  mutate(Date = as.Date(Date, format = "%d %b %y"),
         Date = floor_date(Date, "month"))
  
#growth
df_growth <- df %>% 
  mutate(
    basic = (M4 - lag(M4)) / lag(M4),
    mom = 100 * (M4 - lag(M4)) / lag(M4),
    growth_3m_ann = ((M4 / lag(M4, 3))^4 - 1) * 100,
    growth_6m_ann = ((M4 / lag(M4, 6))^2 - 1) * 100,
    money_growth_yoy = 100 * (M4 - lag(M4, 12)) / lag(M4, 12)
  )



#----
#plots
ggplot(df, aes(x = Date, y = M4)) + 
  geom_line(color = "yellow", linewidth = 1.5) +
  scale_y_continuous(labels = comma) +
  labs("Year", "Growth") +
  ggtitle("Money Supply, sterling millions 1982 - 2026") +
  theme(plot.title = element_text(size = 12 , face = "bold", colour = "white"))+
  theme(panel.background = element_rect("black")) +
  theme(plot.background = element_rect("black"))

ggplot(df_growth, aes(x = Date, y = mom)) + 
  geom_line(color = "yellow", linewidth = 0.5) +
  labs("Year", "Growth") +
  scale_y_continuous(labels = comma) +
  ggtitle("% M4 growth - month-on-month") +
  theme(plot.title = element_text(size = 12 , face = "bold", colour = "white"))+
  theme(panel.background = element_rect("black")) +
  theme(plot.background = element_rect("black"))

ggplot(df_growth, aes(x = Date, y = growth_3m_ann)) + 
  geom_line(color = "yellow", linewidth = 0.5) +
  labs("Year", "Growth", colour = "white") +
  scale_y_continuous(labels = comma) +
  labs(x = "year", y = "growth") +
  ggtitle("% M4 growth - 3-month annualised") +
  theme(plot.title = element_text(size = 12 , face = "bold", colour = "white"))+
  theme(panel.background = element_rect("black")) +
  theme(plot.background = element_rect("black"))

#----
#Inflation
inflation <- read.csv("data/series-290326.csv") %>% 
  slice(193:638) %>% 
  rename(inflation = 2,
         Date = Title) %>% 
  mutate(
    # 1. Parse "1989 JAN" → Date
    Date = as.Date(paste0(Date, " 01"), format = "%Y %b %d"),
    
    # 2. Strip day → first of month
    Date = floor_date(Date, "month")
  ) %>% 
  left_join(df_growth, by = "Date") %>% 
  mutate(inflation = as.numeric(inflation))

ggplot(data = inflation, aes(x = Date, y = inflation)) +
  geom_line(color = "yellow", linewidth = 0.5) +
  labs(x = "Date", y = "Inflation") +
  ggtitle("Annual Inflation (%)") +
  theme(plot.title = element_text(face = "bold", size = 9))+
  theme(axis.title.x = element_text(color = "white"))+
  theme(axis.title.y = element_text(color = "white"))+
  theme(panel.background = element_rect("black")) +
  theme(plot.background = element_rect("black"))+
  theme(plot.title = element_text(color = "white"))

