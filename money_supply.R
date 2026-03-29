#money supply A2.2.1 - Components of M4 https://www.bankofengland.co.uk/statistics/Tables
library(plm)
library(lmtest)
library(dplyr)
library(tidyr)
library(stargazer)
library(ggplot2)
library(tidyverse)
library(scales)
library(ggthemes)
##Retail deposits and cash, M4 - excluding wholsesale 

df <- read.csv("data/Bank of England  Database (3).csv") %>% 
  rename(M4 = 2) %>% 
  mutate(Date = as.Date(Date, format = "%d %b %y")) %>% 
  arrange(Date)

#growth
df_growth <- df %>% 
  mutate(
    mom = 100 * (M4 - lag(M4)) / lag(M4),
    growth_3m_ann = ((M4 / lag(M4, 3))^4 - 1) * 100,
    growth_6m_ann = ((M4 / lag(M4, 6))^2 - 1) * 100,
    growth_yoy = 100 * (M4 - lag(M4, 12)) / lag(M4, 12)
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


