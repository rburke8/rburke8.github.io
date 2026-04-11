#----
#setup
#https://fredblog.stlouisfed.org/2024/12/leveraging-r-for-powerful-data-analysis/
library(dplyr)
library(tidyverse)
library(tidyr)
library(ggplot2)
library(lubridate)
library(fredo)
library(jsonlite)
library(rvest)
library(dplyr)
library(tidyverse)
library(tidyr)
library(scales)
library(lubridate)
library(patchwork)


api_key <- Sys.getenv("FRED_API_KEY")

#----
#fetching some data
# Define the series IDs and date range

#----
#us bank deposits
series_ids <- c("DPSACBW027SBOG")
start_date <- "1973-01-03"
end_date <- "2026-03-18"


df <- fredo(api_key, series_ids, start_date, end_date)

ggplot(df, aes(x = date, y = value)) + 
  geom_line(color = "yellow", linewidth = 1) +
  labs(
    x = "Year",
    y = "Billions of Dollars",
    title = "All deposits, commercial banks"
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme(
    plot.title = element_text(size = 9, face = "bold", colour = "white"),
    panel.background = element_rect(fill = "black"),
    plot.background = element_rect(fill = "black"),
    axis.title.x = element_text(color = "white"),
    axis.title.y = element_text(color = "white")
  )

#----
series_ids2 <- "NGDPRSAXDCGBQ"
start_date2 <- "1955-01-01"
end_date2 <- "2025-10-01"


df2 <- fredo(api_key, series_ids2, start_date2, end_date2)

ggplot(df2, aes(x = date, y = value)) + 
  geom_line(color = "yellow", linewidth = 1) +
  labs(
    x = "Year",
    y = "£ millions",
    title = "UK Real GDP"
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme(
    plot.title = element_text(size = 9, face = "bold", colour = "white"),
    panel.background = element_rect(fill = "black"),
    plot.background = element_rect(fill = "black"),
    axis.title.x = element_text(color = "white"),
    axis.title.y = element_text(color = "white")
  )

#----
series_ids3 <- "LPRGDPUKA"
start_date3 <- "1856-01-01"
end_date3 <- "2016-01-01"


df3 <- fredo(api_key, series_ids3, start_date3, end_date3)

ggplot(df3, aes(x = date, y = value)) + 
  geom_line(color = "yellow", linewidth = 1) +
  labs(
    x = "Year",
    y = "Index: 2013 = 100",
    title = "Real GDP Per Person in the United Kingdom"
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme(
    plot.title = element_text(size = 9, face = "bold", colour = "white"),
    panel.background = element_rect(fill = "black"),
    plot.background = element_rect(fill = "black"),
    axis.title.x = element_text(color = "white"),
    axis.title.y = element_text(color = "white"), 
    axis.text = element_text(color = "white")
  )

#----
series_ids4 <- "DEBTTLGBA188A"
start_date4 <- "1990-01-01"
end_date4 <- "2024-01-01"


df4 <- fredo(api_key, series_ids4, start_date4, end_date4)

ggplot(df4, aes(x = date, y = value)) + 
  geom_line(color = "yellow", linewidth = 1) +
  labs(
    x = "Year",
    y = "% of GDP",
    title = "UK Government Debt"
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme(
    plot.title = element_text(size = 9, face = "bold", colour = "white"),
    panel.background = element_rect(fill = "black"),
    plot.background = element_rect(fill = "black"),
    axis.title.x = element_text(color = "white"),
    axis.title.y = element_text(color = "white"), 
    axis.text = element_text(color = "white")
  )

#----
series_ids5 <- "LFWA64TTGBQ647S"
start_date5 <- "1971-01-01"
end_date5 <- "2025-10-01"


df5 <- fredo(api_key, series_ids5, start_date5, end_date5)

ggplot(df5, aes(x = date, y = value)) + 
  geom_line(color = "yellow", linewidth = 1) +
  labs(
    x = "Year",
    y = "Count",
    title = "UK Working Age Population - 1971 - 2025"
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme(
    plot.title = element_text(size = 9, face = "bold", colour = "white"),
    panel.background = element_rect(fill = "black"),
    plot.background = element_rect(fill = "black"),
    axis.title.x = element_text(color = "white"),
    axis.title.y = element_text(color = "white"), 
    axis.text = element_text(color = "white")
  )

#----
series_ids6 <- "NYGDPPCAPKDGBR"
start_date6 <- "1960-01-01"
end_date6 <- "2024-01-01"

df6 <- fredo(api_key, series_ids6, start_date6, end_date6) %>% 
  mutate(date = as.Date(date, format = "%Y-%m-%d")) %>% 
  filter(year(date) >=2007)

ggplot(df6, aes(x = date, y = value)) + 
  geom_line(color = "yellow", linewidth = 1) +
  labs(
    x = "Year",
    y = "2010 dollars",
    title = "UK GDP per capita"
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme(
    plot.title = element_text(size = 9, face = "bold", colour = "white"),
    panel.background = element_rect(fill = "black"),
    plot.background = element_rect(fill = "black"),
    axis.title.x = element_text(color = "white"),
    axis.title.y = element_text(color = "white"), 
    axis.text = element_text(color = "white")
  )
