#Fixed effects stuff
library(AER)
library(patchwork)
library(tidyverse)
library(gridExtra)
library(tidyr)
library(dplyr)
library(tidyr)
library(lmtest)
library(dynlm)
library(quantmod)
library(plm)

#----
comp_per_employee <- read.csv("worldbankdata/comp_per_employee.csv", skip = 4)  %>%
  rename_with(
    ~ gsub("^X|\\.x$", "", .x),   
    matches("^X\\d{4}")           
  )%>%
  pivot_longer(
    cols = matches("^\\d{4}$"),  
    names_to = "year",
    values_to = "wage_pc"
  ) %>%
  mutate(year = as.numeric(year)) %>% 
  select(- c("Indicator.Name", "Indicator.Code"))

emp_rate <- read.csv("worldbankdata/emp_rate.csv", skip = 4)%>%
  rename_with(
    ~ gsub("^X|\\.x$", "", .x),   
    matches("^X\\d{4}")           
  )%>%
  pivot_longer(
    cols = matches("^\\d{4}$"),  
    names_to = "year",
    values_to = "emp_rate"
  ) %>%
  mutate(year = as.numeric(year))%>% 
  select(- c("Indicator.Name", "Indicator.Code"))

gdp_per_cap <- read.csv("worldbankdata/gdp_per_cap.csv", skip = 4)%>%
  rename_with(
    ~ gsub("^X|\\.x$", "", .x),   
    matches("^X\\d{4}")           
  )%>%
  pivot_longer(
    cols = matches("^\\d{4}$"),  
    names_to = "year",
    values_to = "gdp_pc"
  ) %>%
  mutate(year = as.numeric(year))%>% 
  select(- c("Indicator.Name", "Indicator.Code"))

gov_cons <- read.csv("worldbankdata/gov_cons.csv", skip = 4)%>%
  rename_with(
    ~ gsub("^X|\\.x$", "", .x),   
    matches("^X\\d{4}")           
  )%>%
  pivot_longer(
    cols = matches("^\\d{4}$"),  
    names_to = "year",
    values_to = "gov_consump") %>%
  mutate(year = as.numeric(year))%>% 
  select(- c("Indicator.Name", "Indicator.Code"))

gross_cap_formation <- read.csv("worldbankdata/gross_cap_formation.csv", skip = 4)%>%
  rename_with(
    ~ gsub("^X|\\.x$", "", .x),   
    matches("^X\\d{4}")           
  )%>%
  pivot_longer(
    cols = matches("^\\d{4}$"),  
    names_to = "year",
    values_to = "cap_form") %>%
  mutate(year = as.numeric(year))%>% 
  select(- c("Indicator.Name", "Indicator.Code"))

labour_produc <- read.csv("worldbankdata/labour_produc.csv", skip = 4)%>%
  rename_with(
    ~ gsub("^X|\\.x$", "", .x),   
    matches("^X\\d{4}")           
  )%>%
  pivot_longer(
    cols = matches("^\\d{4}$"),  
    names_to = "year",
    values_to = "lab_produc"
  ) %>%
  mutate(year = as.numeric(year))%>% 
  select(- c("Indicator.Name", "Indicator.Code"))

unemp_rate <- read.csv("worldbankdata/unemp_rate.csv", skip = 4)%>%
  rename_with(
    ~ gsub("^X|\\.x$", "", .x),   
    matches("^X\\d{4}")           
  )%>%
  pivot_longer(
    cols = matches("^\\d{4}$"),  
    names_to = "year",
    values_to = "unemployment_rate"
  ) %>%
  mutate(year = as.numeric(year))%>% 
  select(- c("Indicator.Name", "Indicator.Code"))

agri <- read.csv("worldbankdata/agri_va.csv", skip = 4)%>%
  rename_with(
    ~ gsub("^X|\\.x$", "", .x),   
    matches("^X\\d{4}")           
  )%>%
  pivot_longer(
    cols = matches("^\\d{4}$"),  
    names_to = "year",
    values_to = "agri_vala"
  ) %>%
  mutate(year = as.numeric(year))%>% 
  select(- c("Indicator.Name", "Indicator.Code"))

exports <- read.csv("worldbankdata/exports.csv", skip = 4)%>%
  rename_with(
    ~ gsub("^X|\\.x$", "", .x),   
    matches("^X\\d{4}")           
  )%>%
  pivot_longer(
    cols = matches("^\\d{4}$"),  
    names_to = "year",
    values_to = "exportss"
  ) %>%
  mutate(year = as.numeric(year))%>% 
  select(- c("Indicator.Name", "Indicator.Code"))

imports <- read.csv("worldbankdata/imports.csv", skip = 4)%>%
  rename_with(
    ~ gsub("^X|\\.x$", "", .x),   
    matches("^X\\d{4}")           
  )%>%
  pivot_longer(
    cols = matches("^\\d{4}$"),  
    names_to = "year",
    values_to = "importss"
  ) %>%
  mutate(year = as.numeric(year))%>% 
  select(- c("Indicator.Name", "Indicator.Code"))

industry_va <- read.csv("worldbankdata/industry_va.csv", skip = 4)%>%
  rename_with(
    ~ gsub("^X|\\.x$", "", .x),   
    matches("^X\\d{4}")           
  )%>%
  pivot_longer(
    cols = matches("^\\d{4}$"),  
    names_to = "year",
    values_to = "industry_vala"
  ) %>%
  mutate(year = as.numeric(year))%>% 
  select(- c("Indicator.Name", "Indicator.Code"))

pop_growth <- read.csv("worldbankdata/pop_growth.csv", skip = 4)%>%
  rename_with(
    ~ gsub("^X|\\.x$", "", .x),   
    matches("^X\\d{4}")           
  )%>%
  pivot_longer(
    cols = matches("^\\d{4}$"),  
    names_to = "year",
    values_to = "popu_growth"
  ) %>%
  mutate(year = as.numeric(year))%>% 
  select(- c("Indicator.Name", "Indicator.Code"))

randd <- read.csv("worldbankdata/randd.csv", skip = 4)%>%
  rename_with(
    ~ gsub("^X|\\.x$", "", .x),   
    matches("^X\\d{4}")           
  )%>%
  pivot_longer(
    cols = matches("^\\d{4}$"),  
    names_to = "year",
    values_to = "r_and_d"
  ) %>%
  mutate(year = as.numeric(year))%>% 
  select(- c("Indicator.Name", "Indicator.Code"))

trade_openness <- read.csv("worldbankdata/trade_openness.csv", skip = 4)%>%
  rename_with(
    ~ gsub("^X|\\.x$", "", .x),   
    matches("^X\\d{4}")           
  )%>%
  pivot_longer(
    cols = matches("^\\d{4}$"),  
    names_to = "year",
    values_to = "trade_open"
  ) %>%
  mutate(year = as.numeric(year))%>% 
  select(- c("Indicator.Name", "Indicator.Code"))


comp_per_employee <- comp_per_employee %>% 
  left_join(gdp_per_cap,       by = c("Country.Code","year", "Country.Name")) %>%
  left_join(emp_rate,          by = c("Country.Code","year","Country.Name")) %>%
  left_join(gov_cons,          by = c("Country.Code","year","Country.Name")) %>%
  left_join(gross_cap_formation, by = c("Country.Code","year","Country.Name")) %>%
  left_join(labour_produc,     by = c("Country.Code","year","Country.Name")) %>%
  left_join(unemp_rate,       by = c("Country.Code","year","Country.Name")) %>% 
  left_join(agri,     by = c("Country.Code","year","Country.Name")) %>%
  left_join(exports,     by = c("Country.Code","year","Country.Name")) %>%
  left_join(imports,     by = c("Country.Code","year","Country.Name")) %>%
  left_join(industry_va,     by = c("Country.Code","year","Country.Name")) %>%
  left_join(pop_growth,     by = c("Country.Code","year","Country.Name")) %>%
  left_join(randd,     by = c("Country.Code","year","Country.Name")) %>% 
  left_join(trade_openness,     by = c("Country.Code","year","Country.Name"))




#--------------------
df <- comp_per_employee%>%
  select(
    -matches("X\\."),   
    -matches("^X"))

reg1 <- plm(gdp_pc ~ gov_consump + cap_form + agri_vala + trade_open + industry_vala + r_and_d + popu_growth,
            data = df,
            index = c("Country.Name", "year"),
            model = "within", 
            effect = "twoways")

coeftest(reg1, vcov. = vcovHC, type = "HC1")
summary(reg1)
