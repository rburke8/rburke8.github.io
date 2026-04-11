#Rwandan genocide
library(AER)
library(patchwork)
library(tidyverse)
library(dplyr)
library(gridExtra)
library(purrr)
library(readr)
library("MatchIt")
library("cobalt")
library("matrixStats")
library(ivreg)
library("rdrobust")
library("estimatr")
library("Synth") 
library("tidysynth")  
library("jtools")
library(tidyr)
library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(tidysynth)


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


countries_r <- c(
  "Benin","Togo","Ghana","Mali","Niger","Burkina Faso","Senegal","Cameroon","Gabon","Zambia","Malawi","Tanzania","Kenya","Uganda","Lesotho","Botswana","Madagascar","Mauritius","Sao Tome and Principe","Cape Verde","Gambia","Guinea","Equatorial Guinea","Eritrea","Djibouti","Namibia","Swaziland","Central African Republic","Comoros","Seychelles","Fiji","Papua New Guinea","Solomon Islands","Vanuatu","Bangladesh","Nepal","Laos","Bhutan","Vietnam","Mongolia","Honduras","Nicaragua","Bolivia","Paraguay","Guyana","Suriname"
  
)


#----
predictors <- c("gov_consump", "cap_form", "agri_vala", "trade_open","industry_vala", "popu_growth")

panel_rw <- comp_per_employee %>%
  select(
    -matches("X\\."),   
    -matches("^X"),
    -wage_pc,
    -r_and_d) %>% 
  filter(Country.Name %in% countries_r) %>% 
  select(Country.Name, year, c("gov_consump", "cap_form", "lab_produc", "agri_vala", "trade_open","industry_vala", "popu_growth", "gdp_pc"))


panel_r <- panel_rw %>% 
  filter(!Country.Name %in% c("Malawi", "Zambia", "Lesotho", "Tanzania", "Uganda", "Ghana")) %>% 
  select(-lab_produc) %>% 
  filter(year != 2025)

test <- panel_r %>%
  filter(year>=1984) %>% 
  group_by(Country.Name) %>%
  summarise(across(all_of(predictors), ~ sum(is.na(.x)))) %>%
  filter(if_any(all_of(predictors), ~ .x > 0))

panel_r <- panel_r %>% 
  filter(!Country.Name %in% test$Country.Name)
# Set up everything & construct the synthetic control
gdp_out_r <- panel_r %>%
  
  synthetic_control(
    outcome = gdp_pc,
    unit = Country.Name,
    time = year,
    i_unit = "Rwanda",
    i_time = 1993,
    generate_placebos = TRUE
  ) %>%
  
  # ---- Average macro predictors ----
generate_predictor(
  time_window = 1984:1993,
  avg_gdp          = mean(gdp_pc, na.rm = TRUE),
  avg_gov_consump  = mean(gov_consump, na.rm = TRUE),
  avg_cap_form     = mean(cap_form, na.rm = TRUE),
  avg_agri     = mean(agri_vala, na.rm = TRUE),
  avg_trade = mean(trade_open, na.rm = TRUE),
  avg_industry    = mean(industry_vala, na.rm = TRUE),
  avg_pop_growth    = mean(popu_growth, na.rm = TRUE)
) %>%
  
  # ---- Lagged GDP predictors ----
generate_predictor(
  time_window = 1984:1993,
  lag5_gdp = mean(gdp_pc, na.rm = TRUE)
) %>%
  
  generate_predictor(
    time_window = 1993,
    lag1_gdp = gdp_pc
  ) %>%
  
  # ---- Fit weights ----
generate_weights(
  optimization_window = 1984:1993,
  margin_ipop = .02,
  sigf_ipop = 7,
  bound_ipop = 6
) %>%
  
  # ---- Build synthetic control ----
generate_control()

# Weighting of units and variables
gdp_out_r %>% plot_weights()

# Comparability of synthetic control to treated unit
gdp_out_r %>% grab_balance_table()

#graph comparing paths
gdp_out_r %>% plot_trends()
# Graph of difference between the two
gdp_out_r %>% plot_differences()
