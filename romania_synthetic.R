
#----
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
comp_per_employee <- read.csv("Barbara/worldbankdata/comp_per_employee.csv", skip = 4)  %>%
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

emp_rate <- read.csv("Barbara/worldbankdata/emp_rate.csv", skip = 4)%>%
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

gdp_per_cap <- read.csv("Barbara/worldbankdata/gdp_per_cap.csv", skip = 4)%>%
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

gov_cons <- read.csv("Barbara/worldbankdata/gov_cons.csv", skip = 4)%>%
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

gross_cap_formation <- read.csv("Barbara/worldbankdata/gross_cap_formation.csv", skip = 4)%>%
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

labour_produc <- read.csv("Barbara/worldbankdata/labour_produc.csv", skip = 4)%>%
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

unemp_rate <- read.csv("Barbara/worldbankdata/unemp_rate.csv", skip = 4)%>%
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

agri <- read.csv("Barbara/worldbankdata/agri_va.csv", skip = 4)%>%
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

exports <- read.csv("Barbara/worldbankdata/exports.csv", skip = 4)%>%
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

imports <- read.csv("Barbara/worldbankdata/imports.csv", skip = 4)%>%
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

industry_va <- read.csv("Barbara/worldbankdata/industry_va.csv", skip = 4)%>%
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

pop_growth <- read.csv("Barbara/worldbankdata/pop_growth.csv", skip = 4)%>%
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

randd <- read.csv("Barbara/worldbankdata/randd.csv", skip = 4)%>%
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
  left_join(randd,     by = c("Country.Code","year","Country.Name"))

countries <- c(
  "Argentina","Belarus","Brazil","Chile","China","Colombia","Costa Rica",
  "Indonesia","India","Iran, Islamic Rep.","Jordan","Korea, Rep.","Mexico",
  "Morocco","Malaysia","Panama","Peru","Romania","Russian Federation","Thailand",
  "Turkiye","Ukraine","Uruguay","South Africa"
  
)


#----
panel <- comp_per_employee %>%
  select(
    -matches("X\\."),   
    -matches("^X"),
    -wage_pc,
    -r_and_d) %>% 
  filter(Country.Name %in% countries)

predictors <- c("gov_consump", "cap_form", "lab_produc", "agri_vala", "exportss", "importss","industry_vala", "popu_growth")

test <- panel %>%
  filter(year %in% 1991:2021) %>%
  group_by(Country.Name) %>%
  summarise(across(all_of(predictors), ~ sum(is.na(.x)))) %>%
  filter(if_any(all_of(predictors), ~ .x > 0))

panel <- panel %>% 
  filter(year>=1991)
# Set up everything & construct the synthetic control
gdp_out <- panel %>%
  
  synthetic_control(
    outcome = gdp_pc,
    unit = Country.Name,
    time = year,
    i_unit = "Romania",
    i_time = 2006,
    generate_placebos = TRUE
  ) %>%
  
  # ---- Average macro predictors ----
generate_predictor(
  time_window = 1991:2006,
  avg_gdp          = mean(gdp_pc, na.rm = TRUE),
  avg_gov_consump  = mean(gov_consump, na.rm = TRUE),
  avg_cap_form     = mean(cap_form, na.rm = TRUE),
  avg_lab_produc   = mean(lab_produc, na.rm = TRUE),
  avg_agri     = mean(agri_vala, na.rm = TRUE),
  avg_xports     = mean(exportss, na.rm = TRUE),
  avg_imports     = mean(importss, na.rm = TRUE),
  avg_industry    = mean(industry_vala, na.rm = TRUE),
  avg_pop_growth    = mean(popu_growth, na.rm = TRUE)
) %>%
  
  # ---- Lagged GDP predictors ----
generate_predictor(
  time_window = 2002:2006,
  lag5_gdp = mean(gdp_pc, na.rm = TRUE)
) %>%
  
  generate_predictor(
    time_window = 2006,
    lag1_gdp = gdp_pc
  ) %>%
  
  # ---- Fit weights ----
generate_weights(
  optimization_window = 1991:2006,
  margin_ipop = .02,
  sigf_ipop = 7,
  bound_ipop = 6
) %>%
  
  # ---- Build synthetic control ----
generate_control()

# Weighting of units and variables
gdp_out %>% plot_weights()

# Comparability of synthetic control to treated unit
gdp_out %>% grab_balance_table()

#graph comparing paths
gdp_out %>% plot_trends()
# Graph of difference between the two
gdp_out %>% plot_differences()


#----
#plots
plot_data <- panel %>% 
  filter(year>=1991,
         Country.Name == "Romania")
maxgdp <- max(plot_data$gdp_pc)




p1 <- ggplot(data = plot_data, aes(x = year, y = gdp_pc)) +
  geom_line(linewidth = 0.5, color = "yellow") +
  labs(x = "Year", y = "GDP per capita") +
  theme(plot.background = element_rect("black")) +
  theme(panel.background = element_rect("black")) +
  ggtitle("Romania GDP_pc") +
  theme(axis.title.x = element_text(color = "white")) +
  theme(axis.title.y = element_text(color = "white")) +
  theme(plot.title = element_text(color = "white", face = "bold", size = 9)) +
  geom_vline(xintercept = 2007, linetype = "solid" , size = 1, color = "red") +
  geom_text(aes(x = 2007,y = 12000, label = "Accession to EU"),color = "green", size = 2)

p2 <- ggplot(data = plot_data, aes(x = year, y = cap_form)) +
  geom_line(linewidth = 0.5, color = "yellow") +
  labs(x = "Year", y = "Capital formation") +
  theme(plot.background = element_rect("black")) +
  theme(panel.background = element_rect("black")) +
  ggtitle("Romania Capital Formation") +
  theme(axis.title.x = element_text(color = "white")) +
  theme(axis.title.y = element_text(color = "white")) +
  theme(plot.title = element_text(color = "white", face = "bold", size = 9))+
  geom_vline(xintercept = 2007, linetype = "solid" , size = 1, color = "red") +
  geom_text(aes(x = 2007,y = 32.5, label = "Accession to EU"),color = "green", size = 2)

p3 <- ggplot(data = plot_data, aes(x = year, y = lab_produc)) +
  geom_line(linewidth = 0.5, color = "yellow") +
  labs(x = "Year", y = "Labour productivity") +
  theme(plot.background = element_rect("black")) +
  theme(panel.background = element_rect("black")) +
  ggtitle("Romania Labour Productivity") +
  theme(axis.title.x = element_text(color = "white")) +
  theme(axis.title.y = element_text(color = "white")) +
  theme(plot.title = element_text(color = "white", face = "bold", size = 9))+
  geom_vline(xintercept = 2007, linetype = "solid" , size = 1, color = "red") +
  geom_text(aes(x = 2007,y = 100000, label = "Accession to EU"),color = "green", size = 2)

p4 <- ggplot(data = plot_data, aes(x = year, y = gov_consump)) +
  geom_line(linewidth = 0.5, color = "yellow") +
  labs(x = "Year", y = "Government Consumption") +
  theme(plot.background = element_rect("black")) +
  theme(panel.background = element_rect("black")) +
  ggtitle("Romania Government Consumption") +
  theme(axis.title.x = element_text(color = "white")) +
  theme(axis.title.y = element_text(color = "white")) +
  theme(plot.title = element_text(color = "white", face = "bold", size = 9)) +
  geom_vline(xintercept = 2007, linetype = "solid" , size = 1, color = "red") +
  geom_text(aes(x = 2007,y = 19, label = "Accession to EU"),color = "green", size = 2)


grid.arrange(p1,p2,p3,p4, nrow = 2)


p <- (p1 | p2) /
  (p3 | p4)  
p
ggsave(filename = "rom_synthetic.png", plot = p)