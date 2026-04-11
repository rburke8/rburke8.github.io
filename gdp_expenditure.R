library(readxl)
library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(zoo)
library(scales)

consumption <- read_xlsx("gdpexpenditure.xlsx",
                         sheet = "ABJR",
                         skip = 3) %>% 
  select("Publication date and time period", starts_with("2025")) %>% 
  select("Publication date and time period", "2025 Q4 QNA") %>% 
  rename("Date" = "Publication date and time period",
          "cons" = "2025 Q4 QNA")

inventory <- read_xlsx("gdpexpenditure.xlsx",
                         sheet = "CAFU",
                         skip = 3) %>% 
  select("Publication date and time period", starts_with("2025")) %>% 
  select("Publication date and time period", "2025 Q4 QNA") %>% 
  rename("Date" = "Publication date and time period",
         "inventory" = "2025 Q4 QNA")

exports <- read_xlsx("gdpexpenditure.xlsx",
                       sheet = "IKBK",
                       skip = 3) %>% 
  select("Publication date and time period", starts_with("2025")) %>% 
  select("Publication date and time period", "2025 Q4 QNA") %>% 
  rename("Date" = "Publication date and time period",
         "xports" = "2025 Q4 QNA")

imports <- read_xlsx("gdpexpenditure.xlsx",
                     sheet = "IKBL",
                     skip = 3) %>% 
  select("Publication date and time period", starts_with("2025")) %>% 
  select("Publication date and time period", "2025 Q4 QNA") %>% 
  rename("Date" = "Publication date and time period",
         "importss" = "2025 Q4 QNA")

gov <- read_xlsx("gdpexpenditure.xlsx",
                     sheet = "NMRY",
                     skip = 3) %>% 
  select("Publication date and time period", starts_with("2025")) %>% 
  select("Publication date and time period", "2025 Q4 QNA") %>% 
  rename("Date" = "Publication date and time period",
         "gov_cons" = "2025 Q4 QNA")

capital <- read_xlsx("gdpexpenditure.xlsx",
                 sheet = "NPQT",
                 skip = 3) %>% 
  select("Publication date and time period", starts_with("2025")) %>% 
  select("Publication date and time period", "2025 Q4 QNA") %>% 
  rename("Date" = "Publication date and time period",
         "capital" = "2025 Q4 QNA")

df <- capital %>% 
  left_join(consumption, by = "Date") %>% 
  left_join(exports, by = "Date") %>% 
  left_join(gov, by = "Date") %>%
  left_join(imports, by = "Date") %>% 
  left_join(inventory, by = "Date") %>% 
  mutate(NX = xports - importss,
         Date = as.yearqtr(Date),
         invest = inventory + capital) %>% 
  select(-c("xports", "importss", "inventory", "capital"))

ggplot(data = df, aes(x = Date, y = NX)) +
         geom_line() +
  scale_y_continuous(labels = comma)

long <- df %>% 
  pivot_longer(cols = !Date, names_to = "metric", values_to = "value")

ggplot(data = long, aes(fill = metric, x = Date, y = value)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_y_continuous(labels = comma)

ggplot(data = df, aes(x = Date, y = NX)) +
  geom_line(color = "forestgreen") +
  scale_y_continuous(labels = comma) +
  labs(x = "Date", y = "£millions")

       