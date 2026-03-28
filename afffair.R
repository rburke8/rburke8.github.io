##Affair
library(AER)
library(plm)
library(lmtest)
library(dplyr)
library(tidyr)
library(stargazer)
library(webshot2)
library(htmltools)
library(webshot2)
data("Affairs")
?Affairs
df <- Affairs %>% 
  mutate(male = if_else(gender == "male",1,0),
         affairs = if_else(affairs > 0,1,0),
         degree = if_else(education >= 16,1,0),
         children = if_else(children == "yes",1,0)) %>% 
  select(-rating)

probit <- glm(affairs ~ male + yearsmarried + children + degree + religiousness,
              data = df,
              family = binomial(link = "probit"))
logit <- glm(affairs ~ male + yearsmarried + children + degree + religiousness,
             data = df,
             family = binomial(link = "logit"))
# Create the HTML table
rob_se <- sqrt(diag(vcovHC(probit, type = "HC1")))
html_code <- stargazer(probit,
                       se = rob_se,
                       type = "html",
                       title = "Probit Model with Robust Standard Errors")

# Save HTML to a file
html_file <- "probit_table.html"
writeLines(html_code, html_file)

webshot(html_file, file = "probit_table.png", zoom = 2)

                    
                    
                    