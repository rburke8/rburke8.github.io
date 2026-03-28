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

coeftest(probit)
# Create the HTML table
html_code <- stargazer(probit,
                       type = "html",
                       title = "Probit Model")
# Save HTML to a file
html_file <- "probit_table.html"
writeLines(html_code, html_file)

webshot(html_file, file = "probit_table.png", zoom = 2)

# Create the HTML table
html_code2 <- stargazer(logit,
                       type = "html",
                       title = "Logit Model")

# Save HTML to a file
html_file2 <- "logit_table.html"
writeLines(html_code2, html_file2)

webshot(html_file2, file = "logit_table.png", zoom = 2)

predictions <- predict(probit, 
                       newdata = data.frame("male" = c(mean(df$male), mean(df$male)),
                                            "children" = c(mean(df$children), mean(df$children)),
                                            "degree" = c(mean(df$degree), mean(df$degree)),
                                            "religiousness" = c(mean(df$religiousness), mean(df$religiousness)),
                                            "yearsmarried" = c(1, 10)),
                       type = "response")

yearsmarried_change <- diff(predictions)        

predictions2 <- predict(probit, 
                       newdata = data.frame("male" = c(mean(df$male), mean(df$male)),
                                            "children" = c(0, 1),
                                            "degree" = c(mean(df$degree), mean(df$degree)),
                                            "religiousness" = c(mean(df$religiousness), mean(df$religiousness)),
                                            "yearsmarried" = c(mean(df$yearsmarried), mean(df$yearsmarried))),
                       type = "response")
children_change <- diff(predictions2)              


predictions3 <- predict(probit, 
                        newdata = data.frame("male" = c(mean(df$male), mean(df$male)),
                                             "children" = c(mean(df$children), mean(df$children)),
                                             "degree" = c(mean(df$degree), mean(df$degree)),
                                             "religiousness" = c(1, 5),
                                             "yearsmarried" = c(mean(df$yearsmarried), mean(df$yearsmarried))),
                        type = "response")
relig_change <- diff(predictions3)              
