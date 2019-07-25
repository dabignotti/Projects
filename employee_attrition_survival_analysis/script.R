## HR ATTRITION DATA ANALYSIS ##


#load libraries
library(ggplot2)
library(ggthemes)
library(dplyr)
library(survival)
library(survminer)


#load and clean data
dat <- read_csv("~/R/data/human-resources-analytics/HR_comma_sep.csv")
dat$salary<-ordered(data$salary,levels=c("low","medium","high"))
dat$Work_accident <- factor(data$Work_accident)
dat$promotion_last_5years <- factor(data$promotion_last_5years)
data <- dat %>% select(-sales)
str(data)


#create survival object
survival.object <- Surv(data$time_spend_company, data$left)
head(survival.object)


#print KM survival curve
fit.all <- survfit(survival.object~1)
summary(fit.all)
print(fit.all)


#primary survival curve
ggsurvplot(fit.all,
           break.time.by = 1,
           palette = c("#E7B800", "#2E9FDF"),
           xlim = c(0,6),
           conf.int = TRUE, #add confidence interval
           pval = TRUE, #add p value
           risk.table = TRUE, #include risk table
           risk.table.height = 0.25,
           ggtheme = theme_light() # Change ggplot2 theme
)


#stratify by salary range
fit.salary <- survfit(survival.object~data$salary)
print(fit.salary)
ggsurvplot(fit.salary,
           break.time.by = 1,
           conf.int = TRUE,
           pval = TRUE,
           risk.table = TRUE,
           risk.table.col = "strata", #color by strata
           risk.table.height = 0.50,
           ggtheme = theme_light()
)


#stratify by number of simultaneous projects
fit.projects <- survfit(survival.object~as.factor(data$number_project))
print(fit.projects)
ggsurvplot(fit.projects,
           break.time.by = 1,
           conf.int = TRUE,
           pval = TRUE,
           risk.table = TRUE,
           risk.table.col = "strata",
           risk.table.height = 0.6,
           surv.plot.height = 0.9,
           ggtheme = theme_light()
)


#promoted in the last five years
fit.promoted <- survfit(survival.object~as.factor(data$promotion_last_5years))
print(fit.promoted)
ggsurvplot(fit.promoted,
           break.time.by = 1,
           conf.int = TRUE,
           pval = TRUE,
           risk.table = TRUE,
           risk.table.col = "strata",
           risk.table.height = 0.3,
           ggtheme = theme_light()
)


#