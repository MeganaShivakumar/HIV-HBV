# HIV-HBV
---
title: "Logistic Regressions"
output:
  word_document: default
  pdf_document: default
  html_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(knitr)
library(aod)
knitr::opts_chunk$set(echo = FALSE, fig.pos='H')
```

```{r}
library(readxl)
hivhbv <- read_excel("~/Desktop/Drain Lab/Crypto_MS_Dataset_20210329.xlsx")
  guess_max = min(3100, n_max = NULL)

#model <- glm(outcome ~ exposure, family = “binomial”, data=data)

hivhbv$sexcat <- as.factor(hivhbv$sex_0m)
levels(hivhbv$sexcat) <- c('male', 'female')
levels(hivhbv$sexcat)

hivhbv$alcohol_0mcat <- as.factor(hivhbv$alcohol_0m)
levels(hivhbv$alcohol_0mcat) <-c('Never', 'Not in last year', 'within last year', 'within last month')
levels(hivhbv$alcohol_0mcat)

hivhbv$ivducat <- as.factor(hivhbv$ivdu_0m)
levels(hivhbv$ivducat) <- c('Never', 'Not in last year', 'within last year', 'within last month')
levels(hivhbv$ivducat)

hivhbv$condomcat <- as.factor(hivhbv$condom1_end_12m)
levels(hivhbv$condomcat) <- c('yes', 'no')
levels(hivhbv$condomcat)

hivhbv$stigmacat <- as.factor(hivhbv$stigma2_end_12m)
levels(hivhbv$stigmacat) <- c('yes', 'no')
levels(hivhbv$stigmacat)

hivhbv$incomecat <- as.factor(hivhbv$income_0m)
levels(hivhbv$incomecat) <- c('<2,000 ZAR/month', '2,000 - 10,000 ZAR/month', '>10,000 ZAR/month')
levels(hivhbv$incomecat)

hivhbv$employmentcat <- as.factor(hivhbv$currently_employed_0m)
levels(hivhbv$employmentcat) <- c('No', 'Yes, <20 hrs/wk', 'Yes, >20 hrs/wk')
levels(hivhbv$employmentcat)

hivhbv$educat <- as.factor(hivhbv$education_0m)
levels(hivhbv$educat) <- c('None', 'Primary School', 'Some High School', 'Completed High School', 'Higher Degree')
levels(hivhbv$educat)

table(hivhbv$alcohol_0mcat)

mylogit <- glm(hbv_end_12m ~ sexcat + alcohol_0mcat + age_0m, family = "binomial" (link="logit"), data = hivhbv)
summary(mylogit)
exp(coef(mylogit))

multivar2 <- glm(hbv_end_12m ~ sexcat + condomcat + age_0m, family = "binomial" (link="logit"), data = hivhbv)
summary(multivar2)
exp(coef(multivar2))

multivar3 <- glm(hbv_end_12m ~ sexcat + ivducat + age_0m, family = "binomial" (link="logit"), data = hivhbv)
summary(multivar3)
exp(coef(multivar3))

multivar4 <- glm(hbv_end_12m ~ sexcat + incomecat + age_0m, family = "binomial" (link="logit"), data = hivhbv)
summary(multivar4)
exp(coef(multivar4))

multivar5 <- glm(hbv_end_12m ~ sexcat + employmentcat + age_0m, family = "binomial" (link="logit"), data = hivhbv)
summary(multivar5)
exp(coef(multivar5))

multivar6 <- glm(hbv_end_12m ~ sexcat + educat + age_0m, family = "binomial" (link="logit"), data = hivhbv)
summary(multivar6)
exp(coef(multivar6))

multivar7 <- glm(hbv_end_12m ~ sexcat + educat + employmentcat + incomecat + age_0m, family = "binomial" (link="logit"), data = hivhbv)
summary(multivar7)
exp(coef(multivar7))

multivar8 <- glm(hbv_end_12m ~ sexcat + educat + incomecat + alcohol_0mcat + age_0m + ivducat + condomcat + stigmacat, family = "binomial" (link="logit"), data = hivhbv)
summary(multivar8)
exp(coef(multivar8))

datax <- hivhbv[which(hivhbv$age_0m >= 25),]

datay <- hivhbv[which(hivhbv$age_0m < 25),]

datam <- hivhbv[which(hivhbv$sexcat == 'male'),]

dataf <-hivhbv[which(hivhbv$sexcat == 'female'),]

multivarx <- glm(hbv_end_12m ~ sexcat + educat + incomecat + alcohol_0mcat + ivducat, family = binomial(link = "logit"),  data = datax)
summary(multivarx)
exp(coef(multivarx))

multivary <- glm(hbv_end_12m ~ sexcat + educat + incomecat + alcohol_0mcat + ivducat, family = binomial(link = "logit"),  data = datay)
summary(multivary)
exp(coef(multivary))
 
multivarm <- glm(hbv_end_12m ~ age_0m + educat + incomecat + alcohol_0mcat + ivducat, family = binomial(link = "logit"),  data = datam)
summary(multivarm)
exp(coef(multivarm))

multivarf <- glm(hbv_end_12m ~ age_0m + educat + incomecat + alcohol_0mcat + ivducat, family = binomial(link = "logit"),  data = dataf)
summary(multivarf)
exp(coef(multivarf))

table(hivhbv$hbv_end_12m)

summarymylogit <- summary(mylogit)
expmodel <- exp(summarymylogit$coef[1,1])
summary(expmodel)
summarymylogit$coef

summarymultivar2 <- summary(multivar2)
expmodel2 <- exp(summarymultivar2$coef[1,1])
summary(expmodel2)
summarymultivar2$coef

summarymultivar3 <- summary(multivar3)
expmodel3 <- exp(summarymultivar3$coef[1,1])
summary(expmodel3)
summarymultivar3$coef

hivhbv$Agecat<-cut(hivhbv$age_0m, c(0,25,100))
table(hivhbv$hbv_end_12m, hivhbv$Agecat)

mylogit <- glm(hbv_end_12m ~ ivducat, family = "binomial" (link="logit"), data = hivhbv)
summary(mylogit)
exp(coef(mylogit))

model5 <- glm(hbv_end_12m ~ condomcat, family = "binomial" (link="logit"), data = hivhbv)
summary(model5)
exp(coef(model5))

model6 <- glm(hbv_end_12m ~ stigmacat, family = "binomial" (link="logit"), data = hivhbv)
summary(model6)
exp(coef(model6))

model7 <- glm(hbv_end_12m ~ sexcat, family = "binomial" (link="logit"), data = hivhbv)
summary(model7)
exp(coef(model7))

model8 <- glm(hbv_end_12m ~ alcohol_0mcat, family = "binomial" (link="logit"), data = hivhbv)
summary(model8)
exp(coef(model8))