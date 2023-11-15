#The file OverallCode.R needs to be run first for this file to run


# 1. Libraries #####
library(foreign)#load spss 
library(dplyr)#data wrangling
library(ggplot2)#graphs
library(tidyr)#gather function
library(lme4)#models

options(scipen = 999)#no scientific notation


############################################## SENSITIVITY ANALYSIS #################################################



# 5.1 recoding 'not reached', 'not applicable', 'invalid' and 'no response' as NA. Partial credit=incorrect ####
sensitivity1 <- dfvariables_used2
sapply(sensitivity1, table, useNA = 'ifany')#checking how the levels are coded 
for(i in 4:73)sensitivity1[,i] <- recode(sensitivity1[,i],
                                         'No credit' = 'incorrect',
                                         'Full credit' = 'correct',
                                         'Not Reached' = NA_character_,
                                         'Not Applicable' = NA_character_,
                                         'Invalid' = NA_character_,
                                         'No Response' = NA_character_,
                                         '0 - No credit' = 'incorrect',
                                         '1 - Full credit' = 'correct',
                                         '00 - No credit' = 'incorrect',
                                         '11 - Partial credit' = 'incorrect',
                                         '12 - Partial credit' = 'incorrect',
                                         '13 - Partial credit' = 'incorrect',#
                                         '21 - Full credit' = 'correct',
                                         '22 - Full credit' = 'correct',
                                         '23 - Full credit' = 'correct',
                                         '01 - No credit' = 'incorrect',#
                                         '02 - No credit' = 'incorrect',
                                         '1 - Partial credit' = 'incorrect',#
                                         '2 - Full credit' = 'correct')#ya
sapply(sensitivity1, table, useNA = 'ifany')#checking how the levels are coded 

#
effectivesample_sensitivity1  <- sensitivity1
effectivesample_sensitivity1 <- effectivesample_sensitivity1[rowSums(!is.na(effectivesample_sensitivity1[,4:73])) > 0,]

# numeric data ####
numeric_effectivesample_sensitivity1 <- effectivesample_sensitivity1 
numeric_effectivesample_sensitivity1 [3:73] <- sapply(numeric_effectivesample_sensitivity1 [3:73], as.numeric)# 1 and 2

#recoding to female 0 male 1; incorrect 0 and correct 1
for(i in 3:73)numeric_effectivesample_sensitivity1 [,i] <- recode(numeric_effectivesample_sensitivity1 [,i],
                                                    '1' <- 0,
                                                    '2' <- 1)

#  shaping dataset####

numeric_effectivesample_sensitivity1_long <- gather(numeric_effectivesample_sensitivity1, key = item, value = Y, starts_with(c("M","O")))
dim(numeric_effectivesample_sensitivity1_long)#longdata set

#erasing NA values
numeric_effectivesample_sensitivity1_long = numeric_effectivesample_sensitivity1_long[complete.cases(numeric_effectivesample_sensitivity1_long),]
dim(numeric_effectivesample_sensitivity1_long)

#add a column with the item format information
numeric_effectivesample_sensitivity1_long <- numeric_effectivesample_sensitivity1_long %>%
  mutate(format = case_when(
    startsWith(item, "M") ~ 0,
    startsWith(item, "O") ~ 1
  ))

#variables names
colnames(numeric_effectivesample_sensitivity1_long) <- c('school','student','gender','item','response','format')

#predictors as factor
numeric_effectivesample_sensitivity1_long$format=factor(numeric_effectivesample_sensitivity1_long$format)
numeric_effectivesample_sensitivity1_long$gender=factor(numeric_effectivesample_sensitivity1_long$gender)
#outcome integer
numeric_effectivesample_sensitivity1_long$response= as.integer(numeric_effectivesample_sensitivity1_long$response)

#overview
str(numeric_effectivesample_sensitivity1_long)
dim(numeric_effectivesample_sensitivity1_long)

# models ####

model0_sensitivity1 <- glmer(response ~ 1+(1|student)+(1|item), data = numeric_effectivesample_sensitivity1_long, family = binomial(logit), control=control)
summary(model0_sensitivity1)
var.decomposition1(model0_sensitivity1)
model3_sensitivity1 <- glmer(response ~ 1+(1|student)+(1|item) + format + gender, data = numeric_effectivesample_sensitivity1_long, family = binomial(logit), control=control)
summary(model3_sensitivity1)
# model 1 format
model1_sensitivity1 <- glmer(response ~ 1+(1|student)+(1|item) + format, data = numeric_effectivesample_sensitivity1_long, family = binomial(logit), control=control)
summary(model1_sensitivity1)
# model 2 gender
model2_sensitivity1 <- glmer(response ~ 1+(1|student)+(1|item) + gender, data = numeric_effectivesample_sensitivity1_long, family = binomial(logit), control=control)
summary(model2_sensitivity1)
model4_sensitivity1 <- glmer(response ~ 1+(1|student)+(1|item) + format*gender, data = numeric_effectivesample_sensitivity1_long, family = binomial(logit), control=control)
summary(model4_sensitivity1)

anova(model3_sensitivity1, model4_sensitivity1)

numeric_effectivesample_sensitivity1_long$format0=(numeric_effectivesample_sensitivity1_long$format==0)+0
numeric_effectivesample_sensitivity1_long$format1=(numeric_effectivesample_sensitivity1_long$format==1)+0
numeric_effectivesample_sensitivity1_long$gender0=(numeric_effectivesample_sensitivity1_long$gender==0)+0
numeric_effectivesample_sensitivity1_long$gender1=(numeric_effectivesample_sensitivity1_long$gender==1)+0

model4c.sensitivity1 = glmer(response ~1+(-1+gender0|student)+(-1+gender1|student)+(-1+format0|item)+(-1+format1|item)+format*gender,data=numeric_effectivesample_sensitivity1_long,family=binomial("logit"), control=control)
summary(model4c.sensitivity1)

anova(model4_sensitivity1,model4c.sensitivity1)

# 5.2 recoding 'not reached', 'not applicable' and 'invalid' as NA. No response = incorrect. Partial credit=NA ####
sensitivity2 <- dfvariables_used2
sapply(sensitivity2, table, useNA = 'ifany')
for(i in 4:73)sensitivity2[,i] <- recode(sensitivity2[,i],
                                         'No credit' = 'incorrect',
                                         'Full credit' = 'correct',
                                         'Not Reached' = NA_character_,
                                         'Not Applicable' = NA_character_,
                                         'Invalid' = NA_character_,
                                         'No Response' = 'incorrect',
                                         '0 - No credit' = 'incorrect',
                                         '1 - Full credit' = 'correct',
                                         '00 - No credit' = 'incorrect',
                                         '11 - Partial credit' = NA_character_,
                                         '12 - Partial credit' = NA_character_,
                                         '13 - Partial credit' = NA_character_,#
                                         '21 - Full credit' = 'correct',
                                         '22 - Full credit' = 'correct',
                                         '23 - Full credit' = 'correct',
                                         '01 - No credit' = 'incorrect',#
                                         '02 - No credit' = 'incorrect',
                                         '1 - Partial credit' = NA_character_,#
                                         '2 - Full credit' = 'correct')#ya

sapply(sensitivity2, table, useNA = 'ifany')

effectivesample_sensitivity2  <- sensitivity2
effectivesample_sensitivity2 <- effectivesample_sensitivity2[rowSums(!is.na(effectivesample_sensitivity2[,4:73])) > 0,]

# numeric data ####
numeric_effectivesample_sensitivity2 <- effectivesample_sensitivity2 
numeric_effectivesample_sensitivity2 [3:73] <- sapply(numeric_effectivesample_sensitivity2 [3:73], as.numeric)# 1 and 2

#recoding to female 0 male 1; incorrect 0 and correct 1
for(i in 3:73)numeric_effectivesample_sensitivity2 [,i] <- recode(numeric_effectivesample_sensitivity2 [,i],
                                                                  '1' <- 0,
                                                                  '2' <- 1)

# shaping dataset####

numeric_effectivesample_sensitivity2_long <- gather(numeric_effectivesample_sensitivity2, key = item, value = Y, starts_with(c("M","O")))
dim(numeric_effectivesample_sensitivity2_long)#longdata set

#erasing NA values
numeric_effectivesample_sensitivity2_long = numeric_effectivesample_sensitivity2_long[complete.cases(numeric_effectivesample_sensitivity2_long),]
dim(numeric_effectivesample_sensitivity2_long)

#add a column with the item format information
numeric_effectivesample_sensitivity2_long <- numeric_effectivesample_sensitivity2_long %>%
  mutate(format = case_when(
    startsWith(item, "M") ~ 0,
    startsWith(item, "O") ~ 1
  ))

#variables names
colnames(numeric_effectivesample_sensitivity2_long) <- c('school','student','gender','item','response','format')

#predictors as factor
numeric_effectivesample_sensitivity2_long$format=factor(numeric_effectivesample_sensitivity2_long$format)
numeric_effectivesample_sensitivity2_long$gender=factor(numeric_effectivesample_sensitivity2_long$gender)
#outcome integer
numeric_effectivesample_sensitivity2_long$response= as.integer(numeric_effectivesample_sensitivity2_long$response)

#overview
str(numeric_effectivesample_sensitivity2_long)
dim(numeric_effectivesample_sensitivity2_long)

# models ####

model0_sensitivity2 <- glmer(response ~ 1+(1|student)+(1|item), data = numeric_effectivesample_sensitivity2_long, family = binomial(logit), control=control)
summary(model0_sensitivity2)
var.decomposition1(model0_sensitivity2)
model3_sensitivity2 <- glmer(response ~ 1+(1|student)+(1|item) + format + gender, data = numeric_effectivesample_sensitivity2_long, family = binomial(logit), control=control)
summary(model3_sensitivity2)
# model 1 format
model1_sensitivity2 <- glmer(response ~ 1+(1|student)+(1|item) + format, data = numeric_effectivesample_sensitivity2_long, family = binomial(logit), control=control)
summary(model1_sensitivity2)
# model 2 gender
model2_sensitivity2 <- glmer(response ~ 1+(1|student)+(1|item) + gender, data = numeric_effectivesample_sensitivity2_long, family = binomial(logit), control=control)
summary(model2_sensitivity2)
model4_sensitivity2 <- glmer(response ~ 1+(1|student)+(1|item) + format*gender, data = numeric_effectivesample_sensitivity2_long, family = binomial(logit), control=control)
summary(model4_sensitivity2)

anova(model3_sensitivity2, model4_sensitivity2)

numeric_effectivesample_sensitivity2_long$format0=(numeric_effectivesample_sensitivity2_long$format==0)+0
numeric_effectivesample_sensitivity2_long$format1=(numeric_effectivesample_sensitivity2_long$format==1)+0
numeric_effectivesample_sensitivity2_long$gender0=(numeric_effectivesample_sensitivity2_long$gender==0)+0
numeric_effectivesample_sensitivity2_long$gender1=(numeric_effectivesample_sensitivity2_long$gender==1)+0

model4c.sensitivity2 = glmer(response ~1+(-1+gender0|student)+(-1+gender1|student)+(-1+format0|item)+(-1+format1|item)+format*gender,data=numeric_effectivesample_sensitivity2_long,family=binomial("logit"), control=control)
summary(model4c.sensitivity2)

anova(model4_sensitivity2,model4c.sensitivity2)


# 5.3 recoding 'not reached', 'not applicable' and 'invalid' as NA. No response = incorrect. Partial credit=correct ####
sensitivity3 <- dfvariables_used2
sapply(sensitivity3, table, useNA = 'ifany')#checking how the levels are coded 
for(i in 4:73)sensitivity3[,i] <- recode(sensitivity3[,i],
                                         'No credit' = 'incorrect',
                                         'Full credit' = 'correct',
                                         'Not Reached' = NA_character_,
                                         'Not Applicable' = NA_character_,
                                         'Invalid' = NA_character_,
                                         'No Response' = 'incorrect',
                                         '0 - No credit' = 'incorrect',
                                         '1 - Full credit' = 'correct',
                                         '00 - No credit' = 'incorrect',
                                         '11 - Partial credit' = 'correct',
                                         '12 - Partial credit' = 'correct',
                                         '13 - Partial credit' = 'correct',#
                                         '21 - Full credit' = 'correct',
                                         '22 - Full credit' = 'correct',
                                         '23 - Full credit' = 'correct',
                                         '01 - No credit' = 'incorrect',#
                                         '02 - No credit' = 'incorrect',
                                         '1 - Partial credit' = 'correct',#
                                         '2 - Full credit' = 'correct')#ya

effectivesample_sensitivity3  <- sensitivity3
effectivesample_sensitivity3 <- effectivesample_sensitivity3[rowSums(!is.na(effectivesample_sensitivity3[,4:73])) > 0,]

#numeric data ####
numeric_effectivesample_sensitivity3 <- effectivesample_sensitivity3 
numeric_effectivesample_sensitivity3 [3:73] <- sapply(numeric_effectivesample_sensitivity3 [3:73], as.numeric)# 1 and 2

#recoding to female 0 male 1; incorrect 0 and correct 1
for(i in 3:73)numeric_effectivesample_sensitivity3 [,i] <- recode(numeric_effectivesample_sensitivity3 [,i],
                                                                  '1' <- 0,
                                                                  '2' <- 1)

# shaping dataset####

# shaping data to run the models ####
numeric_effectivesample_sensitivity3_long <- gather(numeric_effectivesample_sensitivity3, key = item, value = Y, starts_with(c("M","O")))
dim(numeric_effectivesample_sensitivity3_long)#longdata set

#erasing NA values
numeric_effectivesample_sensitivity3_long = numeric_effectivesample_sensitivity3_long[complete.cases(numeric_effectivesample_sensitivity3_long),]
dim(numeric_effectivesample_sensitivity3_long)

#add a column with the item format information
numeric_effectivesample_sensitivity3_long <- numeric_effectivesample_sensitivity3_long %>%
  mutate(format = case_when(
    startsWith(item, "M") ~ 0,
    startsWith(item, "O") ~ 1
  ))

#variables names
colnames(numeric_effectivesample_sensitivity3_long) <- c('school','student','gender','item','response','format')

#predictors as factor
numeric_effectivesample_sensitivity3_long$format=factor(numeric_effectivesample_sensitivity3_long$format)
numeric_effectivesample_sensitivity3_long$gender=factor(numeric_effectivesample_sensitivity3_long$gender)
#outcome integer
numeric_effectivesample_sensitivity3_long$response= as.integer(numeric_effectivesample_sensitivity3_long$response)

#overview
str(numeric_effectivesample_sensitivity3_long)
dim(numeric_effectivesample_sensitivity3_long)

# models ####

model0_sensitivity3 <- glmer(response ~ 1+(1|student)+(1|item), data = numeric_effectivesample_sensitivity3_long, family = binomial(logit), control=control)
summary(model0_sensitivity3)
var.decomposition1(model0_sensitivity3)
model3_sensitivity3 <- glmer(response ~ 1+(1|student)+(1|item) + format + gender, data = numeric_effectivesample_sensitivity3_long, family = binomial(logit), control=control)
summary(model3_sensitivity3)
# model 1 format
model1_sensitivity3 <- glmer(response ~ 1+(1|student)+(1|item) + format, data = numeric_effectivesample_sensitivity3_long, family = binomial(logit), control=control)
summary(model1_sensitivity3)
# model 2 gender
model2_sensitivity3 <- glmer(response ~ 1+(1|student)+(1|item) + gender, data = numeric_effectivesample_sensitivity3_long, family = binomial(logit), control=control)
summary(model2_sensitivity3)
model4_sensitivity3 <- glmer(response ~ 1+(1|student)+(1|item) + format*gender, data = numeric_effectivesample_sensitivity3_long, family = binomial(logit), control=control)
summary(model4_sensitivity3)

anova(model3_sensitivity3, model4_sensitivity3)

numeric_effectivesample_sensitivity3_long$format0=(numeric_effectivesample_sensitivity3_long$format==0)+0
numeric_effectivesample_sensitivity3_long$format1=(numeric_effectivesample_sensitivity3_long$format==1)+0
numeric_effectivesample_sensitivity3_long$gender0=(numeric_effectivesample_sensitivity3_long$gender==0)+0
numeric_effectivesample_sensitivity3_long$gender1=(numeric_effectivesample_sensitivity3_long$gender==1)+0

model4c.sensitivity3 = glmer(response ~1+(-1+gender0|student)+(-1+gender1|student)+(-1+format0|item)+(-1+format1|item)+format*gender,data=numeric_effectivesample_sensitivity3_long,family=binomial("logit"), control=control)
summary(model4c.sensitivity3)

anova(model4_sensitivity3,model4c.sensitivity3)

# 5.5 considering the variation between schools: very few variance by schools
model0_used_school <- glmer(response ~ 1+(1|student)+(1|item) + (1|school), data = DATAused, family = binomial(logit), control=control)
summary(model0_used_school)
var.decomposition1(model0_used_school) #super small school  0.01246838 
round(0.01246838 , digits = 2)

model3_used_school <- glmer(response ~ 1+(1|student)+(1|item) + (1|school) + format + gender, data = DATAused, family = binomial(logit), control=control)
summary(model3_used_school)#0.09165
model4_used_school <- glmer(response ~ 1+(1|student)+(1|item) + (1|school)+ format*gender, data = DATAused, family = binomial(logit), control=control)
summary(model4_used_school)#0.09189 
model4c.used.school = glmer(response ~1+(-1+gender0|student)+(-1+gender1|student)+(-1+format0|item)+(-1+format1|item)+(1|school)+format*gender,data=DATAused,family=binomial("logit"), control=control)
summary(model4c.used.school)#0.08828 

