# 1. Libraries #####
library(foreign)#load spss 
library(dplyr)#data wrangling
library(ggplot2)#graphs
library(tidyr)#gather function
library(lme4)#models

options(scipen = 999)#no scientific notation

#################################################### DATA WRANGLING ##################################################################
# 2. Load cognitive datafile and questionnaire ####
#fulldata <- read.spss("/home/ejcd/Documents/1. Master thesis UiO/1. Master Thesis UIO (old blue laptop)/DOCUMENTATION PISA/data/cognitive item data file SPSS_STU_COG/SPSS_STU_COG.sav", to.data.frame = TRUE, use.missings = FALSE)
#fullquestionnaire <- read.spss("/home/ejcd/Documents/1. Master thesis UiO/1. Master Thesis UIO (old blue laptop)/DOCUMENTATION PISA/data/student questionnaire data file STU/student questionnaire data file.sav", to.data.frame = TRUE, use.missings = FALSE)

#path <- "/home/ejcd/Documents/1. Master thesis UiO/datafiles/"
#fulldata <- read.spss(paste0(path,"CY07_MSU_STU_COG.sav",collapse=""), to.data.frame = TRUE, use.missings = FALSE)

#str(fulldata)

#path2 <- "/home/ejcd/Documents/1. Master thesis UiO/datafiles/"
#questionnaire <- read.spss(paste0(path,"CY07_MSU_STU_QQQ.sav",collapse=""), to.data.frame = TRUE, use.missings = FALSE)

# 2.1 subset Norway

#norwaydata <- subset(fulldata, CNT == "Norway")
#norwayquestionnaire <- subset(questionnaire, CNT == "Norway")

#saveRDS(norwaydata, "/home/ejcd/Documents/1. Master thesis UiO/datafiles/norwaydata.RDS")
#saveRDS(norwayquestionnaire, "/home/ejcd/Documents/1. Master thesis UiO/datafiles/norwayquestionnaire.RDS")
norwaydata <- readRDS(file = "/home/ejcd/Documents/1. Master thesis UiO/datafiles/norwaydata.RDS")
norwayquestionnaire <- readRDS(file = "/home/ejcd/Documents/1. Master thesis UiO/datafiles/norwayquestionnaire.RDS")

vectorschools <- table(norwaydata$CNTSCHID)
dfschools <-as.data.frame(vectorschools)
nrow(dfschools)#251 schools

nrow(norwayquestionnaire)#5813 students
prop.table(table(norwayquestionnaire$ST004D01T))#gender proportions

# 2.2. subset mathematical literacy domain

#subset of clusters M01 to M06A (only the items Norway took). 
mathnorwaydata <- norwaydata[,c('CNT','CNTSCHID','CNTSTUID', 'CM033Q01S',
                                'CM474Q01S',
                                'DM155Q02C',
                                'CM155Q01S',
                                'DM155Q03C',
                                'CM155Q04S',
                                'CM411Q01S',
                                'CM411Q02S',
                                'CM803Q01S',
                                'CM442Q02S',
                                'DM462Q01C',
                                'CM034Q01S',
                                'CM305Q01S',
                                'CM496Q01S',
                                'CM496Q02S',
                                'CM423Q01S',
                                'CM192Q01S',
                                'DM406Q01C',
                                'DM406Q02C',
                                'CM603Q01S',
                                'CM571Q01S',
                                'CM564Q01S',
                                'CM564Q02S',
                                'CM447Q01S',
                                'CM273Q01S',
                                'CM408Q01S',
                                'CM420Q01S',
                                'CM446Q01S',
                                'DM446Q02C',
                                'CM559Q01S',
                                'DM828Q02C',
                                'CM828Q03S',
                                'CM464Q01S',
                                'CM800Q01S',
                                'CM982Q01S',
                                'CM982Q02S',
                                'CM982Q03S',
                                'CM982Q04S',
                                'CM992Q01S',
                                'CM992Q02S',
                                'DM992Q03C',
                                'CM915Q01S',
                                'CM915Q02S',
                                'CM906Q01S',
                                'DM906Q02C',
                                'DM00KQ02C',
                                'CM909Q01S',
                                'CM909Q02S',
                                'CM909Q03S',
                                'CM949Q01S',
                                'CM949Q02S',
                                'DM949Q03C',
                                'CM00GQ01S',
                                'DM955Q01C',
                                'DM955Q02C',
                                'CM955Q03S',
                                'DM998Q02C',
                                'CM998Q04S',
                                'CM905Q01S',
                                'DM905Q02C',
                                'CM919Q01S',
                                'CM919Q02S',
                                'CM954Q01S',
                                'DM954Q02C',
                                'CM954Q04S',
                                'CM943Q01S',
                                'CM943Q02S',
                                'DM953Q02C',
                                'CM953Q03S',
                                'DM953Q04C')]

#take out all NA (people who didn't take math per design)
mathnorwaydata_nona <- mathnorwaydata[!apply(is.na(mathnorwaydata[4:73]), 1, all),]
nrow(mathnorwaydata_nona)

#adding gender to the items dataset
norwayquestionnaire_gender <- norwayquestionnaire[,c('CNTSTUID','ST004D01T')]
dfvariables_used <- left_join(mathnorwaydata_nona, norwayquestionnaire_gender, by = 'CNTSTUID')

# 3. missing values ####

# 3.1. missing values types in the whole dataset

cells <- 70*3141#219870 possible responses of a person on an item
sapply(dfvariables_used [4:73], table, useNA = 'ifany')
total_NA <- sum(is.na(dfvariables_used [,c(4:73)]))#151978

validresponses <- cells-total_NA #67892 (taking out the missing by design)
(total_noresponse <- sum(dfvariables_used[,c(4:73)] == "No Response", na.rm = TRUE))#4853
(total_noresponse/validresponses)*100 #7.148118% no response of the valid responses
(total_notreached <- sum(dfvariables_used[,c(4:73)] == "Not Reached", na.rm = TRUE))#2723
(total_notreached/validresponses)*100 #4.010782 % not reached of the valid responses
(total_invalid <- sum(dfvariables_used[,c(4:73)] == "Invalid", na.rm = TRUE))#0 
(total_notapplicable <- sum(dfvariables_used[,c(4:73)] == "Not Applicable", na.rm = TRUE))#29

total_validresponses <- validresponses - total_notapplicable#67863 (taking out the not applicable, we get real valid responses as a result)
(total_noresponse/total_validresponses )*100 #7.151172 percentage 'no response' from total_validresponses 
(total_notreached/total_validresponses )*100#4.012496 percentage 'not reached' from total_validresponses 

#organising dataset by format
dfvariables_used2 <- dfvariables_used[,c('CNTSCHID','CNTSTUID','ST004D01T',
                                        'CM033Q01S',	'CM474Q01S',	'CM155Q01S',	'CM155Q04S',
                                        'CM411Q02S',	'CM442Q02S',	'CM305Q01S',	'CM496Q01S',
                                        'CM423Q01S',	'CM192Q01S',	'CM603Q01S',	'CM571Q01S',
                                        'CM564Q01S',	'CM564Q02S',	'CM447Q01S',	'CM273Q01S',
                                        'CM408Q01S',	'CM420Q01S',	'CM559Q01S',	'CM800Q01S',
                                        'CM982Q03S',	'CM982Q04S',	'CM915Q01S',	'CM906Q01S',
                                        'CM909Q02S',	'CM949Q01S',	'CM949Q02S',	'CM998Q04S',
                                        'CM905Q01S',	'CM943Q01S',#30 mc until here
                                        'DM155Q02C',	'DM155Q03C',	'CM411Q01S',	'CM803Q01S',
                                        'DM462Q01C',	'CM034Q01S',	'CM496Q02S',	'DM406Q01C',
                                        'DM406Q02C',	'CM446Q01S',	'DM446Q02C',	'DM828Q02C',
                                        'CM828Q03S',	'CM464Q01S',	'CM982Q01S',	'CM982Q02S',
                                        'CM992Q01S',	'CM992Q02S',	'DM992Q03C',	'CM915Q02S',
                                        'DM906Q02C',	'DM00KQ02C',	'CM909Q01S',	'CM909Q03S',
                                        'DM949Q03C',	'CM00GQ01S',	'DM955Q01C',	'DM955Q02C',
                                        'CM955Q03S',	'DM998Q02C',	'DM905Q02C',	'CM919Q01S',
                                        'CM919Q02S',	'CM954Q01S',	'DM954Q02C',	'CM954Q04S',
                                        'CM943Q02S',	'DM953Q02C',	'CM953Q03S',	'DM953Q04C')]#40 cr until here (all are ocr)

#adding the prefix MC_ or CR_ to categorize the items
colnames(dfvariables_used2)
colnames(dfvariables_used2)[4:33] <- paste('MC', colnames(dfvariables_used2)[4:33], sep = "_")
colnames(dfvariables_used2)[34:73] <- paste('OCR', colnames(dfvariables_used2)[34:73], sep = "_")
colnames(dfvariables_used2)
                                        
# 4. recoding ####
#'not reached', 'not applicable' and 'invalid' as NA. No response = incorrect. Partial credit=incorrect

data_used <- dfvariables_used2
sapply(data_used, table, useNA = 'ifany')#checking how the levels are coded 
for(i in 4:73)data_used[,i] <- recode(data_used[,i],
                                             'No credit' = 'incorrect',
                                             'Full credit' = 'correct',
                                             'Not Reached' = NA_character_,
                                             'Not Applicable' = NA_character_,
                                             'Invalid' = NA_character_,
                                             'No Response' = 'incorrect',
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
                                             '2 - Full credit' = 'correct')#

sapply(data_used, table, useNA = 'ifany')

# 5. drop unnecesary gender levels ####
str(data_used)
data_used$ST004D01T <- droplevels(data_used$ST004D01T)
str(data_used)

# 6. effective sample size ####

# 6.1 drop columns with all NA 
effectivesample <- data_used
effectivesample<- effectivesample[rowSums(!is.na(effectivesample[,4:73])) > 0,]
str(effectivesample)#3122 obs

# 6.2 gender proportion of effective sample size
prop.table(table(effectivesample$ST004D01T))

# 6.3 schools effective sample size
vectorschoolseff <- table(effectivesample$CNTSCHID)
dfschoolseff <-as.data.frame(vectorschoolseff)
nrow(dfschoolseff)#250 schools

# 7. averages ####

numeric_effectivesample <- effectivesample
numeric_effectivesample[3:73] <- sapply(numeric_effectivesample[3:73], as.numeric)# 1 and 2

#recoding to female 0 male 1; incorrect 0 and correct 1
for(i in 3:73)numeric_effectivesample[,i] <- recode(numeric_effectivesample[,i],
                                                    '1' <- 0,
                                                    '2' <- 1)

# 7.1 on average a person answered to x items (4. Chart.R)
personsanswered <- numeric_effectivesample
personsanswered$n_ianswered_personp <- rowSums(!is.na(personsanswered[,4:73]))#adding column with number of items responded by each person (both 0 and 1)
min(personsanswered$n_ianswered_personp)#1 #minimum # of items responded by a person
max(personsanswered$n_ianswered_personp)#24 #maximum # of items responded by a person
mean(personsanswered$n_ianswered_personp)#20.86483 <- on average a person answered to 21 items

#graph
ggplot(personsanswered , aes(x = n_ianswered_personp)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust = 0)

table(personsanswered$n_ianswered_personp)

summary(personsanswered$n_ianswered_personp)

# 7.2 on average an item was answered by x persons: 930.5714 
itemswereanswered <- numeric_effectivesample
itemswereanswered[nrow(itemswereanswered) + 1, ] <- colSums(!is.na(itemswereanswered))#create row with the sum of columns (each column is one item)
itemswereanswered[3123,c(1:3)] <- NA##assigning NA to CNTSCHID CNTSTUID ST004D01T to that last column created

rowMeans(itemswereanswered[3123,], na.rm = TRUE)#930.5714 
itemswereanswered[3123,]#
(min(itemswereanswered[3123,], na.rm = TRUE))#min 599
(max(itemswereanswered[3123,], na.rm = TRUE))#971


# 8. shaping long data to run the models ####
DATAused <- gather(numeric_effectivesample, key = item, value = Y, starts_with(c("M","O")))
dim(DATAused)

#using information available
DATAused = DATAused[complete.cases(DATAused),]
dim(DATAused)

#add a column with the item format information
DATAused <- DATAused %>%
  mutate(format = case_when(
    startsWith(item, "M") ~ 0,
    startsWith(item, "O") ~ 1
  ))

#variables names
colnames(DATAused) <- c('school','student','gender','item','response','format')

#predictors as factor
DATAused$format=factor(DATAused$format)
DATAused$gender=factor(DATAused$gender)
#outcome integer
DATAused$response= as.integer(DATAused$response)

#overview
str(DATAused)

#################################################### CUSTOM FUNCTIONS ##################################################################

# 9. custom functions ####

control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5))

logit<-function(x){return(1/(1+exp(-x)))}

### VARIANCE DECOMPOSITION: estimate based on the population sample, model based ###

var.decomposition1 <-function(m){
  VC0 = c(unlist(lapply(VarCorr(m), diag)),(pi^2)/3)
  ICC = VC0/sum(VC0)
  decomposition = c(variance = VC0, icc = ICC)
  return(decomposition)
}

################################################### MODELS ##################################################################

# 10. MODELS ####
#format: 0=mc, 1=cr
#gender: 0=female, 1=male
#response: 0=incorrect, 1=correct

# 10.1 descriptive null model0 ####
model0_used <- glmer(response ~ 1+(1|student)+(1|item), data = DATAused, family = binomial(logit), control=control)
summary(model0_used)

#probability average person on average item
b0 = -0.2846
(1/(1+exp(-(b0))))#0.4293264

#variance decomposition null model
VC0used <- c(unlist(lapply(VarCorr(model0_used), diag)),(pi^2)/3)
(VC0used) # 1.514026            2.546078            3.289868 
#in %
ICC1used <- VC0used/sum(VC0used)
(ICC1used) #in % 0.2059907  0.3464065    0.4476028 

# 10.2 main effects models ####

# main effects model: format and gender
model3_used <- glmer(response ~ 1+(1|student)+(1|item) + format + gender, data = DATAused, family = binomial(logit), control=control)
summary(model3_used)
anova(model0_used, model3_used)#delta df=2, chisq=10.707, p= 0.004733

#odds format
eta.m3.used.format = -1.09409
exp(-(eta.m3.used.format))# 2.986464
(ORformat = exp(eta.m3.used.format))# 0.3348442 odds ratio

# model 1 format
model1_used <- glmer(response ~ 1+(1|student)+(1|item) + format, data = DATAused, family = binomial(logit), control=control)
summary(model1_used)

# model 2 gender
model2_used <- glmer(response ~ 1+(1|student)+(1|item) + gender, data = DATAused, family = binomial(logit), control=control)
summary(model2_used)

#probabilities 
#pr correct mc
b0_m1_used = 0.3404
b1_m1_used = -1.0941
eta.mc.m1.used <- b0_m1_used + (0*(b1_m1_used))
1/(1+exp(-(eta.mc.m1.used)))# 0.5842877

#pr correct cr
eta.cr.m1.used <-b0_m1_used + (1*(b1_m1_used))
1/(1+exp(-(eta.cr.m1.used)))#0.3200156

#format accounts for % of item difficulty
item_variance <- VC0used[2]
(item_variance)
item_residual_variance_m3_used <- 2.253
(1-item_residual_variance_m3_used/item_variance)#format accounts for 0.1151097 

summary(model3_used)
anova(model2_used, model3_used)#chisq = 8.5361 p = 0.003482

# odds gender
eta.m3.used.gender = -0.07188
exp(-(eta.m3.used.gender))# 1.074526
(ORgender = exp(eta.m3.used.gender))# 0.9306426 odds ratio gender

#pr correct female
b0_m2_used = -0.24836
b1_m2_used = -0.07187 
eta.female.m2.used <- b0_m2_used + (0*(b1_m2_used))
(1/(1+exp(-(eta.female.m2.used))))# 0.4382272

#pr correct male
eta.male.m2.used <-  b0_m2_used + (1*(b1_m2_used))
1/(1+exp(-(eta.male.m2.used)))#0.4206197

#gender accounts for % of person ability
person_variance <- VC0used[1]
(person_variance)
person_residual_variance_m3_used <- 1.513
(1-person_residual_variance_m3_used/person_variance)#person accounts for 0.0006782673

summary(model3_used)#person residual variance 1.513 
anova(model1_used, model3_used)#chisq =  2.171  p =  0.1406

# 10.3 interaction model ####

model4_used <- glmer(response ~ 1+(1|student)+(1|item) + format*gender, data = DATAused, family = binomial(logit), control=control)
summary(model4_used)#b12=-0.16140 
anova(model3_used,model4_used)#chisq 15.195, p=  0.00009695

#plot interaction model 4 ####
fixed4_used <- fixef(model4_used)
fixed4_used
pseudos2.4.used = unique(DATAused[,c("format","gender")])#four possible combinations of covariates
pseudos2.4.used
pseudos3.4.used <- cbind(1,pseudos2.4.used,c(0,0,1,0))#adding the intercept and the interaction multiplication for pseudos2.4.used
colnames(pseudos3.4.used) <- c('intercept','format','gender', 'interaction')
pseudos3.4.used#note: cases are 2,5 ,93662 and 93665

#predicted values for whole DATAused
FE.model4.used <- predict(model4_used,type="link", re.form=NA)#re.form=NA -> Random effects at 0 
FE.model4.used <- as.data.frame(FE.model4.used)
data_keep_rows <- c("2", "5", "93662", '93665')
data_subset4_used <- FE.model4.used[rownames(FE.model4.used) %in% data_keep_rows, ]
data_subset4_used
pseudos3.4.used$logit <- data_subset4_used
pseudos3.4.used

intplot <- ggplot(data=pseudos3.4.used, aes(x=factor(format), y=logit, col=factor(gender))) +
  geom_line(data=pseudos3.4.used, aes(group=gender)) +
  geom_point() +
  theme_bw() +
  scale_x_discrete(labels = c('Multiple Choice','Constructed Response')) +
  scale_color_manual(values=c("gray50","black"), labels=c("Female","Male")) +
  scale_y_continuous(name="Logit", limits=c(-0.9,0.4), breaks = seq(-0.9,0.4, by = 0.2)) +
  labs(x = "Format", color = "Gender")+
  theme(panel.border = element_blank(), axis.line = element_line())+
  theme(text = element_text(family = "Times New Roman"))

#odds format: females reference group
eta.m4.used.format.femref = -1.01301 
exp(-(eta.m4.used.format.femref))# 2.753878
(OR = exp(eta.m4.used.format.femref))#  0.3631243

#format: male reference group: groups to be compared are CR male vs MC male
DATAused_recoded <- DATAused
DATAused_recoded$gender <- ifelse(test = DATAused_recoded$gender %in% c(1), 0, 
                              no = 1)
model4used.recodedmales = glmer(response ~ 1+(1|student)+(1|item) + format*gender, data = DATAused_recoded, family = binomial(logit), control=control)
summary(model4used.recodedmales)

#odds format: males reference group
eta.m4.used.format.recod.maleref = -1.17438
(exp(-eta.m4.used.format.recod.maleref))# 3.236136
exp(eta.m4.used.format.recod.maleref)#0.3090105

#odds gender: mc reference group.  groups to be compared are mc boys vs mc girls
eta.m4.used.gender.mcref = 0.01178
exp(-(eta.m4.used.gender.mcref))# 0.9882891
(OR = exp(eta.m4.used.gender.mcref))#1.01185

#gender: cr reference group: groups to be compared are CR boys vs CR girls
DATAused_recoded2 <- DATAused
DATAused_recoded2$format <- ifelse(test = DATAused_recoded2$format %in% c(1), 0, 
                               no = 1)
model4used_recodedformat <- glmer(response ~ 1+(1|student)+(1|item) + format*gender, data = DATAused_recoded2, family = binomial(logit), control=control)
summary(model4used_recodedformat)

#odds gender: cr reference group
eta.m4.used.gender.crref = -0.14962
(exp(-eta.m4.used.gender.crref))#1.161393
exp(eta.m4.used.gender.crref)# 0.8610351

# 10.4 Heterocedasticity model: format and gender ####

#dummy creation
DATAused$format0=(DATAused$format==0)+0
DATAused$format1=(DATAused$format==1)+0
DATAused$gender0=(DATAused$gender==0)+0
DATAused$gender1=(DATAused$gender==1)+0

model4c.used = glmer(response ~1+(-1+gender0|student)+(-1+gender1|student)+(-1+format0|item)+(-1+format1|item)+format*gender,data=DATAused,family=binomial("logit"), control=control)
summary(model4c.used)
anova(model4_used,model4c.used)

#p value calculation for anova(model4_used,model4c.used)
deviance_m4used <- 66463.2 
deviance_m4cused <- 66442.3 
1-pchisq((deviance_m4used-deviance_m4cused), df = 1)#0.000004838946

# 10.5 Heteroscedastic plot ####

#preparing data
re4c.used <- ranef(model4c.used)
re4c.2.used <- rowSums(re4c.used$item)
re4c.2.used <-as.data.frame(re4c.2.used)
MC_mean_used <- rep(0, 30)
CR_mean_used <- rep(-1.00809, 40)
fe.used <- c(MC_mean_used, CR_mean_used)
re4c.2.used$slope <- fe.used
colnames(re4c.2.used) <- c('re','slope')
re4c.2.used$total <- rowSums(re4c.2.used)

#Sample data
MC.used <- rep('MC',30) 
CR.used <- rep('CR',40)
Format <- c(MC.used,CR.used)
re4c.2.used$Format <- Format
re4c.3.used <- re4c.2.used[,c('total','Format')] 

#plot
varplot4 <-ggplot(re4c.3.used, aes(x = -total, fill = Format)) + 
  geom_histogram(alpha = 0.3, position = "identity")+
  scale_x_continuous(breaks = c(-3, -2, -1, 0, 1, 2, 3, 4, 5))+
  theme_bw()+
  theme(panel.border = element_blank(), axis.line = element_line())+
  labs(x = "Item Difficulty", y = "Item Count")+
  scale_fill_manual(values = c("MC" = "gray55", "CR" = "gray25"))+
  theme(text = element_text(family = "Times New Roman"))





