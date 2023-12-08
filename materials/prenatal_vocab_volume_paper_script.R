#######CHILD: script for paper titled "Prenatal volume in the bilateral superior temporal gyrus predicts 
#######children’s expressive vocabulary at 24-36 months "
#######Annika Werwach

#set up
rm(list=ls())
setwd("/Users/werwach/Documents/CHILD/tables")
options(scipen = 999)

#load packages
library(readxl)
library(dplyr)
library(tidyr)
library(psych)
library(lm.beta)

#load tables
data_volume_fet = read_excel("Fetal_Volumes(1).xlsx") #fetal brain volume
names(data_volume_fet)[1] <- 'ID'

data_general_and_cdi = read.csv("CHILD_main_VA_final.csv", header = TRUE, sep = ",")
names(data_general_and_cdi)[2] <- 'ID'

data_maternalage = read.csv("CHILD_VA_maternal_Age.csv", header = TRUE, sep = ",")
names(data_maternalage)[1] <- 'ID'

#calculate IFG left & right and bilateral volume
data_volume_fet$IFG_left_fet = data_volume_fet$Frontal_Inf_Tri_L + data_volume_fet$Frontal_Inf_Oper_L + data_volume_fet$Frontal_Inf_Orb_L
data_volume_fet$IFG_right_fet = data_volume_fet$Frontal_Inf_Tri_R + data_volume_fet$Frontal_Inf_Oper_R + data_volume_fet$Frontal_Inf_Orb_R
data_volume_fet$IFG_fet = data_volume_fet$IFG_left_fet + data_volume_fet$IFG_right_fet

#create STG columns & calculate bilateral volume
data_volume_fet$STG_left_fet = data_volume_fet$Temporal_Sup_L
data_volume_fet$STG_right_fet = data_volume_fet$Temporal_Sup_R
data_volume_fet$STG_fet = data_volume_fet$Temporal_Sup_L + data_volume_fet$Temporal_Sup_R

#rename total volume column
names(data_volume_fet)[names(data_volume_fet) == "Total"] <- "total_volume_fet"

#filter only relevant columns from volume data
data_volume_fet = data_volume_fet |> 
  select(ID, 
         IFG_left_fet, IFG_right_fet, IFG_fet,
         STG_left_fet, STG_right_fet, STG_fet,
         total_volume_fet)

#filter only relevant columns from general & language data
data_general_and_cdi = data_general_and_cdi |> 
  select(ID,sex,age_child, age_18, age_24,cdi1,cdi2)

#filter only relevant columns from maternal age data
data_maternalage = data_maternalage |> 
  filter(redcap_event_name == "prenatal_arm_1") |> 
  select(ID, age_mother)

#combine datasets
data = data_volume_fet |> 
  inner_join(data_general_and_cdi, by = "ID") |> 
  inner_join(data_maternalage, by = "ID")

#filter for only the kids who have cdi1 or cdi2 or both (n = 30) -> for descriptive statistics
data <- data |> 
  filter(!is.na(cdi1) | !is.na(cdi2))

#filter data for all kids who have the cdi1 (n = 25)
data_cdi1 <- data |> 
  filter(!is.na(cdi1))

#filter data for all kids who have the cdi2 (n = 24)
data_cdi2 <- data |> 
  filter(!is.na(cdi2))

#outlier exclusion
describe(data_cdi1$cdi1)
#M = 21.15, SD = 14.48 -> exclusion of all kids with a score > 50.11 -> ID 104 (score 51), ID 106 (score 52)
data_cdi1 = data_cdi1 |> 
  filter(cdi1 <= 50.11)

describe(data_cdi2$cdi2)
#M = 82.04, SD = 20.79 -> exclusion of all kids with a score < 40.46 (as 100 is the maximum anyway) -> ID 123 (score 19)
data_cdi2 = data_cdi2 |>
  filter(cdi2 >= 40.46)

#add percentile scores manually
data_cdi1$cdi1_per = c(25,55,50,65,60,25,50,10,5,5,50,65,20,35,5,15,10,60,40,5,45,40,5,5,5)
data_cdi2$cdi2_per = c(99,65,60,60,85,99,30,75,10,25,70,25,58,15,70,90,40,99,10,80,85,20,5,10)

#######################################Descriptive statistics######################################
#gestation week at MRI scan
describe(data$age_child)

#age at cdi1 assessment
describe(data_cdi1$age_18)

#age at cdi2 assessment
describe(data_cdi2$age_24)

#sex
table(data$sex)
#female(1): 14
#male(0): 16

table(data_cdi1$sex)
#female: 11
#male: 14

table(data_cdi2$sex)
#female: 13
#male: 11

##foetal brain volume
##STG - left
describe(data$STG_left_fet)

##STG - right
describe(data$STG_right_fet)

##IFG - left
describe(data$IFG_left_fet)

##IFG - right
describe(data$IFG_right_fet)


#STG - by sex
describeBy(data$STG_fet,data$sex)
#left
describeBy(data$STG_left_fet,data$sex)
#right
describeBy(data$STG_right_fet,data$sex)
#IFG
describeBy(data$IFG_fet,data$sex)
#left
describeBy(data$IFG_left_fet,data$sex)
#right
describeBy(data$IFG_right_fet,data$sex)


#vocabulary scores
#cdi1
describe(data_cdi1$cdi1)
describe(data_cdi1$cdi1_per)

#cdi2
describe(data_cdi2$cdi2)
describe(data_cdi2$cdi2_per)

#break-up descriptive statistics by sex
t.test(data$STG_left_fet~data$sex)
t.test(data$STG_right_fet~data$sex)

t.test(data$IFG_left_fet~data$sex)
t.test(data$IFG_right_fet~data$sex)

##correleation of IFG and STG volume with potential covariates
cor.test(data$IFG_right_fet, data$age_child)
cor.test(data$IFG_right_fet, data$total_volume_fet)
cor.test(data$IFG_right_fet, data$age_mother)

cor.test(data$IFG_left_fet, data$age_child)
cor.test(data$IFG_left_fet, data$total_volume_fet)
cor.test(data$IFG_left_fet, data$age_mother)


cor.test(data$STG_right_fet, data$age_child)
cor.test(data$STG_right_fet, data$total_volume_fet)
cor.test(data$STG_right_fet, data$age_mother)

cor.test(data$STG_left_fet, data$age_child)
cor.test(data$STG_left_fet, data$total_volume_fet)
cor.test(data$STG_left_fet, data$age_mother)


##first-order correlations
#split by hemisphere
cor.test(data_cdi1$STG_left_fet, data_cdi1$cdi1_per)
cor.test(data_cdi1$STG_right_fet, data_cdi1$cdi1_per)
cor.test(data_cdi1$IFG_left_fet, data_cdi1$cdi1_per)
cor.test(data_cdi1$IFG_right_fet, data_cdi1$cdi1_per)

cor.test(data_cdi2$STG_left_fet, data_cdi2$cdi2_per)
cor.test(data_cdi2$STG_right_fet, data_cdi2$cdi2_per)
cor.test(data_cdi2$IFG_left_fet, data_cdi2$cdi2_per)
cor.test(data_cdi2$IFG_right_fet, data_cdi2$cdi2_per)


###############################Regression analysis#######################################
#select relevant columns for regression
data_cdi1_reg = data_cdi1 |> 
  select(ID, cdi1_per,
         IFG_left_fet, IFG_right_fet,
         STG_left_fet, STG_right_fet,
         total_volume_fet, age_child, age_18)

data_cdi2_reg = data_cdi2 |> 
  select(ID, cdi2_per,
         IFG_left_fet, IFG_right_fet,
         STG_left_fet, STG_right_fet,
         total_volume_fet, age_child, age_24)

#transform from wide to long format
data_cdi1_reg_long <- data_cdi1_reg %>%
  pivot_longer(
    cols = starts_with(c("IFG_left_fet", "IFG_right_fet", "STG_left_fet", "STG_right_fet")),
    names_to = c(".value", "hem"),
    names_pattern = "(.+)_(left|right)"
  )

data_cdi2_reg_long <- data_cdi2_reg %>%
  pivot_longer(
    cols = starts_with(c("IFG_left_fet", "IFG_right_fet", "STG_left_fet", "STG_right_fet")),
    names_to = c(".value", "hem"),
    names_pattern = "(.+)_(left|right)"
  )

##regressions
model1 = cdi1_per ~  STG * hem + IFG * hem + total_volume_fet + age_child
fit1 = lm(formula = model1, data=data_cdi1_reg_long)
summary(fit1)
lm.beta(fit1)

model2 = cdi2_per ~  STG * hem + IFG * hem + total_volume_fet + age_child
fit2 = lm(formula = model2, data=data_cdi2_reg_long)
summary(fit2)
lm.beta(fit2)

#####adjust for multiple comparisons#######
#IFG
p11 = c(0.700,0.64830)
p.adjust(p11, method = "holm", n = length(p11))
#1 1

#hemisphere
p12 = c(0.527,0.59984)
p.adjust(p12, method = "holm", n = length(p12))
# 1 1

#STG
p13 = c(0.777,0.01899)
p.adjust(p13, method = "holm", n = length(p13))
#0.77700 0.03798


#total volume
p14 = c(0.285, 0.00966)
p.adjust(p14, method = "holm", n = length(p14))
#0.28500 0.01932

#gestational age at scan
p114 = c(0.123,0.09106)
p.adjust(p114, method = "holm", n = length(p114))
#0.18212 0.18212

#interaction IFG x hemisphere
p15 = c(0.897,0.47180)
p.adjust(p15, method = "holm", n = length(p15))
# 0.9436 0.9436

#interaction STG x hemisphere
p16 = c(0.699,0.67134)
p.adjust(p16, method = "holm", n = length(p16))
# 1 1

#whole model
p17 = c(0.7725, 0.01693)
p.adjust(p17, method = "holm", n = length(p17))
#0.77250 0.03386



#############################################Analysis without high-likelihood children###############################################
data_cdi1_reg_long_norisk = data_cdi1_reg_long[-c(45:50),]
data_cdi2_reg_long_norisk = data_cdi2_reg_long[-c(43:48),]

##regressions
model11 = cdi1_per ~  STG * hem + IFG * hem + total_volume_fet + age_child
fit11 = lm(formula = model11, data=data_cdi1_reg_long_norisk)
summary(fit11)
lm.beta(fit11)

model12 = cdi2_per ~  STG * hem + IFG * hem + total_volume_fet + age_child
fit12 = lm(formula = model12, data=data_cdi2_reg_long_norisk)
summary(fit12)
lm.beta(fit12)

#####adjust for multiple comparisons#######
#IFG
p11 = c(0.736,0.55295 )
p.adjust(p11, method = "holm", n = length(p11))
#1 1

#hemisphere
p12 = c(0.523,0.62301)
p.adjust(p12, method = "holm", n = length(p12))
# 1 1

#STG
p13 = c(0.982,0.00303)
p.adjust(p13, method = "holm", n = length(p13))
#0.98200 0.00606


#total volume
p14 = c(0.486, 0.00256)
p.adjust(p14, method = "holm", n = length(p14))
#0.48600 0.00512

#gestational age at scan
p114 = c(0.435,0.23297)
p.adjust(p114, method = "holm", n = length(p114))
#0.46594 0.46594

#interaction IFG x hemisphere
p15 = c(0.727,0.44105)
p.adjust(p15, method = "holm", n = length(p15))
# 0.8821 0.8821

#interaction STG x hemisphere
p16 = c(0.809,0.56268)
p.adjust(p16, method = "holm", n = length(p16))
# 1 1

#whole model
p17 = c(0.9797, 0.01364)
p.adjust(p17, method = "holm", n = length(p17))
#0.97970 0.02728
