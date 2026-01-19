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

#rename total volume column
names(data_volume_fet)[names(data_volume_fet) == "Total"] <- "total_volume_fet"

#calculate IFG left & right and bilateral volume
data_volume_fet$IFG_left_fet = (data_volume_fet$Frontal_Inf_Tri_L + data_volume_fet$Frontal_Inf_Oper_L + data_volume_fet$Frontal_Inf_Orb_L)
data_volume_fet$IFG_right_fet = (data_volume_fet$Frontal_Inf_Tri_R + data_volume_fet$Frontal_Inf_Oper_R + data_volume_fet$Frontal_Inf_Orb_R)
data_volume_fet$IFG_fet = (data_volume_fet$IFG_left_fet + data_volume_fet$IFG_right_fet)

#create STG columns & calculate bilateral volume
data_volume_fet$STG_left_fet = (data_volume_fet$Temporal_Sup_L)
data_volume_fet$STG_right_fet = (data_volume_fet$Temporal_Sup_R)
data_volume_fet$STG_fet = (data_volume_fet$Temporal_Sup_L + data_volume_fet$Temporal_Sup_R)

#calculate residuals from regression of STG/IFG volume ~ total intracranial volume
model_resIFG_left = IFG_left_fet ~  total_volume_fet
fit_resIFG_left = lm(formula = model_resIFG_left, data=data_volume_fet)
summary(fit_resIFG_left)
data_volume_fet$IFG_left_fet_resid = fit_resIFG_left$residuals

model_resIFG_right = IFG_right_fet ~  total_volume_fet
fit_resIFG_right = lm(formula = model_resIFG_right, data=data_volume_fet)
summary(fit_resIFG_right)
data_volume_fet$IFG_right_fet_resid = fit_resIFG_right$residuals

model_resIFG = IFG_fet ~  total_volume_fet
fit_resIFG = lm(formula = model_resIFG, data=data_volume_fet)
summary(fit_resIFG)
data_volume_fet$IFG_fet_resid = fit_resIFG$residuals

model_resSTG_left = STG_left_fet ~  total_volume_fet
fit_resSTG_left = lm(formula = model_resSTG_left, data=data_volume_fet)
summary(fit_resSTG_left)
data_volume_fet$STG_left_fet_resid = fit_resSTG_left$residuals

model_resSTG_right = STG_right_fet ~  total_volume_fet
fit_resSTG_right = lm(formula = model_resSTG_right, data=data_volume_fet)
summary(fit_resSTG_right)
data_volume_fet$STG_right_fet_resid = fit_resSTG_right$residuals

model_resSTG = STG_fet ~  total_volume_fet
fit_resSTG = lm(formula = model_resSTG, data=data_volume_fet)
summary(fit_resSTG)
data_volume_fet$STG_fet_resid = fit_resSTG$residuals

#filter only relevant columns from volume data
data_volume_fet = data_volume_fet |> 
  select(ID, 
         IFG_left_fet, IFG_right_fet, IFG_fet,
         STG_left_fet, STG_right_fet, STG_fet,
         IFG_left_fet_resid, IFG_right_fet_resid, IFG_fet_resid,
         STG_left_fet_resid, STG_right_fet_resid, STG_fet_resid,
         total_volume_fet)

#filter only relevant columns from general & language data & dummy-code sex variable
data_general_and_cdi = data_general_and_cdi |> 
  select(ID,sex,age_child, age_18, age_24,cdi1,cdi2)

# Assuming your data frame is named 'data'
data_general_and_cdi <- data_general_and_cdi %>%
  mutate(sex_dummy = ifelse(sex == "male", 1, 0))

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

###Check sample size##
#Calculate amount of overlapping IDs in the two datasets
overlapping_ids <- intersect(data_cdi1$ID, data_cdi2$ID)
length(overlapping_ids) #n = 19

#Calculate amount of "unique" IDs in each dataset (sanity check)
unique_ids_cdi1 <- setdiff(data_cdi1$ID, data_cdi2$ID)
length(unique_ids_cdi1) #n = 6

unique_ids_cdi2 <- setdiff(data_cdi2$ID, data_cdi1$ID)
length(unique_ids_cdi2) #n = 5




### Perform the Shapiro-Wilk test
#cdi1
cdi1_vocab = data_cdi1$cdi1_per
shapiro_test_cdi1 <- shapiro.test(cdi1_vocab) #shapiro wilk test
print(shapiro_test_cdi1) #p > .05 -> normal distribution
# Basic histogram
hist(cdi1_vocab, main="Histogram of Vocabulary Scores", xlab="Vocabulary Scores", col="lightblue", border="black")

#cdi2
cdi2_vocab = data_cdi2$cdi2_per
shapiro_test_cdi2 <- shapiro.test(cdi2_vocab) #shapiro wilk test
print(shapiro_test_cdi2) #p > .05 -> normal distribution
# Basic histogram
hist(cdi2_vocab, main="Histogram of Vocabulary Scores", xlab="Vocabulary Scores", col="lightblue", border="black")



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
describeBy(data$STG_fet_resid,data$sex)
#left
describeBy(data$STG_left_fet_resid,data$sex)
#right
describeBy(data$STG_right_fet_resid,data$sex)
#IFG
describeBy(data$IFG_fet_resid,data$sex)
#left
describeBy(data$IFG_left_fet_resid,data$sex)
#right
describeBy(data$IFG_right_fet_resid,data$sex)


#vocabulary scores
#cdi1
describe(data_cdi1$cdi1)
describe(data_cdi1$cdi1_per)

#cdi2
describe(data_cdi2$cdi2)
describe(data_cdi2$cdi2_per)

#test cdi scores of bi-/multilingual vs. monolingual kids
data_cdi1_mono = data_cdi1[-c(3,4,6,7,9,19,21,22,23),]
data_cdi1_multi = data_cdi1[c(3,4,6,7,9,19,21,22,23),]
t.test(data_cdi1_mono$cdi1,data_cdi1_multi$cdi1) #no significant difference (actually multilingual kids higher average)

data_cdi2_mono = data_cdi2[-c(2,4,7,8,12,18,21,22),]
data_cdi2_multi = data_cdi2[c(2,4,7,8,12,18,21,22),]
t.test(data_cdi2_mono$cdi2,data_cdi2_multi$cdi2) #no significant difference (actually multilingual kids higher average)


#break-up descriptive statistics by sex
t.test(data$STG_left_fet_resid~data$sex)
t.test(data$STG_right_fet_resid~data$sex)

t.test(data$IFG_left_fet_resid~data$sex)
t.test(data$IFG_right_fet_resid~data$sex)


##test if the two hemispheres are significantly different from each other
t.test(data$IFG_left_fet_resid,data$IFG_right_fet_resid)
t.test(data$STG_left_fet_resid,data$STG_right_fet_resid)

t.test(data$IFG_left_fet,data$IFG_right_fet)
t.test(data$STG_left_fet,data$STG_right_fet) # right one is bigger

##correlation of IFG and STG volume with potential covariates
cor.test(data$IFG_right_fet_resid, data$age_child)
#cor.test(data$IFG_right_fet_resid, data$total_volume_fet)
cor.test(data$IFG_right_fet_resid, data$age_mother)

cor.test(data$IFG_left_fet_resid, data$age_child)
#cor.test(data$IFG_left_fet_resid, data$total_volume_fet)
cor.test(data$IFG_left_fet_resid, data$age_mother)

cor.test(data$IFG_fet_resid, data$age_child)
#cor.test(data$IFG_fet, data$total_volume_fet)
cor.test(data$IFG_fet_resid, data$age_mother)

cor.test(data$STG_right_fet_resid, data$age_child)
#cor.test(data$STG_right_fet, data$total_volume_fet)
cor.test(data$STG_right_fet_resid, data$age_mother)

cor.test(data$STG_left_fet_resid, data$age_child)
#cor.test(data$STG_left_fet_resid, data$total_volume_fet)
cor.test(data$STG_left_fet_resid, data$age_mother)

cor.test(data$STG_fet_resid, data$age_child)
#cor.test(data$STG_fet, data$total_volume_fet)
cor.test(data$STG_fet_resid, data$age_mother)

##first-order correlations
#split by hemisphere
cor.test(data_cdi1$STG_left_fet_resid, data_cdi1$cdi1)
cor.test(data_cdi1$STG_right_fet_resid, data_cdi1$cdi1)
cor.test(data_cdi1$IFG_left_fet_resid, data_cdi1$cdi1)
cor.test(data_cdi1$IFG_right_fet_resid, data_cdi1$cdi1)

cor.test(data_cdi2$STG_left_fet_resid, data_cdi2$cdi2)
cor.test(data_cdi2$STG_right_fet_resid, data_cdi2$cdi2)
cor.test(data_cdi2$IFG_left_fet_resid, data_cdi2$cdi2)
cor.test(data_cdi2$IFG_right_fet_resid, data_cdi2$cdi2)


###############################Regression analysis#######################################
#select relevant columns for regression
data_cdi1_reg = data_cdi1 |> 
  select(ID, cdi1,
         IFG_left_fet, IFG_right_fet,
         STG_left_fet, STG_right_fet,
         IFG_left_fet_resid, IFG_right_fet_resid, IFG_fet_resid,
         STG_left_fet_resid, STG_right_fet_resid, STG_fet_resid,
         total_volume_fet, age_child, age_18, sex, sex_dummy)

data_cdi2_reg = data_cdi2 |> 
  select(ID, cdi2,
         IFG_left_fet, IFG_right_fet,
         STG_left_fet, STG_right_fet,
         IFG_left_fet_resid, IFG_right_fet_resid, IFG_fet_resid,
         STG_left_fet_resid, STG_right_fet_resid, STG_fet_resid,
         total_volume_fet, age_child, age_24, sex, sex_dummy)

#transform from wide to long format
data_cdi1_reg_long <- data_cdi1_reg %>%
  pivot_longer(
    cols = starts_with(c("IFG_left_fet_resid", "IFG_right_fet_resid", "STG_left_fet_resid", "STG_right_fet_resid")),
    names_to = c(".value", "hem"),
    names_pattern = "(.+)_(left|right)")

data_cdi2_reg_long <- data_cdi2_reg %>%
  pivot_longer(
    cols = starts_with(c("IFG_left_fet_resid", "IFG_right_fet_resid", "STG_left_fet_resid", "STG_right_fet_resid")),
    names_to = c(".value", "hem"),
    names_pattern = "(.+)_(left|right)")


##regressions
model1 = cdi1 ~  STG * hem + IFG * hem + age_18 + sex_dummy
fit1 = lm(formula = model1, data=data_cdi1_reg_long)
summary(fit1)
lm.beta(fit1)

residuals1 <- resid(fit1)
qqnorm(residuals1)
qqline(residuals1)

model2 = cdi2 ~  STG * hem + IFG * hem + age_24 + sex_dummy
fit2 = lm(formula = model2, data=data_cdi2_reg_long)
summary(fit2)
lm.beta(fit2)

residuals2 <- resid(fit2)
qqnorm(residuals2)
qqline(residuals2)

#####adjust for multiple comparisons#######
#IFG
p11 = c(0.68515,0.21567)
p.adjust(p11, method = "holm", n = length(p11))
#0.68515 0.43134

#hemisphere
p12 = c(0.98599,0.84460)
p.adjust(p12, method = "holm", n = length(p12))
# 1 1

#STG
p13 = c(0.93243,0.02176)
p.adjust(p13, method = "holm", n = length(p13))
#0.93243 0.04352


#age_18
p14 = c(0.12892, 0.00324)
p.adjust(p14, method = "holm", n = length(p14))
#0.12892 0.00648

#sex
p114 = c(0.00322,0.59043)
p.adjust(p114, method = "holm", n = length(p114))
#0.00644 0.59043

#interaction IFG x hemisphere
p15 = c(0.92402,0.90627)
p.adjust(p15, method = "holm", n = length(p15))
# 1 1

#interaction STG x hemisphere
p16 = c(0.66845,0.94847)
p.adjust(p16, method = "holm", n = length(p16))
# 1 1

#whole model
p17 = c(0.08805, 0.0009793)
p.adjust(p17, method = "holm", n = length(p17))
#0.0880500 0.0019586



#############################################Analysis without high-likelihood children###############################################
data_cdi1_norisk = data_cdi1[-c(23:25),]
data_cdi1_reg_long_norisk = data_cdi1_reg_long[-c(45:50),]
data_cdi2_norisk = data_cdi2[-c(22:24),]
data_cdi2_reg_long_norisk = data_cdi2_reg_long[-c(43:48),]

##regressions
model11 = cdi1 ~  STG * hem + IFG * hem + age_18 + sex
fit11 = lm(formula = model11, data=data_cdi1_reg_long_norisk)
summary(fit11)
lm.beta(fit11)

model12 = cdi2 ~  STG * hem + IFG * hem + age_24 + sex
fit12 = lm(formula = model12, data=data_cdi2_reg_long_norisk)
summary(fit12)
lm.beta(fit12)

#####adjust for multiple comparisons#######
#IFG
p11 = c(0.2351,0.5825)
p.adjust(p11, method = "holm", n = length(p11))
#0.4702 0.5825

#hemisphere
p12 = c(0.7901,0.8339)
p.adjust(p12, method = "holm", n = length(p12))
# 1 1

#STG
p13 = c(0.2684,0.0167)
p.adjust(p13, method = "holm", n = length(p13))
#0.2684 0.0334


#age
p14 = c(0.0337,0.0538)
p.adjust(p14, method = "holm", n = length(p14))
#0.0674 0.0674

#sex
p114 = c(0.0000401,0.6311)
p.adjust(p114, method = "holm", n = length(p114))
#0.0000802 0.6311000

#interaction IFG x hemisphere
p15 = c(0.7551,0.7202)
p.adjust(p15, method = "holm", n = length(p15))
# 1 1

#interaction STG x hemisphere
p16 = c(0.3606,0.8499)
p.adjust(p16, method = "holm", n = length(p16))
# 0.7212 0.8499

#whole model
p17 = c(0.001543, 0.0009227)
p.adjust(p17, method = "holm", n = length(p17))
#0.0018454 0.0018454





######plot_ with whole sample (including ASD risk children)########
library(ggplot2)
library(gridExtra)
library(cowplot)

plot1 = ggplot(data_cdi1, aes(x=IFG_fet_resid, y=cdi1)) + 
  geom_point() +
  geom_smooth(fullrange = TRUE,method='lm', se = FALSE, color = "black")  +
  theme_classic() +
  theme(axis.title.y = element_text(margin=margin(r=8), size = 10),
        axis.title.x = element_text(size = 10),
        plot.title = element_text(size=14)) +
  xlim(-1000,1000) +
  xlab("Foetal IFG volume")+
  ylim(0,50) +
  ylab("CDI score at 18 months")+
  ggtitle("A")
#plot1
plot2 = ggplot(data_cdi1, aes(x=STG_fet_resid, y=cdi1)) + 
  geom_point() +
  geom_smooth(fullrange = TRUE,method='lm', se = FALSE, color = "black")  +
  theme_classic() +
  theme(axis.title.y = element_text(margin=margin(r=8), size = 10),
        axis.title.x = element_text(size = 10),
        plot.title = element_text(size=14)) +
  xlim(-500,500) +
  xlab("Foetal STG volume")+
  ylim(0,50) +
  ylab("CDI score at 18 months")+
  ggtitle("B")
#plot2        
plot3 = ggplot(data_cdi2, aes(x=IFG_fet_resid, y=cdi2)) + 
  geom_point() +
  geom_smooth(fullrange = TRUE,method='lm', se = FALSE, color = "black")  +
  theme_classic()+
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        plot.title = element_text(size=14))+
  xlim(-1000,1000) +
  xlab("Foetal IFG volume")+
  ylim(50,100) +
  ylab("CDI score at 24-36 months")+
  ggtitle("C")
#plot3
plot4 = ggplot(data_cdi2, aes(x=STG_fet_resid, y=cdi2)) + 
  geom_point() +
  geom_smooth(fullrange = TRUE,method='lm', se = FALSE, color = "black")  +
  theme_classic() +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        plot.title = element_text(size=14))+
  xlim(-500,500) +
  xlab("Foetal STG volume")+
  ylim(50,100) +
  ylab("CDI score at 24-36 months")+
  ggtitle("D")
plot4   

p1 = grid.arrange(plot1, plot2, plot3, plot4, nrow = 2) 


ggsave(p1, file="/Users/werwach/Documents/CHILD/paper_prenatal_volume_vocab/figures/regression_plots_withrisk.eps", device="eps")





######plot_ without ASD risk children########
library(ggplot2)
library(gridExtra)
library(cowplot)

plot1 = ggplot(data_cdi1_norisk, aes(x=IFG_fet, y=cdi1_per)) + 
  geom_point() +
  geom_smooth(fullrange = TRUE,method='lm', se = FALSE, color = "black")  +
  theme_classic() +
  theme(axis.title.y = element_text(margin=margin(r=8), size = 10),
        axis.title.x = element_text(size = 10),
        plot.title = element_text(size=14)) +
  xlim(2000,5300) +
  xlab("Foetal IFG volume")+
  ylim(0,100) +
  ylab("CDI score at 18 months")+
  ggtitle("A")
#plot1
plot2 = ggplot(data_cdi1_norisk, aes(x=STG_fet, y=cdi1_per)) + 
  geom_point() +
  geom_smooth(fullrange = TRUE,method='lm', se = FALSE, color = "black")  +
  theme_classic() +
  theme(axis.title.y = element_text(margin=margin(r=8), size = 10),
        axis.title.x = element_text(size = 10),
        plot.title = element_text(size=14)) +
  xlim(1500,3300) +
  xlab("Foetal STG volume")+
  ylim(0,100) +
  ylab("CDI score at 18 months")+
  ggtitle("B")
#plot2        
plot3 = ggplot(data_cdi2_norisk, aes(x=IFG_fet, y=cdi2_per)) + 
  geom_point() +
  geom_smooth(fullrange = TRUE,method='lm', se = FALSE, color = "black")  +
  theme_classic()+
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        plot.title = element_text(size=14))+
  xlim(2000,5300) +
  xlab("Foetal IFG volume")+
  ylim(0,100) +
  ylab("CDI score at 24-36 months")+
  ggtitle("C")
#plot3
plot4 = ggplot(data_cdi2_norisk, aes(x=STG_fet, y=cdi2_per)) + 
  geom_point() +
  geom_smooth(fullrange = TRUE,method='lm', se = FALSE, color = "black")  +
  theme_classic() +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        plot.title = element_text(size=14))+
  xlim(1500,3300) +
  xlab("Foetal STG volume")+
  ylim(0,100) +
  ylab("CDI score at 24-36 months")+
  ggtitle("D")
#plot4   

p2 = grid.arrange(plot1, plot2, plot3, plot4, nrow = 2) 


ggsave(p2, file="/Users/werwach/Documents/CHILD/paper_prenatal_volume_vocab/figures/regression_plots_norisk.eps", device="eps")





#################################Mediation via stress#######################################

#load stress data
data_stress = read.csv("CHILD_VA_stress.csv", header = TRUE, sep = ",")
names(data_stress)[1] <- 'ID'

#filter only relevant data from stress dataframe
data_stress = data_stress |>
  filter(redcap_event_name == "prenatal_arm_1")  |>
  dplyr::select(ID, cohen_total)

data_cdi_stress = data |> 
  inner_join(data_stress, by = "ID")

describe(data_cdi_stress$cohen_total)

#filter out stress outlier
data_cdi_stress = data_cdi_stress |> 
  filter(cohen_total<30 | is.na(cohen_total))

#create sex dummy variable
data_cdi_stress$sex_dummy <- ifelse(data_cdi_stress$sex == "male", 1, 0)

#ad percentile scores for cdi2
data_cdi_stress$cdi2_per = c(NA,99,65,60,60,85,99,30,75,10,NA,25,70,25,58,15,70,90,40,99,10,80,85,20,NA,NA,NA,5,10)

###check potential covariates

t.test(data_cdi_stress$cohen_total~data_cdi_stress$sex_dummy) #yes?

t.test(data_cdi_stress$STG_fet~data_cdi_stress$sex_dummy) #no

t.test(data_cdi_stress$age_child~data_cdi_stress$sex_dummy) #no

cor.test(data_cdi_stress$STG_fet, data_cdi_stress$total_volume_fet) #yes

cor.test(data_cdi_stress$STG_fet, data_cdi_stress$age_child) #yes

cor.test(data_cdi_stress$cohen_total, data_cdi_stress$age_child) #no

cor.test(data_cdi_stress$cohen_total, data_cdi_stress$age_mother) #no

cor.test(data_cdi_stress$cdi2_per, data_cdi_stress$age_mother) #no


#######regression: PSS-10
model1 = STG_fet ~  cohen_total + age_child + total_volume_fet + sex_dummy
fit1 = lm(formula = model1, data=data_cdi_stress)
summary(fit1)

model2 = cdi2_per ~  cohen_total + age_mother +  STG_fet + total_volume_fet #+sex_dummy
fit2 = lm(formula = model2, data=data_cdi_stress)
summary(fit2)

#filter kids who dont have any missing values on relevant variables
data_cdi_stress_nomiss= data_cdi_stress |> 
  filter(!is.na(cohen_total) & !is.na(cdi2_per) & !is.na(STG_fet))

#plot
plot1 = ggplot(data, aes(x=cohen_total, y=STG_fet)) + 
  geom_point() +
  geom_smooth(fullrange = TRUE,method='lm', se = FALSE, color = "black")  +
  theme_classic() +
  theme(axis.title.y = element_text(margin=margin(r=8), size = 10),
        axis.title.x = element_text(size = 10),
        plot.title = element_text(size=14)) +
  #  xlim(500,1800) +
  xlab("Maternal PSS-10 score")+
  #  ylim(50,100) +
  ylab("Superior temporal brain volume")+
  ggtitle("A")


plot2 = ggplot(data, aes(x=cohen_total, y=cdi2_per)) + 
  geom_point() +
  geom_smooth(fullrange = TRUE,method='lm', se = FALSE, color = "black")  +
  theme_classic() +
  theme(axis.title.y = element_text(margin=margin(r=8), size = 10),
        axis.title.x = element_text(size = 10),
        plot.title = element_text(size=14)) +
  #  xlim(500,1800) +
  xlab("Maternal PSS-10 score")+
  #  ylim(50,100) +
  ylab("CDI score at 24-36 months")+
  ggtitle("B")

plot3 = ggplot(data, aes(x=STG_fet, y=cdi2_per)) + 
  geom_point() +
  geom_smooth(fullrange = TRUE,method='lm', se = FALSE, color = "black")  +
  theme_classic() +
  theme(axis.title.y = element_text(margin=margin(r=8), size = 10),
        axis.title.x = element_text(size = 10),
        plot.title = element_text(size=14)) +
  #  xlim(500,1800) +
  xlab("Superior temporal brain volume")+
  #  ylim(50,100) +
  ylab("CDI score at 24-36 months")+
  ggtitle("C")



grid.arrange(plot1, plot2, plot3,  ncol = 3) 


####mediation
library(lavaan)

plot(data_cdi_stress$cdi2_per,data_cdi_stress$cohen_total)

#data_volume_stress=data_volume_stress[-c(15:17,21:23),]


model_mediation = '
# mediator
STG_fet ~ a * cohen_total +  +a1*sex_dummy +  a3*total_volume_fet + a2*age_child 
#total effect
cdi2_per ~ c * cohen_total + b * STG_fet + b4*total_volume_fet 

#indirect effect
ind1 := a*b
tot1 := (a*b) + c

'

fit_mediation <- lavaan::sem(model_mediation, data = data_cdi_stress, missing = "fiml")
summary(fit_mediation, fit = T, standardized = T, rsquare = T)


##impute data
library(mice)

#about 10% average missing data, so maxit = 10
tempdata <- mice(data_cdi_cohen_med,m=5,maxit=10,meth='pmm',seed=500)
summary(tempdata)
data_cdi_cohen_med_miss = complete(tempdata,1)

# Plots of imputed vs. orginal data
library(lattice)
# Scatterplot Ozone vs all
xyplot(tempdata,cdi2_per ~ STG_fet,pch=18,cex=1)

# Density plot original vs imputed dataset
densityplot(tempdata)

# Another take on the density: stripplot()
stripplot(tempdata)


pfad_a = lm(STG_fet~cohen_total+total_volume_fet+sex_dummy+age_child,data_cdi_stress)
summary(pfad_a)
pfad_b_c = lm(cdi2_per~cohen_total + STG_fet + total_volume_fet,data_cdi_stress)
summary(pfad_b_c)

library(mediation)

results <- mediate(pfad_a, pfad_b_c, 
                   treat = "cohen_total", mediator = "STG_fet", 
                   boot = TRUE)
summary(results)

library(sem)
library(bmem)



model_l <- '

# mediator
STG_fet ~ a * cohen_total + a1*sex + a2*age_child + a3*total_volume_fet
#total effect
cdi2_per ~ c * cohen_total + b * STG_fet + + b4*total_volume_fet 

#indirect effect
ind1 := a*b
tot1 := (a*b) + c
'

effects<-c('a*b', 'cp+a*b') 
nlsy.res<-bmem(data_cdi_cohen_med, model = model_1, method = "list",indirect = effects)

