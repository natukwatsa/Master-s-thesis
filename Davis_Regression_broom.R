#Davis's Code
rm(list=ls())
version
library(broom)
library(lubridate)
library(tidyverse)
library(gtsummary)
library(broom)
library(visdat)
library(rstatix)
library(reshape2)
library(pheatmap)
pacman::p_load(shiny, flextable, rio, sjmisc)
library(shiny)
library(flextable)
library(officer)
library(dplyr)
library(GGally)
library(rio)
library(janitor)


setwd("D:/Proposal writing/Data folder")
the_data <- read.csv("clean data.csv", 
                     header=TRUE, 
                     sep=",",
                     stringsAsFactors = TRUE)

str(the_data)
summary(the_data)
names(the_data)

##selecting only numeric variables to help in running the correlation matrix
data_nm <- select_if(the_data, is.numeric)
data_num <- within(data_nm, rm(snum))
names(data_num)

##generating data values for all the categorical variables
the_data1 <- the_data %>%
  mutate(cat_sx_outcome = case_when(sx_outcome == c("Failure")~0, 
                                    sx_outcome==c("Success")~ 1)) %>%
  mutate(gender= case_when(sex==c("F")~0,
                           sex==c("M")~1)) %>%
  mutate(c_section= case_when(csection==c("C-section")~0,
                              csection==c("SVD")~1)) %>%
  mutate(birthseason= case_when(season_birth==c("Dry")~0,
                                season_birth==c("Rainy")~1)) %>%
  mutate(Region= case_when(region==c("Central")~0,
                           region==c("Northern")~1,
                           region==c("Eastern")~2,
                           region==c("Western")~3)) %>%
  mutate(birthplace= case_when(plac_b==c("Health Centre")~0,
                               plac_b==c("Clinic")~1,
                               plac_b==c("Home")~2,
                               plac_b==c("Hospital")~3)) %>%
  mutate(gestation= case_when(gest==c("Preterm")~0,
                              gest==c("Term")~1,
                              gest==c("Unknown")~2)) %>%
  mutate(HC_etiology= case_when(hydro_etiology==c("Hydranencephaly")~0,
                                hydro_etiology==c("Cyst")~1,
                                hydro_etiology==c("Anatomic Malformation")~2,
                                hydro_etiology==c("Schizencephaly")~3,
                                hydro_etiology==c("Aqueduct Stenosis")~4,
                                hydro_etiology==c("Dandy-Walker")~5,
                                hydro_etiology==c("PHH")~6,
                                hydro_etiology==c("Tumor")~7,
                                hydro_etiology==c("Congenital Hydrocephalus")~8,
                                hydro_etiology==c("Holoprosencephaly")~9,
                                hydro_etiology==c("PIH")~10))


#adding value labels to data
the_data1 <- the_data1 %>% add_labels(gender, labels=c("Female"=0, "Male"=1)) %>%
  add_labels(cat_sx_outcome, labels = c("Failure"=0, "Success"=1)) %>%
  add_labels(csection, labels = c("C-section"=0, "SVD"=1)) %>%
  add_labels(Region, labels = c("Central"=0, "Northern"=1, "Eastern"=2, "Western"=3)) %>%
  add_labels(birthplace, labels = c("Health Centre"=0, "Clinic"=1, "Home"=2, "Hospital"=3)) %>%
  add_labels(gestation, labels = c("Preterm"=0, "Term"=1, "Unknown"=2)) %>%
  add_labels(HC_etiology, labels = c("Hydranencephaly"=0, "Cyst"=1, "Anatomic Malformation"=2, "Schizencephaly"=3, "Aqueduct Stenosis"=4, "Dandy-Walker"=5, "PHH"=6, "Tumor"=7, "Congenital Hydrocephalus"=8, "Holoprosencephaly"=9, "PIH"=10))


#getting the value labels
get_labels(the_data1$gender)
get_labels(the_data1$birthplace)

ncol(the_data1)
nrow(the_data1)

#dropping data variables that have been transformed
the_data2 <- subset(the_data1, select= -c(snum, sx_outcome, season_birth, sex, csection, place_of_delivery, time_infection, region, plac_b, gest, hydro_etiology))

##You need rownames here
kk<-the_data1$snum
ncol(the_data1)
rownames(the_data2)<-kk
kk

print(head(the_data2))
names(the_data2)


#univariate plots
hist(the_data1$child_weight, main = "Child's weight", xlab = "weight", ylab = "count", col="red")
picking<-the_data1[, 5:19]
boxplot(picking)
#dropping useless data frames
rm(data_nm, picking)

##bivariate plots showing distribution
plot(the_data1$region, the_data1$csf_vol)
plot(the_data1$time_admission, the_data1$csf_vol)

ggplot(the_data1, aes(brain_vol, child_weight)) + geom_point(aes(color=cat_sx_outcome, size=time_admission))

print(head(the_data1))


###Note if you are set on using this on everything you just need to change everything to 0 and 1 for categorical


##You need to make sure you do this
i <- sapply(the_data2, is.factor)
the_data1[i] <- lapply(the_data2[i], as.character)

ia <- sapply(the_data2, is.integer)
the_data1[ia] <- lapply(the_data2[ia], as.numeric)

##correction matrix for all the numeric variables
#Weid odd function is not automatically detecting
#method1
cor.mat <- data_num %>% cor_mat()
cor.mat
cor_pvalues <- cor.mat %>% cor_get_pval() #getting significant levels
cor_pvalues

cor.mat %>%
  pull_lower_triangle() %>%
  cor_plot(label = FALSE)
View(cor.mat)

#correlation matrix plots
cor_plot(cor.mat)
cor_plot(cor_pvalues, method = "number",
         label = TRUE,
         insignificant = "blank")

##drawing a heatmap of correlation values and p-values respectively
pheatmap(as.matrix(cor.mat[,-1]), display_numbers=TRUE, cluster_rows = FALSE, cluster_cols = FALSE, labels_row = c("time_headincrease", "time_admission", "head_circum", "child_weigh", "wbc", "brain_vol", "csf_vol"))
pheatmap(as.matrix(cor_pvalues[,-1]), display_numbers=TRUE, cluster_rows = FALSE, cluster_cols = FALSE, labels_row = c("time_headincrease", "time_admission", "head_circum", "child_weigh", "wbc", "brain_vol", "csf_vol"))


names(data_num)

##LOGISTIC REGRESSION
names(the_data2)

#checking missing values
visdat::vis_miss(the_data2) #i.e no missingness observed

#categorical variables

##obtaining p-values and R squared using ranked linear regression method below for all the variables
#cat_sx_outcome
#using ranked linear regression to obtain p_values and R squared
table(the_data2$gest)
lm(rank(cat_sx_outcome) ~ cat_sx_outcome, data = the_data2) %>% summary()
lm(rank(cat_sx_outcome) ~ HC_etiology, data = the_data2) %>% summary()
lm(rank(cat_sx_outcome) ~ birthplace, data = the_data2) %>% summary()
lm(rank(cat_sx_outcome) ~ gestation, data = the_data2) %>% summary()
lm(rank(cat_sx_outcome) ~ gender, data = the_data2) %>% summary()
lm(rank(cat_sx_outcome) ~ c_section, data = the_data2) %>% summary()
lm(rank(cat_sx_outcome) ~ Region, data = the_data2) %>% summary()
lm(rank(cat_sx_outcome) ~ birthseason, data = the_data2) %>% summary()
lm(rank(cat_sx_outcome) ~ time_headincrease, data = the_data2) %>% summary()
lm(rank(cat_sx_outcome) ~ child_weight, data = the_data2) %>% summary()
lm(rank(cat_sx_outcome) ~ csf_vol, data = the_data2) %>% summary()
lm(rank(cat_sx_outcome) ~ brain_vol, data = the_data2) %>% summary()
lm(rank(cat_sx_outcome) ~ time_admission, data = the_data2) %>% summary()
lm(rank(cat_sx_outcome) ~ wbc, data = the_data2) %>% summary()
lm(rank(cat_sx_outcome) ~ head_circum, data = the_data2) %>% summary()

names(the_data2)
#gender
#using ranked linear regression to obtain p_values and R squared
lm(rank(gender) ~ HC_etiology, data = the_data2) %>% summary()
lm(rank(gender) ~ birthplace, data = the_data2) %>% summary()
lm(rank(gender) ~ gestation, data = the_data2) %>% summary()
lm(rank(gender) ~ gender, data = the_data2) %>% summary()
lm(rank(gender) ~ c_section, data = the_data2) %>% summary()
lm(rank(gender) ~ Region, data = the_data2) %>% summary()
lm(rank(gender) ~ birthseason, data = the_data2) %>% summary()
lm(rank(gender) ~ time_headincrease, data = the_data2) %>% summary()
lm(rank(gender) ~ child_weight, data = the_data2) %>% summary()
lm(rank(gender) ~ csf_vol, data = the_data2) %>% summary()
lm(rank(gender) ~ brain_vol, data = the_data2) %>% summary()
lm(rank(gender) ~ time_admission, data = the_data2) %>% summary()
lm(rank(gender) ~ wbc, data = the_data2) %>% summary()
lm(rank(gender) ~ head_circum, data = the_data2) %>% summary()
lm(rank(gender) ~ cat_sx_outcome, data = the_data2) %>% summary()


#birthseason
#using ranked linear regression to obtain p_values and R squared
lm(rank(birthseason) ~ HC_etiology, data = the_data2) %>% summary()
lm(rank(birthseason) ~ birthplace, data = the_data2) %>% summary()
lm(rank(birthseason) ~ gestation, data = the_data2) %>% summary()
lm(rank(birthseason) ~ gender, data = the_data2) %>% summary()
lm(rank(birthseason) ~ c_section, data = the_data2) %>% summary()
lm(rank(birthseason) ~ Region, data = the_data2) %>% summary()
lm(rank(birthseason) ~ birthseason, data = the_data2) %>% summary()
lm(rank(birthseason) ~ time_headincrease, data = the_data2) %>% summary()
lm(rank(birthseason) ~ child_weight, data = the_data2) %>% summary()
lm(rank(birthseason) ~ csf_vol, data = the_data2) %>% summary()
lm(rank(birthseason) ~ brain_vol, data = the_data2) %>% summary()
lm(rank(birthseason) ~ time_admission, data = the_data2) %>% summary()
lm(rank(birthseason) ~ wbc, data = the_data2) %>% summary()
lm(rank(birthseason) ~ head_circum, data = the_data2) %>% summary()
lm(rank(birthseason) ~ cat_sx_outcome, data = the_data2) %>% summary()


#c_section
#using ranked linear regression to obtain p_values and R squared
lm(rank(c_section) ~ HC_etiology, data = the_data2) %>% summary()
lm(rank(c_section) ~ birthplace, data = the_data2) %>% summary()
lm(rank(c_section) ~ gestation, data = the_data2) %>% summary()
lm(rank(c_section) ~ gender, data = the_data2) %>% summary()
lm(rank(c_section) ~ c_section, data = the_data2) %>% summary()
lm(rank(c_section) ~ Region, data = the_data2) %>% summary()
lm(rank(c_section) ~ birthseason, data = the_data2) %>% summary()
lm(rank(c_section) ~ time_headincrease, data = the_data2) %>% summary()
lm(rank(c_section) ~ child_weight, data = the_data2) %>% summary()
lm(rank(c_section) ~ csf_vol, data = the_data2) %>% summary()
lm(rank(c_section) ~ brain_vol, data = the_data2) %>% summary()
lm(rank(c_section) ~ time_admission, data = the_data2) %>% summary()
lm(rank(c_section) ~ wbc, data = the_data2) %>% summary()
lm(rank(c_section) ~ head_circum, data = the_data2) %>% summary()
lm(rank(c_section) ~ cat_sx_outcome, data = the_data2) %>% summary()

#region, gestation, HC_etiology, birthplace


#Region
#using ranked linear regression to obtain p_values and R squared
lm(rank(Region) ~ HC_etiology, data = the_data2) %>% summary()
lm(rank(Region) ~ birthplace, data = the_data2) %>% summary()
lm(rank(Region) ~ gestation, data = the_data2) %>% summary()
lm(rank(Region) ~ gender, data = the_data2) %>% summary()
lm(rank(Region) ~ c_section, data = the_data2) %>% summary()
lm(rank(Region) ~ Region, data = the_data2) %>% summary()
lm(rank(Region) ~ birthseason, data = the_data2) %>% summary()
lm(rank(Region) ~ time_headincrease, data = the_data2) %>% summary()
lm(rank(Region) ~ child_weight, data = the_data2) %>% summary()
lm(rank(Region) ~ csf_vol, data = the_data2) %>% summary()
lm(rank(Region) ~ brain_vol, data = the_data2) %>% summary()
lm(rank(Region) ~ time_admission, data = the_data2) %>% summary()
lm(rank(Region) ~ wbc, data = the_data2) %>% summary()
lm(rank(Region) ~ head_circum, data = the_data2) %>% summary()
lm(rank(Region) ~ cat_sx_outcome, data = the_data2) %>% summary()

#gestation
#using ranked linear regression to obtain p_values and R squared
lm(rank(gestation) ~ HC_etiology, data = the_data2) %>% summary()
lm(rank(gestation) ~ birthplace, data = the_data2) %>% summary()
lm(rank(gestation) ~ gestation, data = the_data2) %>% summary()
lm(rank(gestation) ~ gender, data = the_data2) %>% summary()
lm(rank(gestation) ~ c_section, data = the_data2) %>% summary()
lm(rank(gestation) ~ Region, data = the_data2) %>% summary()
lm(rank(gestation) ~ birthseason, data = the_data2) %>% summary()
lm(rank(gestation) ~ time_headincrease, data = the_data2) %>% summary()
lm(rank(gestation) ~ child_weight, data = the_data2) %>% summary()
lm(rank(gestation) ~ csf_vol, data = the_data2) %>% summary()
lm(rank(gestation) ~ brain_vol, data = the_data2) %>% summary()
lm(rank(gestation) ~ time_admission, data = the_data2) %>% summary()
lm(rank(gestation) ~ wbc, data = the_data2) %>% summary()
lm(rank(gestation) ~ head_circum, data = the_data2) %>% summary()
lm(rank(gestation) ~ cat_sx_outcome, data = the_data2) %>% summary()

#HC_etiology
#using ranked linear regression to obtain p_values and R squared
lm(rank(HC_etiology) ~ HC_etiology, data = the_data2) %>% summary()
lm(rank(HC_etiology) ~ birthplace, data = the_data2) %>% summary()
lm(rank(HC_etiology) ~ gestation, data = the_data2) %>% summary()
lm(rank(HC_etiology) ~ gender, data = the_data2) %>% summary()
lm(rank(HC_etiology) ~ c_section, data = the_data2) %>% summary()
lm(rank(HC_etiology) ~ Region, data = the_data2) %>% summary()
lm(rank(HC_etiology) ~ birthseason, data = the_data2) %>% summary()
lm(rank(HC_etiology) ~ time_headincrease, data = the_data2) %>% summary()
lm(rank(HC_etiology) ~ child_weight, data = the_data2) %>% summary()
lm(rank(HC_etiology) ~ csf_vol, data = the_data2) %>% summary()
lm(rank(HC_etiology) ~ brain_vol, data = the_data2) %>% summary()
lm(rank(HC_etiology) ~ time_admission, data = the_data2) %>% summary()
lm(rank(HC_etiology) ~ wbc, data = the_data2) %>% summary()
lm(rank(HC_etiology) ~ head_circum, data = the_data2) %>% summary()
lm(rank(HC_etiology) ~ cat_sx_outcome, data = the_data2) %>% summary()

#birthplace
#using ranked linear regression to obtain p_values and R squared
lm(rank(birthplace) ~ HC_etiology, data = the_data2) %>% summary()
lm(rank(birthplace) ~ birthplace, data = the_data2) %>% summary()
lm(rank(birthplace) ~ gestation, data = the_data2) %>% summary()
lm(rank(birthplace) ~ gender, data = the_data2) %>% summary()
lm(rank(birthplace) ~ c_section, data = the_data2) %>% summary()
lm(rank(birthplace) ~ Region, data = the_data2) %>% summary()
lm(rank(birthplace) ~ birthseason, data = the_data2) %>% summary()
lm(rank(birthplace) ~ time_headincrease, data = the_data2) %>% summary()
lm(rank(birthplace) ~ child_weight, data = the_data2) %>% summary()
lm(rank(birthplace) ~ csf_vol, data = the_data2) %>% summary()
lm(rank(birthplace) ~ brain_vol, data = the_data2) %>% summary()
lm(rank(birthplace) ~ time_admission, data = the_data2) %>% summary()
lm(rank(birthplace) ~ wbc, data = the_data2) %>% summary()
lm(rank(birthplace) ~ head_circum, data = the_data2) %>% summary()
lm(rank(birthplace) ~ cat_sx_outcome, data = the_data2) %>% summary()


##using linear regression on both continuous variables to obtain R squared
#time taken from birth to notice a head increase
lm(time_headincrease ~ time_headincrease, data=the_data2) %>% broom::glance()
lm(time_headincrease ~ child_weight, data=the_data2) %>% broom::glance()
lm(time_headincrease ~ csf_vol, data=the_data2) %>% broom::glance()
lm(time_headincrease ~ brain_vol, data=the_data2) %>% broom::glance()
lm(time_headincrease ~ time_admission, data=the_data2) %>% broom::glance()
lm(time_headincrease ~ wbc, data=the_data2) %>% broom::glance()
lm(time_headincrease ~ head_circum, data=the_data2) %>% broom::glance()

#child_weight
lm(child_weight ~ time_headincrease, data=the_data2) %>% broom::glance()
lm(child_weight ~ child_weight, data=the_data2) %>% broom::glance()
lm(child_weight ~ csf_vol, data=the_data2) %>% broom::glance()
lm(child_weight ~ brain_vol, data=the_data2) %>% broom::glance()
lm(child_weight ~ time_admission, data=the_data2) %>% broom::glance()
lm(child_weight ~ wbc, data=the_data2) %>% broom::glance()
lm(child_weight ~ head_circum, data=the_data2) %>% broom::glance()

#csf_vol
lm(csf_vol ~ time_headincrease, data=the_data2) %>% broom::glance()
lm(csf_vol ~ child_weight, data=the_data2) %>% broom::glance()
lm(csf_vol ~ csf_vol, data=the_data2) %>% broom::glance()
lm(csf_vol ~ brain_vol, data=the_data2) %>% broom::glance()
lm(csf_vol ~ time_admission, data=the_data2) %>% broom::glance()
lm(csf_vol ~ wbc, data=the_data2) %>% broom::glance()
lm(csf_vol ~ head_circum, data=the_data2) %>% broom::glance()

#brain_vol
lm(brain_vol ~ time_headincrease, data=the_data2) %>% broom::glance()
lm(brain_vol ~ child_weight, data=the_data2) %>% broom::glance()
lm(brain_vol ~ csf_vol, data=the_data2) %>% broom::glance()
lm(brain_vol ~ brain_vol, data=the_data2) %>% broom::glance()
lm(brain_vol ~ time_admission, data=the_data2) %>% broom::glance()
lm(brain_vol ~ wbc, data=the_data2) %>% broom::glance()
lm(brain_vol ~ head_circum, data=the_data2) %>% broom::glance()

#time_admission
lm(time_admission ~ time_headincrease, data=the_data2) %>% broom::glance()
lm(time_admission ~ child_weight, data=the_data2) %>% broom::glance()
lm(time_admission ~ csf_vol, data=the_data2) %>% broom::glance()
lm(time_admission ~ brain_vol, data=the_data2) %>% broom::glance()
lm(time_admission ~ time_admission, data=the_data2) %>% broom::glance()
lm(time_admission ~ wbc, data=the_data2) %>% broom::glance()
lm(time_admission ~ head_circum, data=the_data2) %>% broom::glance()

#wbc
lm(wbc ~ time_headincrease, data=the_data2) %>% broom::glance()
lm(wbc ~ child_weight, data=the_data2) %>% broom::glance()
lm(wbc ~ csf_vol, data=the_data2) %>% broom::glance()
lm(wbc ~ brain_vol, data=the_data2) %>% broom::glance()
lm(wbc ~ time_admission, data=the_data2) %>% broom::glance()
lm(wbc ~ wbc, data=the_data2) %>% broom::glance()
lm(wbc ~ head_circum, data=the_data2) %>% broom::glance()

#head_circum
lm(head_circum ~ time_headincrease, data=the_data2) %>% broom::glance()
lm(head_circum ~ child_weight, data=the_data2) %>% broom::glance()
lm(head_circum ~ csf_vol, data=the_data2) %>% broom::glance()
lm(head_circum ~ brain_vol, data=the_data2) %>% broom::glance()
lm(head_circum ~ time_admission, data=the_data2) %>% broom::glance()
lm(head_circum ~ wbc, data=the_data2) %>% broom::glance()
lm(head_circum ~ head_circum, data=the_data2) %>% broom::glance()
