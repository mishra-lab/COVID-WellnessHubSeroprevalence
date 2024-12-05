######################################## Code Summary #######################################################
### TITLE:            Analysis_Paper1
### BY:               Siyi Wang
### DESCRIPTION:      Part 1. Load data set and set the working directory
###                   Part 2. Prepare data for the analysis of POR and PR
###                   Part 3. Generate Table 2 of POR and PR analysis (age-adjusted and unadjusted)
### DATASETS USED:    Staffdbs_p1.csv
### OUTPUT:           Part 3 (Table). PR_Table2, POR_Table2, PR_adjusted_Table2, POR_adjusted_Table2
####################################### Code Setup ###########################################################
library(tidyverse)
library(stringr)
library(lubridate)
# Get the current R script location as working directory
library(rstudioapi)
#############################################################
########################## PART 1 ###########################
############################################################

# Set working directory as current R script location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# Read the dataset 
staffdbs_p1 = read.csv("../data/Staffdbs_p1.csv", header = TRUE)

##########################################################################
################################ PART 2 ####################################
##############################################################################
 
# Prepare data set for analysis of POR and PR
staffdbs_p1 = staffdbs_p1 %>%
  mutate(p1_age = as.numeric(p1_age),
         p1_gender = factor(p1_gender, levels = c("Man",
                                                  "Woman",
                                                  'Missing/unknown')),
         p1_education = factor(p1_education, levels = c("Up to high school graduation",
                                                        "College degree or trades certificate",
                                                        "University bachelor’s degree",
                                                        "Graduate or professional degree",
                                                        "Missing/unknown")),
       p1_race = factor(p1_race, levels = c("White",
                                              "Black",
                                              "East or Southeast Asian",
                                              "Other racialized",
                                              "Missing/unknown")),
         p1_employment_status = factor(p1_employment_status, levels = c("Full-time",
                                                                        "Part-time or agency/contract",
                                                                        "Other",
                                                                        'Missing/unknown')),
         p1_housing = factor(p1_housing,levels = c("Apartment/condo",
                                                   "House",
                                                   "Other",
                                                   "Missing/unknown")),
         p1_FSAhotspot_binary = factor(p1_FSAhotspot_binary, levels =c("Non-hotspot", "Hotspot", "Missing/unknown")),
         p1_DAincome_tertile = factor(p1_DAincome_tertile, levels = c("1","2","3", 'Missing/unknown')),
         p1_DAessential_tertile = factor(p1_DAessential_tertile, levels = c("1","2","3", 'Missing/unknown')),
         p1_occupation = factor(p1_occupation, levels = c("Administration or management",
                                                          "Personal support worker or support staff (e.g. kitchen, housekeeping, laundry)",
                                                          "Physician, nurse or registered practical nurses",
                                                          "Other",
                                                          'Missing/unknown')),
         p1_LTCHRH = factor(p1_LTCHRH, levels = c("Long-term care home","Retirement home")),
         
         p1_staff_work_transport = factor(p1_staff_work_transport, levels = c("Drive alone",
                                                                              "Walk or cycle",
                                                                              "Rideshare or carpool (including taxi or uber)",
                                                                              "Public transport",
                                                                              "Work from home, or not currently working",
                                                                              "Missing/unknown")),
         
         p1_PaidSickLeave = factor(p1_PaidSickLeave, levels = c("Yes",
                                                                "No"))
  )

# Clean missing data
staffdbs_p1[staffdbs_p1 == 'Missing/unknown'] <- NA
staffdbs_p1 = staffdbs_p1 %>% droplevels()


######################################################################
############################### PART 3 ##############################
######################################################################

# Use log link function for estimating log of RR (which is prevalence ratio here)
staffdbs_adjusted = staffdbs_p1 %>%
  mutate(Serostatus = ifelse(Serostatus == 'Positive', 1, 0))

# Period - age adjusted
period_rr_adjusted <- glm(Serostatus ~ p1_period + p1_age, family = binomial(link = "log"), data = staffdbs_adjusted)
period_rr_adjusted
ci=exp(confint.default(period_rr_adjusted))
rr=exp(coef(period_rr_adjusted))
table_period_rr_adjusted = round(cbind(rr,ci),digits=2)
table_period_rr_adjusted

# Gender - age adjusted
gender_rr_adjusted <- glm(Serostatus ~ p1_gender + p1_age, family = binomial(link = "log"), data = staffdbs_adjusted)
gender_rr_adjusted
ci=exp(confint.default(gender_rr_adjusted))
rr=exp(coef(gender_rr_adjusted))
table_gender_rr_adjusted = round(cbind(rr,ci),digits=2)
table_gender_rr_adjusted

# Education - age adjusted
education_rr_adjusted <- glm(Serostatus ~ p1_education + p1_age, family = binomial(link = "log"), data = staffdbs_adjusted)
education_rr_adjusted
ci=exp(confint.default(education_rr_adjusted))
rr=exp(coef(education_rr_adjusted))
table_education_rr_adjusted = round(cbind(rr,ci),digits=2)
table_education_rr_adjusted

# Race - age adjusted
race_rr_adjusted <- glm(Serostatus ~ p1_race + p1_age, family = binomial(link = "log"), data = staffdbs_adjusted)
race_rr_adjusted
ci=exp(confint.default(race_rr_adjusted))
rr=exp(coef(race_rr_adjusted))
table_race_rr_adjusted = round(cbind(rr,ci),digits=2)
table_race_rr_adjusted

# Income - age adjusted
income_rr_adjusted <- glm(Serostatus ~ p1_household_income + p1_age, family = binomial(link = "log"), data = staffdbs_adjusted)
income_rr_adjusted
ci=exp(confint.default(income_rr_adjusted))
rr=exp(coef(income_rr_adjusted))
table_income_rr_adjusted = round(cbind(rr,ci),digits=2)
table_income_rr_adjusted

# Household_member_number - age adjusted
household_member_number_rr_adjusted <- glm(Serostatus ~ p1_household_member_number + p1_age, family = binomial(link = "log"), data = staffdbs_adjusted)
household_member_number_rr_adjusted
ci=exp(confint.default(household_member_number_rr_adjusted))
rr=exp(coef(household_member_number_rr_adjusted))
table_household_member_number_rr_adjusted = round(cbind(rr,ci),digits=2)
table_household_member_number_rr_adjusted

# Housing - age adjusted
housing_rr_adjusted <- glm(Serostatus ~ p1_housing + p1_age, family = binomial(link = "log"), data = staffdbs_adjusted)
housing_rr_adjusted
ci=exp(confint.default(housing_rr_adjusted))
rr=exp(coef(housing_rr_adjusted))
table_housing_rr_adjusted = round(cbind(rr,ci),digits=2)
table_housing_rr_adjusted

# Hotspot - age adjusted
hotspot_rr_adjusted <- glm(Serostatus ~ p1_FSAhotspot_binary + p1_age, family = binomial(link = "log"), data = staffdbs_adjusted)
hotspot_rr_adjusted
ci=exp(confint.default(hotspot_rr_adjusted))
rr=exp(coef(hotspot_rr_adjusted))
table_hotspot_rr_adjusted = round(cbind(rr,ci),digits=2)
table_hotspot_rr_adjusted

# DAincome - age adjusted
DAincome_rr_adjusted <- glm(Serostatus ~ p1_DAincome_tertile + p1_age, family = binomial(link = "log"), data = staffdbs_adjusted)
DAincome_rr_adjusted
ci=exp(confint.default(DAincome_rr_adjusted))
rr=exp(coef(DAincome_rr_adjusted))
table_DAincome_rr_adjusted = round(cbind(rr,ci),digits=2)
table_DAincome_rr_adjusted

# DAessential - age adjusted
DAessential_rr_adjusted <- glm(Serostatus ~ p1_DAessential_tertile + p1_age, family = binomial(link = "log"), data = staffdbs_adjusted)
DAessential_rr_adjusted
ci=exp(confint.default(DAessential_rr_adjusted))
rr=exp(coef(DAessential_rr_adjusted))
table_DAessential_rr_adjusted = round(cbind(rr,ci),digits=2)
table_DAessential_rr_adjusted

# Occupation - age adjusted
occupation_rr_adjusted <- glm(Serostatus ~ p1_occupation + p1_age, family = binomial(link = "log"), data = staffdbs_adjusted)
occupation_rr_adjusted
ci=exp(confint.default(occupation_rr_adjusted))
rr=exp(coef(occupation_rr_adjusted))
table_occupation_rr_adjusted = round(cbind(rr,ci),digits=2)
table_occupation_rr_adjusted

# Employment - age adjusted
employment_rr_adjusted <- glm(Serostatus ~ p1_employment_status + p1_age, family = binomial(link = "log"), data = staffdbs_adjusted)
employment_rr_adjusted
ci=exp(confint.default(employment_rr_adjusted))
rr=exp(coef(employment_rr_adjusted))
table_employment_rr_adjusted = round(cbind(rr,ci),digits=2)
table_employment_rr_adjusted

# LTCHRH - age adjusted
LTCHRH_rr_adjusted <- glm(Serostatus ~ p1_LTCHRH + p1_age, family = binomial(link = "log"), data = staffdbs_adjusted)
LTCHRH_rr_adjusted
ci=exp(confint.default(LTCHRH_rr_adjusted))
rr=exp(coef(LTCHRH_rr_adjusted))
table_LTCHRH_rr_adjusted = round(cbind(rr,ci),digits=2)
table_LTCHRH_rr_adjusted

# Transport - age adjusted
transport_rr_adjusted <- glm(Serostatus ~ p1_staff_work_transport + p1_age, family = binomial(link = "log"), data = staffdbs_adjusted)
transport_rr_adjusted
ci=exp(confint.default(transport_rr_adjusted))
rr=exp(coef(transport_rr_adjusted))
table_transport_rr_adjusted = round(cbind(rr,ci),digits=2)
table_transport_rr_adjusted

# PaidSickLeave - age adjusted
PaidSickLeave_rr_adjusted <- glm(Serostatus ~ p1_PaidSickLeave + p1_age, family = binomial(link = "log"), data = staffdbs_adjusted)
PaidSickLeave_rr_adjusted
ci=exp(confint.default(PaidSickLeave_rr_adjusted))
rr=exp(coef(PaidSickLeave_rr_adjusted))
table_PaidSickLeave_rr_adjusted = round(cbind(rr,ci),digits=2)
table_PaidSickLeave_rr_adjusted

####################
# Generate the draft of Table 2
table2_rr_adjusted = rbind(table_period_rr_adjusted, table_gender_rr_adjusted,
                  table_education_rr_adjusted, table_race_rr_adjusted, table_income_rr_adjusted, 
                  table_household_member_number_rr_adjusted, table_housing_rr_adjusted, table_hotspot_rr_adjusted,
                  table_DAincome_rr_adjusted, table_DAessential_rr_adjusted, table_occupation_rr_adjusted,
                  table_employment_rr_adjusted, table_LTCHRH_rr_adjusted, table_transport_rr_adjusted,
                  table_PaidSickLeave_rr_adjusted)
table2_rr_adjusted[(str_detect(row.names(table2_rr_adjusted), "Intercept")),] <- NA
table2_rr_adjusted = table2_rr_adjusted[!(str_detect(row.names(table2_rr_adjusted), "p1_age")),]
rownames(table2_rr_adjusted)=c("Period=1", "Period=2",
                               "Man", "Woman",
                               
                               "Up to high school graduation",
                               "College degree or trades certificate",
                               "University bachelor's degree",
                               "Graduate or professional degree",
                               
                               "White","Black","East or Southeast Asian","Other racialized",
                               "Household income $0 - $59,999", "Household income $60,000 - $89,999", "Household income $90,000 or more",
                               "Household member number 1", "Household member number 2-4", "Household member number 5+",
                               "Apartment/condo","House","Other",
                               "FSA-level non-hotspot", "FSA-level hotspot",
                               "DA-level income tertile 1", "DA-level income tertile 2", "DA-level income tertile 3",
                               "DA-level essential worker tertile 1", "DA-level essential worker tertile 2", "DA-level essential worker tertile 3",
                               
                               "Administration or management",
                               "Personal support worker or support staff (e.g. kitchen, housekeeping, laundry)",
                               "Physician, nurse or registered practical nurses",
                               "Other",
                               
                               "Full-time", "Part-time or agency/contract", "Other",
                               "Long-term care home", "Retirement home",
                               
                               "Drive alone",
                               "Walk or cycle",
                               "Rideshare or carpool (including taxi or uber)",
                               "Public transport",
                               "Work from home, or not currently working",
                               
                               "Have paid sick leave",
                               "No paid sick leave")

table2_rr_adjusted

