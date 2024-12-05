######################################## Code Summary #######################################################
### TITLE:            Table_Paper1
### BY:               Siyi Wang
### DESCRIPTION:      Part 1. Generate Table 1 (cross table)
### DATASETS USED:    Staffdbs_p1.csv
### OUTPUT:           Part 1 (Tables). BivariableTable_DBSStaff_paper1.xlsx
###                                    BivariableTable_DBSStaff_seroprevalence.xlsx
####################################### Code Setup ###########################################################
library(tidyverse)
library(stringr)
library(labelled)
# Generate crosstable for Table 1
library(crosstable)
library(flextable)
# Get the current R script location as working directory
library(rstudioapi)
###########################################
################ PART 1 ####################
###############################################

# Set working directory as current R script location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# Read the dataset 
staffdbs_p1 = read.csv("../data/Staffdbs_p1.csv", header = TRUE)

# Clean missing data
staffdbs_p1[is.na(staffdbs_p1)] <- 'Missing/unknown'

# Recoder the factor levels
staffdbs_crosstable = staffdbs_p1 %>%
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


# Save the column names for the later crosstable function
staffdbs_crosstable_variable_name = c(colnames(staffdbs_crosstable)[2:19])


# Label the variable names in the data set
labels_selection = c('ID',
                     'Time period (before/after 2022-01-01).',
                     'Age',
                     'Age group',
                     
                     'Gender',
                     'Educational level',
                     'Race',
                     'Income (household level)',
                     'Number of people in household',
                     
                     'Housing Type',
                     # 'FSA-level hotspot decile (high to low risk)',
                     'FSA-level hotspot/non-hotspot indicator',
                     # 'DA-level income quintile' ,
                     'DA-level income tertile',
                     # 'DA-level essential worker quintile',
                     'DA-level essential worker tertile',
                     
                     "Occupation (title)",
                     "Employment status",
                     'Home Type (LTCH/RH)',
                     'Transportation to work',
                     "Paid Sick Leave",
                     'Serostatus')
var_label(staffdbs_crosstable) <- labels_selection


# Crosstable with proportion of columns (first part of Table 1: Negative, Positive, Total)
ct1 = crosstable(staffdbs_crosstable, 
                cols = staffdbs_crosstable_variable_name, 
                by = Serostatus,
                total = 'row',
                percent_pattern="{n}", 
                # percent_digits=0, 
                showNA = 'ifany',
                # funs=c(quantile), funs_arg=list(probs=c(0.25,0.75),dig=2)
                # test = TRUE
) %>% as_flextable(header_show_n=1:2)

ct1

# Crosstable with proportion of rows (second part of Table 1: Seroprevalence)
ct2 = crosstable(staffdbs_crosstable, 
                cols = staffdbs_crosstable_variable_name, 
                by = Serostatus,
                total = 'row',
                percent_pattern="{p_row}", 
                percent_digits=0, 
                showNA = 'ifany',
                #test = TRUE
) %>% as_flextable(header_show_n=1:2)

ct2

