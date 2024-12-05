######################################## Code Summary #######################################################
### TITLE:            Figure_Paper1
### BY:               Siyi Wang
### DESCRIPTION:      Part 1. Draw Figure 1 (Forest plot of PR) for paper 1
### DATASETS USED:    forest_plot_figure
### OUTPUT:           Part 1 (Figures). Figure1_forest_plot.png
####################################### Code Setup ###########################################################
library(ggplot2)
library(reshape2)
library(gridExtra)
library(grid)
library(dplyr)
library(ggforce)
# Get the current R script location as working directory
library(rstudioapi)
##########################################################
########################### PART 1 ##########################
##############################################################
# Set working directory as current R script location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# Draw partial forest table: PR with age adjustment
forest_table<-read.csv("../tables/forest_plot_figure.csv")
nrow(forest_table)

# Rename lables
forest_table$domain = factor(forest_table$domain,levels = c("Age adjusted PR"))
forest_table$group = factor(forest_table$group,levels = c("Period","Socio-demographic characteristics","Household characteristics",
                                                          "Neighbourhood-level characteristics", "Occupational characteristics"))
domain_lab <- c("Age adjusted prevalence ratio")
group_lab <- c("Period","Socio-demographic \n characteristics","Household characteristics",
               "Neighbourhood-level \ncharacteristics", "Occupational characteristics")
names(domain_lab) <- c("Age adjusted PR")
names(group_lab) <- c("Period","Socio-demographic characteristics","Household characteristics",
                      "Neighbourhood-level characteristics", "Occupational characteristics")

# Draw forest plot
forest_plot <- ggplot(forest_table, aes(y= PR, x = reorder(label_level,45:1),color=domain)) +
  geom_point(position = position_dodge(.5)) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2, position = position_dodge(.8)) +
  scale_y_continuous(limits = c(0,3),
                     breaks = c(0, 1, 2, 3),
                     minor_breaks = NULL) +
  geom_hline(yintercept = 1, linetype=2) +
  scale_color_manual(name = "Model",
                     values = '#0072B2')+
  scale_x_discrete(breaks = c("Period (baseline: Before 2022-01-01)",
                              "After vs Before 2022-01-01",
                              "Gender (baseline: Man)",
                              "Woman vs Man",
                              "Education (baseline: Up to high school graduation)",
                              "College degree or trades certificate vs Up to high school graduation",
                              "University bachelor's degree vs Up to high school graduation",
                              "Graduate or professional degree vs Up to high school graduation",
                              "Race (baseline: White)",
                              "Black vs White",
                              "East or Southeast Asian vs White",
                              "Other racialized vs White",
                              "Household income (baseline: $0-$59,999)",
                              "$60,000-$89,999 vs $0-$59,999",
                              "$90,000 or more vs $0-$59,999",
                              "Number of people in household (baseline: 1)",
                              "2-4 vs 1",
                              "5+ vs 1",
                              "Housing type (baseline: Apartment/condo)",
                              "House vs Apartment/condo",
                              "Other vs Apartment/condo",
                              "FSA-level hotspot (baseline: Non-hotspot)",
                              "Hotspot vs Non-hotspot",
                              "DA-level income (baseline: 1-Lowest)",
                              "income 2 vs 1",
                              "income 3 vs 1",
                              "DA-level proportion essential workers (baseline: 1-Lowest)",
                              "proportion essential workers 2 vs 1",
                              "proportion essential workers 3 vs 1",
                              "Occupation (baseline: Administration or management)",
                              "Personal support worker or support staff vs Administration or management",
                              "Physician, nurse or registered practical nurses vs Administration or management",
                              "Other vs Administration or management",
                              "Employment status (baseline: Full-time)",
                              "Part-time or agency/contract vs Full-time",
                              "Other vs Full-time",
                              "Home type (baseline: Long-term care home)",
                              "Retirement home vs Long-term care home",
                              "Transportation to work (baseline: Drive alone)",
                              "Walk or cycle vs Drive alone",
                              "Rideshare or carpool vs Drive alone",
                              "Public transport vs Drive alone",
                              "Work from home, or not currently working vs Drive alone",
                              "Paid sick leave (baseline: Yes)",
                              "No vs Yes"
  ),

  labels = c(expression(bold("Period (baseline: Before 2022-01-01)")),
             "After vs Before 2022-01-01",
             expression(bold("Gender (baseline: Man)")),
             "Woman vs Man",
             expression(bold("Education (baseline: Up to high school graduation)")),
             "College degree or trades certificate vs Up to high school graduation",
             "University bachelor's degree vs Up to high school graduation",
             "Graduate or professional degree vs Up to high school graduation",
             expression(bold("Race (baseline: White)")),
             "Black vs White",
             "East or Southeast Asian vs White",
             "Other racialized vs White",
             expression(bold("Household income (baseline: $0-$59,999)")),
             "$60,000-$89,999 vs $0-$59,999",
             "$90,000 or more vs $0-$59,999",
             expression(bold("Number of people in household (baseline: 1)")),
             "2-4 vs 1",
             "5+ vs 1",
             expression(bold("Housing type (baseline: Apartment/condo)")),
             "House vs Apartment/condo",
             "Other vs Apartment/condo",
             expression(bold("FSA-level hotspot (baseline: Non-hotspot)")),
             "Hotspot vs Non-hotspot",
             expression(bold("DA-level income (baseline: 1-Lowest)")),
             "2 vs 1",
             "3 vs 1",
             expression(bold("DA-level proportion essential workers (baseline: 1-Lowest)")),
             "2 vs 1",
             "3 vs 1",
             expression(bold("Occupation (baseline: Administration or management)")),
             "Personal support worker or support staff vs Administration or management",
             "Physician, nurse or registered practical nurses vs Administration or management",
             "Other vs Administration or management",
             expression(bold("Employment status (baseline: Full-time)")),
             "Part-time or agency/contract vs Full-time",
             "Other vs Full-time",
             expression(bold("Home type (baseline: Long-term care home)")),
             "Retirement home vs Long-term care home",
             expression(bold("Transportation to work (baseline: Drive alone)")),
             "Walk or cycle vs Drive alone",
             "Rideshare or carpool vs Drive alone",
             "Public transport vs Drive alone",
             "Work from home, or not currently working vs Drive alone",
             expression(bold("Paid sick leave (baseline: Yes)")),
             "No vs Yes")
  )+
  coord_flip() +
  labs(title="", x ='', y = "Prevalence ratio and 95% confidence interval") +
  theme_bw()+
  theme(legend.position = "none",
        panel.background = element_rect(fill='white', colour='grey50'),
        axis.title = element_text(hjust=0.5, size=9),
        axis.text.x = element_text( hjust=0.5, size = 9),
        axis.text.y = element_text(size = 9)) +
  facet_grid(group~domain,scales = "free",space = "free",
             labeller = labeller(domain = domain_lab,
                                 group = group_lab))

forest_plot


