# This script was written by Alaina Pearce in March 2025
# to process  data for SSIB 2025
#
#     Copyright (C) 2025 Alaina L Pearce
#
#     This program is free software: you can redistribute it and/or modify
#     it under the terms of the GNU General Public License as published by
#     the Free Software Foundation, either version 3 of the License, or
#     (at your option) any later version.
#
#     This program is distributed in the hope that it will be useful,
#     but WITHOUT ANY WARRANTY; without even the implied warranty of
#     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#     GNU General Public License for more details.
#
#     You should have received a copy of the GNU General Public License
#     along with this program.  If not, see <https://www.gnu.org/licenses/>.

# load libraries
library(childsds)
library(lubridate)

## BRAKE ####
# load data
brake_pheno_data <- read.csv('data/brake_phenotype.csv', header = TRUE)

# clean nih toolbox data
brake_nihtoolbox <- read.table('data/brake_nih_toolbox_scores.tsv', header = TRUE, sep ='\t')

brake_nihtoolbox[['participant_id']] <- as.numeric(sapply(brake_nihtoolbox[['sub']], function(x) substr(x, unlist(gregexpr('-', x))+1, nchar(x))))

# clean up data
brake_pheno_data$age <- month(as.period(interval(brake_pheno_data$demo_c_dob, brake_pheno_data$v1_date), unit = 'month'))/12

brake_pheno_data$demo_race <- ifelse(brake_pheno_data$demo_race == 0, 'American Indian/Alaskan Native', ifelse(brake_pheno_data$demo_race == 1, 'Asian', ifelse(brake_pheno_data$demo_race == 2, 'Black or African American', ifelse(brake_pheno_data$demo_race == 3, 'White', ifelse(brake_pheno_data$demo_race == 4, 'Hawaiian/Pacific Islander', ifelse(brake_pheno_data$demo_race == 5, 'Other/Mixed', brake_pheno_data$demo_race))))))

brake_pheno_data$demo_ethnicity <- ifelse(brake_pheno_data$demo_ethnicity == 0, 'Not HL', ifelse(brake_pheno_data$demo_ethnicity == 1, 'HL', brake_pheno_data$demo_ethnicity))

brake_pheno_data$demo_c_sex <- ifelse(brake_pheno_data$demo_c_sex == 0, 'Male', ifelse(brake_pheno_data$demo_c_sex == 1, 'Female', brake_pheno_data$demo_c_sex))

brake_pheno_data$demo_income <- ifelse(brake_pheno_data$demo_income == 0, '<$50,000', ifelse(brake_pheno_data$demo_income == 1, '<$50,000', ifelse(brake_pheno_data$demo_income == 2, '<$50,000', ifelse(brake_pheno_data$demo_income == 3, '$51,000 - $100,000', ifelse(brake_pheno_data$demo_income == 4, '$51,000 - $100,000', ifelse(brake_pheno_data$demo_income == 5, '>$100,000', brake_pheno_data$demo_income))))))

brake_pheno_data$c_weightstatus <- ifelse(brake_pheno_data$c_weightstatus == 0, 'Healthy Weight', ifelse(brake_pheno_data$c_weightstatus == 1, 'Healthy Weight', ifelse(brake_pheno_data$c_weightstatus == 2, 'Overweight', ifelse(brake_pheno_data$c_weightstatus == 3, 'Obese', brake_pheno_data$c_weightstatus))))

brake_pheno_data$p_weightstatus <- ifelse(brake_pheno_data$p_weightstatus == 0, 'Healthy Weight', ifelse(brake_pheno_data$p_weightstatus == 1, 'Healthy Weight', ifelse(brake_pheno_data$p_weightstatus == 2, 'Overweight', ifelse(brake_pheno_data$p_weightstatus == 3, 'Obese', brake_pheno_data$p_weightstatus))))

brake_pheno_data$c_owob <- ifelse(brake_pheno_data$c_weightstatus == 'Overweight'| brake_pheno_data$c_weightstatus == 'Obese', 'Overweight/Obesity', 'Helathy Weight')
brake_pheno_data$c_owob <- as.factor(brake_pheno_data$c_owob)

brake_pheno_data$bmi_z <- round(childsds::sds(value = brake_pheno_data[["c_bmi"]], age = brake_pheno_data[["age"]], sex = brake_pheno_data[['demo_c_sex']], item = "bmi", ref = childsds::cdc.ref, type = "SDS", male = 'Male', female = 'Female'), digits = 2)

# merge with nihtoolbox
brake_data <- merge(brake_pheno_data, brake_nihtoolbox[, !grepl('ses', names(brake_nihtoolbox))], by = 'participant_id', all = TRUE)
brake_data[['study']] <- 'brake'

## REACH ####
reach_data <- read.csv('data/reach_compiled.csv', header = TRUE)

## merge ####
#brake prep
brake_subset_data <- brake_data[c('participant_id', 'ses', 'demo_c_sex', 'demo_ethnicity', 'demo_race', 'age', 'c_height_avg_cm', 'c_weight_avg_kg', 'c_bmi', 'bmi_z', 'c_bmi_pcent', 'demo_mod_ed', 'demo_income', 'pds_score', 'pds_tanner_cat', 'cfq_resp', 'cfq_pcw', 'cfq_ppw', 'cfq_cwc', 'cfq_rest', 'cfq_pressure', 'cfq_mon', 'test', 'computed_score', 'uncorrected_ss', 'age_corrected_ss', 'national_percentile_age_adjusted', 'fully_corrected_t_score', 'study')]

names(brake_subset_data) <- c('participant_id', 'session', 'sex', 'ethnicity', 'race', 'age', 'c_height_avg_cm', 'c_weight_avg_kg', 'c_bmi', 'bmi_z', 'c_bmi_pcent', 'mom_ed', 'income', 'pds_score', 'pds_tanner_cat', 'cfq_resp', 'cfq_pcw', 'cfq_ppw', 'cfq_cwc', 'cfq_rest', 'cfq_pressure', 'cfq_mon', 'test', 'computed_score', 'uncorrected_ss', 'age_corrected_ss', 'national_percentile_age_adjusted', 'fully_corrected_t_score', 'study')

brake_subset_data[['sex']] <- tolower(brake_subset_data[['sex']])

brake_subset_data <- brake_subset_data[!(brake_subset_data[['participant_id']] %in% c(6, 13, 16, 18, 28, 32, 40, 47, 55, 58, 61, 62, 75, 76)), ]


# reach prep
reach_subset_data <- reach_data[c('participant_id', 'session_id',  'sex', 'ethnicity', 'race', 'child_age', 'child_height_average', 'child_weight_average', 'child_bmi', 'child_bmi_z', 'child_bmi_p', 'demo_education_mom', 'demo_income', 'pds_score', 'pds_tanner_cat' , 'cfq_resp', 'cfq_pcw', 'cfq_ppw', 'cfq_cwc', 'cfq_rest', 'cfq_pressure', 'cfq_mon', 'test', 'computed_score', 'uncorrected_ss', 'age_corrected_ss', 'national_percentile_age_adjusted', 'fully_corrected_t_score', 'study')]

names(reach_subset_data) <- c('participant_id', 'session', 'sex', 'ethnicity', 'race', 'age', 'c_height_avg_cm', 'c_weight_avg_kg', 'c_bmi', 'bmi_z', 'c_bmi_pcent', 'mom_ed', 'income', 'pds_score', 'pds_tanner_cat', 'cfq_resp', 'cfq_pcw', 'cfq_ppw', 'cfq_cwc', 'cfq_rest', 'cfq_pressure', 'cfq_mon', 'test', 'computed_score', 'uncorrected_ss', 'age_corrected_ss', 'national_percentile_age_adjusted', 'fully_corrected_t_score', 'study')

reach_subset_data[['participant_id']] <- as.numeric(sapply(reach_subset_data[['participant_id']], function(x) substr(x, unlist(gregexpr('-', x))+1, nchar(x))))

reach_subset_data[['race']] <- ifelse(reach_subset_data[['race']] == 'Other', 'Other/Mixed', ifelse(is.na(reach_subset_data[['race']]), NA, reach_subset_data[['race']]))

reach_subset_data[['session']] <- ifelse(reach_subset_data[['session']] == 'ses-1', 'baseline', 'followup')

#remove REACH participants
reach_subset_data <- reach_subset_data[!(reach_subset_data[['participant_id']] %in% c(31, 61, 84, 86, 95, 93)), ]

# combine
ssib_data <- rbind.data.frame(brake_subset_data, reach_subset_data)

ssib_flanker <- ssib_data[ssib_data[['test']] == 'flanker', ]
ssib_listsort <- ssib_data[ssib_data[['test']] == 'listsort', ]
ssib_dccs <- ssib_data[ssib_data[['test']] == 'dccs', ]
