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
brake_pheno_data <- read.csv('data/kyle_brake_phenotype.csv', header = TRUE, , na.strings = c('NA', 'n/a'))

# clean nih toolbox data
brake_nihtoolbox <- read.table('data/brake_nih_toolbox_scores.tsv', header = TRUE, sep ='\t', na.strings = c('NA', 'n/a'))

brake_nihtoolbox[['participant_id']] <- as.numeric(sapply(brake_nihtoolbox[['sub']], function(x) substr(x, unlist(gregexpr('-', x))+1, nchar(x))))

# clean up data
brake_pheno_data[['age']] = NA
brake_pheno_data[brake_pheno_data[['ses']] == 'baseline', 'age'] <- month(as.period(interval(brake_pheno_data[brake_pheno_data[['ses']] == 'baseline', 'demo_c_dob'], brake_pheno_data[brake_pheno_data[['ses']] == 'baseline', 'v1_date']), unit = 'month'))/12

brake_pheno_data[['demo_race']] <- ifelse(brake_pheno_data[['demo_race']] == 0, 'American Indian/Alaskan Native', ifelse(brake_pheno_data[['demo_race']] == 1, 'Asian', ifelse(brake_pheno_data[['demo_race']] == 2, 'Black or African American', ifelse(brake_pheno_data[['demo_race']] == 3, 'White', ifelse(brake_pheno_data[['demo_race']] == 4, 'Hawaiian/Pacific Islander', ifelse(brake_pheno_data[['demo_race']] == 5, 'Other/Mixed', brake_pheno_data[['demo_race']]))))))

brake_pheno_data[['demo_ethnicity']] <- ifelse(brake_pheno_data[['demo_ethnicity']] == 0, 'Not HL', ifelse(brake_pheno_data[['demo_ethnicity']] == 1, 'HL', brake_pheno_data[['demo_ethnicity']]))

brake_pheno_data[['demo_c_sex']] <- ifelse(brake_pheno_data[['demo_c_sex']] == 0, 'Male', ifelse(brake_pheno_data[['demo_c_sex']] == 1, 'Female', brake_pheno_data[['demo_c_sex']]))

brake_pheno_data[['demo_income']] <- ifelse(brake_pheno_data[['demo_income']] == 0, '<$50,000', ifelse(brake_pheno_data[['demo_income']] == 1, '<$50,000', ifelse(brake_pheno_data[['demo_income']] == 2, '<$50,000', ifelse(brake_pheno_data[['demo_income']] == 3, '$51,000 - $100,000', ifelse(brake_pheno_data[['demo_income']] == 4, '$51,000 - $100,000', ifelse(brake_pheno_data[['demo_income']] == 5, '>$100,000', brake_pheno_data[['demo_income']]))))))

brake_pheno_data[['c_weightstatus']] <- ifelse(brake_pheno_data[['c_weightstatus']] == 0, 'Healthy Weight', ifelse(brake_pheno_data[['c_weightstatus']] == 1, 'Healthy Weight', ifelse(brake_pheno_data[['c_weightstatus']] == 2, 'Overweight', ifelse(brake_pheno_data[['c_weightstatus']] == 3, 'Obese', brake_pheno_data[['c_weightstatus']]))))

brake_pheno_data[['p_weightstatus']] <- ifelse(brake_pheno_data[['p_weightstatus']] == 0, 'Healthy Weight', ifelse(brake_pheno_data[['p_weightstatus']] == 1, 'Healthy Weight', ifelse(brake_pheno_data[['p_weightstatus']] == 2, 'Overweight', ifelse(brake_pheno_data[['p_weightstatus']] == 3, 'Obese', brake_pheno_data[['p_weightstatus']]))))

brake_pheno_data[['c_owob']] <- ifelse(brake_pheno_data[['c_weightstatus']] == 'Overweight'| brake_pheno_data[['c_weightstatus']] == 'Obese', 'Overweight/Obesity', 'Helathy Weight')
brake_pheno_data[['c_owob']] <- as.factor(brake_pheno_data[['c_owob']])

brake_pheno_data[['bmi_z']] <- round(childsds::sds(value = brake_pheno_data[["c_bmi"]], age = brake_pheno_data[["age"]], sex = brake_pheno_data[['demo_c_sex']], item = "bmi", ref = childsds::cdc.ref, type = "SDS", male = 'Male', female = 'Female'), digits = 2)

# merge with nihtoolbox
brake_data <- merge(brake_pheno_data, brake_nihtoolbox[, !grepl('ses', names(brake_nihtoolbox))], by = 'participant_id', all = TRUE)

brake_data[brake_data[['date_finished']] == '6/16/23 18:07', 'date_finished'] <- '2023-06-16'
brake_data[['date_finished']] <- as.Date(brake_data[['date_finished']])

brake_data[brake_data[['ses']] == 'followup', 'age'] <- month(as.period(interval(brake_data[brake_data[['ses']] == 'followup', 'demo_c_dob'], brake_data[brake_data[['ses']] == 'followup', 'date_finished']), unit = 'month'))/12

brake_data[['study']] <- 'brake'

## REACH ####
reach_data <- read.csv('data/reach_compiled.csv', header = TRUE, na.strings = c('NA', 'n/a'))

## merge ####

# Reach 42 Brake 6 - keep B 6 (no followup for R 42)
# Reach 61 Brake 1 - keep B 1 (used BRAKE for Reach)
# Reach 31 Brake 11 - no NIH for reach keep B 11
# Reach 36 Brake 13 - keep R 36 (used Reach for BRAKE)
# Reach 27 Brake 16 - keep B 16 (no followup for R 27)
# Reach 29 Brake 18 - keep R 29 (used Reach for BRAKE)
# Reach 86 Brake 26 - no NIH for reach keep B 26
# Reach 13 Brake 32 - keep R 13 (first)
# Reach 18 Brake 40 - keep R 18 (first)
# Reach 21 Brake 47 - keep R 21 (used Reach for BRAKE)
# Reach 95 Brake 49 - no NIH for reach keep B 49
# Reach 84 Brake 60 - keep B 60 (no followup for R 84)
# Reach 32 Brake 55 - keep R 32 (first)
# Reach 16 Brake 61 - keep B 61 (no followup for R 16)
# Reach 55 Brake 58 - keep R 55 (first)
# Reach 23 Brake 62 - keep R 23 (first)
# Reach 93 Brake 65 - no NIH for reach keep B 65
# Reach 28 Brake 43 - keep B 43 (used BRAKE for Reach)
# Reach 7 Brake 75 - keep B 75 (no HW data followup data for Reach)
# Reach 8 Brake 76 - keep R 8 (first)

#brake prep
brake_subset_data <- brake_data[c('participant_id', 'ses', 'demo_c_sex', 'demo_ethnicity', 'demo_race', 'age', 'c_height_avg_cm', 'c_weight_avg_kg', 'c_bmi', 'bmi_z', 'c_bmi_pcent', 'demo_mod_ed', 'demo_income', 'tanner_choice', 'pds_score', 'pds_tanner_cat', 'cfq_resp', 'cfq_pcw', 'cfq_ppw', 'cfq_cwc', 'cfq_rest', 'cfq_pressure', 'cfq_mon', 'test', 'computed_score', 'uncorrected_ss', 'age_corrected_ss', 'national_percentile_age_adjusted', 'fully_corrected_t_score', 'study')]

names(brake_subset_data) <- c('participant_id', 'session', 'sex', 'ethnicity', 'race', 'age', 'c_height_avg_cm', 'c_weight_avg_kg', 'c_bmi', 'bmi_z', 'c_bmi_pcent', 'mom_ed', 'income', 'tanner_choice', 'pds_score', 'pds_tanner_cat', 'cfq_resp', 'cfq_pcw', 'cfq_ppw', 'cfq_cwc', 'cfq_rest', 'cfq_pressure', 'cfq_mon', 'test', 'computed_score', 'uncorrected_ss', 'age_corrected_ss', 'national_percentile_age_adjusted', 'fully_corrected_t_score', 'study')

brake_subset_data[['sex']] <- tolower(brake_subset_data[['sex']])

brake_subset_data <- brake_subset_data[!(brake_subset_data[['participant_id']] %in% c(13, 18, 32, 40, 47, 55, 58, 62, 76)), ]

# reach prep
reach_subset_data <- reach_data[c('participant_id', 'session_id',  'sex', 'ethnicity', 'race', 'child_age', 'child_height_average', 'child_weight_average', 'child_bmi', 'child_bmi_z', 'child_bmi_p', 'demo_education_mom', 'demo_income', 'tanner_choice', 'pds_score', 'pds_tanner_cat' , 'cfq_resp', 'cfq_pcw', 'cfq_ppw', 'cfq_cwc', 'cfq_rest', 'cfq_pressure', 'cfq_mon', 'test', 'computed_score', 'uncorrected_ss', 'age_corrected_ss', 'national_percentile_age_adjusted', 'fully_corrected_t_score', 'study')]

names(reach_subset_data) <- c('participant_id', 'session', 'sex', 'ethnicity', 'race', 'age', 'c_height_avg_cm', 'c_weight_avg_kg', 'c_bmi', 'bmi_z', 'c_bmi_pcent', 'mom_ed', 'income', 'tanner_choice', 'pds_score', 'pds_tanner_cat', 'cfq_resp', 'cfq_pcw', 'cfq_ppw', 'cfq_cwc', 'cfq_rest', 'cfq_pressure', 'cfq_mon', 'test', 'computed_score', 'uncorrected_ss', 'age_corrected_ss', 'national_percentile_age_adjusted', 'fully_corrected_t_score', 'study')

reach_subset_data[['participant_id']] <- as.numeric(sapply(reach_subset_data[['participant_id']], function(x) substr(x, unlist(gregexpr('-', x))+1, nchar(x))))

reach_subset_data[['race']] <- ifelse(reach_subset_data[['race']] == 'Other', 'Other/Mixed', ifelse(is.na(reach_subset_data[['race']]), NA, reach_subset_data[['race']]))

reach_subset_data[['session']] <- ifelse(reach_subset_data[['session']] == 'ses-1', 'baseline', 'followup')

#remove REACH participants
reach_subset_data <- reach_subset_data[!(reach_subset_data[['participant_id']] %in% c(7, 16, 27, 28, 31, 42, 61, 84, 86, 93, 95)), ]

# combine
ssib_data <- rbind.data.frame(brake_subset_data, reach_subset_data)

ssib_flanker <- ssib_data[ssib_data[['test']] == 'flanker', ]
ssib_flanker <- ssib_flanker[!is.na(ssib_flanker[['participant_id']]), ]

ssib_listsort <- ssib_data[ssib_data[['test']] == 'listsort', ]
ssib_listsort <- ssib_listsort[!is.na(ssib_listsort[['participant_id']]), ]

ssib_dccs <- ssib_data[ssib_data[['test']] == 'dccs', ]
ssib_dccs <- ssib_dccs[!is.na(ssib_dccs[['participant_id']]), ]

