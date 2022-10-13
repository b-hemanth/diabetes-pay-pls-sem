library(tidyverse)
library(icd)
library(touch)


# x <- readr::read_csv("diabetic_data_initial.csv")

x <- read_csv("Diabetes_Data_1999_2008.csv") %>% janitor::clean_names()
ad_source <- read_csv("Admission_Source_Mapping.csv")
discharge <- read_csv("Discharge_Disposition_Mapping.csv")
ad_type <- read_csv("Admission_Type_Mapping.csv")
## NA 
x[x == "?"] <- NA
# Explain ICD Codes
x$diag_1_label <- dplyr::coalesce(explain_table(x$diag_1)$long_desc)
x$diag_2_label <- dplyr::coalesce(explain_table(x$diag_2)$long_desc)
x$diag_3_label <- dplyr::coalesce(explain_table(x$diag_3)$long_desc)

# merges
x <- x %>% 
  left_join(ad_source, by = "admission_source_id") %>% 
  left_join(ad_type, by = "admission_type_id") %>% 
  left_join(discharge, by = "discharge_disposition_id")

# comorbid
cmbd <- x %>% select(encounter_id, patient_nbr, diag_1, diag_2, diag_3)

cmbd1 <- charlson(cmbd, return_df = T) %>%
  as_tibble() %>% 
  rename("cmbd_charlson_score" = "Charlson")

cmbd2 <- comorbid_charlson(cmbd) %>% 
  as_tibble(rownames = "encounter_id") %>% 
  janitor::clean_names()
colnames(cmbd2) <- paste("cmbd", colnames(cmbd2), sep = "_")
cmbd2 <- rename(cmbd2, "encounter_id" = "cmbd_encounter_id")
cmbd <- left_join(cmbd1, cmbd2, by = "encounter_id")
rm(cmbd1, cmbd2)

write_csv(cmbd, "comorbidities.csv")
x$encounter_id <- as.character(x$encounter_id)
x <- left_join(x, cmbd, by = "encounter_id")


dead <- x %>% 
  filter(discharge_disposition_description == "Expired")


x <- readr::read_csv("clean_data.csv")
x$age <- abs(as.numeric(substr(x$age,4, str_length(x$age)-2)))*10 -5
x$expiry <- ifelse(x$discharge_disposition_description == "Expired", 1, 0)
# MODEL

library(randomForest)
library(caTools)

subset <- x %>%
  select(
    encounter_id,
    race,
    gender,
    age,
    expiry,
    number_inpatient,
    number_outpatient,
    number_emergency,
    number_diagnoses,
    num_procedures,
    time_in_hospital,
    starts_with("cmbd")
  )

subset$race <- as.factor(subset$race)
subset$gender <- as.factor(subset$gender)
subset$age <- as.factor(subset$age)
subset$payer_code <- as.factor(subset$payer_code)

subset <- subset %>% filter(!is.na(race), !is.na(expiry))
# drop race nas cuts expired from 1642 to 1608

fit <- lm(expiry ~ race + gender + age + (number_inpatient + number_outpatient + number_emergency) + (number_inpatient + number_outpatient + number_emergency)*number_diagnoses + number_diagnoses + num_procedures + time_in_hospital + cmbd_charlson_score*number_diagnoses + cmbd_charlson_score + cmbd_charlson_score*(number_inpatient + number_outpatient + number_emergency) + cmbd_mi + cmbd_chf + cmbd_pvd + cmbd_stroke + cmbd_dementia + cmbd_pulmonary + cmbd_rheumatic + cmbd_pud + cmbd_liver_mild + cmbd_dm + cmbd_d_mcx + cmbd_paralysis + cmbd_renal + cmbd_cancer + cmbd_liver_severe + cmbd_mets + cmbd_hiv, data = subset)
fit2 <- lm(number_inpatient + number_outpatient + number_emergency ~ age + gender + race + cmbd_charlson_score + cmbd_mi + cmbd_chf + cmbd_pvd + cmbd_stroke + cmbd_dementia + cmbd_pulmonary + cmbd_rheumatic + cmbd_pud + cmbd_liver_mild + cmbd_dm + cmbd_d_mcx + cmbd_paralysis + cmbd_renal + cmbd_cancer + cmbd_liver_severe + cmbd_mets + cmbd_hiv, data = subset)
