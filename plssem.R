library(colortools)
library(tidyverse)
library(plspm)

x <- readr::read_csv("clean_data.csv")

subset <-  x %>%
  select(
    starts_with("cmbd"),
    a1cresult,
    max_glu_serum,
    age,
    gender,
    race,
    payer_code,
    num_medications,
    num_procedures,
    change,
    diabetes_med,
    readmitted,
    expiry,
    -cmbd_liver_severe,
  )

subset$self_pay <- ifelse(subset$payer_code == "SP", 1, 0)
subset$change <- ifelse(subset$change == "Ch", 1, 0)
subset$diabetes_med <- ifelse(subset$diabetes_med == "Yes", 1, 0)
subset$readmitted_within_30 <- ifelse(subset$readmitted == "<30", 1, 0)
subset$readmitted_after_30 <- ifelse(subset$readmitted == ">30", 1, 0)
subset$male <- ifelse(subset$gender == "Male", 1, 0)

subset <-  subset %>%
  select(
    starts_with("cmbd"),
    a1cresult,
    max_glu_serum,
    age,
    male,
    race,
    self_pay,
    num_medications,
    num_procedures,
    change,
    diabetes_med,
    readmitted_within_30,
    readmitted_after_30,
    expiry
  )

# col types
subset[sapply(subset, is.character)] <-
  lapply(subset[sapply(subset, is.character)],
         as.factor)

subset <- subset[complete.cases(subset), ]
copy <- subset
subset <- lapply(subset, as.numeric) %>% as_tibble()


# rows of the path matrix
patient_conditions = c(0, 0, 0, 0, 0)
patient_demog = c(0, 0, 0, 0, 0)
self_pay = c(1, 1, 0, 0, 0)
doc_order_set = c(1, 1, 1, 0, 0)
outcomes = c(0, 0, 0, 1, 0)

# path matrix (inner model)
foot_path = rbind(patient_conditions, patient_demog, self_pay, doc_order_set, outcomes)
# add column names
colnames(foot_path) = rownames(foot_path)
plspm::innerplot(foot_path)

# blocks of indicators (outer model)
foot_blocks = list(
  1:19, 
  20:22,
  23,
  24:27,
  28:30
  )

# vector of modes (reflective)
foot_modes = c("A", "A", "A", "A", "A")

# run plspm analysis
foot_pls = plspm(subset, foot_path, foot_blocks, modes = foot_modes)

# interaction 
# get the latent variable scores in data frame format
Scores = as.data.frame(foot_pls$scores)
# create the interaction term
Scores$Inter = (Scores$patient_conditions + Scores$patient_demog) * Scores$self_pay
# regression analysis
reg = lm(doc_order_set ~ patient_conditions +  patient_demog + self_pay + Inter - 1, data = Scores)

c1 = c(0, 0, 0, 0, 0)
c2 = c(0, 0, 0, 0, 0)
c3 = c(0, 0, 0, 0, 0)
c4 = c(0, 0, 0, 0, 0)
c5 = c(reg$coefficients, 0)
reg_path = rbind(c1, c2, c3, c4, c5)
rownames(reg_path) = c("patient_conditions", "patient_demog", "self_pay", "Inter", "doc_order_set")
colnames(reg_path) = c("patient_conditions", "patient_demog", "self_pay", "Inter", "doc_order_set")

innerplot(reg_path, show.values = TRUE)

# results
foot_pls$path_coefs
foot_pls$inner_model
plot(foot_pls)
plot(foot_pls, what = "loadings", arr.width = 0.1)
