library(tidyverse)
data <- read_csv("diabetic_data_initial.csv")
discharge_ids <- read_csv("discharge.csv")
admission_type_ids <- read_csv("admission_type.csv")
admission_source_ids <- read_csv("admission_type.csv")

glimpse(data)

data %>% 
  group_by(readmitted, A1Cresult) %>% 
  summarize(count = n()) %>% 
  ungroup() %>% 
  pivot_wider(names_from = readmitted, values_from = count) %>% 
  ungroup() %>% 
  mutate(total = `<30` + `>30` + `NO`) %>% 
  mutate(`<30` = `<30` / total,
         `>30` = `>30` / total,
         `NO` = `NO` / total)


data <- read_csv("clean_data.csv")

left_join(data, discharge_ids, by = c("discharge_disposition_id"))