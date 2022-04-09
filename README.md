# diabeats

# DATA NOTES
- `cmbd_charlson_score` is an aggregate risk score
- the other `cmbd_.` variables are logicals representing comorbidity risk
- encounter_id is currently a string
- the numeric labeled factors (admission source, discharge, etc.) have new additional description columns

- coerce age to numeric : dia.data$age <- abs(as.numeric(substr(dia.data$age,4, str_length(dia.data$age)-2)))*10 -5
