# PLS-SEM Model of Self-Pay Influencing Diabetic Treatment and Outcomes
## with Eli Garcia, Ben Hoffner-Brodsky, and Em McGlone

Submission in the Citadel Datathon. Code temporarily dumped without explanation, paper [available](https://github.com/b-hemanth/diabetes-pay-pls-sem/blob/main/pls-sem-paper.pdf).

# DATA NOTES

- Files
	- data <- `diabetic_data_initial.csv`
	- discharge_ids <- `discharge.csv`
	- admission_type_ids <- `admission_type.csv`
	- admission_source_ids <- `admission_type.csv`
- `cmbd_charlson_score` is an aggregate risk score
- the other `cmbd_.` variables are logicals representing comorbidity risk
- encounter_id is currently a string
- the numeric labeled factors (admission source, discharge, etc.) have new additional description columns
