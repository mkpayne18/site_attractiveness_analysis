## Contains scripts used to tailor data, develop model and perform various supplemental analyses with the overall goal of developing a model to predict stream attractiveness to stray hatchery-origin chum salmon

### Molly Payne
### Main analysis scripts

____

**01_stray_data_load_and_clean.R** Load and tailor stray data (Master_dataset.csv),   which contains the number of strays and surveys by stream and year as well as the covariate data <br>

**02_EDA.R** Exploratory data analysis for response variable and possible covariates to predict the number of hatchery-origin chum salmon straying into streams <br>

**03_fit_NB_model.R** Fit GLMM with negative binomial error distribution to predict numbers of strays in streams. Perform preliminary diagnostics for global model, then proceed to choose final model based on AICc values <br>

**04_model_diagnostics.R** Complete diagnostics for best candidate model fit in 03_fit_NB_model.R <br>

**05_figures.R** Create figures for manuscript and thesis using results from scripts 01-04 (primarily 03_fit_NB_model.R) <br>

**06_tables.R** Create tables for manuscript and thesis using results from scripts 01-04 (primarily 03_fit_NB_model.R) <br>

**07_supplemental_materials.R** Create figures and tables for supplemental sections of manuscript and thesis


### Other scripts

____

**CV_flow_side_analysis.R** In thinking about what mechanism might explain the significant U-shaped quadratic relationship of CV_flow with my model response variable (average number of strays), I checked to see what other watershed characteristics might be associated with the streams that had the attractive CV_flow values in this script. The conclusion was that [attractive] high and low CVs of flow were the streams that were fed by snowmelt, hence the CV_flow effect might be a proxy for snowmelt water sources being attractive. I create a figure in 05_figures.R to show this.   <br>

**response_var_update.R** This script deals with potential bias in my response variable, whereby fewer total strays are detected in streams where the proportion of total dead that got sampled was not 100% In this script, I show that there is indeed bias and I adjust for it by inflating the number of strays detected in a survey by the inverse of the proportion sampled (so that streams where hatchery fish might have been missed bc not all fish were sampled were 'inflated' to have more strays). The main output of the script was an object containing the new model response variable to be used in the model in scripts 01-03.





