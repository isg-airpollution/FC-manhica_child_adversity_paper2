
# Childhood Adversity Analysis

## Getting started

### Data

> **NOTE**: The data is not tracked by git. If cloning the repository
> from GitHub, create the appropriate data folders after cloning.

Raw data is stored in a folder `../data/raw/` in the pre-root project folder. (to make sure all of us is using the same base data)
Processed data is stored in `outputs/data/`.

### Analysis

This project uses the {targets} R package to manage the analysis
workflow. You can see the entire workflow in the file
[`_targets.R`](_targets.R).

Here's some information of this package:

**Purpose**: The "targets" package helps you define and execute a workflow where each step of the analysis depends on other steps. It's particularly useful when dealing with projects that have a lot of interdependencies between data processing, modeling, and visualization steps.

**Dependency Management**: The package helps you manage dependencies by automatically tracking which steps need to be re-run when input data or code changes. This can save time and ensure that only necessary computations are repeated.

**Automated Pipelines**: With "targets," you can define a directed acyclic graph (DAG) that represents the workflow. Each node in the graph represents a step or target in the analysis, such as loading data, cleaning data, training models, and generating reports.

**Incremental Builds**: The package supports incremental builds, which means that only the steps that are affected by changes need to be re-run. This can significantly speed up the iterative development process.

**Parallel Execution**: "targets" supports parallel and distributed computing, allowing you to leverage multiple cores or even a cluster to speed up the execution of your workflow.

**Data-Driven Workflow**: The "targets" package encourages a data-driven approach to workflow design. Instead of manually specifying the order of execution, you describe the dependencies between targets, and the package figures out the optimal order.

**Monitoring and Reporting**: The package provides tools for monitoring the progress of your workflow, visualizing the DAG, and generating reports.

**Reproducibility**: By tracking dependencies and changes, the "targets" package enhances the reproducibility of your analyses.

### Code

The functions used in `_targets.R` are defined in the [`R/`](R) folder.

### othe way to read the workflow

see inside [`scratch/`](scratch) that is the first development of the analysis, but the final pipeline is in the target files.

The order of the code:

- 1) Children_df_generation_v2.R
- 2) Children_df_transformation_v2.R
- 3) Children_df_ipw_generation.R
- 4) Children_modelling_v6.R

or follow the `_targets.R` file.

## How to undestand the enviroment

In this section you will find a keyword for undestand and edit the current proyect.

first of all, you will understand `_targets.R` structure and typologi of the objects.

In the first line you will find a load of all functions. then The target plan is where we define all the objects and steps we use to manage this data.

Here you will find three types of objects:

- tar_target can save all objects on R, similar to saving as Rda.
- tar_parquet can save tables using the library arrow, which optimizes and improves the load time. 
- tar_file - for recording an object into a path.

Then whe will check an object and explain the logic to make sense of this: 

- **start_date**: *tar_target* defined as 2016-01-01
- **end_date**: *tar_target* defined as 2021-01-01
- **survery_visits_file**: *tar_target* defined as characte path
- **survery_visits_raw**: *tar_parquet* reading a table with populacao information
- **res_history_file**: *tar_target* defined as character path
- **res_history_raw**: *tar_parquet*  loading a table countaining the intervals that individuals were in the system
- **fu_survey_data**: *tar_parquet* is a process if survey_visits_raw, filtering the date of birth using start_date and end_date, and a creation of new file with the union of the perm_id and date of birth fields.
- **min_dob_fu**: *tar_parquet* a process of survey data where we are extracting the min date of birth per indivudual with multiple register of this information.
- **fu_survey_data_1**: *tar_parquet* we made to min_dob_fu a left join of fu_survey_data for remain just one register per individual and add the information of gender and mother id. it is using the field created on fu_survey_data to made the join.
- **fu_survey_data_final**: *tar_parquet* is a process of fu_survey_data_1 we extrat the information of the individual, id and child dob from key_value. And in this step is taking out the id presents in mother_perm_id, that imply mothers are at the same time childs. 
- **all_mother**: *tar_target* is a vector with all mother id available on fu_survey_data_final
- **min_dob_mother**, **mother_information**, **all_mother_data**: *tar_parquet* we replicate the same proces to get just one register per individual, but in this case for the mother information. In this proces we are getting the mother education and the date of birth using the function of getMostCommonSurveyResponse.
- **final_base_data**: *tar_parquet* is the join of fu_survey_data_final and all_mother_data in a data.frame remaining the complete cases and filtering mother date of birth between 1971-01-01 and 2001-01-01.
- **all_id_with_start**: *tar_target* is a list of all individual id with the base data complete.
- **end_data_fu**, **start_data_fu**: *tar_parquet* that step is using res_history_raw to get the first event and the last event created on the system.
- **fu_data_base**: *tar_parquet* is the join of final_base_data with the information of end and start event on the FU period. , here we filter all the entry type as birth and we create the field of end_date_fu that is a conditional to put the last date we have an event or the end_date defined above. And we create a field to define if the last event is the death.
- **all_id_with_start_2**: *tar_target* is a list of all id present in follow up data after the last filters. that type of steps is for get a reduced data and reduce the time of process.
- **hh_id_and_perm_id1**, **hh_id_and_perm_id2**, **hh_id_an_perm_id**: *tar_parquet* in this section we get the information of al household that the individual been in the follow up period, and asociate it with the id of every child.
- **hh_id_and_perm_id_hh_info**: *tar_file* in this step we save the above information on csv file.
- **houses_file**, **houses_raw**, **houses_clean**: *tar_target*, *tar_parquet* x2, we load the information of household features. In this part we are loading the field to study - ilumination source.
- **hh_base**: *tar_parquet* is the filtering of all houses that are present in the follow up data that we defined above.
- **hh_most_common_final**: *tar_parquet* is the next step to get the most common survey response of ilumination source. and in this step we are segregating the sources in two types: clean an pollutants. (maybe the accuracy is improving if we made this separatation before of the extraction of most common response.)
- **fu_data**: *tar_parquet* in this step we join the information of ilumination fuel to the follow up data.
- **all_fu_id**: *tar_target* is a list of perm id in this point
- **diag**: tar_parquet is a table with the definiton of the diagnosis and codes.
- **h_visits_file**, **h_visits_raw**, **visits_clean_pre**: reading and getting a causes of hospital events, in this case visits.
- **code_respiratory_extract**: in this section we get all de codes thats mean respiratory causes, generated in this development.
- **code_respiratory_extract_all_respiratory**:  the same as the before step, but using a script used in older version of development. 
- **h_visit_clean**: in this step we create add to h_visits_clean_pre the fields respiratory_v2, respiratory_v3, respiratory_all_respiratory.  the first is created by the with the code created in these development, and getting (diag1 > diag2 > diag3 > diag4) this order. for v3 we made the reverse order. and finally for all_respiratory causes we get de code create in old studys and use the v2 order. 
- **h_visits_fu**: tar_parquet in this case we made a filter for all the childs are present in the period of follow up and we generate a new field that count every visit to the hospitan without taking in account the causes. 
- **h_weigth_all_pre**, **h_weight_all**: in this section we are getting information of every child that go to the hospital and have a mesurement of his weight, we get the engine from WHO and estandarise these weight in base of his years in days and gender. Then we are creating a group by every individual an geting the mean of this standarization,  and generating these file for future analysis.
- **h_visits_total_fu**: tar_parquet in this step we are getting for every individual the number of total vistis they have in the follow up period.
- **df_child_month_year_visits**, **fu_data_h_month_year_visits_write**: the same the before step, but segregating by year, month and children. and write.
- **data_fu_visits**, **fu_data_h_total_visits_write**: the join of follow up data and number of visits in all the follow up period. and write.
- **data_old_version**, **location**: tar_parquet loading requiered information of the previous development -- no current use -- , and location extra information.
- **econ_assets_file**, **econ_assets_raw**, **econ_assets_clean**, **names_econ_assets_clean**: loading and cleaning household economic assets panel data. 
- **hh_id_and_perm_id_fu_v**: loading all houses are present for every individual (we can use the target already created)
- **data_fu_complete_pre**: in that point we join health distance calculated with for every individual and the information of region name to the new follow up period.
- **data_fu_complete**: in this step we are generation a inputation of the missing values for the healt distance that we are generating in the above point.
- **data_fu_final**: at this step we are giving the correct formats to the dates and number of days of follow up, mother age at birth of child, year of birth,  healt distance (we are scaling the original variable), grouping the mother education and finally filtering that the child have a end date of follow up before as date of birth. if you want more information go to de `R/fc-process-data.R`
- **pre_mca_econ_assets**: we are selecting the variable we will use to generate the dimension reduction and defining the correct format for all of them.
- **pre_mca_houses_clean**: the same as the before step but for the houses features.
- **mca_1**, **mca_2**: in this point we are geting the most common responses for every field.
- **final_mca**: join the information of MCA 1 and MCA 2, by perm id.
- **id_per_row_mca**: the list of the id on mca data for map the coordinate we will obtain.
- **res_mca**: applying a mjca function to reduce the dimension of this data.
- **SES_pre**: getting and maping te coordinates for every individual.
- **SES_variable**: creating a categorical variable for these cordinate, in this step we use the analysis `scratch/mca_analysis/` to map which cordinates are referenced to high class or medium or low.
- **data_fu_complet**, **write_complet_prefinal**: we join the information relative to socio economic variable to the data_fu_final.
- **data_fu_final_pre_ipw**, **model_ipw_1..4**, **data_final_fu_ipw**, **write_data_final_ipw**: in this step we are generating the inverse probability weighting field, for this we are creating a map field to apply a logistic regrsion to the ilumination source, and as important thing in this point we are filtering the outlaier presents in the number of visits (we remain the 97.5% of the distribution). we use the same covariables as the maining models, but changing the output in this case ilumination source, and as we see the best choice to use as weight are the results obtained with the most complete model. and then we write the model.
- **data_final_fu_ipw_and_weight**, **write_data_final_fu_ipw_and_weight**: the same data as above target but including the mean of standarised weight. 






**IMPORTANT**: `scratch\children_modelling_v6.R` in this script you will find the report generation, we are working to made it more easy to deploy for any user, to generate his own analisis and made his own modification. 



