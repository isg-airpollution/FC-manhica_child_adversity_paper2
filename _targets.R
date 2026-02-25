# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(here)
# library(tmap)

source(here::here( "R", "packages.R"))

# library(tarchetypes) # Load other packages as needed. # nolint

# Set target options:
# tar_option_set(
#   packages = c("tibble"), # packages that your targets need to run
#   format = "rds" # default storage format
#   # Set other options as needed.
# )
tar_option_set(memory = "transient", garbage_collection = TRUE)

# tar_make_clustermq() configuration (okay to leave alone):
options(clustermq.scheduler = "multiprocess")

# tar_make_future() configuration (okay to leave alone):
# Install packages {{future}}, {{future.callr}}, and {{future.batchtools}} to allow use_targets() to configure tar_make_future() options.

# Run the R scripts in the R/ folder with your custom functions:
source(here::here("R", "read-data.R"))
source(here::here("R", "clean-data.R"))
source(here::here("R", "process-data.R"))
source(here::here("R", "modelling-data.R"))
source(here::here("R", "write-data.R"))
source(here::here("R", "report-execution.R"))

# Functions
# source("Y:/fcoloma/package_fabi/R/basics_analisis.R")  
# Y:/fcoloma/package_fabi/R/basics_analisis.R
source("/PROJECTES/AIRPOLLUTION/fcoloma/package_fabi/R/basics_analisis.R")
source("/PROJECTES/AIRPOLLUTION/fcoloma/package_fabi/R/hr_ic.R")
source("/PROJECTES/AIRPOLLUTION/fcoloma/package_fabi/R/plot_hr_ic.R")


# source("other_functions.R") # Source other scripts as needed. # nolint

# Replace the target list below with your own:
list(

#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
#    START PARAMETERS
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

  tar_target(vStart_date,
             as.Date("2016-01-01")),

  tar_target(vEnd_date,
             as.Date("2021-01-01")),

#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
#    LOAD MEMBER HISTORY SYSTEM - VERSION 06-2024
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

  ##########
  ##########
  ## Member history - entry and end type
  ##########
  ##########

  # START and END TYPE  data dictionary
  #
  # START: ENU =  internal migration event
  # ENT = EXTERNAL migration
  # BIR = ENTRY by BIRTH

  # END_TYPES
  # EXT = SE mueve de casa
  # DTH = DEATH
  # NA = NO end

# "Y:\MOZAMBIQUE\DATA"
# Y:\MOZAMBIQUE\ANALYSIS\FC-manhica_child_adversity_data_paper2_v2
  tar_target(res_history_file_v2,
             "../../DATA/ORIGINAL/raw_information_062024/permid_member_start_end/permid_member_start_end_20240607.csv"), #nolint


  tar_parquet(res_history_raw_v2,
              readResidentialHistory_v2(res_history_file_v2)),




 # In this part we will made a change and first treat the perm_id that we
 # we will use inside the study but having on account the externalization
 # If the child gets out of the surveillance system for 6 or more months it
 # is considered out migration

 tar_parquet( res_history_treated_v2,
              process_res_history( data_system = res_history_raw_v2,
                                   start = vStart_date,
                                   end = vEnd_date )),

##########
##########
## limit 31-12-2025 - review process
##########
##########
 tar_parquet( res_history_treated_v2_rev_check,
              process_res_history( data_system = res_history_raw_v2,
                                   start = vStart_date,
                                   end = as.Date("2020-01-01") )),

  ##########
  ##########
  ## Member history - getting the the complete period, and summary
  ##########
  ##########
  # in this secction we will obtain a row per participant
  # we will select the last date and the first date,
  # to get the period that we want look on the caracteristics.. 

  ###
  # Present Flags - 
  ###

  # Flag migration: if any of exit is superior to 6 months, 
  # Flag migration_gen: if the sum of the exits is superior to 6 month, 
  # Flag_any_exit: if diff between next_start and before end is different of 0
  # Flag_mov_number: if the child move one or more times
  # Flag_house_change: if the register of 
  #         movement represent a change in house too
  # flag_negative_EXT: is this child has a negative exit. 
  #          That represent  the ntrey before they exit
  # Flag_ext_cero_days: is any of the movents has 0 daysof exit
  # Flag_fu_cero_days: if as any 0 days of FU in any movement
  # flag_negative_fu: if any FU period is stricly negative.. maybe we have more cases
  # than the summary ones ... what if we delete this casesbefor the summarising ??

  tar_parquet(
    res_history_base_v2,
    gettingFUperParticipant(res_history_treated_v2)
  ),

##########
##########
## limit 31-12-2025
##########
##########
  tar_parquet(
    res_history_base_v2_rev_check,
    gettingFUperParticipant(res_history_treated_v2_rev_check)
  ),
  ##########
  ##########
  ## Getting List of id to Delete by errors
  ##########
  ##########
  # In this section we will create a list of id 
  # that we will delete, but not all at the same time
  # Because we will want see the distribution  
  # of this cases to know which type of information we are lossing

  # for this reason we will just 

  tar_target(
    list_of_deletions_ids_from_system_data,
    generation_list_del_sysData(res_history_base_v2)
  ),

  tar_target(
    list_of_deletions_ids_from_system_data_rev_check,
    generation_list_del_sysData(res_history_base_v2_rev_check)
  ),

 # for the cases that we have 0 days on the FU... 
 # we will delete this entry has household 
 # cause is innecessary .. just remaining the cases 
 # we have days on this household


 # now we will create the data by household.. to see which housea have movent
 # or something...is the smae has res hisotry base but geeting all 
 # house and period there is in this house... 

 # we can extract two information here..

 # max house present
 # base line house taking in acount that child has cero days and move
 # a mos common feature but with weight

 ###
 # at this point we need to check how many cases the hild have house changes 
 # adding that childs that has cero days of FU and Ccero day on ext and change in
 # this  cero period 
 ###

  tar_parquet(
    list_houses_participants,
    getting_houseId_partID_w_periods(
      data = res_history_treated_v2
      , remove_list = list_of_deletions_ids_from_system_data
      )
  ),

  tar_parquet(
    list_houses_participants_rev_check,
    getting_houseId_partID_w_periods(
      data = res_history_treated_v2_rev_check
      , remove_list = list_of_deletions_ids_from_system_data_rev_check
      )
  ),


#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
#    DATA BASE which will be filled with multiple 
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

  ##########
  ##########
  ## Member information
  ##########
  ##########

  tar_parquet(
    data_fu_base,
    clean_fu_data_from_scratch(
      data = res_history_base_v2
      , perm_id_vector = unique(list_houses_participants$perm_id)
    )
  ),

##########
##########
## limit 31-12-2025
##########
##########

  tar_parquet(
    data_fu_base_rev_check,
    clean_fu_data_from_scratch(
      data = res_history_base_v2_rev_check
      , perm_id_vector = unique(list_houses_participants$perm_id)
    )
  ),

#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
#    LOAD MEMBER DATA - VERSION: 21-06-2023
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

  ##########
  ##########
  ## Member information
  ##########
  ##########

  tar_target( survey_visits_file_v2,
              "../../DATA/ORIGINAL/raw_data_v2_21062023/Member.csv" ),

  tar_parquet( survey_visits_raw_v2,
              readChildrenInfo_member(survey_visits_file_v2) ),

  ##########
  ##########
  ## Member history information
  ##########
  ##########

  tar_target( member_details_history_file_v2,
              "../../DATA/ORIGINAL/raw_data_v2_21062023/member_details_history.csv" ),

  tar_parquet( member_details_history_raw_v2,
               read_member_detail_v2(member_details_history_file_v2) ),

# maybe we will not use this

#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
#    For this secction i will made extract mother id and father id
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
# We apply a most common process w interval dates 
# and see that the member_details_history_raw_v2 has less
# missing father and mother ids. 
# for this reason this information we will this data as base

## memeber : father unk= 13.472 mother unk = 207
## member hisory =  father unk = 11.668 mother unk = 19

# it is explained because sometime when the child is 
# too young the survey is not done till they are older
##

# some fields they 

# the objetive of this is get dob of the mother and the most 
# possible perm+id for the mother or father

# tar_parquet(
#   mother_basic_info_prev_version, 
#   clean_get_mother_basic_information(
#     data = member_details_history_raw_v2
#     , interval_data = list_houses_participants
#     , data_member = survey_visits_raw_v2
#   )
# ),

#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
#    reading the last sended by teod
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
# after a validate that the almos all the information is the same with the new one
# and the differences are looking a bit better for new one we will continuos the new onw
# we have 67 changes on sex
# we have 106 differents mother perm id

##########
##########
## lets see which is the differences btw mother info by us and by CIMS
##########
##########

tar_target(
  mother_file,
  "../../DATA/ORIGINAL/raw_information_062024/population_mother_perm_id/population_mother_perm_id.csv"
),

tar_parquet(
  mother_w_basic_info_raw,
    read_mother_info(mother_file)
),

###
# In this point we will limit the age from 15 to 48
###
tar_parquet(
  mother_w_basic_info_cleaned,
  clean_mother_info(
    data = mother_w_basic_info_raw,
    perm_id_vector = data_fu_base$perm_id,
    min_mother_age = as.Date("1971-01-01"), # 48 no used
    max_mother_age = as.Date("2001-01-01")  #15 no used - we calculated the age in the data
    )
),

tar_parquet(
  mother_w_basic_info_cleaned_all_childs,
  clean_mother_info(
    data = mother_w_basic_info_raw,
    perm_id_vector = mother_w_basic_info_raw$perm_id,
    min_mother_age = as.Date("1971-01-01"), # 48 no used
    max_mother_age = as.Date("2001-01-01")  #15 no used - we calculated the age in the data
    )
),

###
# In this point we will limit the age from 15 to 48
###
tar_parquet(
  mother_w_basic_info_cleaned_rev_check,
  clean_mother_info(
    data = mother_w_basic_info_raw,
    perm_id_vector = data_fu_base_rev_check$perm_id,
    min_mother_age = as.Date("1971-01-01"), # 48 no used
    max_mother_age = as.Date("2001-01-01")  #15 no used - we calculated the age in the data
    )
),

#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
#    Mother Education, occupation
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

tar_target(
  inv_edu_ocu_file,
  "../../DATA/ORIGINAL/raw_information_062024/individual_edu_ocupation/individual_edu_ocupation.csv"
),

tar_parquet(
  inv_edu_ocu_raw,
    read_inv_edu_ocu_info(inv_edu_ocu_file)
),

# NOTE:: we can delete that cases that not are in of the INTERVAL
# tar_parquet( indv_ocupation_treated_btw, 
#   clean_interval_data_and_deploy(
#     inv_edu_ocu_raw, 
#     list_houses_participants,
#     id_1 = "perm_id",
#     id_2 = "perm_id",
#     field_date = "start"
#     )
#   ),

# NOTE:: we can delete that cases that not are in of the INTERVAL
# recheck _ by mother_ id ########################################################################################
tar_parquet( indv_ocupation_treated_allSys, 
  clean_interval_data_and_deploy(
    data = inv_edu_ocu_raw, 
    interval_data = data_fu_base %>% inner_join(mother_w_basic_info_cleaned, by = "perm_id"),
    id_1 = "perm_id",
    id_2 = "mother_perm_id",
    field_date = "start"
    ) 
  ),
tar_parquet( indv_ocupation_treated_allSys_rev_check, 
  clean_interval_data_and_deploy(
    data = inv_edu_ocu_raw, 
    interval_data = data_fu_base_rev_check %>% inner_join(mother_w_basic_info_cleaned_rev_check, by = "perm_id"),
    id_1 = "perm_id",
    id_2 = "mother_perm_id",
    field_date = "start"
    ) 
  ),

tar_target(
  vInv_edu_ocu_names,
  read_variables_inv_edu_ocu()
),

tar_parquet(
  indv_edu_ocu_btw_fu,
  which_common_features_is_needed(
    data = indv_ocupation_treated_allSys
    , w_sensitivity = c("between_dates")
    , w_common_variables = vInv_edu_ocu_names
    )
),
tar_parquet(
  indv_edu_ocu_btw_3mth_fu,
  which_common_features_is_needed(
    data = indv_ocupation_treated_allSys
    , w_sensitivity = c("between_dates"
                        ,"between_3months_plus")
    , w_common_variables = vInv_edu_ocu_names
    )
),
tar_parquet(
  indv_edu_ocu_btw_6mth_fu,
  which_common_features_is_needed(
    data = indv_ocupation_treated_allSys
    , w_sensitivity = c("between_dates"
                        ,"between_3months_plus"
                        ,"between_6months_plus")
    , w_common_variables = vInv_edu_ocu_names
    )
),
tar_parquet(
  indv_edu_ocu_btw_1yr_fu,
  which_common_features_is_needed(
    data = indv_ocupation_treated_allSys
    , w_sensitivity = c("between_dates"
                        ,"between_3months_plus"
                        ,"between_6months_plus"
                        ,"between_1yr_plus")
    , w_common_variables = vInv_edu_ocu_names
    )
),
tar_parquet(
  indv_edu_ocu_btw_1_5yr_fu,
  which_common_features_is_needed(
    data = indv_ocupation_treated_allSys
    , w_sensitivity = c("between_dates"
                        ,"between_3months_plus"
                        ,"between_6months_plus"
                        ,"between_1yr_plus"
                       ,"between_1_5yr_plus")
    , w_common_variables = vInv_edu_ocu_names
    )
),
tar_parquet(
  indv_edu_ocu_btw_2yr_fu,
  which_common_features_is_needed(
    data = indv_ocupation_treated_allSys
    , w_sensitivity = c("between_dates"
                        ,"between_3months_plus"
                        ,"between_6months_plus"
                        ,"between_1yr_plus"
                        ,"between_1_5yr_plus"
                        ,"between_2yr_plus")
    , w_common_variables = vInv_edu_ocu_names
    )
),

tar_parquet(
  indv_edu_ocu_btw_or_2yr_fu,
  which_common_features_is_needed(
    data = indv_ocupation_treated_allSys
    , w_sensitivity = TRUE
    , w_common_variables = vInv_edu_ocu_names
    )
),
tar_parquet(
  indv_edu_ocu_btw_or_2yr_fu_rev_check,
  which_common_features_is_needed(
    data = indv_ocupation_treated_allSys_rev_check
    , w_sensitivity = TRUE
    , w_common_variables = vInv_edu_ocu_names
    )
),



#################
#################
## CHOICING INDIVIDUAL EDUCATION AND OCUPATION
#################
#################

tar_parquet(
  indv_edu_ocu_choice,
  indv_edu_ocu_btw_or_2yr_fu %>%
    rename(mother_perm_id = perm_id) %>%
    filter(education != "no_information")
),

tar_parquet(
  indv_edu_ocu_choice_rev_check,
  indv_edu_ocu_btw_or_2yr_fu_rev_check %>%
    rename(mother_perm_id = perm_id) %>%
    filter(education != "no_information")
),

    # join this by mother perm id
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
#    FAMILY INESTABILITY
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
# "Y:\MOZAMBIQUE\data\raw_information_062024\Familyinstability\Familyinstability_20240611.csv"

tar_target(
  family_inestability_file,
  "../../DATA/ORIGINAL/raw_information_062024/Familyinstability/Familyinstability_20240611.csv"
),

tar_parquet(
  family_inestability_raw,
    read_family_inestability(family_inestability_file)
),

# NOTE:: we can delete that cases that not are in of the INTERVAL
tar_parquet( family_inestability_treated_allSys, 
  clean_interval_data_and_deploy(
    family_inestability_raw, 
    data_fu_base,
    id_1 = "perm_id",
    id_2 = "perm_id",
    field_date = "start"
    )
  ),

tar_target(
  vFamily_inestability_names,
  read_variables_fam_inestability()
),

tar_parquet(
  family_inestability_btw_fu,
  which_common_features_is_needed(
    data = family_inestability_treated_allSys
    , w_sensitivity = c("between_dates")
    , w_common_variables = vFamily_inestability_names
    )
),
tar_parquet(
  family_inestability_btw_3mth_fu,
  which_common_features_is_needed(
    data = family_inestability_treated_allSys
    , w_sensitivity = c("between_dates"
                        ,"between_3months_plus")
    , w_common_variables = vFamily_inestability_names
    )
),
tar_parquet(
  family_inestability_btw_6mth_fu,
  which_common_features_is_needed(
    data = family_inestability_treated_allSys
    , w_sensitivity = c("between_dates"
                        ,"between_3months_plus"
                        ,"between_6months_plus")
    , w_common_variables = vFamily_inestability_names
    )
),
tar_parquet(
  family_inestability_btw_1yr_fu,
  which_common_features_is_needed(
    data = family_inestability_treated_allSys
    , w_sensitivity = c("between_dates"
                        ,"between_3months_plus"
                        ,"between_6months_plus"
                        ,"between_1yr_plus")
    , w_common_variables = vFamily_inestability_names
    )
),
tar_parquet(
  family_inestability_btw_1_5yr_fu,
  which_common_features_is_needed(
    data = family_inestability_treated_allSys
    , w_sensitivity = c("between_dates"
                        ,"between_3months_plus"
                        ,"between_6months_plus"
                        ,"between_1yr_plus"
                       ,"between_1_5yr_plus")
    , w_common_variables = vFamily_inestability_names
    )
),
tar_parquet(
  family_inestability_btw_2yr_fu,
  which_common_features_is_needed(
    data = family_inestability_treated_allSys
    , w_sensitivity = c("between_dates"
                        ,"between_3months_plus"
                        ,"between_6months_plus"
                        ,"between_1yr_plus"
                        ,"between_1_5yr_plus"
                        ,"between_2yr_plus")
    , w_common_variables = vFamily_inestability_names
    )
),

tar_parquet(
  family_inestability_btw_or_2yr_fu,
  which_common_features_is_needed(
    data = family_inestability_treated_allSys
    , w_sensitivity = TRUE
    , w_common_variables = vFamily_inestability_names
    )
),


#################
#################
## CHOICING FAMILY INESTABILITY
#################
#################

tar_parquet(
  family_inestability_choice,
  family_inestability_btw_or_2yr_fu
),


#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
#   SIBLING DEATH EXTRATTION
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
tar_parquet(
  family_siblings_death_extraction,
  siblings_treatment(
    data_mother = mother_w_basic_info_cleaned,
    data_base = data_fu_base
  )
),
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
#   PARENTAL MARITAL STATUS
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
# Cause to the missnes of information for the father information ... we will 
#get the marital status of the mother and get the parental status field

# NOTE:: we can delete that cases that not are in of the INTERVAL
tar_parquet( parental_marital_status_treated_allSys, 
  clean_interval_data_and_deploy(
    family_inestability_raw, 
    data_fu_base %>% inner_join(mother_w_basic_info_cleaned, by = "perm_id"),
    id_1 = "perm_id",
    id_2 = "mother_perm_id",
    field_date = "start"
    )
  ),

# tar_parquet(
#   parental_marital_status_choice,
#   which_common_features_is_needed(
#     data = parental_marital_status_treated_allSys
#     , w_sensitivity = c("between_dates"
#                         ,"between_3months_plus"
#                         ,"between_6months_plus"
#                         ,"between_1yr_plus"
#                         ,"between_1_5yr_plus"
#                         ,"between_2yr_plus"
#                         )
#     , w_common_variables = c("marital_status")
#     )
# ),
tar_parquet(
  parental_marital_status_choice,
  which_common_features_is_needed(
    data = parental_marital_status_treated_allSys
    , w_sensitivity = TRUE
    , w_common_variables = c("marital_status")
    )
),

#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
#    LOADING GEO REFERENCES - to filter people on manhica
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

## Boundary data ---- bairros

tar_target(
  bairros_geo_file,
"../../DATA/ORIGINAL/gis_carles_base/boundaries/bairros.gpkg"
),

tar_target(
  bairros,
  geoRead_bairros(
    path = bairros_geo_file
  )

),


## Boundary data ---- postos


tar_target(
  postos,
  Grouping_deo_by_postos(bairros)
),

## Boundary data ---- manhica

# TODO: see the diference between postos ans manhica - whats do the summarise??
tar_target(
  manhica,
 {
    postos %>%
      summarise() 
 }
),


#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
#    HOUSE CHARACTERISTICS
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

## Household features panel data

# tar_target(houses_file,  "../../DATA/raw/dss_house_features.csv"),
tar_target(houses_file,  "../../DATA/ORIGINAL/raw_information_062024/household_details/household_details_20240611.csv"),

# data_test <- read_csv(houses_file) existe una variable que se llama accuract y que nos
# da laprecision de la longitud y latitud...

tar_parquet(houses_raw, readHouseData_v3(houses_file)),

# Check cleanHouseData, how is affecting this 
# NOTE:: we can delete that cases that not are in of the INTERVAL
tar_parquet( houses_cleaned, 
  clean_interval_data_and_deploy(
    houses_raw, 
    list_houses_participants,
    id_1 = "house_number",
    id_2 = "household",
    field_date = "start"
    ) %>% 
  inside_manhica(
    .,
    geo = manhica
  )
  ),

tar_parquet( houses_cleaned_rev_check, 
  clean_interval_data_and_deploy(
    houses_raw, 
    list_houses_participants_rev_check,
    id_1 = "house_number",
    id_2 = "household",
    field_date = "start"
    ) %>% 
  inside_manhica(
    .,
    geo = manhica
  )
  ),

##########
##########
## we will group the variables before get most common values
##########
##########

# se puede dar el caso de que hayan un periodo con
# 5 encuestas en las cuales tres sean pollutant, pero 
# como dos iguales han sido clean se ponga clean
# y se pierda la caracteristica pollutant por esta razon

tar_parquet(
  houses_cleaned_group, 
  grouping_variable_by_house_features(
   data =  houses_cleaned
  )
  ),
tar_parquet(
  houses_cleaned_group_rev_check, 
  grouping_variable_by_house_features(
   data =  houses_cleaned_rev_check
  )
  ),

##########
##########
## looking for the missing values we have inside this questionary
##########
##########
# 35340  = has any information with 2 years of margen
# 16 = no information in these
tar_parquet(
  check_number_of_participant_w_info,
  check_info_HH_on_sys_info(houses_cleaned)
),

#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
#    Now is time to create the most common values in different sensityvities
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

tar_target(
  vFeatures_names,
  read_variables_features_houses()
),

tar_target(
  house_features_btw_fu,
  which_common_features_is_needed(
    data = houses_cleaned_group
    , w_sensitivity = c("between_dates")
    , w_common_variables = vFeatures_names
    )
),

tar_target(
  house_features_btw_fu_3mth,
  which_common_features_is_needed(
    data = houses_cleaned_group
    , w_sensitivity = c("between_dates"
                       ,"between_3months_plus")
    , w_common_variables = vFeatures_names
    )
),

tar_target(
  house_features_btw_fu_6mth,
  which_common_features_is_needed(
    data = houses_cleaned_group
    , w_sensitivity = c("between_dates"
                       ,"between_3months_plus"
                       ,"between_6months_plus")
    , w_common_variables = vFeatures_names
    )
),

tar_target(
  house_features_btw_fu_1yr,
  which_common_features_is_needed(
    data = houses_cleaned_group
    , w_sensitivity = c("between_dates"
                       ,"between_3months_plus"
                       ,"between_6months_plus"
                       ,"between_1yr_plus")
    , w_common_variables = vFeatures_names
    )
),

tar_target(
  house_features_btw_fu_1_5yr,
  which_common_features_is_needed(
    data = houses_cleaned_group
    , w_sensitivity = c("between_dates"
                       ,"between_3months_plus"
                       ,"between_6months_plus"
                       ,"between_1yr_plus"
                       ,"between_1_5yr_plus")
    , w_common_variables = vFeatures_names
    )
),

tar_target(
  house_features_btw_fu_2yr,
  which_common_features_is_needed(
    data = houses_cleaned_group
    , w_sensitivity = c("between_dates"
                       ,"between_3months_plus"
                       ,"between_6months_plus"
                       ,"between_1yr_plus"
                       ,"between_1_5yr_plus"
                       ,"between_2yr_plus")
    , w_common_variables = vFeatures_names
    )
),

tar_target(
  house_features_btw_fu_or_2yr,
  which_common_features_is_needed(
    data = houses_cleaned_group
    , w_sensitivity = TRUE
    , w_common_variables = c(vFeatures_names, "ilumination_fuel_impolute_clean")
    , impolute = TRUE
    )
),
tar_target(
  house_features_btw_fu_or_2yr_rev_check,
  which_common_features_is_needed(
    data = houses_cleaned_group_rev_check
    , w_sensitivity = TRUE
    , w_common_variables = c(vFeatures_names, "ilumination_fuel_impolute_clean")
    , impolute = TRUE
    )
),

tar_parquet(
  house_features_btw_x_choice,
  house_features_btw_fu_or_2yr 
),

tar_parquet(
  house_features_btw_x_choice_rev_check,
  house_features_btw_fu_or_2yr_rev_check 
),
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
#    Economic assets preparation
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

## Household economic assets panel data
# "Y:/MOZAMBIQUE/data/raw_information_062024/household_economics/household_economics_20240611.csv"

tar_file(
  econ_assets_file,
  "../../DATA/ORIGINAL/raw_information_062024/household_economics/household_economics_20240611.csv"
  ),

tar_parquet(
  econ_assets_raw, 
  readEconomicData(econ_assets_file) %>% as.data.frame()
  ),

# Check cleanHouseData, how is affecting this 
tar_parquet(
  econ_houses_cleaned, 
  clean_interval_data_and_deploy(
    econ_assets_raw, 
    list_houses_participants,
    id_1 = "house_number",
    id_2 = "household",
    field_date = "start"
    ) %>%
    inside_manhica(
      .,
      geo = manhica
    )
  ),
tar_parquet(
  econ_houses_cleaned_rev_check, 
  clean_interval_data_and_deploy(
    econ_assets_raw, 
    list_houses_participants_rev_check,
    id_1 = "house_number",
    id_2 = "household",
    field_date = "start"
    ) %>%
    inside_manhica(
      .,
      geo = manhica
    )
  ),


#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
#    Now is time to create the most common values in different sensityvities
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

# TODO: econ_assets_create_mca_variables_paper2 
# aplicar esta funcion para la creacion del mca

# FALTA UN TAR de agrupacion pero solo de tratamiento con la funcion anterior


tar_target(
  vEcon_names,
  extract_econ_mca_names()
),

tar_target(
  house_econ_btw_fu,
  which_common_features_is_needed(
    data = econ_houses_cleaned
    , w_sensitivity = c("between_dates")
    , w_common_variables = vEcon_names
    ) 
),

tar_target(
  house_econ_btw_fu_3mth,
  which_common_features_is_needed(
    data = econ_houses_cleaned
    , w_sensitivity = c("between_dates"
                       ,"between_3months_plus")
    , w_common_variables = vEcon_names
    )
),

tar_target(
  house_econ_btw_fu_6mth,
  which_common_features_is_needed(
    data = econ_houses_cleaned
    , w_sensitivity = c("between_dates"
                       ,"between_3months_plus"
                       ,"between_6months_plus")
    , w_common_variables = vEcon_names
    )
),

tar_target(
  house_econ_btw_fu_1yr,
  which_common_features_is_needed(
    data = econ_houses_cleaned
    , w_sensitivity = c("between_dates"
                       ,"between_3months_plus"
                       ,"between_6months_plus"
                       ,"between_1yr_plus")
    , w_common_variables = vEcon_names
    )
),

tar_target(
  house_econ_btw_fu_1_5yr,
  which_common_features_is_needed(
    data = econ_houses_cleaned
    , w_sensitivity = c("between_dates"
                       ,"between_3months_plus"
                       ,"between_6months_plus"
                       ,"between_1yr_plus"
                       ,"between_1_5yr_plus")
    , w_common_variables = vEcon_names
    )
),

tar_target(
  house_econ_btw_fu_2yr,
  which_common_features_is_needed(
    data = econ_houses_cleaned
    , w_sensitivity = c("between_dates"
                       ,"between_3months_plus"
                       ,"between_6months_plus"
                       ,"between_1yr_plus"
                       ,"between_1_5yr_plus"
                       ,"between_2yr_plus")
    , w_common_variables = vEcon_names
    )
),
tar_target(
  house_econ_btw_fu_or_2yr,
  which_common_features_is_needed(
    data = econ_houses_cleaned
    , w_sensitivity = TRUE
    , w_common_variables = vEcon_names
    )
),
tar_target(
  house_econ_btw_fu_or_2yr_rev_check,
  which_common_features_is_needed(
    data = econ_houses_cleaned_rev_check
    , w_sensitivity = TRUE
    , w_common_variables = vEcon_names
    )
),

tar_parquet(
  house_econ_btw_x_choice,
  house_econ_btw_fu_or_2yr
),

tar_parquet(
  house_econ_btw_x_choice_rev_check,
  house_econ_btw_fu_or_2yr_rev_check
),

#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
#    GEO INFORMATION - GENERAL INFORMATION
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

tar_target(
  file_location_path,
  "../../DATA/ORIGINAL/raw_information_062024/BairrosDSS.csv"
),

tar_parquet(
  location_raw,
  read_location_information(file_location_path)
),

tar_parquet(
  location_clean,
  clean_location_information(location_raw)
),

##########
##########
## GEO location basics
##########
##########

# This line defines a target named `countries_geo_file` and specifies 
# the path to a shapefile containing country boundaries data.
tar_target(
  countries_geo_file,
  "../../DATA/ORIGINAL/gis_carles_base/boundaries/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp"
),

# This line defines a target named `countries` and specifies a function 
# `geoRead_countries()` which reads the shapefile specified in 
# `countries_geo_file` and returns the countries data.
tar_target(
  countries,
  geoRead_countries(
    path = countries_geo_file
  )
),

## Boundary data ---- provinces

tar_target(
  provinces_geo_file,
  "../../DATA/ORIGINAL/gis_carles_base/boundaries/ne_10m_admin_1_states_provinces/ne_10m_admin_1_states_provinces.shp"
),

tar_target(
  provinces,
  geoRead_provinces(
    path = provinces_geo_file
  )
),

## Boundary data ---- districts

tar_target(
  districts_geo_file,
"../../DATA/ORIGINAL/gis_carles_base/boundaries/Moz_districts.geojson"
),

tar_target(
  districts,
  geoRead_districts(
    path = districts_geo_file
  )

),

# Stations, sugar factories, hospitals
tar_target(
  apstation,
 read_apstation_GIS()
),

tar_target(
  sugar,
 read_sugar_facotry_GIS()
),

tar_target(
  main_hosp,
 read_main_hospital_GIS()
),


tar_target(
  other_hosp,
  read_other_hospitals_GIS()
),


# roads
tar_target(
  highway,
  read_shp_and_filter_GIS(
    file = "../../DATA/ORIGINAL/gis_carles_base/roads/highway.shp",
    filter_geo = manhica
  ) 
),

tar_target(
  roads,
    read_shp_and_filter_GIS(
    file = "../../DATA/ORIGINAL/gis_carles_base/roads/adna_roads.shp",
    filter_geo = manhica
  ) 
),

# Urban LC
tar_target(
  lc_vector,
 read_urban_lc_DIR(
  file = "../../DATA/ORIGINAL/gis_carles_base/LC/E020S20_PROBAV_LC100_global_v3.0.1_2016-conso_Discrete-Classification-map_EPSG-4326.tif",
  filter_geo = manhica)
),

# 2. Preprocess road, sugar cane, and health centre data ----

tar_target(
  health_facilities,
  read_health_information(
    "../../DATA/ORIGINAL/GIS_distance/GPS_unidades_sanitarias_joe_brew.xlsx"
  )
),

##########
##########
## ADD_YOUR_DESCRIPTION
##########
##########

tar_target(
  zona_subRegion,
  {
    bairros %>%
      sf::st_transform(crs = 32736) %>%
      group_by(Zona) %>%
      summarise()
  }
), 

#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
#    Getting the base TO measure the distance
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

tar_target(
  gis_base_data_sf_x_choice,
  transform_base_GIS(
    # house_econ_btw_x_choice
    house_features_btw_x_choice # has less missing gps data
  )
),
tar_target(
  gis_base_data_sf_x_choice_rev_check,
  transform_base_GIS(
    # house_econ_btw_x_choice
    house_features_btw_x_choice_rev_check # has less missing gps data
  )
),

##########
##########
## creating subregion optionnal to the end
##########
##########

tar_target(
  subregion_information,
  { 
    st_join(gis_base_data_sf_x_choice, zona_subRegion["Zona"]) %>% st_drop_geometry(.)
  }
),

##########
##########
## creating distances 
##########
########## In this point to calculate the distance we will use
# the hospital for that we have data.. because some of the hospital we use in the past version 
# are not present on opd data.

 # we have everything , and the line 1596 to continue
tar_target(
  distance_data_treatment_GIS,
  treatment_data_to_get_distance(
    hfeatures = gis_base_data_sf_x_choice,
    manhica = manhica,
    filter_inside_manhica = TRUE,
    health_f = health_facilities,
    roads_f = roads,
    sugar_f = sugar,
    health_facilities_to_distance = c( "Manhica sede",
                                        "Maragra",
                                        "Ilha Josina Machel",
                                        "Taninga",
                                        "Nwamatibjana", # is palmeira - is the name of hospital
                                        "Malavele",
                                        "Xinavane",
                                        "Palmeira" 
                                        # Maluana - otros que no estan
                                        )
  )
),
tar_target(
  distance_data_treatment_GIS_rev_check,
  treatment_data_to_get_distance(
    hfeatures = gis_base_data_sf_x_choice_rev_check,
    manhica = manhica,
    filter_inside_manhica = TRUE,
    health_f = health_facilities,
    roads_f = roads,
    sugar_f = sugar,
    health_facilities_to_distance = c( "Manhica sede",
                                        "Maragra",
                                        "Ilha Josina Machel",
                                        "Taninga",
                                        "Nwamatibjana", # is palmeira - is the name of hospital
                                        "Malavele",
                                        "Xinavane",
                                        "Palmeira" 
                                        # Maluana - otros que no estan
                                        )
  )
),

# OPD places = 
    #  Manhica      Maragra  Ilha Josina      Taninga Nwamatibjana     Malavela
    #    40640        17004         8178         5942        20540        10731
    # Xinavane       others    miss_info
    #     9229            0            1

#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
#    BLOCK: OPD - hospital visits DATA
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
 tar_parquet(
    diag,
    get(load("../../DATA/ORIGINAL/icd10_inpd.RData"))
  ),



  # tar_target(h_visits_file, "../../DATA/raw/opd.csv"),
  tar_target(h_visits_file, "../../DATA/ORIGINAL/new_version_opd/master_opd_ver43.dta"),

  # tar_parquet(h_visits_raw, readHospitalVisits(h_visits_file)),
  tar_parquet(h_visits_raw, readHospitalVisits_dta(h_visits_file)),

  tar_parquet(
    h_visits_treated,
    cleanHospitalVisitsData( data = h_visits_raw, 
                             vStart = vStart_date, 
                             vEnd = vEnd_date) %>%
      check_sameEpisodes(., n_days_same = 2)
    ),

#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
#    Vamos a filtrar - para luego sacar los diagnosticos de forma mas facil
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

tar_parquet(
  h_visits_multi_periods,
  clean_interval_data_and_deploy(
    h_visits_treated, 
    list_houses_participants,
    id_1 = "perm_id",
    id_2 = "perm_id",
    field_date = "start"
    )
),

tar_parquet(
  h_visits_btw_fu,
  which_sensibility_is_need(
    data = h_visits_multi_periods
    , w_sensitivity = c("between_dates")
    )
),

# no se si tiene mucho sentido aqui
tar_parquet(
  h_visits_btw_3mth_fu,
  which_sensibility_is_need(
    data = h_visits_multi_periods
    , w_sensitivity = c("between_dates"
                       ,"between_3months_plus")
)),

tar_parquet(
  h_visits_btw_6mth_fu,
  which_sensibility_is_need(
    data = h_visits_multi_periods
    , w_sensitivity = c("between_dates"
                       ,"between_3months_plus"
                       ,"between_6months_plus"
    )
)),

tar_parquet(
  h_visits_btw_1yrt_fu,
  which_sensibility_is_need(
    data = h_visits_multi_periods
    , w_sensitivity = c("between_dates"
                       ,"between_3months_plus"
                       ,"between_6months_plus"
                       ,"between_1yr_plus"

    )
)),

tar_parquet(
  h_visits_btw_1_5yrt_fu,
  which_sensibility_is_need(
    data = h_visits_multi_periods
    , w_sensitivity = c("between_dates"
                       ,"between_3months_plus"
                       ,"between_6months_plus"
                       ,"between_1yr_plus"
                       ,"between_1_5yr_plus"
    )
)),

tar_parquet(
  h_visits_btw_2yrt_fu,
  which_sensibility_is_need(
    data = h_visits_multi_periods
    , w_sensitivity = c("between_dates"
                       ,"between_3months_plus"
                       ,"between_6months_plus"
                       ,"between_1yr_plus"
                       ,"between_1_5yr_plus"
                       ,"between_2yr_plus"
    )
)),


######
### getting the vistis by FU without taking in account the exits and comeback...
###
######
tar_parquet(
  h_visits_all_period_onSys,
  clean_interval_data_and_deploy(
    h_visits_treated, 
    data_fu_base,
    id_1 = "perm_id",
    id_2 = "perm_id",
    field_date = "start"
    )
),

##########
##########
## limit 31-12-2025
##########
##########

tar_parquet(
  h_visits_all_period_onSys_rev_check,
  clean_interval_data_and_deploy(
    h_visits_treated, 
    data_fu_base_rev_check,
    id_1 = "perm_id",
    id_2 = "perm_id",
    field_date = "start"
    )
),


tar_parquet(
  h_visits_btw_fu_onSys,
  which_sensibility_is_need(
    data = h_visits_all_period_onSys
    , w_sensitivity = c("between_dates")
    )
),

##########
##########
## limit 31-12-2025
##########
##########

tar_parquet(
  h_visits_btw_fu_onSys_rev_check,
  which_sensibility_is_need(
    data = h_visits_all_period_onSys_rev_check
    , w_sensitivity = c("between_dates")
    )
),

############
############
## COICING FLEXIBLE EVENTS..
############
############
# tar_parquet(
#   h_visits_btw_fu_choice,
#   h_visits_btw_fu
# ),

# we will see the two reports options then we will see the impact of this choieceee

tar_parquet(
  h_visits_btw_x_choice,
  h_visits_btw_fu_onSys
),

##########
##########
## limit 31-12-2025
##########
##########
tar_parquet(
  h_visits_btw_x_choice_rev_check,
  h_visits_btw_fu_onSys_rev_check
),
  ############
  ############
  ## EXTRACTING RESPIRATORY CAUSES 
  ############
  ############

  # tar_target(
  #     code_respiratory_extract,
  #     codes_by_id_extraction(
  #       h_visits_btw_fu_choice,
  #       diag
  #   )
  # ),

  ### CREATE A COMPOSITE VARIABLE FOR RESPIRATORY CAUSES ###
  ##########################################################

  ## Generate the variable using ICD10 coding (and not names from diagnostic labels):
  # ICD10 for Diseases of the respiratory system it's J00-J99
  # Also include A15 (respiratory tuberculosis) and P22 (Respiratory distress of newborn) and R04 (Haemorrhage from respiratory passages) 

  # Create a variable that says how many diagnostics starting by J + a15 + p22 + r04 there are:


  tar_target(
      code_respiratory_extract_all_respiratory,
      codes_by_all_respiratory(
        h_visits_btw_x_choice,
        diag
    )
  ),

##########
##########
## limit 31-12-2025
##########
##########
  tar_target(
      code_respiratory_extract_all_respiratory_rev_check,
      codes_by_all_respiratory(
        h_visits_btw_x_choice_rev_check,
        diag
    )
  ),
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
#    Getting visit type and things
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

tar_parquet(
  h_visits_outcoume_creation,
  h_visits_btw_x_choice %>%
      getCauseCategory() %>% 
      get_cause_visits_v3(
          .,
          code_respiratory_extract_all_respiratory,
          name_new_var = "respiratory_all_respiratory"
      ) %>%
      mutate(
        num_v = 1
      )

),

##########
##########
## limit 31-12-2025
##########
##########
tar_parquet(
  h_visits_outcoume_creation_rev_check,
  h_visits_btw_x_choice_rev_check %>%
      getCauseCategory() %>% 
      get_cause_visits_v3(
          .,
          code_respiratory_extract_all_respiratory_rev_check,
          name_new_var = "respiratory_all_respiratory"
      ) %>%
      mutate(
        num_v = 1
      )

),

##########################################################################
# summary - of visits by participant
##########################################################################

  tar_parquet(
    h_visits_total_by_participants, 
    grouping_visits_at_participants_and_cleaning(
      h_visits_outcoume_creation
      )
    ),

##########
##########
## limit 31-12-2025
##########
##########

  tar_parquet(
    h_visits_total_by_participants_rev_check, 
    grouping_visits_at_participants_and_cleaning(
      h_visits_outcoume_creation_rev_check
      )
    ),
# sum(h_visits_total_fu$h_visits)
# sum(h_visits_total_fu$h_respiratory_visits)
# sum(h_visits_total_fu$h_respiratory_visits) / sum(h_visits_total_fu$h_visits) * 100

# sum(h_visits_total_fu$h_malaria_visits)
# sum(h_visits_total_fu$h_malaria_visits) / sum(h_visits_total_fu$h_visits) * 100

# sum(h_visits_total_fu$h_fever_visits)
# sum(h_visits_total_fu$h_fever_visits) / sum(h_visits_total_fu$h_visits) * 100

# sum(h_visits_total_fu$h_bronchitis_visits)
# sum(h_visits_total_fu$h_bronchitis_visits) / sum(h_visits_total_fu$h_visits) * 100

#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
#    SES variables creation 
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

tar_parquet(
  mca_part_1_econ,
  house_econ_btw_x_choice %>% 
    econ_assets_create_mca_variables_paper2(.) %>%
    select(
      perm_id,
      ends_with("_mca")
    )

),
tar_parquet(
  mca_part_1_econ_rev_check,
  house_econ_btw_x_choice_rev_check %>% 
    econ_assets_create_mca_variables_paper2(.) %>%
    select(
      perm_id,
      ends_with("_mca")
    )

),

tar_parquet(
  mca_part_1_features,
  house_features_btw_x_choice %>% 
    houses_mca_variables_creation_paper2(.) %>%
    select(
      perm_id,
      ends_with("_mca")
    )

),
tar_parquet(
  mca_part_1_features_rev_check,
  house_features_btw_x_choice_rev_check %>% 
    houses_mca_variables_creation_paper2(.) %>%
    select(
      perm_id,
      ends_with("_mca")
    )

),


tar_parquet(
  final_mca_to_SES, 
  mca_part_1_econ %>% 
    inner_join(
        mca_part_1_features ,
        by = "perm_id"
    ) %>%
    select(-coverage_mat_mca)
    ),

tar_parquet(
  final_mca_to_SES_rev_check, 
  mca_part_1_econ_rev_check %>%  # falta
    inner_join(
        mca_part_1_features_rev_check ,
        by = "perm_id"
    ) %>%
    select(-coverage_mat_mca)
    ),

##########
##########
## CREATING SES VARIABLE 
##########
##########

# This function transform all to factor

tar_target(res_mca, ca::mjca(
    final_mca_to_SES %>%
        select(-perm_id),
        lambda = "adjusted"
    )),
tar_target(res_mca_rev_check, ca::mjca(
    final_mca_to_SES_rev_check %>%
        select(-perm_id),
        lambda = "adjusted"
    )),

# we will flip the direction of SES variable 

tar_parquet(
  data_SES_created, 
  data.frame(
    perm_id = final_mca_to_SES$perm_id,
    coord_mca = res_mca$rowcoord[, 1] * -1
    )%>%
  mutate(tertiles = ntile(coord_mca, 3)) %>%
  mutate(
    SES_num = coord_mca,
    SES_var = if_else(
    tertiles == 3, 'High',
        if_else(tertiles == 2,
            'Medium', 'Low')
        )
    ) %>%
  select(
    -coord_mca,
    -tertiles
  )
  ),
tar_parquet(
  data_SES_created_rev_check, 
  data.frame(
    perm_id = final_mca_to_SES_rev_check$perm_id,
    coord_mca = res_mca_rev_check$rowcoord[, 1] * -1
    )%>%
  mutate(tertiles = ntile(coord_mca, 3)) %>%
  mutate(
    SES_num = coord_mca,
    SES_var = if_else(
    tertiles == 3, 'High',
        if_else(tertiles == 2,
            'Medium', 'Low')
        )
    ) %>%
  select(
    -coord_mca,
    -tertiles
  )
  ),

# here we can conclude that the highest numbers are asociated to positive answers 
# on the survey.. then we can check the tertile 3 is asociate to high


#   `get(field_grouped)` count  na_n na_n_perc   mean median   min quant1 quant2
#   <chr>                <int> <int>     <dbl>  <dbl>  <dbl> <dbl>  <dbl>  <dbl>
# 1 1                     9192     0         0 -2.97  -2.69  -6.12  -4.30  -3.82
# 2 2                     9192     0         0 -0.577 -0.740 -1.76  -1.67  -1.35
# 3 3                     9192     0         0  3.55   3.15   1.11   1.49   1.84

#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
#    ENVIRONMENT lecture
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

  tar_parquet(
    data_environment_raw,
    readEnvironmentData(

      "../../DATA/ORIGINAL/manhica_gisvars_mcirach_230302.csv"
    ) %>%
    filter(
        !(perm_id == "2004-011-20") # we filter this child because it have a multiple register 
    )
  ),

  tar_parquet(
    data_environment,
    data_environment_raw %>%
      treatment_data_environment(.) 
      # %>%
      # select(
      #   perm_id,
      #   artificial_light_at_night_2015_1000,
      #   artificial_light_at_night_diff_1000,
      #   imperviousness_density_500,
      #   cropland_500,
      #   ndvi_2015_500,
      #   ndvi_diff_500
      # )
  ),

#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
#    JOININ DATA JUST FOR PAPER 1
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
# we need separate the data, if we add more data, 
# more missiness is playing 

# h_visits ~ ilumination_fuel + offset(log(fu_months)) + 
# gender + SES_var + roaddist_std + healthdist_final_std + 
# healthdist_name_facility + as.factor(year_birth) + 
# mother_age_at_birth_of_child_std + mother_education

  tar_parquet(
    data_end_joining_pre_model_paper_1_pre,
    joining_data_w_visits_paper_1(
        data_fu_base = data_fu_base,
        mother_w_basic_info_cleaned = mother_w_basic_info_cleaned,
        indv_edu_ocu_choice = indv_edu_ocu_choice,
      
        house_features_btw_x_choice = house_features_btw_x_choice,
        # house_econ_btw_x_choice = house_econ_btw_x_choice,

        # we will need put 0 in miss values on no visits 
        h_visits_total_by_participants = h_visits_total_by_participants, 
        data_SES_created = data_SES_created,
        distance_data_treatment_GIS = distance_data_treatment_GIS 
      )  %>%
    process_final_variables_paper_1(.)

  ),

  tar_parquet(
    data_end_joining_pre_model_paper_1,
    filter_migration_ilumination_ocupation_paper_1(
      data_end_joining_pre_model_paper_1_pre
    )

  ),

##########
##########
## limit 31-12-2025
##########
##########

  tar_parquet(
    data_end_joining_pre_model_paper_1_pre_rev_check,
    joining_data_w_visits_paper_1(
        data_fu_base = data_fu_base_rev_check,
        mother_w_basic_info_cleaned = mother_w_basic_info_cleaned_rev_check,
        indv_edu_ocu_choice = indv_edu_ocu_choice_rev_check,
      
        house_features_btw_x_choice = house_features_btw_x_choice_rev_check,
        # house_econ_btw_x_choice = house_econ_btw_x_choice,

        # we will need put 0 in miss values on no visits 
        h_visits_total_by_participants = h_visits_total_by_participants_rev_check, 
        data_SES_created = data_SES_created_rev_check,
        distance_data_treatment_GIS = distance_data_treatment_GIS_rev_check
      )  %>%
    process_final_variables_paper_1(.)

  ),

  tar_parquet(
    data_end_joining_pre_model_paper_1_rev_check,
    filter_migration_ilumination_ocupation_paper_1(
      data_end_joining_pre_model_paper_1_pre_rev_check
    )

  ),


#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
#    generating datas for the report
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
tar_target(
  limit_by_geo_90,
  limit_by_geo_and_metric(data_end_joining_pre_model_paper_1
                              ,group_field = "healthdist_name_facility_all"
                              ,metric = "healthdist_all"
                              ,percentage_limit = 0.9)

),

tar_parquet(
  data_end_90_perce_distance_quantile,
  subseting_by_limit_geo(
    data = data_end_joining_pre_model_paper_1,
    limit = limit_by_geo_90,
    health_principal_vector = c( "Manhica sede",
    "Maragra",
    "Ilha Josina Machel",
    "Taninga",
    "Nwamatibjana", # is palmeira - is the name of hospital
    "Malavele",
    "Xinavane",
    "Palmeira" 
    # Maluana - otros que no estan
    )
  )
),


tar_target(
  limit_by_geo_90_rev_check,
  limit_by_geo_and_metric(data_end_joining_pre_model_paper_1_rev_check
                              ,group_field = "healthdist_name_facility_all"
                              ,metric = "healthdist_all"
                              ,percentage_limit = 0.9)

),

tar_parquet(
  data_end_90_perce_distance_quantile_rev_check,
  subseting_by_limit_geo(
    data = data_end_joining_pre_model_paper_1_rev_check,
    limit = limit_by_geo_90_rev_check,
    health_principal_vector = c( "Manhica sede",
    "Maragra",
    "Ilha Josina Machel",
    "Taninga",
    "Nwamatibjana", # is palmeira - is the name of hospital
    "Malavele",
    "Xinavane",
    "Palmeira" 
    # Maluana - otros que no estan
    )
  )
),

##########
##########
## TABLES ilumination source Gropuped 
##########
##########
tar_parquet(
  table_ilumination_source_problematics,
  get_base_group_ilumintaion_source(
    data = data_end_90_perce_distance_quantile,
    base = houses_cleaned_group
  )
),

#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
#    EXECUTING REPORTE
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
# # # # tar_target(
# # # #   report_parper_1, 
# # # #   rmarkdown::render(
# # # #       here::here(
# # # #         #   "FC-manhica_child_adversity_data",Y:\MOZAMBIQUE\FC-manhica_child_adversity_data\reports\mapping_variables_with_geodata_v2
# # # #           "reports",
# # # #           "Templates",
# # # #           "02-BASE_paper_1_results-sensitivity",
# # # #           "index.Rmd"),
# # # #       params = list( data_filtered = data_end_joining_pre_model_paper_1
# # # #                     , bairros = bairros
# # # #                     , health_facilities = health_facilities
# # # #                     , roads = roads
# # # #                     , table_ilumination_source_problematics = table_ilumination_source_problematics
# # # #                     , data_end_90_perce_distance_quantile = data_end_90_perce_distance_quantile
# # # #                     , manhica = manhica
# # # #                     , districts = districts
# # # #                     , zona = zona_subRegion
# # # #                     , postos = postos
# # # #         ),
# # # #       output_format = "all",
# # # #       output_dir = here::here(
# # # #         #   "FC-manhica_child_adversity_data",
# # # #           "reports",
# # # #           "reports-results",
# # # #           "081 - mapas final_NewClust"
# # # #           )
# # # #   )),


#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
#    EXECUTING REPORTE
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
# # # # tar_target(vOutcome1, "h_visits"),

# # # tar_target(
# # #   report_parper_1_review, 
# # #   rmarkdown::render(
# # #       here::here(
# # #         #   "FC-manhica_child_adversity_data",Y:\MOZAMBIQUE\FC-manhica_child_adversity_data\reports\mapping_variables_with_geodata_v2
# # #           "reports",
# # #           "Templates",
# # #           "06-BASE_review_Analysis",
# # #           "index.Rmd"),
# # #       params = list( 
# # #          data_end_90_perce_distance_quantile = data_end_90_perce_distance_quantile
# # #          , data_end_90_perce_distance_quantile_rev_check = data_end_90_perce_distance_quantile_rev_check

# # #         ),
# # #       output_format = "all",
# # #       output_dir = here::here(
# # #         #   "FC-manhica_child_adversity_data",
# # #           "reports",
# # #           "reports-results",
# # #           "06-review_analysis_v2_NewClust"
# # #           )
# # #   )),

# tar_target(vOutcome2, "h_visits_respiratory_byCode"),

# vOutcome = "h_visits_respiratory_byCode",

# # # # #   tar_target(
# # # # #   report_parper_1_review_resp_byCode, 
# # # # #   rmarkdown::render(
# # # # #       here::here(
# # # # #         #   "FC-manhica_child_adversity_data",Y:\MOZAMBIQUE\FC-manhica_child_adversity_data\reports\mapping_variables_with_geodata_v2
# # # # #           "reports",
# # # # #           "Templates",
# # # # #           "06-BASE_review_Analysis_2",
# # # # #           "index.Rmd"),
# # # # #       params = list( 
# # # # #          data_end_90_perce_distance_quantile = data_end_90_perce_distance_quantile
# # # # #          , data_end_90_perce_distance_quantile_rev_check = data_end_90_perce_distance_quantile_rev_check
      

# # # # #         ),
# # # # #       output_format = "all",
# # # # #       output_dir = here::here(
# # # # #         #   "FC-manhica_child_adversity_data",
# # # # #           "reports",
# # # # #           "reports-results",
# # # # #           "06-review_analysis_respiratory_by_code_NewClust"
# # # # #           )
# # # # #   )),

# tar_target(vOutcome3, "h_visits_respiratory"),
# vOutcome = "h_visits_respiratory",

# # # #   tar_target(
# # # #   report_parper_1_review_resp, 
# # # #   rmarkdown::render(
# # # #       here::here(
# # # #         #   "FC-manhica_child_adversity_data",Y:\MOZAMBIQUE\FC-manhica_child_adversity_data\reports\mapping_variables_with_geodata_v2
# # # #           "reports",
# # # #           "Templates",
# # # #           "06-BASE_review_Analysis_3",
# # # #           "index.Rmd"),
# # # #       params = list( 
# # # #          data_end_90_perce_distance_quantile = data_end_90_perce_distance_quantile
# # # #          , data_end_90_perce_distance_quantile_rev_check = data_end_90_perce_distance_quantile_rev_check
       

# # # #         ),
# # # #       output_format = "all",
# # # #       output_dir = here::here(
# # # #         #   "FC-manhica_child_adversity_data",
# # # #           "reports",
# # # #           "reports-results",
# # # #           "06-review_analysis_respiratory_NewClust"
# # # #           )
# # # #   )),

#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
#    JOINING VISISTS WITH FU_DATA - PAPER 2 STARTING 
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
# TO MOD THE NEXT DATA WE NEED TO DELETE:: PAPER 2
# MIGRATION CASES//...
# DON'T KNOW CASES ON ILUMINATION_SOURCE
  tar_parquet(
    data_end_joining_pre_model,
    joining_data_w_visits(
        data_fu_base = data_fu_base,
        mother_w_basic_info_cleaned = mother_w_basic_info_cleaned,
        indv_edu_ocu_choice = indv_edu_ocu_choice,
        family_inestability_choice = family_inestability_choice,
        family_siblings_death_extraction = family_siblings_death_extraction,
        parental_marital_status_choice = parental_marital_status_choice,
        house_features_btw_x_choice = house_features_btw_x_choice,
        house_econ_btw_x_choice = house_econ_btw_x_choice,
        mca_part_1_econ = mca_part_1_econ,
        h_visits_total_by_participants = h_visits_total_by_participants, 
        # we will need put 0 in miss values on no visits 
        data_SES_created = data_SES_created,
        distance_data_treatment_GIS = distance_data_treatment_GIS, # 56 out manhica
        location_clean = location_clean,
        data_environment = data_environment) %>%
    process_final_variables(.,names_to_impute_miss =  names(data_environment))

  ),

tar_parquet(
  data_to_model_and_analise,
  treat_pre_model_data(
    data_end_joining_pre_model
  )
),

# Vemos que hay un caso que por region no podemos imputarlo
# hemos añadido una funcion que si no encuentra popr region le meta por media general

# tar_target(
#   write_data_end_joining_pre_model,
#   saveRDS(
#     data_to_model_and_analise,
#     "/outputs/data-treated/data_to_model_and_analise.rds"
#     )
# ), 

##########
##########
## efective buffer information to GEO
##########
##########

tar_target(
    health_facilities_w,

    create_buffers_pre(
        health_facilities =  health_facilities,
        health_principal_vector =  c( "Manhica sede",
                                        "Maragra",
                                        "Ilha Josina Machel",
                                        "Taninga",
                                        "Nwamatibjana", # is palmeira - is the name of hospital
                                        "Malavele",
                                        "Xinavane",
                                        "Palmeira" 
                                        # Maluana - otros que no estan
                                        )
    )

),

tar_target(
    buffer_by_facilities,
    generate_buffers(
        data_to_model_and_analise = data_to_model_and_analise,
        percentage_of_block = 0.90,
        health_facilities_w = health_facilities_w

    )
),

#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
#    generating datas for the report
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
tar_target(
  limit_by_geo_90_paper2,
  limit_by_geo_and_metric(data_to_model_and_analise
                              ,group_field = "healthdist_name_facility_all"
                              ,metric = "healthdist_all"
                              ,percentage_limit = 0.9)

),

tar_parquet(
  data_end_90_perce_distance_quantile_paper2,
  subseting_by_limit_geo(
    data = data_to_model_and_analise,
    limit = limit_by_geo_90_paper2,
    health_principal_vector = c( "Manhica sede",
    "Maragra",
    "Ilha Josina Machel",
    "Taninga",
    "Nwamatibjana", # is palmeira - is the name of hospital
    "Malavele",
    "Xinavane",
    "Palmeira" 
    # Maluana - otros que no estan
    )
  )
)

#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
#    paper 2 information to report
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

,tar_target(
  environments_list,
  c("SRE", "SEA", "HE", "FI", "ALL")
)

##########
##########
## ExWAS - all and by parts
##########
##########

# load model formulas - fistr set the names of model

,tar_target(
  complete_set_names,
  names(data_end_90_perce_distance_quantile_paper2)
)

##
# SRE - SURROUNDING RESIDENTIAL ENVIRONMENT
##

,tar_target(SRE_variables, get_sre_variables( names = complete_set_names, 
                                    type = environments_list[1] ) )# here the parameters are ilusory xd

##
# SEA - SOCIO-ECONOMIC ADVERSITY
##
,tar_target(
  SEA_variables, 
  c(get_generic_variables( data_names = complete_set_names,
                                        type = environments_list[2],
                                        restrict_variable = c("has_tractor_mca", "long_head_unemployed_mca") )
# names(data)
, "mother_education")
)


##
# HE - HOUSEHOLD ENVIRONMENT
##

,tar_target(
  HE_variables,
   get_generic_variables(  data_names = complete_set_names,
                                        type = environments_list[3],
                                        restrict_variable = c("wat_treatment_proc_cat_HEMCA") )
)

##
# FI - FAMILY INESTABILITY
##
,tar_target(
  FI_variables,
   c( "death_of_mother" 
      ,"death_of_father" 
      ,"living_without_mother_in_household" 
      ,"living_without_father_in_household"
      ,"death_of_sibling"
      )
)

##########
##########
## vector - all variables
##########
##########

,tar_target(
  all_environment_variables,
  c(
    SRE_variables,
    SEA_variables,
    HE_variables,
    FI_variables
  )
)

##########
##########
## SRE models
##########
##########

# individuals variables

,tar_target(
  res_mod_all_grouoped_indv, 
  mod_any_set_of_variable(
    data_end_90_perce_distance_quantile_paper2,
    output_variables = "h_visits", # "h_visits"
    variables_to_mod_or_end = all_environment_variables,
    family_model = "zero inflated", # poisson, zero inflated, logistic, quasipoisson, negative binomial
    fml_base_vars = c(
      "h_visits",
      "fu_months",
      "as.factor(gender)",
      # "SES_var",
      # "roaddist_std",
      "healthdist_final_std",
      "as.factor(healthdist_name_facility)",
      "as.factor(year_birth)",
      "mother_age_at_birth_of_child_std"
      # ,
      # "mother_education"
       ),
    where_to_save = "test_hpc",
    environment_end = "all_together",
    STD_variable = TRUE,
    name_facility_in_order = TRUE
  ))



,tar_target( # haciendo una copia para añadir el random intercept
  res_mod_all_grouoped_indv_random_intercept, 
  mod_any_set_of_variable_RI(
    data_end_90_perce_distance_quantile_paper2,
    output_variables = "h_visits", # "h_visits"
    variables_to_mod_or_end = all_environment_variables,
    family_model = "ri-zero inflated", # poisson, zero inflated, logistic, quasipoisson, negative binomial
    fml_base_vars = c(
      "as.factor(gender)",
      # "SES_var",
      # "roaddist_std",
      "healthdist_final_std",
      "as.factor(healthdist_name_facility)",
      "as.factor(year_birth)",
      "mother_age_at_birth_of_child_std"
      # ,
      # "mother_education"
      , "(1 | house_number)"
       ),
    in_ZI_part = FALSE,
    where_to_save = "Random intercept",
    environment_end = "all_tg_RI",
    STD_variable = TRUE,
    name_facility_in_order = TRUE
  ))


#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
#    Paper 2 - Init - V2
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

########################################################################
########################################################################
#   OBJECTS CREATIONS
########################################################################
########################################################################

##########
##########
## VOLCANO PART
##########
##########

, tar_target(
  volcano_list_paper_2,
  volcano_from_scratch(
    list_coef = res_mod_all_grouoped_indv,
    list_variebles_interest = all_environment_variables,
    where_to_save = here::here(
      "outputs",
      "02_REPORT1")

    )

)


, tar_target(
  volcano_list_paper_22,
  volcano_from_scratch(
    list_coef = res_mod_all_grouoped_indv,
    list_variebles_interest = all_environment_variables,
    where_to_save = here::here(
      "outputs",
      "02_REPORT_volcano")

    )

)


, tar_target(
  volcano_list_paper_randomIntercept,
  volcano_from_scratch_RI_to_join(
    list_coef =  res_mod_all_grouoped_indv_random_intercept,
    list_variebles_interest = all_environment_variables,
    where_to_save = here::here(
      "outputs",
      "03_VOLCANOPLOT_to_CHECK_ri_zi")

    )

)

, tar_target(
  volcano_list_paper_v2_direct_report_v2,
  volcano_from_scratch(
    list_coef = res_mod_all_grouoped_indv,
    list_variebles_interest = all_environment_variables,
    where_to_save = here::here(
      "outputs",
      "02_REPORT_vol")

    )

)

##########
##########
## ELASTIC NET PART
##########
##########

,tar_target(
  elastic_net_paper_2,
  elastic_net_generator_w_penalty(
    data = data_end_90_perce_distance_quantile_paper2,
    all_environment_variables = all_environment_variables,
    covariables = c(
        # "offset(log(fu_months))", 
        "as.factor(gender)",
        "healthdist_final_std",
        "as.factor(healthdist_name_facility)",
        "as.factor(year_birth)",
        "mother_age_at_birth_of_child_std"
    ),
    output_variable = "h_visits",
    where_to_save = here::here(
      "outputs",
      "02_REPORT1"),
    offset_var = "fu_months",
    penalty_vector = TRUE, # NULL if want all the variables are equally penalized
    kfold_n = 10 # si metemeos que kflod = nrow(x) entonces haremos una LOOCV - creo que kfold = 10 es valido

    )
  )

# # pen
# ,tar_target(
#   elastic_net_paper_2_pen,
#   elastic_net_glmmPen(
#   data = data_end_90_perce_distance_quantile_paper2,
#   all_environment_variables = all_environment_variables,
#   covariables = c(
#         "as.factor(gender)",
#         "healthdist_final_std",
#         "as.factor(healthdist_name_facility)",
#         "as.factor(year_birth)",
#         "mother_age_at_birth_of_child_std"
#     ),                 # SIN incluir offset() aquí
#   output_variable = "h_visits",             # p.ej. "h_visits"
#   group_var      = "house_number",
#   offset_var     = "fu_months",
#   where_to_save  = here::here(
#       "outputs",
#       "03_elastic_w_peen"),
#   alpha_grid     = c(0.5),  # mezcla L1/L2 (ENet-like) , 0.7, 0.5, 0.3
#   penalty        = "lasso",   # alternativas: "MCP", "SCAD"
#   nlambda        = 5,        # nº de lambdas por rejilla
#   search_type    = "abbrev",  # búsqueda abreviada λ (rápida)
#   bic_option     = "BICh",    # criterio para escoger modelo
#   penalty_vector = c(
#         "as.factor(gender)F",
#         "healthdist_final_std",
#         "as.factor(healthdist_name_facility)Malavele",
#         "as.factor(healthdist_name_facility)Manhica sede",
#         "as.factor(healthdist_name_facility)Maragra",
#         "as.factor(healthdist_name_facility)Palmeira",
#         "as.factor(healthdist_name_facility)Taninga",
#         "as.factor(healthdist_name_facility)Xinavane",
#         "as.factor(year_birth)2017",
#         "as.factor(year_birth)2018",
#         "as.factor(year_birth)2019",
#         "as.factor(year_birth)2020",
#         "mother_age_at_birth_of_child_std"
#     )     # opcional: nombres-> 0/1 (0=no penalizar)
# )
#   )

# primero reports y luego esta prueba
# ,tar_target(
#   elastic_net_paper_2_LOOCV,
#   elastic_net_generator_w_penalty(
#     data = data_end_90_perce_distance_quantile_paper2,
#     all_environment_variables = all_environment_variables,
#     covariables = c(
#         # "offset(log(fu_months))", 
#         "as.factor(gender)",
#         "healthdist_final_std",
#         "as.factor(healthdist_name_facility)",
#         "as.factor(year_birth)",
#         "mother_age_at_birth_of_child_std"
#     ),
#     output_variable = "h_visits",
#     where_to_save = here::here(
#       "outputs",
#       "elastic_LOOCV"),
#     offset_var = "fu_months",
#     penalty_vector = TRUE, # NULL if want all the variables are equally penalized
#     kfold_n = nrow(data_end_90_perce_distance_quantile_paper2) # si metemeos que kflod = nrow(x) entonces haremos una LOOCV - creo que kfold = 10 es valido

#     )
#   )



##########
##########
## RANDOM FOREST PART
##########
##########
,tar_target(
  random_forest_paper_2,
  forest_plot_generator_paper_2(
  data = data_end_90_perce_distance_quantile_paper2,
  environment_variables = all_environment_variables,
  covariables = c(
        "fu_months", 
        "gender",
        "healthdist_final_std",
        "healthdist_name_facility",
        "year_birth",
        "mother_age_at_birth_of_child_std"
    ),
  output_variable = "h_visits",
  where_to_save = here::here(
      "outputs",
      "02_REPORT1")

    )
)

##########
##########
## all by 300 buffer 
##########
##########
,tar_target(
  all_environment_variables_by_300,
  load_variables_by_300()
)
##########
##########
## all by 500 buffer
##########
##########
,tar_target(
  all_environment_variables_by_500,
  load_variables_by_500()
)


#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
#    FIN - bloque 1 - 
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

##########
##########
## init
##########
########## pero antes -- realizar analisis de contraste de las variables seleccionadas


#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
#    Descriptivo de variables significativas 
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

########################################################################
########################################################################
#   transformacion del set de datos en dummys
########################################################################
########################################################################

, tar_parquet(
  data_paper_2_cluster,
  generator_dummy_vars_and_std_num_vars(
    data = data_end_90_perce_distance_quantile_paper2,
    general_variables = all_environment_variables
  )
)

##########
##########
## descriptivo y plots del reporte:
##########
##########

,tar_target(
  data_paper_2_cluster_descriptive_bygroups,
  descriptive_by_variable_list_corr_part(
    data = data_paper_2_cluster,
    variable_list = list(
      all_environment_variables = all_environment_variables,
      volcano_normal_cut = volcano_list_paper_22$normal_cut_variables,
      volcano_bonfer_cut = volcano_list_paper_22$bonfer_cut_variables,
      elastic_variables = elastic_net_paper_2$list_significative_variables,
      all_env_variables_w_outcome = c("h_visits", "h_visits_respiratory", all_environment_variables)
    ),
    where_to_save =  here::here(
      "outputs",
      "02_REPORT1",
      "correlation_part_v3")
  )
)

#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
#    INICIO BLOQUE 2 - Seleccion y depuracion de variables - CLUSTERS
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

########################################################################
########################################################################
#   seleccion de variables significativas creacion data group 1
########################################################################
########################################################################
 # Notas previas:
 # el asunto principal es que no debemos hacer muchas pruebas en los cluster
 # quitando y poniendo variables 
 # entonces tenemos que pensar en como iterar de forma mas agil estas variaciones
 #
 # 1) creo que debemos crear una lista con las variables sub seleccionas por cada metodo
 # 2) y para variable el primer paso creo que es sacar el K optimo, 
 # 3) ver que subconjunto funciona mejor sin variables
 # 4) probar alguna correciones en las metricas dummys, como frequencia y tal
 # 

 # el punto siguiente sera crear los cluster con la k optima para cada cluster

 # 

 ##########
 ##########
 ## EXTRACCION DE K cluster optimos para diferentes subconjuntos de variables
 ##########
 ##########
#  ,tar_target(
#   optima_k_per_subVariables_paper2_0,
#   generator_optimal_k_generator(
#     data = data_end_90_perce_distance_quantile_paper2,
#     list_variables = volcano_list_paper_2$normal_cut_variables[!str_detect(volcano_list_paper_2$normal_cut_variable, "_diff_")]

#   )
#  )

#  ,tar_target(optimal_k_by_nbclust_normal_cut,
#   get_optimal_k_by_nbcluster(
#     optima_k_per_subVariables_paper2_0,
#     n_order = 1
#   )
#  )

#  ,tar_target(
#   optima_k_per_subVariables_paper2_1,
#   generator_optimal_k_generator(
#     data = data_end_90_perce_distance_quantile_paper2,
#     list_variables = all_environment_variables

#   )
#  )

#   ,tar_target(optimal_k_by_nbclust_all_env_var,
#   get_optimal_k_by_nbcluster(
#     optima_k_per_subVariables_paper2_1,
#     n_order = 1
#   )
#  )


#  ,tar_target(
#   optima_k_per_subVariables_paper2,
#   generator_optimal_k_generator(
#     data = data_end_90_perce_distance_quantile_paper2,
#     list_variables = volcano_list_paper_2$bonfer_cut_variables

#   )
#  )

#   ,tar_target(optimal_k_by_nbclust_bonfe_cut,
#   get_optimal_k_by_nbcluster(
#     optima_k_per_subVariables_paper2,
#     n_order = 1
#   )
#  )

#  ,tar_target(
#   optima_k_per_subVariables_paper2_2,
#   generator_optimal_k_generator(
#     data = data_end_90_perce_distance_quantile_paper2,
#     list_variables = elastic_net_paper_2$list_significative_variables

#   )
#  )

# ,tar_target(optimal_k_by_nbclust_elastic_vars,
#   get_optimal_k_by_nbcluster(
#     optima_k_per_subVariables_paper2_2,
#     n_order = 1
#   )
#  )


########################################################################
########################################################################
#   small and bigger buffer cluster 
########################################################################
########################################################################

##########
##########
## small
##########
##########

#  ,tar_target(
#   optimal_k_per_all_env_list_by_small_buff,
#   generator_optimal_k_generator(
#     data = data_end_90_perce_distance_quantile_paper2,
#     list_variables = all_environment_variables_by_300

#   )
#  )

#  ,tar_target(optimal_k_number_for_all_env_list_by_small_buff,
#   get_optimal_k_by_nbcluster(
#     optimal_k_per_all_env_list_by_small_buff,
#     n_order = 1
#   )
#  )


##########
##########
## bigger
##########
##########

#  ,tar_target(
#   optimal_k_per_all_env_list_by_bigger_buff,
#   generator_optimal_k_generator(
#     data = data_end_90_perce_distance_quantile_paper2,
#     list_variables = all_environment_variables_by_500

#   )
#  )

#  ,tar_target(optimal_k_number_for_all_env_list_by_bigger_buff,
#   get_optimal_k_by_nbcluster(
#     optimal_k_per_all_env_list_by_bigger_buff,
#     n_order = 1
#   )
#  )

 
# ,tar_target(
#   optima_k_per_subVariables_paper2_2,
#   generator_optimal_k_generator(
#     data = data_paper_2_cluster,
#     list_variables = volcano_list_paper_2$normal_cut_variables[!str_detect(volcano_list_paper_2$normal_cut_variables, "mother_educationSecondary or tertiary")]
#   )
#  ) Y:\MOZAMBIQUE\FC-manhica_child_adversity_data_paper2_v2\scratch

########## plot(bagamoyo_shp, max.plot = 6)
##########tt <- surf_to_poly(bagamoyo_shp)
# plot(sf::st_geometry(tt))
## i can execute it from local, but the cluster doesnt work
##########
##########

# , tar_target(
#   optimal_k_cluster_per_elasticnet,
#   readRDS(here::here("scratch", "20-cluster_1_k_optimo_dev", "nb_cluster_elastic_part.rds"))
# )


 # -----------------------------

# number 1, will have all the variable for exWAS
# numeber 2,, will have exwas w bonferroni
# number 3, elastic NET ...
# ,tar_parquet(
#   data_cluster_1,

# )


########################################################################
########################################################################
#   aplicar Remove near-zero variance y Multicollinearty or signular matrix
########################################################################
########################################################################

##########
##########
## intentar evitar 
##########
##########
#  Option 1: Frequency Encoding (if categories are rare)
#  Option 2: Weight Rare Categories (if rare categories are important)
#  Option 3: Group Rare Categories (if many categories exist)
#  Option 4: Convert to Dummy Variables (if appropriate)



##########
##########
## Selecion de la k optima...
##########
##########

# Method 1: Elbow Method (Within-cluster Sum of Squares - WSS)

# library(factoextra) set.seed(123) # For reproducibility 

# fviz_nbclust(data_combined, kmeans, method = "wss") + ggtitle("Elbow Method for Optimal Clusters")

# Method 2: Silhouette Score

# fviz_nbclust(data_combined, kmeans, method = "silhouette") + ggtitle("Silhouette Method for Optimal Clusters")

# Method 3: Gap Statistic

# library(cluster) set.seed(123) gap_stat <- clusGap(data_combined, kmeans, K.max = 10, B = 100) fviz_gap_stat(gap_stat)


# Method 4: NbClust Package (Multiple Indices)


# library(NbClust) 
# set.seed(123) 
# nbclust_results <- NbClust(data_combined, 
#   distance = "euclidean", 
#   min.nc = 2, 
#   max.nc = 10, 
#   method = "ward.D2", 
#   index = "all"
# ) # Visualize results fviz_nbclust(nbclust_results)


#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
#    OBSERVATION FROM K OPTIMAL 
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

# after 25 methods, for normal and elastic variables selecte... 
# we see the same number for all of these are 3


########################################################################
########################################################################
#   clusterizacion con las variables y k optimas 
########################################################################
########################################################################

# Option 1: K-Means Clustering

# Option 2: Hierarchical Clustering

# Option 3: DBSCAN (Density-Based Clustering)

#  Fuzzy C-Means (if rare variables should be preserved)



#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
#    fin cluster
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#


########################################################################
########################################################################
#   all env var
########################################################################
########################################################################

# , tar_target(
#   hierarchical_cluster_paper2_all,
#   hierarchical_clustering_plot(
#     data = data_end_90_perce_distance_quantile_paper2,
#     all_environment_variables = all_environment_variables[!str_detect(all_environment_variables, "_diff_")],
#     where_to_save = here::here(
#       "outputs",
#       "CLUSTER_V2_to_publish",
#       "01_CLUSTER_A"),
#     max_clusters = 12,  # Evaluar hasta 15 clústeres
#     manual_k = 2,      # Dejar en NULL para determinar automáticamente el número óptimo
#     # manual_k = optimal_k_by_nbclust_all_env_var,      # Dejar en NULL para determinar automáticamente el número óptimo
#     manhica = manhica,
#     health_facilities = health_facilities,
#     roads = roads,
#     prefix = "all_hier"
#   )
# )


# # # ,tar_target(
# # #   kprototypes_cluster_paper2_all,
# # #   kpprototype_clustering_plot(
# # #     data = data_end_90_perce_distance_quantile_paper2,
# # #     all_environment_variables = all_environment_variables[!str_detect(all_environment_variables, "_diff_")],
# # #     where_to_save = here::here(
# # #         "outputs",
# # #         "CLUSTER_V2_to_publish",
# # #         "01_CLUSTER_B"),
# # #     max_clusters = 3,  # Evaluar de 2 a 10 clusters
# # #     # manual_k = optimal_k_by_nbclust_all_env_var,
# # #     manual_k = 2,
# # #     manhica = manhica,
# # #     health_facilities = health_facilities,
# # #     roads = roads,
# # #     buffer_by_facilities = buffer_by_facilities,
# # #     zona = postos,
# # #     districts = districts,
# # #     prefix = "all_prot",
# # #     list_entorno  = list( SRE_variables = SRE_variables, 
# # #                           SEA_variables = SEA_variables, 
# # #                           HE_variables = HE_variables, 
# # #                           FI_variables = FI_variables)
# # # )
# # # )

# # # ##########
# # # ##########
# # # ## version  2 second most freqcuent is 4
# # # ##########
# # # ##########

# # # # , tar_target(
# # # #   hierarchical_cluster_paper2_all_v2,
# # # #   hierarchical_clustering_plot(
# # # #     data = data_end_90_perce_distance_quantile_paper2,
# # # #     all_environment_variables = all_environment_variables[!str_detect(all_environment_variables, "_diff_")],
# # # #     where_to_save = here::here(
# # # #       "outputs",
# # # #       "CLUSTER_V2_to_publish",
# # # #       "01_CLUSTER_A"),
# # # #     max_clusters = 12,  # Evaluar hasta 15 clústeres
# # # #     manual_k = 3,      # Dejar en NULL para determinar automáticamente el número óptimo
# # # #     manhica = manhica,
# # # #     health_facilities = health_facilities,
# # # #     roads = roads,
# # # #     prefix = "all_hier_v2"
# # # #   )
# # # # )

# # # ,tar_target(
# # #   kprototypes_cluster_paper2_all_v2,
# # #   kpprototype_clustering_plot(
# # #     data = data_end_90_perce_distance_quantile_paper2,
# # #     all_environment_variables = all_environment_variables[!str_detect(all_environment_variables, "_diff_")],
# # #     where_to_save = here::here(
# # #         "outputs",
# # #         "CLUSTER_V2_to_publish",
# # #         "01_CLUSTER_B"),
# # #     max_clusters = 4,  # Evaluar de 2 a 10 clusters
# # #     manual_k = 3,
# # #     manhica = manhica,
# # #     health_facilities = health_facilities,
# # #     roads = roads,
# # #     buffer_by_facilities = buffer_by_facilities,
# # #     zona = postos,
# # #     districts = districts,
# # #     prefix = "all_prot_v2",
# # #     list_entorno  = list( SRE_variables = SRE_variables, 
# # #                           SEA_variables = SEA_variables, 
# # #                           HE_variables = HE_variables, 
# # #                           FI_variables = FI_variables)
# # # )
# # # )


# # # ########################################################################
# # # ########################################################################
# # # #   for small and bigger - buffer
# # # ########################################################################
# # # ########################################################################

# # # ##########
# # # ##########
# # # ## small
# # # ##########
# # # ##########

# # # # , tar_target(
# # # #   hierarchical_cluster_paper2_all_small_buff,
# # # #   hierarchical_clustering_plot(
# # # #     data = data_end_90_perce_distance_quantile_paper2,
# # # #     all_environment_variables = all_environment_variables_by_300,
# # # #     where_to_save = here::here(
# # # #       "outputs",
# # # #       "CLUSTER_V2_to_publish",
# # # #       "01_CLUSTER_A"),
# # # #     max_clusters = 12,  # Evaluar hasta 15 clústeres
# # # #     manual_k = 2,      # Dejar en NULL para determinar automáticamente el número óptimo
# # # #     # manual_k = optimal_k_number_for_all_env_list_by_small_buff,      # Dejar en NULL para determinar automáticamente el número óptimo
# # # #     manhica = manhica,
# # # #     health_facilities = health_facilities,
# # # #     roads = roads,
# # # #     prefix = "all_hier_small"
# # # #   )
# # # # )

# # # ,tar_target(
# # #   kprototypes_cluster_paper2_all_small_buff,
# # #   kpprototype_clustering_plot(
# # #     data = data_end_90_perce_distance_quantile_paper2,
# # #     all_environment_variables = all_environment_variables_by_300,
# # #     where_to_save = here::here(
# # #         "outputs",
# # #         "CLUSTER_V2_to_publish",
# # #         "01_CLUSTER_B"),
# # #     max_clusters = 3,  # Evaluar de 2 a 10 clusters
# # #     manual_k = 2,
# # #     # manual_k = optimal_k_number_for_all_env_list_by_small_buff,
# # #     manhica = manhica,
# # #     health_facilities = health_facilities,
# # #     roads = roads,
# # #     buffer_by_facilities = buffer_by_facilities,
# # #     zona = postos,
# # #     districts = districts,
# # #     prefix = "all_prot_small",
# # #     list_entorno  = list( SRE_variables = SRE_variables, 
# # #                           SEA_variables = SEA_variables, 
# # #                           HE_variables = HE_variables, 
# # #                           FI_variables = FI_variables)
# # # )
# # # )

# # # ##########
# # # ##########
# # # ## bigger
# # # ##########
# # # ##########

# # # # , tar_target(
# # # #   hierarchical_cluster_paper2_all_bigger_buff,
# # # #   hierarchical_clustering_plot(
# # # #     data = data_end_90_perce_distance_quantile_paper2,
# # # #     all_environment_variables = all_environment_variables_by_500,
# # # #     where_to_save = here::here(
# # # #       "outputs",
# # # #       "CLUSTER_V2_to_publish",
# # # #       "01_CLUSTER_A"),
# # # #     max_clusters = 12,  # Evaluar hasta 15 clústeres
# # # #     # manual_k = optimal_k_number_for_all_env_list_by_bigger_buff,      # Dejar en NULL para determinar automáticamente el número óptimo
# # # #     manual_k = 2,      # Dejar en NULL para determinar automáticamente el número óptimo
# # # #     manhica = manhica,
# # # #     health_facilities = health_facilities,
# # # #     roads = roads,
# # # #     prefix = "all_hier_bigger"
# # # #   )
# # # # )

# # # ,tar_target(
# # #   kprototypes_cluster_paper2_all_bigger_buff,
# # #   kpprototype_clustering_plot(
# # #     data = data_end_90_perce_distance_quantile_paper2,
# # #     all_environment_variables = all_environment_variables_by_500,
# # #     where_to_save = here::here(
# # #         "outputs",
# # #         "CLUSTER_V2_to_publish",
# # #         "01_CLUSTER_B"),
# # #     max_clusters = 3,  # Evaluar de 2 a 10 clusters
# # #     # manual_k = optimal_k_number_for_all_env_list_by_bigger_buff,
# # #     manual_k = 2,
# # #     manhica = manhica,
# # #     health_facilities = health_facilities,
# # #     roads = roads,
# # #     buffer_by_facilities = buffer_by_facilities,
# # #     zona = postos,
# # #     districts = districts,
# # #     prefix = "all_prot_bigger",
# # #     list_entorno  = list( SRE_variables = SRE_variables, 
# # #                           SEA_variables = SEA_variables, 
# # #                           HE_variables = HE_variables, 
# # #                           FI_variables = FI_variables)
# # # )
# # # )

# # # ########################################################################
# # # ########################################################################
# # # #   bofe cut
# # # ########################################################################
# # # ########################################################################

# # # # , tar_target(
# # # #   hierarchical_cluster_paper2,
# # # #   hierarchical_clustering_plot(
# # # #     data = data_end_90_perce_distance_quantile_paper2,
# # # #     all_environment_variables = volcano_list_paper_22$bonfer_cut_variables,
# # # #     where_to_save = here::here(
# # # #       "outputs",
# # # #       "CLUSTER_V2_to_publish",
# # # #       "01_CLUSTER_A"),
# # # #     max_clusters = 12,  # Evaluar hasta 15 clústeres
# # # #     manual_k = 2,      # Dejar en NULL para determinar automáticamente el número óptimo
# # # #     # manual_k = optimal_k_by_nbclust_bonfe_cut,      # Dejar en NULL para determinar automáticamente el número óptimo
# # # #     manhica = manhica,
# # # #     health_facilities = health_facilities,
# # # #     roads = roads,
# # # #     prefix = "bonfe_hier"
# # # #   )
# # # # )

# # # ,tar_target(
# # #   kprototypes_cluster_paper2,
# # #   kpprototype_clustering_plot(
# # #   data = data_end_90_perce_distance_quantile_paper2,
# # #   all_environment_variables = volcano_list_paper_22$bonfer_cut_variables,
# # #   where_to_save = here::here(
# # #       "outputs",
# # #       "CLUSTER_V2_to_publish",
# # #       "01_CLUSTER_B"),
# # #   max_clusters = 6,  # Evaluar de 2 a 10 clusters
# # #   manual_k = 2,
# # # #   manual_k = optimal_k_by_nbclust_bonfe_cut,
# # #     manhica = manhica,
# # #     health_facilities = health_facilities,
# # #     roads = roads,
# # #     buffer_by_facilities = buffer_by_facilities,
# # #     zona = postos,
# # #     districts = districts,
# # #     prefix = "bonfe_prot",
# # #     list_entorno  = list( SRE_variables = SRE_variables, 
# # #                           SEA_variables = SEA_variables, 
# # #                           HE_variables = HE_variables, 
# # #                           FI_variables = FI_variables)
# # # )
# # # )


# # # ########################################################################
# # # ########################################################################
# # # #   normal cut
# # # ########################################################################
# # # ########################################################################

# # # # , tar_target(
# # # #   hierarchical_cluster_paper2_norm,
# # # #   hierarchical_clustering_plot(
# # # #     data = data_end_90_perce_distance_quantile_paper2,
# # # #     all_environment_variables = volcano_list_paper_22$normal_cut_variables,
# # # #     where_to_save = here::here(
# # # #       "outputs",
# # # #       "CLUSTER_V2_to_publish",
# # # #       "01_CLUSTER_A"),
# # # #     max_clusters = 12,  # Evaluar hasta 15 clústeres
# # # #     manual_k = 2,      # Dejar en NULL para determinar automáticamente el número óptimo
# # # #     # manual_k = optimal_k_by_nbclust_normal_cut,      # Dejar en NULL para determinar automáticamente el número óptimo
# # # #     manhica = manhica,
# # # #     health_facilities = health_facilities,
# # # #     roads = roads,
# # # #     prefix = "norm_hier"
# # # #   )
# # # # )

# # # ,tar_target(
# # #   kprototypes_cluster_paper2_norm,
# # #   kpprototype_clustering_plot(
# # #   data = data_end_90_perce_distance_quantile_paper2,
# # #   all_environment_variables = volcano_list_paper_22$normal_cut_variables,
# # #   where_to_save = here::here(
# # #       "outputs",
# # #       "CLUSTER_V2_to_publish",
# # #       "01_CLUSTER_B"),
# # #   max_clusters = 6,  # Evaluar de 2 a 10 clusters
# # #   manual_k = 2,
# # # #   manual_k = optimal_k_by_nbclust_normal_cut,
# # #     manhica = manhica,
# # #     health_facilities = health_facilities,
# # #     roads = roads,
# # #     buffer_by_facilities = buffer_by_facilities,
# # #     zona = postos,
# # #     districts = districts,
# # #     prefix = "norm_prot",
# # #     list_entorno  = list( SRE_variables = SRE_variables, 
# # #                           SEA_variables = SEA_variables, 
# # #                           HE_variables = HE_variables, 
# # #                           FI_variables = FI_variables)
# # # )
# # # )

########################################################################
########################################################################
#   elastic
########################################################################
########################################################################
# list(
#   data_all_pc = data,
#   biplot_path = here::here( where_to_save,
#                   environment_end,
#                   "pca_biplot.jpeg" ),

#   contribution_info = summary(pc)$importance,
#   loadings_info = summary(pc)$rotation,
#   general_path = here::here( where_to_save,
#                   environment_end ) write_csv()
# )

# , tar_target(
#   hierarchical_cluster_paper2_ela,
#   hierarchical_clustering_plot(
#     data = data_end_90_perce_distance_quantile_paper2,
#     all_environment_variables = elastic_net_paper_2$list_significative_variables,
#     where_to_save = here::here(
#       "outputs",
#       "CLUSTER_V2_to_publish",
#       "01_CLUSTER_A"),
#     max_clusters = 12,  # Evaluar hasta 15 clústeres
#     manual_k = 2,      # Dejar en NULL para determinar automáticamente el número óptimo
#     # manual_k = optimal_k_by_nbclust_elastic_vars,      # Dejar en NULL para determinar automáticamente el número óptimo
#     manhica = manhica,
#     health_facilities = health_facilities,
#     roads = roads,
#     prefix = "ela_hier"
#   )
# )
,tar_target(
    select_cluster_variables_report_part,
    all_environment_variables_by_500
)


########################################################################
########################################################################
#   ELA _ K OPTIMO diversus
########################################################################
########################################################################

,tar_target(
  kprototypes_cluster_paper2_ela,
  kpprototype_clustering_plot(
  data = data_end_90_perce_distance_quantile_paper2,
  all_environment_variables = select_cluster_variables_report_part,
  where_to_save = here::here(
      "outputs",
      "CLUSTER_V2255"),
  max_clusters = NULL,  # Evaluar de 2 a 10 clusters
#   manual_k = optimal_k_by_nbclust_elastic_vars,
  manual_k = 2,
    manhica = manhica,
    health_facilities = health_facilities,
    roads = roads,
    buffer_by_facilities = buffer_by_facilities,
    zona = postos,
    districts = districts,
    prefix = "ela_prot_2",
    list_entorno  = list( SRE_variables = SRE_variables, 
                          SEA_variables = SEA_variables, 
                          HE_variables = HE_variables, 
                          FI_variables = FI_variables)
)
)


##########
##########
## K = 3
##########
##########
,tar_target(
  kprototypes_cluster_paper2_ela_3,
  kpprototype_clustering_plot(
  data = data_end_90_perce_distance_quantile_paper2,
  all_environment_variables = select_cluster_variables_report_part,
  where_to_save = here::here(
      "outputs",
      "CLUSTER_V2255"),
  max_clusters = NULL,  # Evaluar de 2 a 10 clusters
  manual_k = 3,
    manhica = manhica,
    health_facilities = health_facilities,
    roads = roads,
    buffer_by_facilities = buffer_by_facilities,
    zona = postos,
    districts = districts,
    prefix = "prototype_k3_r",
    list_entorno  = list( SRE_variables = SRE_variables, 
                          SEA_variables = SEA_variables, 
                          HE_variables = HE_variables, 
                          FI_variables = FI_variables)
)
)
# ##########
# ##########
# ## K = 4
# ##########
# # ##########
# ,tar_target(
#   kprototypes_cluster_paper2_ela_4,
#   kpprototype_clustering_plot(
#   data = data_end_90_perce_distance_quantile_paper2,
#   all_environment_variables = select_cluster_variables_report_part,
#   where_to_save = here::here(
#       "outputs",
#       "CLUSTER_V2255"),
#   max_clusters = 10,  # Evaluar de 2 a 10 clusters
#   manual_k = 4,
#     manhica = manhica,
#     health_facilities = health_facilities,
#     roads = roads,
#     buffer_by_facilities = buffer_by_facilities,
#     zona = postos,
#     districts = districts,
#     prefix = "ela_prot_4",
#     list_entorno  = list( SRE_variables = SRE_variables, 
#                           SEA_variables = SEA_variables, 
#                           HE_variables = HE_variables, 
#                           FI_variables = FI_variables)
# )
# )
# ##########
# ##########
# ## K = 5
# ##########
# ##########
# ,tar_target(
#   kprototypes_cluster_paper2_ela_5,
#   kpprototype_clustering_plot(
#   data = data_end_90_perce_distance_quantile_paper2,
#   all_environment_variables = select_cluster_variables_report_part,
#   where_to_save = here::here(
#       "outputs",
#       "CLUSTER_V2255"),
#   max_clusters = 10,  # Evaluar de 2 a 10 clusters
#   manual_k = 5,
#     manhica = manhica,
#     health_facilities = health_facilities,
#     roads = roads,
#     buffer_by_facilities = buffer_by_facilities,
#     zona = postos,
#     districts = districts,
#     prefix = "ela_prot_5",
#     list_entorno  = list( SRE_variables = SRE_variables, 
#                           SEA_variables = SEA_variables, 
#                           HE_variables = HE_variables, 
#                           FI_variables = FI_variables)
# )
# )
# ##########
# ##########
# ## K = 6
# ##########
# ##########
# ,tar_target(
#   kprototypes_cluster_paper2_ela_6,
#   kpprototype_clustering_plot(
#   data = data_end_90_perce_distance_quantile_paper2,
#   all_environment_variables = select_cluster_variables_report_part,
#   where_to_save = here::here(
#       "outputs",
#       "CLUSTER_V2255"),
#   max_clusters = 10,  # Evaluar de 2 a 10 clusters
#   manual_k = 6,
#     manhica = manhica,
#     health_facilities = health_facilities,
#     roads = roads,
#     buffer_by_facilities = buffer_by_facilities,
#     zona = postos,
#     districts = districts,
#     prefix = "ela_prot_6",
#     list_entorno  = list( SRE_variables = SRE_variables, 
#                           SEA_variables = SEA_variables, 
#                           HE_variables = HE_variables, 
#                           FI_variables = FI_variables)
# )
# )
# ##########
# ##########
# ## K = optima
# ##########
# ##########

# ,tar_target(
#   kprototypes_cluster_paper2_ela_nolim,
#   kpprototype_clustering_plot(
#   data = data_end_90_perce_distance_quantile_paper2,
#   all_environment_variables = select_cluster_variables_report_part,
#   where_to_save = here::here(
#       "outputs",
#       "CLUSTER_V2255"),
#   max_clusters = 8,  # Evaluar de 2 a 10 clusters
#   manual_k = NULL,
#     manhica = manhica,
#     health_facilities = health_facilities,
#     roads = roads,
#     buffer_by_facilities = buffer_by_facilities,
#     zona = postos,
#     districts = districts,
#     prefix = "ela_prot_nolim",
#     list_entorno  = list( SRE_variables = SRE_variables, 
#                           SEA_variables = SEA_variables, 
#                           HE_variables = HE_variables, 
#                           FI_variables = FI_variables)
# )
# )


#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
#    MODELLING VARIABLES - Significative from EXwas, elastic
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

##########
##########
## XWAS / ELASTIC NET
##########
##########

,tar_target(
  model_significative_exWAS_normal_cut,
  mod_complete_sig_variables(
    data = data_end_90_perce_distance_quantile_paper2,
    covariables = c(
            "offset(log(fu_months))", 
            "as.factor(gender)",
            "healthdist_final_std",
            "as.factor(healthdist_name_facility)",
            "as.factor(year_birth)",
            "mother_age_at_birth_of_child_std"
        ),
    significative_variables = volcano_list_paper_22$normal_cut_variables[volcano_list_paper_22$normal_cut_variables != "artificial_light_at_night_diff_1000_final"],
    set_model = "zero inflated",
    outcome = "h_visits",
    name_facility_in_order = TRUE,
    STD_variable = TRUE,
    add_decriptive = TRUE
    )
)


,tar_target(
  model_significative_exWAS_normal_cut_ri_zi,
  mod_complete_sig_variables_2(
    data = data_end_90_perce_distance_quantile_paper2,
    covariables = c(
            "offset(log(fu_months))", 
            "as.factor(gender)",
            "healthdist_final_std",
            "as.factor(healthdist_name_facility)",
            "as.factor(year_birth)",
            "mother_age_at_birth_of_child_std",
             "(1 | house_number)"
        ),
    significative_variables = volcano_list_paper_randomIntercept$normal_cut_variables,
    set_model = "ri-zero inflated",
    outcome = "h_visits",
    name_facility_in_order = TRUE,
    STD_variable = TRUE,
    add_decriptive = TRUE
    )
)

,tar_target(
  model_significative_exWAS_bonfe_cut,
  mod_complete_sig_variables(
    data = data_end_90_perce_distance_quantile_paper2,
    covariables = c(
            "offset(log(fu_months))", 
            "as.factor(gender)",
            "healthdist_final_std",
            "as.factor(healthdist_name_facility)",
            "as.factor(year_birth)",
            "mother_age_at_birth_of_child_std"
        ),
    significative_variables = volcano_list_paper_22$bonfer_cut_variables,
    set_model = "zero inflated",
    outcome = "h_visits",
    name_facility_in_order = TRUE,
    STD_variable = TRUE,
    add_decriptive = TRUE
    )
)


,tar_target(
  model_significative_elasticNet,
  mod_complete_sig_variables(
    data = data_end_90_perce_distance_quantile_paper2,
    covariables = c(
            "offset(log(fu_months))", 
            "as.factor(gender)",
            "healthdist_final_std",
            "as.factor(healthdist_name_facility)",
            "as.factor(year_birth)",
            "mother_age_at_birth_of_child_std"
        ),
    significative_variables = elastic_net_paper_2$list_significative_variables,
    set_model = "zero inflated",
    outcome = "h_visits",
    name_facility_in_order = TRUE,
    STD_variable = TRUE,
    add_decriptive = TRUE
    )
)

#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
#    modelling_clusters 
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

# ,tar_target(
#   model_significative_elasticNet,
#   mod_complete_sig_variables(
#     data = data_end_90_perce_distance_quantile_paper2,
#     covariables = c(
#             "offset(log(fu_months))", 
#             "as.factor(gender)",
#             "healthdist_final_std",
#             "as.factor(healthdist_name_facility)",
#             "as.factor(year_birth)",
#             "mother_age_at_birth_of_child_std"
#         ),
#     significative_variables = elastic_net_paper_2$list_significative_variables[elastic_net_paper_2$list_significative_variables != "artificial_light_at_night_diff_1000_final"],
#     set_model = "zero inflated",
#     outcome = "h_visits",
#     name_facility_in_order = TRUE,
#     STD_variable = TRUE
#     )
# )

#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
#    EXECUTING REPORT - VARIABLE SELECTION TO PAPER 2
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
# , tar_target(
#   report_variable_selection_paper_2,
#   rmarkdown::render(
#       here::here(
#         #   "FC-manhica_child_adversity_data",Y:\MOZAMBIQUE\FC-manhica_child_adversity_data\reports\mapping_variables_with_geodata_v2
#           "reports",
#           "Templates",
#           "05-BASE_variable_selection",
#           "index.Rmd"),
#       params = list(
#         volcano_list_paper_2 = volcano_list_paper_2
#         , elastic_net_paper_2 = elastic_net_paper_2
#         , random_forest_paper_2 = random_forest_paper_2
#         , hierarchical_cluster_paper2 = hierarchical_cluster_paper2
#         ),
#       output_format = "all",
#       output_dir = here::here(
#         #   "FC-manhica_child_adversity_data",
#           "reports",
#           "reports-results",
#           "05-variable_selectio_paper_2_v7"
#           )
#   ))

#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
#    DATA TREATMEN FOR DIAGNOSIS DISTRIBUTION ANALYSIS - DDA
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
# Las distribuciones que veremos son las carrespondientes al paper
# por eso quitaremos tambien las que clasificamos como shame episode

,tar_target(
    diagnosis_raw_for_paper2,

    TFL_diagnosis_for_paper2(
        h_visits_outcoume_creation = h_visits_outcoume_creation,
        data_end_90_perce_distance_quantile_paper2 = data_end_90_perce_distance_quantile_paper2
    )
)

,tar_target(
    diagnosis_long_for_paper2,
    make_dx_long(
    df       = diagnosis_raw_for_paper2,
    diag_cols = paste0("diag", 1:4),
    lab_cols  = paste0("labdiag", 1:4),
    id_cols   = c("perm_id"),
    keep_cols = c("place_opd"
                 ,"date_event"
                 ,"respiratory"
                 ,"malaria"
                 ,"fever"
                 ,"bronchitis"
                 ,"uri"
                 ,"control"
                 ,"respiratory_all_respiratory")  # opcional
    )
)

####
# finsish
####

#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
#    PAPER 2 - VARIABLE SELECTION REPORT Version  2
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

########################################################################
########################################################################
#   New version 29 01 2025
########################################################################
########################################################################

# , tar_target(
#   report_variable_selection_paper_2_v2,
#   rmarkdown::render(
#       here::here(
#         #   "FC-manhica_child_adversity_data",Y:\MOZAMBIQUE\FC-manhica_child_adversity_data\reports\mapping_variables_with_geodata_v2
#           "reports",
#           "Templates",
#           "07-BASE_paper2_variable_selection_v2",
#           "index.Rmd"),
#       params = list(
#         volcano_list_paper_2 = volcano_list_paper_2
#         , volcano_list_paper_v2_direct_report_v2 = volcano_list_paper_v2_direct_report_v2
#         , model_significative_exWAS_normal_cut = model_significative_exWAS_normal_cut
#         , model_significative_exWAS_bonfe_cut = model_significative_exWAS_bonfe_cut
#         , model_significative_elasticNet = model_significative_elasticNet
#         , elastic_net_paper_2 = elastic_net_paper_2
#         , random_forest_paper_2 = random_forest_paper_2
#         , optima_k_per_subVariables_paper2_1 = optima_k_per_subVariables_paper2_1
#         , hierarchical_cluster_paper2_all = hierarchical_cluster_paper2_all
#         , kprototypes_cluster_paper2_all = kprototypes_cluster_paper2_all
#         , hierarchical_cluster_paper2_all_v2 = hierarchical_cluster_paper2_all_v2
#         , kprototypes_cluster_paper2_all_v2 = kprototypes_cluster_paper2_all_v2
#         , hierarchical_cluster_paper2_all_bigger_buff = hierarchical_cluster_paper2_all_bigger_buff
#         , kprototypes_cluster_paper2_all_bigger_buff = kprototypes_cluster_paper2_all_bigger_buff
#         , optimal_k_per_all_env_list_by_bigger_buff= optimal_k_per_all_env_list_by_bigger_buff,
#         , hierarchical_cluster_paper2_all_small_buff = hierarchical_cluster_paper2_all_small_buff
#         , kprototypes_cluster_paper2_all_small_buff = kprototypes_cluster_paper2_all_small_buff
#         , optimal_k_per_all_env_list_by_small_buff = optimal_k_per_all_env_list_by_small_buff
#         , optima_k_per_subVariables_paper2 = optima_k_per_subVariables_paper2
#         , hierarchical_cluster_paper2 = hierarchical_cluster_paper2
#         , kprototypes_cluster_paper2 = kprototypes_cluster_paper2
#         , optima_k_per_subVariables_paper2_0 = optima_k_per_subVariables_paper2_0
#         , hierarchical_cluster_paper2_norm = hierarchical_cluster_paper2_norm
#         , kprototypes_cluster_paper2_norm = kprototypes_cluster_paper2_norm
#         , optima_k_per_subVariables_paper2_2 = optima_k_per_subVariables_paper2_2
#         , hierarchical_cluster_paper2_ela = hierarchical_cluster_paper2_ela
#         , kprototypes_cluster_paper2_ela = kprototypes_cluster_paper2_ela
#         , data_paper_2_cluster_descriptive_bygroups = data_paper_2_cluster_descriptive_bygroups
#         ),
#       output_format = "all",
#       output_dir = here::here(
#         #   "FC-manhica_child_adversity_data",
#           "reports",
#           "reports-results",
#           "08-complete_version_w_different_buffer_list_v1"
#           )
#   ))


, tar_target(
  report_v7,
  rmarkdown::render(
      here::here(
        #   "FC-manhica_child_adversity_data",Y:\MOZAMBIQUE\FC-manhica_child_adversity_data\reports\mapping_variables_with_geodata_v2
          "reports",
          "Templates",
          "08-BASE_paper2_main_analysis",
          "index.Rmd"),
      params = list(
          volcano_list_paper_22 = volcano_list_paper_22
        , volcano_list_paper_v2_direct_report_v2 = volcano_list_paper_v2_direct_report_v2
        , model_significative_exWAS_normal_cut = model_significative_exWAS_normal_cut
        , model_significative_exWAS_bonfe_cut = model_significative_exWAS_bonfe_cut
        , model_significative_elasticNet = model_significative_elasticNet
        , elastic_net_paper_2 = elastic_net_paper_2
        , kprototypes_cluster_paper2_ela = kprototypes_cluster_paper2_ela

        # , kprototypes_cluster_paper2_all_small_buff = kprototypes_cluster_paper2_all_small_buff
        , kprototypes_cluster_paper2_ela_3 = kprototypes_cluster_paper2_ela_3
        # , kprototypes_cluster_paper2_ela_4 = kprototypes_cluster_paper2_ela_4
        # , kprototypes_cluster_paper2_ela_5 = kprototypes_cluster_paper2_ela_5
        # , kpototypes_cluster_paper2_ela_6 = kprototypes_cluster_paper2_ela_6
        # , kprototypes_cluster_paper2_ela_nolim = kprototypes_cluster_paper2_ela_nolim
        , rex = "1"
        ),
      output_format = "all",
      output_dir = here::here(
        #   "FC-manhica_child_adversity_data",
          "reports",
          "reports-results",
          "test - A - v2"
        #   "overview_v3-k2andk3 - A - v2"
          )
  ))



#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
#    EXECUTING REPORTE
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

,tar_target(
  report_parper_2_plots_to_summit_paper_1rt_time_v3, 
  rmarkdown::render(
      here::here(
        #   "FC-manhica_child_adversity_data",Y:\MOZAMBIQUE\FC-manhica_child_adversity_data\reports\mapping_variables_with_geodata_v2
          "reports",
          "Templates",
          "09-BASE_paper_2_plots_to_summit_paper",
          "index.Rmd"),
      params = list(  data_to_model_and_analise = data_to_model_and_analise
                    , bairros = bairros
                    , health_facilities = health_facilities
                    , roads = roads
                    # , table_ilumination_source_problematics = table_ilumination_source_problematics
                    , data_end_90_perce_distance_quantile_paper2 = data_end_90_perce_distance_quantile_paper2
                    , manhica = manhica
                    , districts = districts
                    , zona = zona_subRegion
                    , postos = postos
                    , model_significative_exWAS_normal_cut = model_significative_exWAS_normal_cut
                    , model_significative_exWAS_bonfe_cut = model_significative_exWAS_bonfe_cut
                    , data_paper_2_cluster_descriptive_bygroups = data_paper_2_cluster_descriptive_bygroups
                    , rex = "1"
        ),
      output_format = "all",
      output_dir = here::here(
        #   "FC-manhica_child_adversity_data",
          "reports",
          "reports-results",
        #   "overview_v3 - B"
          "test - B"
          )
  ))


#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
#    EXECUTING REPORTE - diagnosis paper 2
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

,tar_target(
  report_diad_paper2, 
  rmarkdown::render(
      here::here(
        #   "FC-manhica_child_adversity_data",Y:\MOZAMBIQUE\FC-manhica_child_adversity_data\reports\mapping_variables_with_geodata_v2
          "reports",
          "Templates",
          "11-BASE_tablas_para_REVIEW_PAPER_diag",
          "index.Rmd"),
      params = list(  
        rex = "4"
        , diagnosis_long_for_paper2 = diagnosis_long_for_paper2
        ),
      output_format = "all",
      output_dir = here::here(
        #   "FC-manhica_child_adversity_data",
          "reports",
          "reports-results",
        #   "overview_v3 - B"
          "24_diagnostics_summary_by_paper2_cohort"
          )
  ))


##########
##########
## save viznetwork
##########
########## recursive execution - doesnt is... s

# ,tar_target(
#   work_flow,
#   {htmltools::save_html(html = targets::tar_visnetwork(), file = "dag.html")}
# )


#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
#    TO DO: with others blocks 
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

####
# EXTRACT THE INFORMATION ON EXTERNALIZATION
####
# 1) we will need know the number of child that never change the hh
# 2) that childs have already changes, but is just because 
# DONE # they have cero days in the first house and de FU period is on othwer

# We will need know the many child are in this case

# MIRAR DIA 3 DE EEPE
# recurrents events
# 1) sacar la distribucion de visitas a hospitalest en los casos de DTH
# sacar la proporcion y ver si puede exisitir algun efecto
# aplicar un modelo de supervivencia con eventos recurrentes que 
# seran las visitas a los hospitales

###
# IMPORTANT THING
###
  # quit the kids that have visits on the hospital and no have 
  # fu days on the register!!!!

###
# FOR COMMON CARACTERISTICS - MAYBE
###

# The first step will be as we have already using the most common values at
# all the houses
# But maybe we can pondering the by FU on any house
# but maybe the same hous change between survey and then we will need work for period beetwen 
# survey insteat of FU periods.. 
##  

###
# FOR HOSPITAL VISITS 
###

  # We will analise if the DTH in cero day has a 1 visit on the hospital/s

  # we need to see if there is a visit hospital with the birth of the child..

  # how is afecting the cases tha we have a start of FU but
  # maybe they don have any visit to the hospital, because they
  # dont have acces or registier till some date
  # We can put a variable to determine this or what
  #


####
# FOR END DATA - TODO:
####

  # (A) SOLVE HEMCA FIELD
  # (B) RENAME THE FIELDS
  # (C)



#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
#    END OF PIPELINE
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

)

#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
#    GENERAL INFORMATION - about GENERAL DATA
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

##########
##########
## (1) - TABLES distribution
##########
##########

# On this secction we will have a brief description on the workflow data
# (a) field worker: they go to every house and 
# (b) tablets: the informations is inputed on the tables
# (c) data base DSSODK: are the data directly from the tables
# (d) there is a application where they treat the data and make some checks
#     - we have no acces to this step, but we are looking some way to -
# (e) data base OPENHDS: many more table that the before block. is treated.
#     list of main tables: death, individual, inmigration, location, outmigratioon
#       residency, round
#

##########
##########
## (2) - information about the relationship between the tables 
##########
##########

# on this secction we will note some caracteristics on the tables
# (a) at open hds. in migration and location the key is "extid"
# (b) between memebership and recidency - we need just take the recidency on
#     membership (errores que ni el soberano conoce).
# (c) at dssodk in pregnacy outcome the field gestional age is better not use
#     because they are comming on weeks and years and we cant know 
# (d) at dssodke - pregnacy outcome core: we can join wit cout out - with uri id
#     the field outcome is abt - aborth, brj - noraml birht, mis expontanbeus abort, sbr death born
# (e) at pregnacy outcome core - the birth place is coded awith the excel sended by TEOD
#     from 1 to 5 and 88 


##########
##########
## GENERAL information
##########
##########

# (a) the data is starting from the round 39.. first survey at 2016..
#     at this start they start a new system to save de data (2015)  before
#     of 2015 is no historical information.. they just update the information


# (b) DISTANCE CALCULATION: In this point to calculate the distance we will use
#     the hospital for that we have data.. because some of the hospital we
#     use in the past version are not present on opd data.


# htmltools::save_html(html = tar_visnetwork(), file = "dag.html")
