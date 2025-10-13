##' add function description
##'
##'
##' @title ADD_YOUR_FUNCTION_TITLE
##' @param data1 ADD_1rt_PARAMETER_DESCRIPTION
##' @param data2 ADD_2on_PARAMETER_DESCRIPTION
##' @param variable ADD_3r_PARAMETER_DESCRIPTION
##' @return data.frame / list / vector
##' @author Fabian Coloma
##' @export
##' @examples
##' 
##'
clean_location_information <- function(data){
  data  %>% as.data.frame(.) %>%
    mutate(
        bairro = ifelse(stringr::str_length(bairro) == 4, bairro, paste0("0", bairro))
    ) %>%
    select(
      neighborhood = bairro
      , common_name = nome_vulgar
      , posto_admnistrativo
      , locality = localidade
      , area
      , observations = OBSERVACAO
    )
}



##' add function description
##'
##'
##' @title clean_mother_info
##' @param data1 ADD_1rt_PARAMETER_DESCRIPTION
##' @param data2 ADD_2on_PARAMETER_DESCRIPTION
##' @param variable ADD_3r_PARAMETER_DESCRIPTION
##' @return data.frame / list / vector
##' @author Fabian Coloma
##' @export
##' @examples
##' 
##'
clean_mother_info <- function(data, perm_id_vector, min_mother_age, max_mother_age){

  data %>%
    filter(
      perm_id %in% perm_id_vector
    ) %>%
  dplyr::select(
    perm_id,
    dob,
    sex,
    mother_perm_id,
    mother_dob
  ) %>%
  dplyr::mutate(
    age_mother_at_birth_1 = as.numeric((dob - mother_dob)/365.25)
    )  %>%
  dplyr::filter(
    age_mother_at_birth_1 > 12,
    age_mother_at_birth_1 < 49
  )  %>%
   as.data.frame()

  #  mutate(
  #   mother_age_at_birth_of_child = (as.numeric(dob) - as.numeric(mother_dob)) / 365.25
  #  ) %>% as.data.frame() %>%
  #  filter(
  #   mother_age_at_birth_of_child <= max_mother_age,
  #   mother_age_at_birth_of_child >= min_mother_age
  #  )

}


##' add function description
##'
##'
##' @title clean_fu_data_from_scratch
##' @param data1 ADD_1rt_PARAMETER_DESCRIPTION
##' @param data2 ADD_2on_PARAMETER_DESCRIPTION
##' @param variable ADD_3r_PARAMETER_DESCRIPTION
##' @return data.frame / list / vector
##' @author Fabian Coloma
##' @export
##' @examples
##' 
##'
clean_fu_data_from_scratch <- function(data, perm_id_vector){
  data %>%
   filter((perm_id %in% perm_id_vector)) %>%
   mutate(region = strtrim(perm_id, 4)) %>%
   filter(out_perc < 1)

}

##' add function description
##'
##'
##' @title ADD_YOUR_FUNCTION_TITLE
##' @param data1 ADD_1rt_PARAMETER_DESCRIPTION
##' @param data2 ADD_2on_PARAMETER_DESCRIPTION
##' @param variable ADD_3r_PARAMETER_DESCRIPTION
##' @return data.frame / list / vector
##' @author Fabian Coloma
##' @export
##' @examples
##' 
##'
grouping_variable_by_house_features <- function(data){

  ##########
  ##########
  ## normal transformation
  ##########
  ##########

  data$coverage_material <- with(data, case_when(
    coverage_material %in% c("Concrete", "Tiles") ~ "Improved",
    coverage_material %in% c("Grass", "Lusalite sheet", "Zinc sheet", "Other") ~ "Unimproved")
  )

  data$wall_material_detail <- data$wall_material 
  
  data$wall_material <- with(data, case_when(
    wall_material %in% c("Cement blocks", "Bricks") ~ "Improved",
    wall_material %in% c("Wood/Zinc", "Adobe", "Bamboo/palm leafs", "Maticado sticks", "Other", "Paper bags") ~ "Unimproved")
  )

  data$floor_material <- with(data, case_when(
    floor_material %in% c("Cement", "Concrete", "Marmor", "Mosaic tiles") ~ "Improved",
    floor_material %in% c("Wood", "Adobe", "Nothing", "Other") ~ "Unimproved")
  ) 

  ##########
  ##########
  ## HEMCA - 
  ##########
  ##########

  data$sanitation_cat_HEMCA <- with(
    data, 
    dplyr::case_when(
      poo_treatment %in% c("Hire septic tank cleaning services") ~ "Improved",
      poo_treatment %in% c( "Close latrine hole and open another one", 
                            "Household member does cleaning", 
                            "Don't know", 
                            "Refused to answer", 
                            "no information") ~ "Unimproved",
      TRUE ~ "Unimproved" 
    )
  )


  ##########
  ##########
  ## No access Electricity
  ##########
  ##########

  data$no_acces_to_electricity_HEMCA <- with( data,
      dplyr::case_when(
        ilumination_fuel %in% c( "Electricity") ~ "Has access",
        ilumination_fuel %in% c( "Battery",
                                "Don't know",
                                "Candles",
                                "Generator",
                                "Wood",
                                "Kerosene",
                                "Gas",
                                "Solar panel") ~ "No has access",
    TRUE ~ "No has access")
      )

  data$kitchen_type <- with(
          data, 
          case_when(
              has_kitchen == "No" ~ "no kitchen",
              has_kitchen == "Yes" & 
              is_kitchen_inside ==  "Yes" ~ "inside",
              has_kitchen == "Yes" & 
              is_kitchen_inside == "No" ~ "outside",
              TRUE ~ "no kitchen"
          )
      )

  ##########
  ##########
  ## Cooking fuel 
  ##########
  ##########

  data$kitchen_fuel_compl <- data$kitchen_fuel

  data$kitchen_fuel_post_HEMCA <- with(data, 
  dplyr::case_when(
          kitchen_fuel %in% c("No fuel", "Electricity") ~ "Clean",
          kitchen_fuel %in% c("Firewood/timber", "Coal", "Gas") ~ "Polluting",
          kitchen_fuel %in% c("Don't know") ~ "Polluting",
        TRUE ~ "Polluting") # no cases on this data 010202024
        # TRUE ~ "Don't know") # no cases on this data 010202024
      )


  data$kitchen_fuel <- with(
        data, 
        case_when(
            kitchen_fuel %in% 
            c("No fuel", "Electricity") ~ "Clean",
            kitchen_fuel %in% 
            c("Coal", 
            "Firewood/timber",  
            "Gas",
            "Paraffin") ~ "Polluting",
         TRUE ~ "Polluting"
        )
    )

  data$ilumination_fuel_compl <- data$ilumination_fuel 

  data$ilumination_fuel <- with(data, 
    case_when(
        ilumination_fuel %in% c("Battery", 
                                "Electricity", 
                                "Solar panel") ~ "Clean",
        ilumination_fuel %in% c("Candles", 
                                "Generator", 
                                "Wood", 
                                "Kerosene", 
                                "Gas") ~ "Polluting",
        # ilumination_fuel %in% c("Don't know", "Don't know" ) ~ "Don't know"
        TRUE ~ "Don't know"
    )
  )


  data$water_source <- with(
        data,
        case_when(
            water_source %in% c("Piped water outside the house",
                                "Piped water inside the house",
                                "Fountain",
                                "Water well manual pump", 
                                "Well/hole dependent on a pump") ~ "Improved",
            water_source %in% c("River/lake/pond") ~ "Unimproved",
            # water_source %in% c("Don't know") ~ "Don't know",
            TRUE ~ "Unimproved"
        )
    )


  # data$is_water_src_inside

  data$wat_treatment_proc  <-  with(
        data, 
        case_when(
            wat_treatment_proc %in% 
            c(
                "Boil water",
                "Cholorination") ~ "Improved",
            wat_treatment_proc %in% 
            c(
                "Filtration", 
                "No treatment",
                "Traditional methods") ~ "Unimproved",
                # wat_treatment_proc %in% c("Don't know") ~ "Don't know",
                # wat_treatment_proc %in% c("no information") ~ "no information",
                TRUE ~ "Unimproved"
        )
    )

  # data$has_latrine

  data$latrine_type <- with(
        data, 
        case_when(
            latrine_type %in% 
            c(  "Toilet with septic tank",
                "Improved latrine",
                "Improved traditional latrine") ~ "Improved",
            latrine_type %in% 
            c("Not-improved traditional latrine", "other") ~ "Unimproved",
                # latrine_type %in% c("Don't know") ~ "Don't know",
                # latrine_type %in% c("no information") ~ "no information",
                TRUE ~ "Unimproved"
        )
    )

  data$poo_treatment <- with(
        data,
        case_when(
            poo_treatment %in% 
            c(
                "Hire septic tank cleaning services") ~ "Improved",
            poo_treatment %in% 
            c(
                "Close latrine hole and open another one",
                "Household member does cleaning",
                "Refused to answer") ~ "Unimproved",
            # poo_treatment %in% c("Don't know", "Refused to answer") ~ "Don't know",
            # poo_treatment %in% c("no information") ~ "no information",
            TRUE ~ "Unimproved"
        )
    )

 data$trash_treatment <- with(
        data, 
        case_when(
            trash_treatment %in% 
            c(
                "Collected by municipality",
                "Partly recycled") ~ "Improved",
            trash_treatment %in% 
            c(
                "Burned",
                "Burried",
                "Deposited in an improved trash deposit site in HH yard",
                "Left in the open",
                "Used as fertilizer",
                "Other") ~ "Unimproved",
            # trash_treatment %in% c("no information") ~ "no information",
            TRUE ~ "Unimproved"
        )
    )

#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
#    Environtmen part 2 - HE creation
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

##########
##########
## unimproved hh material 
##########
##########
data$unimproved_hh_material_HEMCA <-  with( 
  data,
  dplyr::case_when(
    (coverage_material == "Unimproved") | 
    (wall_material == "Unimproved") | 
    (floor_material == "Unimproved") ~ "Unimproved",
    TRUE ~ "Improved")
  )


# # water_treatment
# data$wat_treatment_proc_cat_HEMCA <- with(data, case_when(
#   wat_treatment_proc %in% c("Boil water", "Cholorination") ~ "Improved",
#   wat_treatment_proc %in% c("Don't know", "Filtration", "No treatment", "Traditional methods", "no information") ~ "Unimproved"))






#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
#    finish
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

  data %>%
    select(
      -has_kitchen 
      ,-is_kitchen_inside
    )

}


##' add function description
##'
##'
##' @title ADD_YOUR_FUNCTION_TITLE
##' @param data1 ADD_1rt_PARAMETER_DESCRIPTION
##' @param data2 ADD_2on_PARAMETER_DESCRIPTION
##' @param variable ADD_3r_PARAMETER_DESCRIPTION
##' @return data.frame / list / vector
##' @author Fabian Coloma
##' @export
##' @examples
##' 
##'
check_info_HH_on_sys_info <- function(data_expanded){
 
  d_uniq <- data_expanded %>%
        select(
                house_number
                , perm_id
                , start_fu_date
                , end_fu_date
                , check_sensi
        ) %>% 
        group_by(
                house_number
                , perm_id
                , start_fu_date
                , end_fu_date
        ) %>%
        summarise(
                 flag_between_dates = any(check_sensi == "between_dates", na.rm = TRUE)
                ,flag_3months_plus = any(check_sensi %in% c("between_dates"
                                                            ,"between_3months_plus"), na.rm = TRUE)
                ,flag_6months_plus = any(check_sensi %in% c("between_dates"
                                                            ,"between_3months_plus"
                                                            ,"between_6months_plus") , na.rm = TRUE)
                ,flag_1yr_plus = any(check_sensi %in% c("between_dates"
                                                            ,"between_3months_plus"
                                                            ,"between_6months_plus"
                                                            ,"between_1yr_plus") , na.rm = TRUE)
                ,flag_1_5yr_plus = any(check_sensi %in% c("between_dates"
                                                            ,"between_3months_plus"
                                                            ,"between_6months_plus"
                                                            ,"between_1yr_plus"
                                                            ,"between_1_5yr_plus") , na.rm = TRUE)
                ,flag_2yr_plus = any(check_sensi %in% c("between_dates"
                                                            ,"between_3months_plus"
                                                            ,"between_6months_plus"
                                                            ,"between_1yr_plus"
                                                            ,"between_1_5yr_plus"
                                                            ,"between_2yr_plus"), na.rm = TRUE)
                ,flag_all_no_survey = all(check_sensi == "no_surveys_on_period", na.rm = TRUE)
        )

  d_uniq

}


##' add function description
##'
##'
##' @title clean_household_and_deploy
##' @param data ADD_1rt_PARAMETER_DESCRIPTION
##' @param interval_data ADD_2on_PARAMETER_DESCRIPTION
##' @param variable ADD_3r_PARAMETER_DESCRIPTION
##' @return data.frame / list / vector
##' @author Fabian Coloma
##' @export
##' @examples
##' 
##'
clean_interval_data_and_deploy <- function(data, interval_data, id_1, id_2, field_date){
  # cleanHouseData( ) # how apply it ???

  data <- cleanHouseData(data, id_field = {{id_1}})
  
  # then scratch
  print("some")

  data_expanded <- data %>%
    inner_join(
      interval_data,
      by = join_by({{id_1}} == {{id_2}}),
      relationship = "many-to-many"
    ) %>%
    mutate(
      check_sensi = case_when(
        between(start,
                start_fu_date, 
                end_fu_date) ~ "between_dates",
        between(start, 
                start_fu_date - 365.25 / 4, 
                end_fu_date + 365.25 / 4) ~ "between_3months_plus",
        between(start, 
                start_fu_date - 365.25 / 2, 
                end_fu_date + 365.25 / 2) ~ "between_6months_plus",
        between(start, 
                start_fu_date - 365.25, 
                end_fu_date + 365.25) ~ "between_1yr_plus",
        between(start, 
                start_fu_date - 365.25 * 1.5, 
                end_fu_date + 365.25 * 1.5) ~ "between_1_5yr_plus",
        between(start, 
                start_fu_date - 365.25 * 2, 
                end_fu_date + 365.25 * 2) ~ "between_2yr_plus",
        TRUE ~ "no_surveys_on_period"
      ),
      check_sensi_numeric = case_when(
        between(start,
                start_fu_date, 
                end_fu_date) ~ 0,
        between(start, 
                start_fu_date - 365.25 / 4, 
                end_fu_date + 365.25 / 4) ~ 1,
        between(start, 
                start_fu_date - 365.25 / 2, 
                end_fu_date + 365.25 / 2) ~ 2,
        between(start, 
                start_fu_date - 365.25, 
                end_fu_date + 365.25) ~ 3,
        between(start, 
                start_fu_date - 365.25 * 1.5, 
                end_fu_date + 365.25 * 1.5) ~ 4,
        between(start, 
                start_fu_date - 365.25 * 2, 
                end_fu_date + 365.25 * 2) ~ 5,
        TRUE ~ 99
      )
    )
  
  data_expanded

}


cleanSurveyData <- function(data) {
  ## There are some IDs with multiple different DOBs
  ## TODO Should we exclude everyone with several DOBs or just children?
  multiple_dobs <- data %>%
    select(perm_id, dob) %>%
    distinct() %>%
    count(perm_id) %>%
    filter(n > 1) %>%
    arrange(desc(n))

  data %>%
    filter(!perm_id %in% multiple_dobs$perm_id) %>%
    mutate(
      perm_id = ifelse(perm_id == "9999-999-99", NA, perm_id),
      father_perm_id = ifelse(father_perm_id == "UNK", NA, father_perm_id),
      mother_perm_id = ifelse(mother_perm_id == "UNK", NA, mother_perm_id)
    )
}

##' Remove individuals with date of birth after the start of followu up and
##' individuals with multiple birth entry records.
##'
##' @title Clean residential history data
##' @param data Target data frame res_history_raw.
##' @return A tibble.
##' @author Sergio Olmos
cleanResidentialHistoryData <- function(data) {
  ## NOTE There are a 4 ids with multiple birth entry records.
  multiple_birth_records <- data %>%
    group_by(perm_id) %>%
    summarise(birth_records = sum(start_type == "BIR")) %>%
    arrange(desc(birth_records)) %>%
    filter(birth_records > 1)

  data %>%
    filter(
      start_date <= end_date | is.na(end_date),
      !perm_id %in% multiple_birth_records$perm_id
    ) %>%
    arrange(perm_id, start_date, start_type) %>%
    split(., .$perm_id) %>%
    map_df(~rownames_to_column(., var = "period_ordinal")) %>%
    mutate(period_ordinal = as.integer(period_ordinal))
}

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title Clean raw hospital visits data
##' @param data Target data frame h_visits_raw
##' @return
##' @author Sergio Olmos
# cleanHospitalVisitsData <- function(data) {
#   h_visits_clean <- data %>%
#     select(perm_id, date_birth, date, seas, matches("diag")) %>%
#     filter(perm_id != "") %>%
#     mutate(across(matches("diag"), trimws)) %>%
#     mutate(
#       across(matches("diag"), ~ifelse(.x %in% c(".", ""), NA, .x)),
#       across(matches("labdiag"), trimws)
#     ) %>%
#     mutate(across(matches("^diag[1-4]$"), ~ifelse(.x == "-9", NA, .x))) %>%
#     rename(date_event = date)

#   h_visits_clean
# }
cleanHospitalVisitsData <- function(data, vStart, vEnd) {

  h_visits_clean <- data %>%
    select(
      perm_id,
      place_opd,
      date_birth, 
      date_dth,
      date, 
      seas,
      weigth,
      matches("diag")
    ) %>%
    filter(
      perm_id != "",
      date >= vStart ,
      date < vEnd
    ) %>%
    mutate(
      across(
        matches("diag"),
        trimws
      )
    ) %>%
    mutate(
      across(
        matches("diag"),
        ~ifelse(
          .x %in% c(".", ""),
          NA,
          .x
        )
      ),
      across(
        matches("labdiag"),
        trimws
      )
    ) %>%
    mutate(
      across(
        matches(
          "^diag[1-4]$"
        ), 
        ~ifelse(
          .x == "-9", 
          NA, 
          .x
        )
      )
    ) %>%
    rename(
      date_event = date
    ) %>%
    arrange(perm_id, date_event) %>%
    group_by(perm_id) %>%
    mutate(
        date_lag1 = lag(date_event),
        d_diff = as.integer(as.Date(date_event) - as.Date(date_lag1)),
        across(matches("^labdiag[1-4]$"), ~.x, .names = "{.col}_current"),
        across(matches("^labdiag[1-4]$"), lag, .names = "{.col}_lag")
    )

  ## Check if there is more than one occurrance of
  ## the same diagnostic code in each row.

  h_visits_clean$diag_matches <- h_visits_clean %>%
    select(ends_with("_current") | ends_with("_lag")) %>%
    apply(MARGIN = 1, function(x) ifelse(sum(table(x) > 1) > 0, TRUE, FALSE))



  h_visits_clean %>%
    mutate(
      start = date_event
    )
}



##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title Clean raw hospital admissions data
##' @param data Target data frame h_admissions_raw returned by \code{readHospitalAdmissions()}.
##' @return A tibble.
##' @author Fabian Coloma
check_sameEpisodes <- function(data, n_days_same = 2){
  
  data %>%
    mutate(
      same_episode = ifelse(d_diff < n_days_same & diag_matches, TRUE, FALSE)
      ,same_episode3 = ifelse(d_diff <= 3 & diag_matches, TRUE, FALSE)
      ,same_episode4 = ifelse(d_diff <= 4 & diag_matches, TRUE, FALSE)
      ,same_episode5 = ifelse(d_diff <= 5 & diag_matches, TRUE, FALSE)
    )

}



##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title Clean raw hospital admissions data
##' @param data Target data frame h_admissions_raw returned by \code{readHospitalAdmissions()}.
##' @return A tibble.
##' @author Sergio Olmos
cleanHospitalAdmissionsData <- function(data) {
  h_admissions_clean <- data %>%
    select(perm_id, date_birth, date_visit, seas, matches("diag")) %>%
    ## No missing perm_ids in this dataset
    filter(perm_id != "") %>%
    mutate(
      across(matches("diag"), ~ifelse(.x %in% c(".", ""), NA, .x)),
      across(matches("labdiag"), trimws)
    ) %>%
    rename(date_event = date_visit)

  h_admissions_clean
}

##' Adds a variable indicating the order in time of each visit for all individuals
##' and removes houses which have several records on the same day.
##'
##' @title Clean house characteristics panel data
##' @param data Data frame as returned by readHouseData().
##' @return A tibble.
##' @author Sergio Olmos
cleanHouseData <- function(data, id_field = "house_number") {
  ## Several records of the same house number on the same day

  houses_clean <- data %>%
        group_by(!!! rlang::syms(id_field), start) %>%
        mutate(
            n_row_visit = row_number()
        ) 

  che <- houses_clean %>% 
            filter(n_row_visit > 1 ) %>%
            nrow() 

  if(che == 0 ){
    print("good can continue")
  }else{

   print("not good at:")
   print(che)
   print("cases")

   houses_clean <- houses_clean %>%
    filter(n_row_visit <= 1)

 
  }

  houses_clean

}

# cleanHouseData <- function(data, id_field = "house_number") {
#   ## Several records of the same house number on the same day

#   houses_clean <- data %>%
#         group_by({{id_field}}, start) %>%
#         mutate(
#             n_row_visit = row_number()
#         ) 

#   che <- houses_clean %>% 
#             filter(n_row_visit > 1 ) %>%
#             nrow() == 0
#   if(che){
#     print("good can continue")
#   }else{
#   mult_visit_ids <- data %>%
#     distinct() %>%
#     count({{id_field}}, start) %>%
#     arrange(desc(n)) %>%
#     filter(n > 1) %>%
#     left_join(
#       data %>% select({{id_field}}, start, visit_id),
#       by = c(id_field, "start")
#     ) %>%
#     distinct()

#   houses_clean <- data %>%
#     ## NOTE We will remove these houses until we figure out what this means
#     filter(!visit_id %in% mult_visit_ids$visit_id) %>%
#     arrange({{id_field}}, start)

 
#   }

#   houses_clean

# }

cleanEconAssetsData <- function(data) {
  ## Several records of the same house number on the same day
  mult_visit_ids <- data %>%
    distinct() %>%
    count(house_number, visit_date) %>%
    arrange(desc(n)) %>%
    filter(n > 1) %>%
    left_join(
      data %>% select(house_number, visit_date, visit_id),
      by = c("house_number", "visit_date")
    ) %>%
    distinct()

  econ_assets_clean <- data %>%
    ## NOTE We will remove the visits which had several entries
    filter(!visit_id %in% mult_visit_ids$visit_id) %>%
    arrange(house_number, visit_date) %>%
    split(., .$house_number) %>%
    map_df(~rownames_to_column(., var = "visit_ordinal")) %>%
    mutate(visit_ordinal = as.integer(visit_ordinal))

  econ_assets_clean
}

cleanDeathData <- function(data) {
  repeated_death_records <- data %>%
    count(perm_id) %>%
    filter(n > 1) %>%
    arrange(desc(n))

  death_duplicated <- data %>%
    filter(perm_id %in% repeated_death_records$perm_id) %>%
    arrange(perm_id) %>%
    select(perm_id, date_of_death)

  inconsistent_death_dates <- death_duplicated %>%
    group_by(perm_id) %>%
    summarise(
      death_date = date_of_death,
      death_date_lag = lag(date_of_death),
      ddiff = as.integer(death_date - death_date_lag)
    ) %>%
    drop_na() %>%
    filter(ddiff > 0)

  data %>%
    filter(!perm_id %in% inconsistent_death_dates$perm_id) %>%
    select(perm_id, date_of_death) %>%
    distinct()
}
