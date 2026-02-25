load_variables_names_to_match <- function(){
    read_csv2(
        "/PROJECTES/AIRPOLLUTION/MOZAMBIQUE/ANALYSIS/FC_manhica-child-adversity-data-paper2-v2/mapeado_var_names.csv"
        #         "Y:/MOZAMBIQUE/ANALYSIS/FC-manhica_child_adversity_data_paper2_v2/mapeado_var_names_v5.csv"
    )
}

# load_variables_names_to_match <- function(){
#     read_csv2(
#         "Y:/MOZAMBIQUE/ANALYSIS/FC-manhica_child_adversity_data_paper2_v2/mapeado_var_names_v5.csv"
#     )
# }


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
read_health_information <- function(file){
  read_excel(
    file
    ) %>%
    st_as_sf(
      coords = c("lng", "lat"),
      crs = 4326
    ) %>%
    st_transform(
      crs = 32736
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
read_urban_lc_DIR <- function(file, filter_geo){
    lc <- raster::raster(file)
    lc <- raster::crop(lc, as(st_transform(st_buffer(filter_geo, 100), crs = 4326), "Spatial"))
    lc <- raster::projectRaster(lc, crs = 32736, method = 'ngb')
    lc <- lc==50
    lc[lc == 0] <- NA
    lc <- raster::mask(lc, filter_geo)
    lc_vector <- raster::rasterToPolygons(lc, fun=function(x){x==1})
    st_as_sf(lc_vector)
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
read_shp_and_filter_GIS <- function(file, filter_geo){
   st_read(file) %>%
      st_filter(filter_geo)
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
read_other_hospitals_GIS <- function(){
      st_sfc(st_point(c(32.7741157096856, -25.4565799963245)),
                        st_point(c(32.8004980190196, -25.1573413834126)), 
                        st_point(c(32.9173584833383, -25.093667224647)),
                        st_point(c(32.8521480905196, -25.179130227974)), 
                        st_point(c(32.8647094523748, -25.2624062874067)),
                        st_point(c(32.9335714856778, -25.3530765365982)), # --- no estabnan
                        st_point(c(32.6524315492037, -25.4961935432286)),
                        st_point(c(32.8330052893818, -25.309098773572)),
                        st_point(c(32.7262389927729, -25.5103831457688)),
                        st_point(c(32.7404001673921, -25.287101555361)),
                        crs = 4326) %>% 
      st_transform(crs = 32736) # WGS84 UTM 36S   
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
read_main_hospital_GIS <- function(){

    pre_main_hosp <- st_sfc(st_point(c(32.8066889822971, -25.4084486112516)),
                        st_point(c(32.8048308667543, -25.0469168919434)),
                        crs = 4326) %>% 
      st_transform(crs = 32736) %>%   # WGS84 UTM 36S
      st_as_sf()

    pre_main_hosp$name <- c("Hospital Distrital de Manhiça", "Xinavane Rural Hospital")

    pre_main_hosp
}

##' add function description
##'
##'
##' @title read_sugar_facotry_GIS
##' @param data1 ADD_1rt_PARAMETER_DESCRIPTION
##' @param data2 ADD_2on_PARAMETER_DESCRIPTION
##' @param variable ADD_3r_PARAMETER_DESCRIPTION
##' @return data.frame / list / vector
##' @author Fabian Coloma
##' @export
##' @examples
##' 
##'
read_sugar_facotry_GIS <- function(){
      st_sfc(st_point(c(32.80100697217291, -25.044791418221653)),
                    st_point(c(32.77855499206556, -25.449369616224306)),
                    crs = 4326) %>% 
      st_transform(crs = 32736) # WGS84 UTM 36S
}


##' add function description
##'
##'
##' @title read_apstation_GIS
##' @param data1 ADD_1rt_PARAMETER_DESCRIPTION
##' @param data2 ADD_2on_PARAMETER_DESCRIPTION
##' @param variable ADD_3r_PARAMETER_DESCRIPTION
##' @return data.frame / list / vector
##' @author Fabian Coloma
##' @export
##' @examples
##' 
##'
read_apstation_GIS <- function(){
  st_sfc( st_point(c(32.80575511120886, -25.408570467455732)), 
          crs = 4326
        ) %>% 
    st_transform(crs = 32736) # WGS84 UTM 36S
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
read_variables_fam_inestability <- function(){
  c(
    "father_alive",
    "mother_alive",
    "marital_status",
    "mother_lives_here",
    "father_lives_here"
  )
}
##' add function description
##'
##'
##' @title read_inv_edu_ocu_info
##' @param data1 ADD_1rt_PARAMETER_DESCRIPTION
##' @param data2 ADD_2on_PARAMETER_DESCRIPTION
##' @param variable ADD_3r_PARAMETER_DESCRIPTION
##' @return data.frame / list / vector
##' @author Fabian Coloma
##' @export
##' @examples
##' 
##'
read_family_inestability <- function(file){

 dd <-  read_csv(
    file,
    col_types = cols(
      perm_id = col_character(),
      father_alive = col_character(),
      marital_status = col_character(),
      mother_alive = col_character(),
      mother_lives_here = col_character(),
      father_lives_here = col_character(),
      start = col_date("%Y-%m-%d %H:%M:%S")
    )
    ) %>% as.data.frame() %>%
    dplyr::mutate(

      father_alive =  ifelse(is.na(father_alive), "999", father_alive),

      father_alive = recode_factor(
        father_alive,
        `1` = "yes",
        `2` = "no",
        `99` = "Refused to answer",
        `999` = "miss_information"

      ),

       marital_status = ifelse(is.na(marital_status), "999", marital_status),
      marital_status = recode_factor(
        marital_status,
        `1` = "Single never lived in a marital relationship",
        `2` = "Married",
        `3` = "Common-law union",
        `4` = "Divorced",
        `5` = "Separated",
        `6` = "Widowed",
        `999` = "miss_information"
      ),


      mother_alive =  ifelse(is.na(mother_alive), "9999", mother_alive),
      mother_alive =  recode_factor(
        mother_alive,
            `2` = "No",
            `1` = "Yes",
            # `88` = "Doesn't know",
            `99` = "Refused to answer",
            `9999` =  "miss_info"
      ),

      mother_lives_here =  ifelse(is.na(mother_lives_here), "9999", mother_lives_here),
      mother_lives_here =  recode_factor(
        mother_lives_here,
            `2` = "No",
            `1` = "Yes",
            # `88` = "Doesn't know",
            # `99` = "Refused to answer",
            `9999` =  "miss_info"
      ),
      father_lives_here =  ifelse(is.na(father_lives_here), "9999", father_lives_here),
      father_lives_here =  recode_factor(
        father_lives_here,
            `2` = "No",
            `1` = "Yes",
            # `88` = "Doesn't know",
            # `99` = "Refused to answer",
            `9999` =  "miss_info"
      )

    )

}


##' add function description
##'
##'
##' @title read_location_information
##' @param data1 ADD_1rt_PARAMETER_DESCRIPTION
##' @param data2 ADD_2on_PARAMETER_DESCRIPTION
##' @param variable ADD_3r_PARAMETER_DESCRIPTION
##' @return data.frame / list / vector
##' @author Fabian Coloma
##' @export
##' @examples
##' 
##'
read_location_information <- function(file){
  read_csv(
    file,
    col_types = cols(
      bairro = col_character(),
      nome_vulgar = col_character(),
      posto_admnistrativo = col_character(),
      localidade = col_character(),
      area = col_character(),
      OBSERVACAO = col_character()
    )
  )
}

read_variables_inv_edu_ocu <- function(){
  c(
    "has_education",
    "education",
    "ocupation"

  )
}
##' add function description
##'
##'
##' @title read_inv_edu_ocu_info
##' @param data1 ADD_1rt_PARAMETER_DESCRIPTION
##' @param data2 ADD_2on_PARAMETER_DESCRIPTION
##' @param variable ADD_3r_PARAMETER_DESCRIPTION
##' @return data.frame / list / vector
##' @author Fabian Coloma
##' @export
##' @examples
##' 
##'
read_inv_edu_ocu_info <- function(file){

  read_csv(
    file,
    col_types = cols(
      perm_id = col_character(),
      has_education = col_character(),
      education = col_character(),
      ocupation = col_character(),
      secundary_ocupation = col_character(),
      vist_date = col_date("%Y-%m-%d %H:%M:%S")
    )
    ) %>%
    mutate(
      start = as.Date(format(vist_date, "%Y-%m-%d")),
      has_education =  ifelse(is.na(has_education), "999", has_education),
      has_education = recode_factor(
        has_education,
        `1` = "yes",
        `2` = "no",
        `999` = "miss_information"

      ),
      education = ifelse(is.na(education), "999", education),
      education = recode_factor(
        education,
        `1` = "no education",
        `2` = "Primary",
        `3` = "Primary",
        `4` = "Primary",
        `5` = "Primary",
        `6` = "Primary",
        `7` = "Primary",
        `8` = "Primary",
        `9` = "Primary",
        `10` = "Primary",
        `11` = "Primary",
        `12` = "Primary",
        `13` = "Primary",
        `14` = "Primary",
        `15` = "Primary",
        `16` = "Primary",
        `17` = "Secondary or tertiary",
        `18` = "Secondary or tertiary",
        `19` = "Secondary or tertiary",
        `20` = "Secondary or tertiary",
        `21` = "Secondary or tertiary",
        `22` = "Secondary or tertiary",
        `23` = "Secondary or tertiary",
        `98` = "no_information",
        `88` = "no_information",
        `99` = "no_information",
        `999` = "no_information"
      ),

      ocupation = ifelse(is.na(ocupation), "999", ocupation),
      ocupation = recode_factor(
        ocupation,
        `1` = "Farmer",
        `2` = "Fisherman",
        `3` = "Charcoal maker/Woodcutter",
        `4` = "Fixed vendors",
        `5` = "fix vendors",
        `6` = "Street vendors",
        `7` = "Guards/Security personnel",
        `8` = "Healthcare personnel",
        `9` = "Student",
        `10` = "Teacher",
        `11` = "Military/Police",
        `12` = "Carpenter/Electrician",
        `13` = "Locksmith/Mason",
        `14` = "Disabled",
        `15` = "Volunteer",
        `16` = "Religious",
        `17` = "Retired/Pensioner",
        `18` = "Domestic worker",
        `19` = "Worker in RSA",
        `20` = "Miner in RSA",
        `21` = "Worker in Xinavane Sugar Factory",
        `22` = "Worker in Maragra Sugar Factory",
        `23` = "Unemployed",
        `98` = "Other",
        `999` = "No information")
    )

}

##' add function description
##'
##'
##' @title read_mother_info
##' @param data1 ADD_1rt_PARAMETER_DESCRIPTION
##' @param data2 ADD_2on_PARAMETER_DESCRIPTION
##' @param variable ADD_3r_PARAMETER_DESCRIPTION
##' @return data.frame / list / vector
##' @author Fabian Coloma
##' @export
##' @examples
##' 
##'
read_mother_info <- function(file){

  read_csv(
    file,
    col_types = cols(
      perm_id = col_character(),
      dob = col_date("%Y-%m-%d"),
      sex = col_factor(c("M", "F")),
      mother_perm_id = col_character(),
      mother_dob = col_date("%Y-%m-%d")
    )
  ) 

}


##' add function description
##'
##'
##' @title extract_econ_mca_names
##' @param data1 ADD_1rt_PARAMETER_DESCRIPTION
##' @param data2 ADD_2on_PARAMETER_DESCRIPTION
##' @param variable ADD_3r_PARAMETER_DESCRIPTION
##' @return data.frame / list / vector
##' @author Fabian Coloma
##' @export
##' @examples
##' 
##'
extract_econ_mca_names <- function(){
 
  c( "house_number",
    "has_tv",
    "has_radio",
    "has_dvd", 
    "has_computer",
    "has_bike",
    "has_farm_of_commercial_production",
    "has_freezer",
    "has_glacier",
    "has_cattle",
    "has_goat",
    "has_pigs",
    "has_moto",
    "has_car",
    "has_celular",
    "has_telephone",
    "has_tractor",
    "gps_longitude",
    "gps_latitude",
    "gps_accuracy" )

}


##' add function description
##'
##'
##' @title read_variables_features_houses
##' @param data1 ADD_1rt_PARAMETER_DESCRIPTION
##' @param data2 ADD_2on_PARAMETER_DESCRIPTION
##' @param variable ADD_3r_PARAMETER_DESCRIPTION
##' @return data.frame / list / vector
##' @author Fabian Coloma
##' @export
##' @examples
##' 
##'
read_variables_features_houses <- function(){
  c(
    "region", "coverage_material", "wall_material_detail"
    ,"wall_material"       ,"floor_material"      ,"kitchen_type"   
    ,"kitchen_fuel"        ,"ilumination_fuel"   
    ,"water_source"        ,"is_water_src_inside" ,"wat_treatment_proc"
    ,"has_latrine"         ,"latrine_type"        ,"poo_treatment"
    ,"trash_treatment"     ,"gps_longitude"       ,"gps_latitude"
    ,"gps_accuracy"        ,"check_sensi", "ilumination_fuel_compl", "kitchen_fuel_compl",
"unimproved_hh_material_HEMCA", "sanitation_cat_HEMCA", "no_acces_to_electricity_HEMCA", 
"kitchen_fuel_post_HEMCA", "house_number"

  )

}


readChildrenInfo <- function(file) {
  read_csv(
    file,
    col_types = cols(
      perm_id = col_character(),
      dob = col_date("%Y-%m-%d %H:%M:%S"),
      gender = col_factor(c("M", "F")),
      father_perm_id = col_character(),
      mother_perm_id = col_character()
    )
  )
}

readChildrenInfo_populacao <- function(file) {
  read_csv(
    file,
    col_types = cols(
      perm_id = col_character(),
      dob = col_date("%Y-%m-%d"),
      gender = col_factor(c("M", "F")),
      father_perm_id = col_character(),
      mother_perm_id = col_character()
    ),
    na = c("", "NA", "UNKNOWN")
  )
}

readChildrenInfo_member<- function(file) {
  read_csv(
    file,
    col_types = cols(
      `uuid...1` = col_character()
      ,`deleted...2` = col_integer()
      ,`insertDate...3` = col_date("%Y-%m-%d")
      ,`voidDate...4` = col_date("%Y-%m-%d")
      ,`voidReason...5` = col_character()
      ,`dob` = col_date("%Y-%m-%d")
      ,`dobAspect` = col_character()
      ,`permId` = col_character()
      ,`name` = col_character()
      ,`gender` = col_factor(c("F", "M"))
      ,`insertBy_uuid...11` = col_character()
      ,`voidBy_uuid...12` = col_character()
      ,`collectedBy_uuid...13` = col_character()
      ,`father_uuid` = col_character()
      ,`mother_uuid` = col_character()
      ,`uuid...16` = col_character()
      ,`deleted...17`= col_number()
      ,`insertDate...18` = col_date("%Y-%m-%d")
      ,`voidDate...19` = col_date("%Y-%m-%d")
      ,`voidReason...20` = col_character()
      ,`fatherDeathDate` = col_date("%Y-%m-%d")
      ,`motherDeathDate` = col_date("%Y-%m-%d")
      ,`education` = col_character()
      ,`maritalStatus` = col_character()
      ,`religion` = col_character()
      ,`ocupation` = col_character()
      ,`relationshipWithHead` = col_character()
      ,`insertBy_uuid...28` = col_character()
      ,`voidBy_uuid...29` = col_character()
      ,`collectedBy_uuid...30` = col_character()
      ,`individual_uuid` = col_character()
      ,`spouse_uuid` = col_character()
    ),
    na = c("", "NA", "UNKNOWN", "N/A")
  ) %>% 
    mutate(
      individual_id = uuid...1,
      education = ifelse(is.na(education), "99", education),
      education = recode_factor(
        education,
        `1` = "no education",
        `2` = "Primary",
        `3` = "Primary",
        `4` = "Primary",
        `5` = "Primary",
        `6` = "Primary",
        `7` = "Primary",
        `8` = "Primary",
        `9` = "Primary",
        `10` = "Primary",
        `11` = "Primary",
        `12` = "Primary",
        `13` = "Primary",
        `14` = "Primary",
        `15` = "Primary",
        `16` = "Primary",
        `17` = "Secondary or tertiary",
        `18` = "Secondary or tertiary",
        `19` = "Secondary or tertiary",
        `20` = "Secondary or tertiary",
        `21` = "Secondary or tertiary",
        `22` = "Secondary or tertiary",
        `23` = "Secondary or tertiary",
        `98` = "Primary",
        `88` = "Primary",
        `99` = "no education",
        `999` = "no_information"
        

        #         `1` = "Illiterate/None/Not schooled",
        # `2` = "Literacy 1",
        # `3` = "Literacy 2",
        # `4` = "Literacy 3",
        # `5` = "Grade 1",
        # `6` = "Grade 2",
        # `7` = "Grade 3",
        # `8` = "Grade 4",
        # `9` = "Grade 5",
        # `10` = "Grade 6",
        # `11` = "Grade 7",
        # `12` = "Grade 8",
        # `13` = "Grade 9",
        # `14` = "Grade 10",
        # `15` = "Grade 11",
        # `16` = "Grade 12",
        # `17` = "Elementary technical education",
        # `18` = "Basic technical education",
        # `19` = "Intermediate technical education",
        # `20` = "Bachelor's degree",
        # `21` = "degree",
        # `22` = "Master's degree",
        # `23` = "Doctorate",
        # `98` = "Other",
        # `88` = "Don't know",
        # `99` = "No information"

        # `1` = "Analfabeto / Nenhuma / Nao escolarizado",
        # `2` = "Alfabetizacao 1",
        # `3` = "Alfabetizacao 2",
        # `4` = "Alfabetizacao 3",
        # `5` = "1 Classe",
        # `6` = "2 Classe",
        # `7` = "3 Classe",
        # `8` = "4 Classe",
        # `9` = "5 Classe",
        # `10` = "6 Classe",
        # `11` = "7 Classe",
        # `12` = "8 Classe",
        # `13` = "9 Classe",
        # `14` = "10 Classe",
        # `15` = "11 Classe",
        # `16` = "12 Classe",
        # `17` = "Ensino tecnico elementar",
        # `18` = "Ensino tecnico basico",
        # `19` = "Ensino tecnico medio",
        # `20` = "Bacharel",
        # `21` = "Licenciatura",
        # `22` = "Mestrado",
        # `23` = "Doutoramento",
        # `98` = "Outro",
        # `88` = "Nao sabe",
        # `99` = "no info"


                #         mother_education=="" ~ "No education",
                # mother_education=="999" ~ "No education",
                # mother_education=="88" ~ "Primary",
                # mother_education=="98" ~ "Primary",
                # mother_education=="1" ~ "Primary",
                # mother_education=="2" ~ "Primary",
                # mother_education=="3" ~ "Primary",
                # mother_education=="4" ~ "Primary",
                # mother_education=="5" ~ "Primary",
                # mother_education=="6" ~ "Primary",
                # mother_education=="7" ~ "Primary",
                # mother_education=="8" ~ "Primary",
                # mother_education=="9" ~ "Primary",
                # mother_education=="10" ~ "Primary",
                # mother_education=="11" ~ "Primary",
                # mother_education=="12" ~ "Secondary",
                # mother_education=="13" ~ "Secondary",
                # mother_education=="14" ~ "Secondary",
                # mother_education=="15" ~ "Secondary",
                # mother_education=="16" ~ "Secondary",
                # mother_education=="17" ~ "Secondary",
                # mother_education=="18" ~ "Secondary",
                # mother_education=="19" ~ "Secondary",
                # mother_education=="20" ~ "Secondary",
                # mother_education=="21" ~ "Secondary",
                # mother_education=="22" ~ "Secondary",
                # mother_education=="23" ~ "Secondary",
                # mother_education=="24" ~ "Secondary",
      ),

      maritalStatus = ifelse(is.na(maritalStatus), "99", maritalStatus),
      maritalStatus = recode_factor(
        maritalStatus,
        `1` = "Single",
        `2` = "Married",
        `3` = "Common-law marriage",
        `4` = "Divorced",
        `5` = "Separated",
        `6` = "Widowed",
        `99` = "No information"

        # `1` = "Solteiro",
        # `2` = "Casado",
        # `3` = "Uniao-de-facto",
        # `4` = "Divorciado",
        # `5` = "Separado",
        # `6` = "Viuvo",
        # `99` = "no info"
      ),

      religion = ifelse(is.na(religion), "999", religion),
      religion = recode_factor(
        religion,
        `1` = "Catholic",
        `2` = "Protestant/Anglican",
        `3` = "Undetermined Christian",
        `4` = "Islamic",
        `5` = "Hindu",
        `6` = "Zionist/Sionist",
        `7` = "Animist",
        `8` = "Atheist",
        `9` = "Evangelical",
        `98` = "Other",
        `88` = "Don't know",
        `99` = "Refuses to answer",
        `999` = "No information"

        # `1` = "Catolica",
        # `2` = "Protestante/Anglicana",
        # `3` = "Cristao indeterminadao",
        # `4` = "Islamica",
        # `5` = "Hindus",
        # `6` = "Zione/ Sião",
        # `7` = "Animistas",
        # `8 ` = "Ateus",
        # `9` = "Evangelica",
        # `98` = "Outro",
        # `88` = "Nao sabe",
        # `99` = "Recusa-se a responder",
        # `999` = "no info"
      ),

      ocupation = ifelse(is.na(ocupation), "999", ocupation),
      ocupation = recode_factor(
        ocupation,
        `1` = "Farmer",
        `2` = "Fisherman",
        `3` = "Charcoal maker/Woodcutter",
        `4` = "Fixed vendors",
        `5` = "fix vendors",
        `6` = "Street vendors",
        `7` = "Guards/Security personnel",
        `8` = "Healthcare personnel",
        `9` = "Student",
        `10` = "Teacher",
        `11` = "Military/Police",
        `12` = "Carpenter/Electrician",
        `13` = "Locksmith/Mason",
        `14` = "Disabled",
        `15` = "Volunteer",
        `16` = "Religious",
        `17` = "Retired/Pensioner",
        `18` = "Domestic worker",
        `19` = "Worker in RSA",
        `20` = "Miner in RSA",
        `21` = "Worker in Xinavane Sugar Factory",
        `22` = "Worker in Maragra Sugar Factory",
        `23` = "Unemployed",
        `98` = "Other",
        `999` = "No information"

        # `1` = "Campones",
        # `2` = "Pescador",
        # `3` = "Carvoeiro/lenhador",
        # `4` = "Vendedores fixos",
        # `5` = "Comerciantes",
        # `6` = "Vendedores ambulantes",
        # `7` = "Guardas/Seguranças",
        # `8` = "Pessoal da saude",
        # `9` = "Estudante",
        # `10` = "Professor",
        # `11` = "Militares/Policias",
        # `12` = "Carpinteiro/Electricista",
        # `13` = "Serralheiro/Pedreiro",
        # `14` = "Incapacitado",
        # `15` = "Voluntario",
        # `16` = "Religioso",
        # `17` = "Reformado/pensionista",
        # `18` = "Empregado domestico",
        # `19` = "Trabalhador na RSA",
        # `20` = "Mineiro na RSA",
        # `21` = "Trabalhador na acucareira de Xinavane",
        # `22` = "Trabalhador na acucareira de Maragra",
        # `23` = "Não trabalha",
        # `98` = "Outro",
        # `999` = "no info"
      ),
      perm_id = permId

    ) %>% as.data.frame()



}

readResidentialHistory <- function(file) {
  residential_history <- read_csv(
    file,
    col_types = cols(
      perm_id = col_character(),
      start_type = col_factor(c("BIR", "ENT", "ENU")),
      start_date = col_date("%Y-%m-%d %H:%M:%S"),
      end_type = col_factor(c("EXT", "DTH")),
      end_date = col_date("%Y-%m-%d %H:%M:%S"),
      start_household = col_character(),
      end_household = col_character()
    ),
    na = c("", "NA", "NULL")
  )

  residential_history
}

readResidentialHistory_v2 <- function(file) {
  residential_history <- read_csv(
    file,
    col_types = cols(
      perm_id = col_character(),
      start_type = col_factor(c("BIR", "ENT", "ENU")),
      start_date = col_date("%Y-%m-%d"),
      end_type = col_factor(c("EXT", "DTH")),
      end_date = col_date("%Y-%m-%d"),
      household = col_character()
    ),
    na = c("", "NA", "NULL")
  )  %>%
    mutate(
      perm_id = ifelse( perm_id %in% c( "9999-999-99", 
                                        "0000-000-01" ), 
                        NA, 
                        perm_id )
    ) %>% 
    filter(!is.na(perm_id))

  residential_history
}



readSurveyData <- function(file) {
  read_csv(
    file,
    col_types = cols(
      sys_odk_creation_date = col_datetime("%Y-%m-%d %H:%M:%S"),
      round_number = col_integer(),
      ordinal = col_integer(),
      visit_id = col_character(),
      house_number = col_character(),
      perm_id = col_character(),
      dob = col_date("%Y-%m-%d %H:%M:%S"),
      gender = col_factor(c("M", "F")),
      religion = col_factor(),
      can_read_write = col_factor(),
      has_education = col_factor(),
      education = col_factor(),
      has_deficiency = col_factor(),
      mother_tongue = col_factor(),
      marital_status = col_factor(),
      ocupation = col_factor(),
      secundary_ocupation = col_factor(),
      self_employed = col_factor(),
      works_for_others = col_factor(),
      stopped_work_last_month = col_factor(),
      work_regime = col_factor(),
      head_lives_here = col_factor(),
      relation_with_head = col_factor(),
      relation_with_subs_head = col_factor(),
      father_perm_id = col_character(),
      father_alive = col_factor(),
      father_death_date = col_character(),
      father_lives_here = col_factor(),
      mother_perm_id = col_character(),
      mother_alive = col_factor(),
      mother_death_date = col_character(),
      mother_lives_here = col_factor(),
      consent = col_factor(),
      reason_to_not_update = col_factor(),
      .default = col_skip()
    )
  ) %>%
    rename(visit_date = "sys_odk_creation_date") %>%
    mutate(
      perm_id = ifelse(perm_id == "9999-999-99", NA, perm_id),
      father_perm_id = ifelse(father_perm_id == "UNK", NA, father_perm_id),
      mother_perm_id = ifelse(mother_perm_id == "UNK", NA, mother_perm_id)
    )
}

readHospitalAdmissions <- function(file) {
  inpd <- haven::read_dta(file) %>%
    mutate(
      seas = forcats::as_factor(seas),
      sex = as_factor(sex),
      studya = as_factor(studya),
      heigth = as_factor(heigth),
      outcome = as_factor(outcome),
      hdeath = as_factor(hdeath)
    ) %>%
    rename(date_visit = "date") %>%
    select(perm_id, date_visit, labdiag1, everything())

  inpd
}

readHospitalVisits <- function(file) {
  opd <- read.csv(file) %>%
    mutate(
      seas = as_factor(seas)
      ,sex = as_factor(sex)
      ,studya = as_factor(studya)
      ,place = as_factor(place)
      ,referred = as_factor(referred)
      ,aftericd = as_factor(aftericd)
      ,date_birth = as.Date(date_birth)
      ,date_dth = as.Date(date_dth)
      ,date_birth2 = as.Date(date_birth2)
    ) %>% 
    mutate(
      place = ifelse(is.na(place), 99, place),
      place = recode_factor(
        place,
        `1` = "Manhica",
        `2` = "Maragra",
        `3` = "Ilha Josina",
        `4` = "Taninga",
        `5` = "Nwamatibjana",
        `6` = "Malavela",
        `99` = "no_info_from_opd"
      ))

  opd
}

readHospitalVisits_dta <- function(file) {
  opd <- read_dta(file) %>%
    mutate(
      seas = as_factor(seas)
      ,sex = as_factor(sex)
      ,studya = as_factor(studya)
      ,place = as.character(place)
      ,referred = as_factor(referred)
      ,aftericd = as_factor(aftericd)
      ,date_birth = as.Date(date_birth)
      ,date_dth = as.Date(date_dth)
      # ,date_birth2 = as.Date(date_birth2)
    ) %>% 
    mutate(
      place = as.factor(ifelse(is.na(place) | is.null(place), "99", place)),
      place_opd = recode_factor(
        place,
        `1` = "Manhica",
        `2` = "Maragra",
        `3` = "Ilha Josina",
        `4` = "Taninga",
        `5` = "Nwamatibjana",
        `6` = "Malavela",
        `7` = "Xinavane",
        `8` = "others",
        `9` = "miss_info",
        `99` = "no_info_from_opd"
      )
    )

  opd
}


readHospitalVisits2 <- function(file) {
  opd <- read_dta(file) %>%
    mutate(
      seas = as_factor(seas),
      sex = as_factor(sex),
      studya = as_factor(studya),
      place = as_factor(place),
      referred = as_factor(referred),
      aftericd = as_factor(aftericd),
      date_birth = as.Date(date_birth),
      date_dth = as.Date(date_dth),
      date_birth2 = as.Date(date_birth2)
    )

  opd
}

readHouseData <- function(file) {
  col_types_houses <- cols(
    sys_odk_creation_date = col_datetime(format = "%Y-%m-%d %H:%M:%S"),
    visit_id = col_character(),
    ordinal = col_integer(),
    round_number = col_integer(),
    region = col_character(),
    house_number = col_character(),
    head_perm_id = col_character(),
    nr_bedrooms = col_integer(),
    nr_constructions = col_integer(),
    nr_house_divisions = col_integer(),
    head_perm_id = col_character(),
    gps_accuracy = col_double(),
    gps_altitude = col_double(),
    gps_longitude = col_double(),
    gps_latitude = col_double(),
    ilumination_fuel = col_factor(as.character(c(1:8, 98))),
    coverage_material = col_factor(as.character(c(1:5, 98))),
    floor_material = col_factor(as.character(c(1:6, 98))),
    wall_material = col_factor(as.character(c(1:7, 98))),
    has_kitchen = col_factor(as.character(1:3)),
    is_kitchen_inside = col_factor(as.character(1:3)),
    kitchen_fuel = col_factor(as.character(c(1:5, 7, 98))),
    kitchen_has_coverage = col_factor(as.character(1:2)),
    kitchen_has_wall = col_factor(as.character(1:2)),
    can_see_latrine = col_factor(as.character(1:3)),
    latrine_type = col_factor(as.character(1:4)),
    cleaned_latrine = col_factor(as.character(1:2)),
    other_poop_place = col_factor(as.character(1:7)),
    ## TODO Values 98 and 99 missing from data dictionary. Missing?
    poo_treatment = col_factor(as.character(c(1:3, 98, 99))),
    ## TODO Values 8 and 98 missing from data dictionary.
    water_source = col_factor(as.character(c(1:8, 98))),
    has_water_recipient = col_factor(as.character(1:2)),
    water_recpt_covered = col_factor(as.character(1:2)),
    water_recpt_elevated = col_factor(as.character(1:2)),
    ## TODO Value 5 observed but not in data dictionary.
    wat_treatment_proc = col_factor(as.character(c(1:5, 99))),
    trash_treatment = col_factor(as.character(c(1:7, 98))),
    .default = col_skip()
  )

  hh_raw <- read_csv(file, col_types = col_types_houses)

  hh_with_labels <- hh_raw %>% 
    filter(
      sys_odk_creation_date >= as.Date("2016-01-01"),
      sys_odk_creation_date <= as.Date("2020-12-31")
    ) %>%
    rename(date_time = "sys_odk_creation_date") %>%
    mutate(
      ilumination_fuel = recode_factor(
        ilumination_fuel,
        `1` = "Electricity",
        `2` = "Generator",
        `3` = "Solar panel",
        `4` = "Gas",
        `5` = "Kerosene",
        `6` = "Candles",
        `7` = "Battery",
        `8` = "Wood",
        `98` = "Don't know"
      ),
      coverage_material = recode_factor(
        coverage_material,
        `1` = "Concrete",
        `2` = "Tiles",
        `3` = "Lusalite sheet",
        `4` = "Zinc sheet",
        `5` = "Grass",
        `98` = "Other"
      ),
      floor_material = recode_factor(
        floor_material,
        `1` = "Wood",
        ## TODO Well spelled?
        `2` = "Marmor",
        `3` = "Cement",
        `4` = "Mosaic tiles",
        `5` = "Adobe",
        `6` = "Nothing",
        `98` = "Other"
      ),
      wall_material = recode_factor(
        wall_material,
        `1` = "Cement blocks",
        `2` = "Bricks",
        `3` = "Wood/Zinc",
        `4` = "Adobe",
        `5` = "Bamboo/palm leafs",
        `6` = "Maticado sticks",
        `7` = "Paper bags",
        `98` = "Other"
      ),
      has_kitchen = recode_factor(
        has_kitchen,
        `1` = "Yes",
        `2` = "No",
        `3` = "Don't know"
      ),
      is_kitchen_inside = recode_factor(
        is_kitchen_inside,
        `1` = "Yes",
        `2` = "No",
        `3` = "Don't know"
      ),
      kitchen_fuel = recode_factor(
        kitchen_fuel,
        `1` = "Firewood/timber",
        `2` = "Coal",
        `3` = "Gas",
        `4` = "Electricity",
        ## TODO What is paraffin?
        `5` = "Paraffin",
        ## TODO No observations with value 6
        `7` = "No fuel",
        ## TODO What is the difference between Other and Don't know?
        `98` = "Don't know"
      ),
      kitchen_has_coverage = recode_factor(
        kitchen_has_coverage,
        `1` = "Yes",
        `2` = "No"
      ),
      kitchen_has_wall = recode_factor(
        kitchen_has_wall,
        `1` = "Yes",
        `2` = "No"
      ),
      can_see_latrine = recode_factor(
        can_see_latrine,
        `1` = "Yes",
        `2` = "No",
        `3` = "Don't know"
      ),
      ## TODO Find better labels
      latrine_type = recode_factor(
        latrine_type,
        ## Toilet connected to septic tank
        `1` = "Toilet with septic tank",
        ## TODO What is this?
        `2` = "Improved latrine",
        ## TODO How is this different from "Improved latrine"?
        `3` = "Improved traditional latrine",
        `4` = "Not-improved traditional latrine"
      ),
      cleaned_latrine = recode_factor(
        cleaned_latrine,
        `1` = "Clean",
        `2` = "Dirty"
      ),
      ## TODO What do these really mean?
      other_poop_place = recode_factor(
        other_poop_place,
        `1` = "Toilet connected to neighbour's septic tank",
        `2` = "Neighbour ventilated latrine",
        `3` = "Neighbour latrine with concrete slab",
        `4` = "Neighbour not-improved latrine",
        `5` = "Bucket",
        `6` = "Hole in the bushes",
        `7` = "Open space in the bushes"
      ),
      poo_treatment = ifelse(is.na(poo_treatment), "999", as.character(poo_treatment)),
      poo_treatment = recode_factor(
        poo_treatment,
        `1` = "Close latrine hole and open another one",
        `2` = "Hire septic tank cleaning services",
        `3` = "Household member does cleaning",
        `98` = "Don't know",
        `99` = "Refused to answer",
        `999` = "no information"
      ),
      water_source = recode_factor(
        water_source,
        `1` = "Piped water inside the house",
        `2` = "Piped water outside the house",
        `3` = "Fountain",
        `4` = "Well/hole dependent on a pump",
        `5` = "River/lake/pond",
        `6` = "Pit without a pump",
        ## TODO This is strange... Why isn't it 98 or 99?
        `7` = "Don't know",
        `8` = "Water well with manual pump",
        `98` = "Other"
      ),
      ## TODO recipient or container?
      has_water_recipient = recode_factor(
        has_water_recipient,
        `1` = "Yes",
        `2` = "No"
      ),
      water_recpt_covered = recode_factor(
        water_recpt_covered,
        `1` = "Yes",
        `2` = "No"
      ),
      water_recpt_elevated = recode_factor(
        water_recpt_elevated,
        `1` = "Yes",
        `2` = "No"
      ),
      wat_treatment_proc = ifelse(is.na(wat_treatment_proc), "999", as.character(wat_treatment_proc)),
      wat_treatment_proc = recode_factor(
        wat_treatment_proc,
        `1` = "No treatment",
        `2` = "Boil water",
        `3` = "Chlorination",
        `4` = "Filtration",
        `5` = "Traditional methods",
        `99` = "Don't know",
        `999` = "no information"
      ),
      trash_treatment = recode_factor(
        trash_treatment,
        `1` = "Burried",
        `2` = "Burned",
        `3` = "Left in the open",
        `4` = "Collected by municipality",
        `5` = "Partly recycled",
        `6` = "Deposited in an improvised trash deposit site in HH yard",
        `7` = "Used as fertilizer",
        `98` = "Other"
      ),
      date = as.Date(date_time),
      year_visit = lubridate::year(date)
    )

  hh_with_labels
}

readDeathData <- function(file) {
  read_csv(
    file,
    col_types = cols(
      perm_id = col_character(),
      date_of_death = col_date("%Y-%m-%d %H:%M:%S"),
      cause_of_death = col_factor(),
      is_household_head = col_factor(),
      new_hoh_id = col_character(),
      household_id = col_character(),
      sys_odk_creation_date = col_date("%Y-%m-%d %H:%M:%S"),
      visit_id = col_character()
    )
  )
}

readEconomicData <- function(file) {
  # yes_no_levels <- as.character(c(1:2, 88, 99))
  df_raw <- read_csv(
    file,
    col_types = cols(
      start = col_date("%Y-%m-%d %H:%M:%S"),
      round_number = col_integer(),
      # region = col_character(),
      house_number = col_character(),
      gps_longitude = col_double(),
      gps_latitude  =  col_double(),               
      gps_accuracy = col_double(),
      has_farm_family_production = col_double(),
      has_car = col_double(),
      has_pigs = col_double(),
      has_moto = col_double(),
      has_goat = col_double(),
      # number of duck
      # number of dogs
      has_tent_for_bussiness = col_double(),
      # number of car
      has_duck = col_double(),
      has_radio = col_double(),
      has_dogs = col_double(), 
      has_electric_stove = col_double(),
      # number of pigs
      has_farm_of_commercial_production = col_double(),
      # number of moto
      # 
      has_cattle = col_double(),
      has_tractor = col_double(),
      has_sewing_machine = col_double(),
      has_glacier = col_double(),
      # number of goat
      has_bike = col_double(),
      has_dvd = col_double(),
      has_cats = col_double(),
      # number of tractor
      ## FIXME use_telephone has hundreds of different levels - ITS continuee
      # use_telephone = col_double(),
      has_freezer = col_double(),
      has_chickens = col_double(),
      # number of chickens
      # number of cattle
      has_celular = col_double(),
      use_radio_news = col_double(),
      has_computer = col_double(),
      # Many computer
      # many people  gas telephone
      # View_tv new
      # nr of bikes
      #
      has_telephone = col_double(),
      has_tv = col_double(),
      # number of cats
      
      
      ## TODO Specify "number of" variables
      .default = col_skip()
    )
  ) %>% 
  filter(
    start >= as.Date("2016-01-01"),
    start <= as.Date("2022-12-31")
  )

  nam <- names(df_raw)

  df_labels <- df_raw %>%
    mutate(
      across(
        nam[str_starts(nam, "has_|use_")],
        function(x) {
          recode_factor(
            ifelse(is.na(x), 999, x) ,
            `2` = "No",
            `1` = "Yes",
            `88` = "Doesn't know",
            `99` = "Refused to answer",
            `999` = " no information"
            )
        }
      )
    )

  df_labels
}

readVerbalAutopsyData <- function(file) {
  verbal_autopsy <- read_csv(
    file,
    col_types = cols(
      roundNumber = col_integer(),
      perm_id = col_character(),
      household_no = col_character(),
      date_of_death = col_date("%Y-%m-%d %H:%M:%S"),
      gender = col_factor(c("M", "F")),
      father_permid = col_character(),
      mother_permid = col_character(),
      verbal_autopsy_type = col_factor()
    )
  )

  verbal_autopsy
}



###
#
###

readEnvironmentData <- function(file) {
  read_csv(
    file,
    col_types = cols(
     perm_id = col_character(),
     household = col_character(),
     alan_2015_500 = col_double(),
     alan_2020_500 = col_double(),
     alan_2015_1000 = col_double(),
     alan_2020_1000 = col_double(),
     imp_300 = col_double(),
     imp_500 = col_double(),
     buff300m_10 = col_double(),
     buff300m_20 = col_double(),
     buff300m_30 = col_double(),
     buff300m_40 = col_double(),
     buff300m_50 = col_double(),
     buff300m_60 = col_double(),
     buff300m_80 = col_double(),
     buff500m_10 = col_double(),
     buff500m_20 = col_double(),
     buff500m_30 = col_double(),
     buff500m_40 = col_double(),
     buff500m_50 = col_double(),
     buff500m_60 = col_double(),
     buff500m_80 = col_double(),
     ndvi_2015_300 = col_double(),
     ndvi_2020_300 = col_double(),
     ndvi_2015_500 = col_double(),
     ndvi_2020_500 = col_double()
    )
  )
}

read_data_final_w_coltypes <- function(file_path) {
  read_csv(
    file_path,
    col_types = cols(
      perm_id                      = col_character(),
      min_dob                      = col_date(format = ""),
      key_value                    = col_character(),
      gender                       = col_character(),
      mother_perm_id               = col_character(),
      child_dob                    = col_date(format = ""),
      mother_gender                = col_logical(),
      mother_dob                   = col_date(format = ""),
      mother_education             = col_double(),
      end_date_fu                  = col_date(format = ""),
      end_type                     = col_character(),
      start_date_fu                = col_date(format = ""),
      start_type                   = col_character(),
      fu_death                     = col_double(),
      region                       = col_character(),
      ilumination_fuel             = col_character(),
      ilumination_fuel_v2_post     = col_character(),
      h_visits                     = col_double(),
      h_respiratory_visits         = col_double(),
      h_respiratory_visits_v2      = col_double(),
      h_respiratory_visits_v3      = col_double(),
      h_respiratory_all_visits_v3  = col_double(),
      h_malaria_visits             = col_double(),
      h_fever_visits               = col_double(),
      h_bronchitis_visits          = col_double(),
      posto_admnistrativo          = col_factor(c("Manhica -Sede", "3 de Fevereiro", "Maluana",
                                       "Ilha Josina Machel", "Calanga", "Xinavane")),
      region_name                  = col_character(),
      healthdist_final             = col_double(),
      fu_days                      = col_double(),
      mother_age_at_birth_of_child = col_double(),
      year_birth                   = col_factor(c("2016", "2017", "2018", "2019", "2020")),
      healthdist_final_std          = col_double(),
      mother_education_cat          = col_character(),
      SES_var                      = col_character(),
      SES_num                      = col_double(),
      place                        = col_character(),
      hospital_place               = col_character(),
      ilumination_fuel_v2_post_bn  = col_double(),
      gid                          = col_double(),
      household                    = col_character()
    )
  )
}


read_member_detail_v2 <- function(path) {

read_csv(
    path,
    col_types = cols(
      # General Information
      `sys_odk_uri`            = col_character(),
      `sys_odk_creation_date`  = col_date("%Y-%m-%d %H:%M:%S"),
      `sys_odk_created_by`     = col_character(),
      # `sys_odk_is_complete`
      # `sys_odk_last_update_date`
      # `sys_odk_last_update_by`
      # `sys_odk_marked_as_complete_date`
      # `sys_odk_submission_date`
      # `sys_odk_start`
      # `sys_odk_end`
      # `sys_odk_deviceid`
      # `field_worker_id`
      `individual_id`          = col_character(),
      `location_id`            = col_character(),
      `region`                 = col_character(),
      `house_number`           = col_character(),
      `round_number`           = col_number(),
      `ordinal`                = col_number(),
      `visit_id`               = col_character(),
      `perm_id`                = col_character(),
      `age`                    = col_integer(),
      `dob`                    = col_date("%Y-%m-%d"),
      `gender`                 = col_character(),
      `name`                   = col_character(),
      `spouse_name`            = col_character(),
      `spouse_perm_id`         = col_character(),
      # `father_alive`           = col_character(),
      `father_alive`           = col_character(),
      `father_death_date`      = col_character(),
      `father_id`              = col_character(),
      `father_lives_here`      = col_character(),
      `father_name`            = col_character(),
      `father_perm_id`         = col_character(),
      `mother_alive`           = col_character(),
      `mother_death_date`      = col_character(),
      `mother_id`              = col_character(),
      `mother_lives_here`      = col_character(),
      `mother_name`            = col_character(),
      `mother_perm_id`         = col_character(),
      `consent`                = col_character(),
      `can_read_write`         = col_character(),
      `card_state`             = col_character(),
      `deficiency`             = col_character(),
      `deficiency_causes`      = col_character(),
      `deficiency_causes_name` = col_character(),
      `deficiency_name`        = col_character(),
      `driv_license_exp_stat`  = col_character(),
      `education`              = col_character(),
      `event_date`             = col_date("%Y-%m-%d"),
      `event_place`            = col_character(),
      # `fertility_has_prev_pregnancies`
      # `fertility_nr_abortions`
      # `fertility_nr_alive_boys`
      # `fertility_nr_alive_girls`
      # `fertility_nr_boys`
      # `fertility_nr_girls`
      # `fertility_nr_livebirths`
      # `fertility_nr_miscarriages`
      # `fertility_nr_pregnancies`
      # `fertility_nr_quadr_outcomes`
      # `fertility_nr_stillbirths`
      # `fertility_nr_triplet_outcomes`
      # `fertility_nr_twin_abortions`
      # `fertility_nr_twin_outcomes`
      # `fertility_nr_twin_stillbirths`
      # `fertility_nr_twins_alive`
      # `has_birth_certificate`
      `has_cedula`             = col_character(),
      `has_cism_card`          = col_character(),
      # `has_deficiency`
      # `has_driving_license`
      # `has_id_card`
      # `has_id_receipt`
      # `has_identity_doc`
      # `has_passport`
      # `has_voter_card`
      # `head_lives_here`
      # `id_exp_stat`
      # `id_receipt_exp_stat`
      # `identity_doc_name`
      # `identity_doc_type`
      # `is_present`
      # `is_updatable`
      # `marital_status`
      # `mother_tongue`
      `non_work_days`          = col_number(),
      # `nr_of_husband_wives`
      # `nr_of_wives`
      `ocupation`              = col_character(),
      `ocupation_name`         = col_character(),
      # `passport_exp_stat`
      `place_of_birth`         = col_character(),
      # `reason_to_not_update`
      # `relation_with_head`
      # `relation_with_subs_head`
      # `religion`
      `secundary_ocupation`    = col_character(),
      `secundary_ocupation_name`= col_character(),
      `self_employed`          = col_character(),
      `stopped_work_last_month`= col_character(),
      # `wives_on_same_house`
      `work_regime`            = col_character(),
      `works_for_others`       = col_character(),
      # `processed_by_mirth`
      # `processed_by_sync`
      `can_test_reading`       = col_character(),
      `has_education`          = col_character(),
      `learning_place`         = col_character(),
      `reading_quality`        = col_character()
      # .default = col_skip()
    ) 
  ) %>%
    mutate(

      marital_status = ifelse(is.na(marital_status), "999", marital_status),
      marital_status = recode_factor(
        marital_status,
        `1` = "Single never lived in a marital relationship",
        `2` = "Married",
        `3` = "Common-law union",
        `4` = "Divorced",
        `5` = "Separated",
        `6` = "Widowed",
        `999` = "no info"
      ),

      father_alive =  ifelse(is.na(father_alive), "9999", father_alive),
      father_alive =  recode_factor(
        father_alive,
            `2` = "No",
            `1` = "Yes",
            `88` = "Doesn't know",
            `99` = "Refused to answer",
            `9999` =  "miss_info"
      ),
      mother_alive =  ifelse(is.na(mother_alive), "9999", mother_alive),
      mother_alive =  recode_factor(
        mother_alive,
            `2` = "No",
            `1` = "Yes",
            `88` = "Doesn't know",
            `99` = "Refused to answer",
            `9999` =  "miss_info"
      ),

      place_of_birth = ifelse(is.na(place_of_birth), "9999", place_of_birth),

      place_of_birth = recode_factor(
        place_of_birth,
          `1` = "Manhiça District"
          ,`2` = "Maputo Province"
          ,`3` = "City of Maputo"
          ,`4` = "City of Matola"
          ,`5` = "Gaza"
          ,`6` = "Inhambane"
          ,`7` = "Sofala"
          ,`8` = "Manica"
          ,`9` = "Tete"
          ,`10` = "Zambezia"
          ,`11` = "Nampula"
          ,`12` = "Cabo Delgado"
          ,`13` = "Niassa"
          ,`14` = "South Africa"
          ,`15` = "Swaziland"
          ,`16` = "Other African Country"
          ,`17` = "Europe"
          ,`18` = "Asia"
          ,`19` = "America"
          ,`98` = "Other"
          ,`88` = "Don't know"
          ,`99` = "Refuses to answer"
          ,`9999` = "no info"

      ),
      ocupation = ifelse(is.na(ocupation), "999", ocupation),
      ocupation = recode_factor(
        ocupation,
        `1` = "Farmer",
        `2` = "Fisherman",
        `3` = "Charcoal maker/Woodcutter",
        `4` = "Fixed vendors",
        `5` = "fix vendors",
        `6` = "Street vendors",
        `7` = "Guards/Security personnel",
        `8` = "Healthcare personnel",
        `9` = "Student",
        `10` = "Teacher",
        `11` = "Military/Police",
        `12` = "Carpenter/Electrician",
        `13` = "Locksmith/Mason",
        `14` = "Disabled",
        `15` = "Volunteer",
        `16` = "Religious",
        `17` = "Retired/Pensioner",
        `18` = "Domestic worker",
        `19` = "Worker in RSA",
        `20` = "Miner in RSA",
        `21` = "Worker in Xinavane Sugar Factory",
        `22` = "Worker in Maragra Sugar Factory",
        `23` = "Unemployed",
        `98` = "Other",
        `999` = "No information"

        # `1` = "Campones",
        # `2` = "Pescador",
        # `3` = "Carvoeiro/lenhador",
        # `4` = "Vendedores fixos",
        # `5` = "Comerciantes",
        # `6` = "Vendedores ambulantes",
        # `7` = "Guardas/Seguranças",
        # `8` = "Pessoal da saude",
        # `9` = "Estudante",
        # `10` = "Professor",
        # `11` = "Militares/Policias",
        # `12` = "Carpinteiro/Electricista",
        # `13` = "Serralheiro/Pedreiro",
        # `14` = "Incapacitado",
        # `15` = "Voluntario",
        # `16` = "Religioso",
        # `17` = "Reformado/pensionista",
        # `18` = "Empregado domestico",
        # `19` = "Trabalhador na RSA",
        # `20` = "Mineiro na RSA",
        # `21` = "Trabalhador na acucareira de Xinavane",
        # `22` = "Trabalhador na acucareira de Maragra",
        # `23` = "Não trabalha",
        # `98` = "Outro",
        # `999` = "no info"
      )


    )


}





  
readHouseData_v2 <- function(path){
  read_csv(
    path,
    col_types = cols( # ,`id`
                      `sys_odk_uri` = col_character()
                      ,`sys_odk_start` = col_date("%Y-%m-%d %H:%M:%S")
                      ,`sys_odk_end` = col_date("%Y-%m-%d %H:%M:%S")
                      # ,`sys_odk_deviceid`
                      ,`sys_odk_created_by` = col_character()
                      ,`sys_odk_creation_date` = col_date("%Y-%m-%d %H:%M:%S")
                      # ,`sys_odk_last_update_by`
                      # ,`sys_odk_last_update_date`
                      ,`sys_odk_submission_date`= col_date("%Y-%m-%d %H:%M:%S")         
                      # ,`sys_odk_marked_as_complete_date`
                      ,`field_worker_id` = col_character()
                      ,`location_id` = col_character()
                      ,`visit_id` = col_character()
                      ,`round_number` = col_number()
                      ,`region` = col_character()
                      ,`house_number` = col_character()
                      ,`head_perm_id` = col_character()
                      ,`head_name` = col_character()
                      ,`ordinal` = col_number()
                      ,`gps_accuracy` = col_double()
                      ,`gps_altitude` = col_double()
                      ,`gps_longitude` = col_double()
                      ,`gps_latitude` = col_double()
                      ,`can_see_latrine` = col_character()
                      ,`can_see_recipient` = col_character()
                      ,`cell_numbers` = col_character()
                      ,`cleaned_latrine` = col_character()
                      ,`coverage_material` = col_character()
                      ,`coverage_material_name` = col_character()
                      ,`floor_material` = col_character()
                      ,`floor_material_name` = col_character()
                      ,`habitation_name` = col_character()
                      ,`has_kitchen` = col_character()
                      ,`has_latrine` = col_character()
                      ,`has_subs_head` = col_character()
                      ,`has_water_recipient` = col_character()
                      ,`house_no_located` = col_character()
                      ,`house_no_need_repaint` = col_character()
                      ,`how_water_collected` = col_character()
                      ,`how_water_collected_name` = col_character()
                      ,`ilumination_fuel` = col_character()
                      ,`ilumination_fuel_name` = col_character()
                      ,`is_kitchen_inside` = col_character()
                      ,`is_water_src_inside` = col_character()
                      ,`kitchen_fuel` = col_character()
                      ,`kitchen_fuel_name` = col_character()
                      ,`kitchen_has_coverage` = col_character()
                      ,`kitchen_has_wall` = col_character()
                      ,`latrine_has_privacy` = col_character()
                      ,`latrine_type` = col_character()
                      ,`nr_bedrooms` = col_character()
                      ,`nr_constructions` = col_character()
                      ,`nr_house_div_used` = col_character()
                      ,`nr_house_divisions` = col_character()
                      ,`other_poop_place`  = col_character()              
                      ,`outside_water_src` = col_character()
                      ,`outside_water_src_name` = col_character()
                      ,`poo_treatment` = col_character()
                      ,`subs_head_name` = col_character()
                      ,`subs_head_perm_id` = col_character()
                      ,`time_to_collect_water` = col_character()
                      ,`trash_deposit` = col_character()
                      ,`trash_deposit_name` = col_character()
                      ,`trash_recycle_method` = col_character()
                      ,`trash_recycled_part` = col_character()
                      ,`trash_treatment` = col_character()
                      ,`trash_treatment_name` = col_character()
                      ,`trash_used_as_fertilizer` = col_character()
                      ,`wall_material` = col_character()
                      ,`wall_material_name` = col_character()
                      ,`wash_after_poop` = col_character()
                      ,`wat_treatment_proc` = col_character()
                      ,`wat_treatment_proc_name` = col_character()
                      ,`water_money` = col_character()
                      ,`water_recpt_covered` = col_character()
                      ,`water_recpt_elevated` = col_character()
                      ,`water_source` = col_character()
                      ,`water_source_name` = col_character()
                      ,`water_supplier` = col_character()
                      ,`wu_well_maintained` = col_character()
                      ,`habitation_type_1` = col_character()
                      ,`habitation_type_2` = col_character()
                      ,`habitation_type_3` = col_character()
                      ,`habitation_type_4` = col_character()
                      ,`habitation_type_5` = col_character()
                      ,`habitation_type_98` = col_character()
                      # ,`processed_by_sync` = col_character()
                      ,`processed_by_mirth` = col_character()
    )
    ) %>%
    filter(
      sys_odk_creation_date >= as.Date("2016-01-01"),
      sys_odk_creation_date <= as.Date("2020-12-31")
    ) %>%
    mutate( uuid = gsub("uui:", "", sys_odk_uri),
            ilumination_fuel = recode_factor(
        ilumination_fuel,
        `1` = "Electricity",
        `2` = "Generator",
        `3` = "Solar panel",
        `4` = "Gas",
        `5` = "Kerosene",
        `6` = "Candles",
        `7` = "Battery",
        `8` = "Wood",
        `98` = "Don't know"
      ),
      coverage_material = recode_factor(
        coverage_material,
        `1` = "Concrete",
        `2` = "Tiles",
        `3` = "Lusalite sheet",
        `4` = "Zinc sheet",
        `5` = "Grass",
        `98` = "Other"
      ),
      floor_material = recode_factor(
        floor_material,
        `1` = "Wood",
        ## TODO Well spelled?
        `2` = "Marmor",
        `3` = "Cement",
        `4` = "Mosaic tiles",
        `5` = "Adobe",
        `6` = "Nothing",
        `98` = "Other"
      ),
      wall_material = recode_factor(
        wall_material,
        `1` = "Cement blocks",
        `2` = "Bricks",
        `3` = "Wood/Zinc",
        `4` = "Adobe",
        `5` = "Bamboo/palm leafs",
        `6` = "Maticado sticks",
        `7` = "Paper bags",
        `98` = "Other"
      ),
      has_kitchen = recode_factor(
        has_kitchen,
        `1` = "Yes",
        `2` = "No",
        `3` = "Don't know"
      ),
      is_kitchen_inside = recode_factor(
        is_kitchen_inside,
        `1` = "Yes",
        `2` = "No",
        `3` = "Don't know"
      ),
      kitchen_fuel = recode_factor(
        kitchen_fuel,
        `1` = "Firewood/timber",
        `2` = "Coal",
        `3` = "Gas",
        `4` = "Electricity",
        ## TODO What is paraffin?
        `5` = "Paraffin",
        ## TODO No observations with value 6
        `7` = "No fuel",
        ## TODO What is the difference between Other and Don't know?
        `98` = "Don't know"
      ),
      kitchen_has_coverage = recode_factor(
        kitchen_has_coverage,
        `1` = "Yes",
        `2` = "No"
      ),
      kitchen_has_wall = recode_factor(
        kitchen_has_wall,
        `1` = "Yes",
        `2` = "No"
      ),
      can_see_latrine = recode_factor(
        can_see_latrine,
        `1` = "Yes",
        `2` = "No",
        `3` = "Don't know"
      ),
      ## TODO Find better labels
      latrine_type = recode_factor(
        latrine_type,
        ## Toilet connected to septic tank
        `1` = "Toilet with septic tank",
        ## TODO What is this?
        `2` = "Improved latrine",
        ## TODO How is this different from "Improved latrine"?
        `3` = "Improved traditional latrine",
        `4` = "Not-improved traditional latrine"
      ),
      cleaned_latrine = recode_factor(
        cleaned_latrine,
        `1` = "Clean",
        `2` = "Dirty"
      ),
      ## TODO What do these really mean?
      other_poop_place = recode_factor(
        other_poop_place,
        `1` = "Toilet connected to neighbour's septic tank",
        `2` = "Neighbour ventilated latrine",
        `3` = "Neighbour latrine with concrete slab",
        `4` = "Neighbour not-improved latrine",
        `5` = "Bucket",
        `6` = "Hole in the bushes",
        `7` = "Open space in the bushes"
      ),
      poo_treatment = ifelse(is.na(poo_treatment), "999", as.character(poo_treatment)),
      poo_treatment = recode_factor(
        poo_treatment,
        `1` = "Close latrine hole and open another one",
        `2` = "Hire septic tank cleaning services",
        `3` = "Household member does cleaning",
        `98` = "Don't know",
        `99` = "Refused to answer",
        `999` = "no information"
      ),
      water_source = recode_factor(
        water_source,
        `1` = "Piped water inside the house",
        `2` = "Piped water outside the house",
        `3` = "Fountain",
        `4` = "Well/hole dependent on a pump",
        `5` = "River/lake/pond",
        `6` = "Pit without a pump",
        ## TODO This is strange... Why isn't it 98 or 99?
        `7` = "Don't know",
        `8` = "Water well with manual pump",
        `98` = "Other"
      ),
      ## TODO recipient or container?
      has_water_recipient = recode_factor(
        has_water_recipient,
        `1` = "Yes",
        `2` = "No"
      ),
      water_recpt_covered = recode_factor(
        water_recpt_covered,
        `1` = "Yes",
        `2` = "No"
      ),
      water_recpt_elevated = recode_factor(
        water_recpt_elevated,
        `1` = "Yes",
        `2` = "No"
      ),
      wat_treatment_proc = ifelse(is.na(wat_treatment_proc), "999", as.character(wat_treatment_proc)),
      wat_treatment_proc = recode_factor(
        wat_treatment_proc,
        `1` = "No treatment",
        `2` = "Boil water",
        `3` = "Chlorination",
        `4` = "Filtration",
        `5` = "Traditional methods",
        `99` = "Don't know",
        `999` = "no information"
      ),
      trash_treatment = recode_factor(
        trash_treatment,
        `1` = "Burried",
        `2` = "Burned",
        `3` = "Left in the open",
        `4` = "Collected by municipality",
        `5` = "Partly recycled",
        `6` = "Deposited in an improvised trash deposit site in HH yard",
        `7` = "Used as fertilizer",
        `98` = "Other"
      )
      
      )
}



  
readHouseData_v3 <- function(path){
  read_csv(
    path,
    col_types = cols( # ,`id`
                      `start` = col_date("%Y-%m-%d %H:%M:%S")
                      # ,`id` = col_character()
                      ,`round_number` = col_number()
                      ,`region` = col_character()
                      ,`house_number` = col_character()
                      ,`gps_accuracy` = col_double()
                      # ,`gps_altitude` = col_double()
                      ,`gps_longitude` = col_double()
                      ,`gps_latitude` = col_double()
                      ,`coverage_material` = col_character()
                      ,`wall_material` = col_character()
                      ,`floor_material` = col_character()
                      ,`has_kitchen` = col_character()
                      ,`is_kitchen_inside` = col_character()
                      ,`kitchen_fuel` = col_character()
                      ,`ilumination_fuel` = col_character()
                      ,`water_source` = col_character()
                      ,`is_water_src_inside` = col_character()
                      ,`wat_treatment_proc` = col_character()
                      ,`has_latrine` = col_character()
                      ,`latrine_type` = col_character()
                      ,`poo_treatment` = col_character()
                      ,`trash_treatment` = col_character()
    )
    ) %>%
    filter(
      start >= as.Date("2016-01-01"),
      start <= as.Date("2022-12-31")
    ) %>%
    mutate( 
      coverage_material = ifelse(is.na(coverage_material), "999", as.character(coverage_material)),
      coverage_material = recode_factor(
        coverage_material,
        `1` = "Concrete",
        `2` = "Tiles",
        `3` = "Lusalite sheet",
        `4` = "Zinc sheet",
        `5` = "Grass",
        `98` = "Other",
        `999` = "No information"
      ),
      wall_material = ifelse(is.na(wall_material), "999", as.character(wall_material)),
      wall_material = recode_factor(
        wall_material,
        `1` = "Cement blocks",
        `2` = "Bricks",
        `3` = "Wood/Zinc",
        `4` = "Adobe",
        `5` = "Bamboo/palm leafs",
        `6` = "Maticado sticks",
        `7` = "Paper bags",
        `98` = "Other",
        `999` = "No information"
      ),
      floor_material = ifelse(is.na(floor_material), "999", as.character(floor_material)),
      floor_material = recode_factor(
        floor_material,
        `1` = "Wood",
        ## TODO Well spelled?
        `2` = "Marmor",
        `3` = "Cement",
        `4` = "Mosaic tiles",
        `5` = "Adobe",
        `6` = "Nothing",
        `98` = "Other",
        `999` = "No information"
      ),
      has_kitchen = ifelse(is.na(has_kitchen), "999", as.character(has_kitchen)),
      has_kitchen = recode_factor(
        has_kitchen,
        `1` = "Yes",
        `2` = "No",
        `3` = "Don't know",
        `999` = "No information"
      ),
      is_kitchen_inside = ifelse(is.na(is_kitchen_inside), "999", as.character(is_kitchen_inside)),
      is_kitchen_inside = recode_factor(
        is_kitchen_inside,
        `1` = "Yes",
        `2` = "No",
        `3` = "Don't know",
        `999` = "No information"
      ),
      kitchen_fuel = ifelse(is.na(kitchen_fuel), "999", as.character(kitchen_fuel)),
      kitchen_fuel = recode_factor(
        kitchen_fuel,
        `1` = "Firewood/timber",
        `2` = "Coal",
        `3` = "Gas",
        `4` = "Electricity",
        ## TODO What is paraffin?
        `5` = "Paraffin",
        ## TODO No observations with value 6
        `7` = "No fuel",
        ## TODO What is the difference between Other and Don't know?
        `98` = "Don't know",
        `999` = "No information"
      ),
      ilumination_fuel = ifelse(is.na(ilumination_fuel), "999", as.character(ilumination_fuel)),
      ilumination_fuel = recode_factor(
        ilumination_fuel,
        `1` = "Electricity",
        `2` = "Generator",
        `3` = "Solar panel",
        `4` = "Gas",
        `5` = "Kerosene",
        `6` = "Candles",
        `7` = "Battery",
        `8` = "Wood",
        `98` = "Don't know",
        `999` = "No information"
      ),
      water_source = ifelse(is.na(water_source), "999", as.character(water_source)),
      water_source = recode_factor(
        water_source,
        `1` = "Piped water inside the house",
        `2` = "Piped water outside the house",
        `3` = "Fountain",
        `4` = "Well/hole dependent on a pump",
        `5` = "River/lake/pond",
        `6` = "Pit without a pump",
        ## TODO This is strange... Why isn't it 98 or 99?
        `7` = "Don't know",
        `8` = "Water well with manual pump",
        `98` = "Other",
        `999` = "No information"
      ),
      is_water_src_inside = ifelse(is.na(is_water_src_inside), "999", as.character(is_water_src_inside)),
      is_water_src_inside = recode_factor(
        is_water_src_inside,
        `1` = "Yes",
        `2` = "No",
        `999` = "No information"
      ),
      wat_treatment_proc = ifelse(is.na(wat_treatment_proc), "999", as.character(wat_treatment_proc)),
      wat_treatment_proc = recode_factor(
        wat_treatment_proc,
        `1` = "No treatment",
        `2` = "Boil water",
        `3` = "Chlorination",
        `4` = "Filtration",
        `5` = "Traditional methods",
        `99` = "Don't know",
        `999` = "no information"
      ),
      has_latrine = ifelse(is.na(has_latrine), "999", as.character(has_latrine)),
      has_latrine = recode_factor(
        has_latrine,
        `1` = "Yes",
        `2` = "No",
        `999` = "No information"
      ),
      latrine_type = ifelse(is.na(latrine_type), "999", as.character(latrine_type)),
      ## TODO Find better labels
      latrine_type = recode_factor(
        latrine_type,
        ## Toilet connected to septic tank
        `1` = "Toilet with septic tank",
        ## TODO What is this?
        `2` = "Improved latrine",
        ## TODO How is this different from "Improved latrine"?
        `3` = "Improved traditional latrine",
        `4` = "Not-improved traditional latrine",
        `98` = "other",
        `999` = "no information"
      ),
      poo_treatment = ifelse(is.na(poo_treatment), "999", as.character(poo_treatment)),
      poo_treatment = recode_factor(
        poo_treatment,
        `1` = "Close latrine hole and open another one",
        `2` = "Hire septic tank cleaning services",
        `3` = "Household member does cleaning",
        `98` = "Don't know",
        `99` = "Refused to answer",
        `999` = "no information"
      ),
      trash_treatment = ifelse(is.na(trash_treatment), "999", as.character(trash_treatment)),
      trash_treatment = recode_factor(
        trash_treatment,
        `1` = "Burried",
        `2` = "Burned",
        `3` = "Left in the open",
        `4` = "Collected by municipality",
        `5` = "Partly recycled",
        `6` = "Deposited in an improvised trash deposit site in HH yard",
        `7` = "Used as fertilizer",
        `98` = "Other",
        `999` = "no information"
      )
      
      )
}

# -- -- -- -- ---------------------------------------
  # countries,
  # geoRead_countries(
  #   path = countries_geo_file
  # )
# -- -- -- -- ---------------------------------------

geoRead_countries <- function(path){

  st_read(path) %>%
    dplyr::select(NAME_EN) %>%
    filter(NAME_EN %in% c("Zimbabwe", "Eswatini", "Mozambique", "South Africa", 
                          "Botswana", "Zambia", "Malawi", "Tanzania")) %>%
    st_transform(crs = 32736) # WGS84 UTM 36S

}


# -- -- -- -- ---------------------------------------
  # provinces,
  # geoRead_provinces(
  #   path = countries_geo_file
  # )
# -- -- -- -- ---------------------------------------

geoRead_provinces <- function(path){

  st_read(path) %>%
    filter(admin == "Mozambique") %>%
    dplyr::select(name) %>%
    st_transform(crs = 32736) %>% # WGS84 UTM 36S
    group_by(name) %>%
    summarise

}

# -- -- -- -- ---------------------------------------
  # districts,
  # geoRead_districts(
  #   path = districts_geo_file
  # )
# -- -- -- -- ---------------------------------------

geoRead_districts <- function(path){

  st_read(path) %>%
    dplyr::select(NAME_2) %>%
    st_transform(crs = 32736) # WGS84 UTM 36S

}

# -- -- -- -- ---------------------------------------
  # bairros,
  # geoRead_bairros(
  #   path = bairros_geo_file
  # )
# -- -- -- -- ---------------------------------------

geoRead_bairros <- function(path){

  st_read(path) %>%
    mutate(Admin_Post = gsub("_", " ", Admin_Post), 
          Admin_Post = str_to_title(Admin_Post))

}





