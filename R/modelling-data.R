##########################################################################
##########################################################################
## Main: support functions to made the models 
##  AND analisys of distribution 
##
## date: 27/04/2023 #nolint
##
## Autor: Fabian Francisco Coloma Velez
##########################################################################
##########################################################################

#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
#    modelling in rmd
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
##'add function description
##'
##'
##'@title ADD_YOUR_FUNCTION_TITLE
##'@param data1 ADD_1rt_PARAMETER_DESCRIPTION
##'@param data2 ADD_2on_PARAMETER_DESCRIPTION
##'@param variable ADD_3r_PARAMETER_DESCRIPTION
##'@return data.frame / list / vector
##'@author Fabian Coloma
##'@export
##'@examples
##'
##'
load_model_variables_rmd <- function(outcome){

  SES_var_selected <- "SES_var" # or SES_var or SES_num

  source_il <- "ilumination_fuel" # clasiffication after - but separating dont know

  fu_days_select <- "offset(log(fu_months))"

  outcome1 <- outcome # h_visits_respiratory_byCode

  region_selected <- "healthdist_name_facility" # healthdist_name_facility_all7, healthdist_name_facility,posto_admnistrativo
  # i think i prefer use health dist name because is most certained

  
  cov_1tt <- c(
            source_il
            , fu_days_select
            , "gender"
          )

  cov_2tt <- c(
          cov_1tt
          , SES_var_selected
        )

  cov_3tt <- c(
          cov_2tt
          , "roaddist_std" # roaddist, healthdist_final_std
          , "healthdist_final_std"
        )

  cov_4tt <- c(
          cov_3tt
          , region_selected
          # , "distan"
        )

  cov_5tt <- c(
          cov_4tt
          , "as.factor(year_birth)"
        )

  cov_6tt <- c(
          cov_5tt
          , "mother_age_at_birth_of_child_std"
          , "mother_education"
      ) 

  fml_list <- list(
    cov_1 = cov_1tt,
    cov_2 = cov_2tt,
    cov_3 = cov_3tt,
    cov_4 = cov_4tt,
    cov_5 = cov_5tt,
    cov_6 = cov_6tt
  )

  
  fmls_to_mod <- c()
  for(i in 1:length(fml_list)){
    fmls_to_mod <- c(fmls_to_mod,
     
      paste0(
        outcome1,
        " ~ ",
        paste0(
          fml_list[[i]],
          collapse = " + "
        )
      )
    )

    
  }

fmls_to_mod


}




#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
#    Fin - modelling in rmd
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

##' function to see the remaining
##' cases of filtering a X quantile of variable
##'
##' @title quant_filt_info
##' @param data data to filter
##' @param variables variables that we want filter
##' @param quantile_p quantile as filter
##' @return character with information
##' @author Fabian Coloma
##' @export
##' @examples
##'
##'
quant_filt_info <- function(data, variables, quantile_p = 0.95){

    for(i in variables){

        print("  ")

        print("   ")

        print(paste("Informacion de la variable : ", i ))

        quant <- quantile(
            pull(data, i),
            quantile_p
        )

        print("el valor del quantile es: ")

        print(round(quant,2))

        dim <- data %>%
            filter(
                get(i) <= quant
            ) %>%
            dim()
        
        print("la dimension es : ")
        print(dim)

        print("---------------------------------------")
        print("---------------------------------------")
        
    }

}

##' add function description
##'
##'
##' @title density_plot_1_var
##' @param data ADD_1rt_PARAMETER_DESCRIPTION
##' @param variable ADD_2on_PARAMETER_DESCRIPTION
##' @param quantile_p ADD_3r_PARAMETER_DESCRIPTION
##' @return data.frame / list / vector
##' @author Fabian Coloma
##' @export
##' @examples
##' 
##'
density_plot_1_var <- function(data, variable, quantile_p = 0.95){
    
    quant <- quantile(pull(data, variable), quantile_p)

    long_n <- length(pull(data, variable)[pull(data, variable) <= quant])

    ggplot(
        data %>%
            filter(
                get(variable) <= quant
            ), 
        aes(
            x = get(variable)
        )
    ) + 
    geom_density(
        fill = "blue",
        alpha = 0.5
    ) +
    theme_bw() +
    labs(
        x = variable,
        y = "density"
    ) +
    ggtitle(
       paste(
            "Density distribution with ",
            long_n, 
            " cases "
       ) 
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
covariables_paste_fml <- function(fml_anterior = "", vector_cov_current, outcome_var){

    if(fml_anterior[1] == ""){
        cov <-  paste(vector_cov_current, collapse = " + ")
    } else {
        
        fml_ant <- as.character(fml_anterior)

        cov_pre <- paste(vector_cov_current, collapse = " + ")
        
        cov <- paste(fml_ant[3], cov_pre, sep = " + ")
    }

    fml <- paste(outcome_var, cov, sep = " ~ ")

    as.formula(fml)
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
include_ipw <- function(data, model_ipw_1, model_ipw_2, model_ipw_3, model_ipw_4){

    data$prob_ml_1 <- predict(model_ipw_1, type = "response") 

    data$prob_ml_2 <- predict(model_ipw_2, type = "response") 

    data$prob_ml_3 <- predict(model_ipw_3, type = "response")

    data$prob_ml_4 <- predict(model_ipw_4, type = "response")

##########
##########
##  PART 2 - we will create the ipw as variables of the 
##########
##########

data %>%
    mutate(
        ipw_ml_1 = 1 / prob_ml_1,
        # sprw_ml_1 = mean(prob_ml_1, na.rm = TRUE) / prob_ml_1,

        ipw_ml_2 = 1 / prob_ml_2,
        # sprw_ml_2 = mean(prob_ml_2, na.rm = TRUE) / prob_ml_2,

        ipw_ml_3 = 1 / prob_ml_3,
        # sprw_ml_3 = mean(prob_ml_3, na.rm = TRUE) / prob_ml_3,

        ipw_ml_4 = 1 / prob_ml_4
    
    )

}




##' On this function we are creating a descriptive definition for the models, that is 
##' used to create de x axis on the plots
##'
##' @title model desc creation
##'
##' @param variables_to_create vector. is a vector with a name list of the variables which are used
##' @param recursive_model boolean. TRUE if the model is adding information to the past model
##' @param if_no_recursive_model_base_model character. definition of the base model
##'
##' @return A data frame.
##' @export
##'
##' @author Fabian Coloma
data_model_desc_function <- function(
    variables_to_create,
    recursive_model = FALSE,
    if_no_recursive_model_base_model = "M6",
    forest_t = FALSE
){
  # TODO: realizar el recursive correctamente 
  # TODO: - 

    model_id_vec <- c()

    model_desc_vec <- c()

    for(i in 1:length(variables_to_create)){

        if(i<10){
          nn <- paste0("0",i)
        }else(
          nn <- i
        )

        mod_id <- paste0("model_", nn)

        model_id_vec <-c(model_id_vec, mod_id)
        if(forest_t == TRUE){
            mod_desc <- paste0( 
                                 nn,
                                 " - ",
                                 variables_to_create[i])
        } else {
          if(recursive_model == FALSE){
              mod_desc <- paste0(
                  "M",
                  nn, 
                  "(",
                  if_no_recursive_model_base_model,
                  " + ",
                  variables_to_create[i],
                  ")"
                  )
          } else{
              mod_desc <- paste0(
                  "M",
                  nn, 
                  "(M",
                  nn - 1,
                  " + ",
                  variables_to_create[i],
                  ")"
                  )
          }
      }

        

        model_desc_vec <- c(model_desc_vec, mod_desc)

    }

    data.frame(
        model_id = model_id_vec,
        model_desc = model_desc_vec,
        term_id = variables_to_create
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
vif_check <- function(
    model, 
    check = 5
){
    vif_res <- as.data.frame(car::vif(model))

    vif_names <- row.names(vif_res)
    
    vif_sig_1 <- vif_res[1] >= check

    

    if(dim(vif_res)[2] == 1){

      vif_norm <- paste(vif_names[vif_sig_1], collapse = " |-| ")

      vif_std <- " "

    }else{

      vif_sig_2 <- vif_res[2] >= check

      vif_norm <- paste(vif_names[vif_sig_1], collapse = " |-| ")

      vif_std <- paste(vif_names[vif_sig_2], collapse = " |-| ")

    }


  # vis normal indicate if any vif no standarise is greater than 5
  # vis std indicate if the vif is greather tha 5
    c(vif_norm, vif_std)

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
desc_models <- function(fml, has_ipw = FALSE, ipw_selected = "ipw_ml_4"){
  data_ini <- purrr::map_df(
    fml, 
    function(x){
      fml_parts <- as.character(as.formula(x))
      
      data.frame(
        fml_complet = paste(c(
          fml_parts[2],
          fml_parts[1],
          fml_parts[3]
        ), collapse = " "),
        outcome = fml_parts[2],
        cov = fml_parts[3],
        has_ipw_def = has_ipw,
        which_ipw = ipw_selected

      )
    }
  )

  data_ini$row_number <- seq(1:nrow(data_ini))
  
  data_ini %>%
    mutate(
      model_id = ifelse( row_number < 10, 
                         paste("model_0", row_number, sep = ""), 
                         paste("model_", row_number, sep = "") )
    ) %>%
    dplyr::select(-row_number)

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
fit_nb <- function(model_id_i, data_1, model_info, ipw_select){

  mod_info <- model_info %>%
    filter(model_id == model_id_i)

  formula <- mod_info %>% pull(., "fml_complet") %>% as.formula(.)

  ipw_mod <- mod_info %>% pull(., "has_ipw_def")

  ipw_select <- mod_info %>% pull(., "which_ipw")

  if(ipw_mod == FALSE){
    mod <- MASS::glm.nb(
      formula,
      data = data_1 
    )
  }

  if(ipw_mod == TRUE){
    mod <- MASS::glm.nb(
      formula,
      data = data_1,
      weights = dplyr::pull(data_1, ipw_select)
    )
  }

  aic_mod <- AIC(mod)
  bic_mod <- BIC(mod)

  vif_result <- vif_check(mod, check = 5)


  coef <- summary(mod)$coefficients %>%
    as.data.frame() %>%
    mutate(
      term = rownames(.),
      model_id = model_id_i,
      mod_info = "negative binomial",
      aic = aic_mod,
      bic = bic_mod
      ,
      vif_normal = vif_result[1],
      vif_std = vif_result[2]
    ) %>%
    `rownames<-`( NULL )

    coef$Pr_FDR <- stats::p.adjust(coef$`Pr(>|z|)`, method="BH")

    coef
}
##' add function description
##'
##'
##' @title fit_pois
##' @param data1 ADD_1rt_PARAMETER_DESCRIPTION
##' @param data2 ADD_2on_PARAMETER_DESCRIPTION
##' @param variable ADD_3r_PARAMETER_DESCRIPTION
##' @return data.frame / list / vector
##' @author Fabian Coloma
##' @export
##' @examples
##' 
##'
fit_pois <- function(model_id_i, data_1, model_info, ipw_select){

  mod_info <- model_info %>%
    filter(model_id == model_id_i)

  formula <- mod_info %>% pull(., "fml_complet") %>% as.formula(.)

  ipw_mod <- mod_info %>% pull(., "has_ipw_def")

  ipw_select <- mod_info %>% pull(., "which_ipw")

  if(ipw_mod == FALSE){
    mod <- stats::glm(
      formula,
      data = data_1,
      family = poisson(link = "log")
    )
  }

  if(ipw_mod == TRUE){
    mod <- stats::glm(
      formula,
      data = data_1,
      family = poisson(link = "log"),
      weights = pull(data_1, ipw_select)
    )
  }

  aic_mod <- AIC(mod)
  bic_mod <- BIC(mod)

  vif_result <- vif_check(mod, check = 5)

  summary(mod)$coefficients %>%
    as.data.frame() %>%
    mutate(
      term = rownames(.),
      model_id = model_id_i,
      mod_info = "poisson",
      aic = aic_mod,
      bic = bic_mod,
      vif_normal = vif_result[1],
      vif_std = vif_result[2]
    ) %>%
    `rownames<-`( NULL )

}

##'
##'iterative model apply, and saving the plot on 
##'
##'@title fit_zi
##'@param model_id_i XXX_xxx_XXX
##'@param data_1 XXX_xxx_XXX_xxx
##'@param ipw_select xxx_XXX_xxx_XXX
##'@param fam_zi can choice: "negbin", "poisson", geometric
##'@return data.frame / list / vector
##'@author Fabian Coloma
##'@export
##'@examples
##'
##
fit_RI_hurdle <- function( model_id_i, 
                    data_1, 
                    model_info, 
                    ipw_select,
                    fam_zi = "negbin"){

  mod_info <- model_info %>%
    filter(model_id == model_id_i)

  formula <- mod_info %>% pull(., "fml_complet") %>% as.formula(.)
  formula_ri <- mod_info %>% pull(., "RI_formula") %>% as.formula(.)

  ipw_mod <- mod_info %>% pull(., "has_ipw_def")

  ipw_select <- mod_info %>% pull(., "which_ipw")

  if(ipw_mod == FALSE){
    mod <- glmmTMB(formula,
                   ziformula = formula_ri,
                   family = truncated_nbinom2,
                   data = data_1)

  }

  if(ipw_mod == TRUE){

    cat("at the moment it is not developed,maybe will, maybe not")
    # mod <- pscl::zeroinfl(
    #   formula,
    #   dist = fam_zi,
    #   data = data_1,
    #   weights = pull(data_1, ipw_select)
    # )
  }

  aic_mod <- AIC(mod)
  bic_mod <- BIC(mod)

  a <- summary(mod)$coefficients$zi %>%
    as.data.frame() %>%
    mutate(
      term = rownames(.),
      model_id = model_id_i,
      mod_info = "HRD zero"
    ) %>%
    `rownames<-`( NULL )

  b <- summary(mod)$coefficients$cond %>%
    as.data.frame() %>%
    mutate(
      term = rownames(.),
      model_id = model_id_i,
      mod_info = "HRD count"
    ) %>%
    `rownames<-`( NULL )

  rbind(a,b) %>%
  mutate(
      aic = aic_mod,
      bic = bic_mod
  )

}


fit_zi <- function( model_id_i, 
                    data_1, 
                    model_info, 
                    ipw_select,
                    fam_zi = "negbin"){

  mod_info <- model_info %>%
    filter(model_id == model_id_i)

  formula <- mod_info %>% pull(., "fml_complet") %>% as.formula(.)

  ipw_mod <- mod_info %>% pull(., "has_ipw_def")

  ipw_select <- mod_info %>% pull(., "which_ipw")

  if(ipw_mod == FALSE){
    mod <- pscl::zeroinfl(
      formula,
      dist = fam_zi,
      data = data_1
    )
  }

  if(ipw_mod == TRUE){
    mod <- pscl::zeroinfl(
      formula,
      dist = fam_zi,
      data = data_1,
      weights = pull(data_1, ipw_select)
    )
  }

  aic_mod <- AIC(mod)
  bic_mod <- BIC(mod)

  a <- summary(mod)$coefficients$zero %>%
    as.data.frame() %>%
    mutate(
      term = rownames(.),
      model_id = model_id_i,
      mod_info = "ZI zero"
    ) %>%
    `rownames<-`( NULL )

  b <- summary(mod)$coefficients$count %>%
    as.data.frame() %>%
    mutate(
      term = rownames(.),
      model_id = model_id_i,
      mod_info = "ZI count"
    ) %>%
    `rownames<-`( NULL )

  rbind(a,b) %>%
  mutate(
      aic = aic_mod,
      bic = bic_mod
  )

}


##'
##'iterative model apply, and saving the plot on 
##'
##'@title fit_zi
##'@param model_id_i XXX_xxx_XXX
##'@param data_1 XXX_xxx_XXX_xxx
##'@param ipw_select xxx_XXX_xxx_XXX
##'@param fam_zi can choice: "negbin", "poisson", geometric
##'@return data.frame / list / vector
##'@author Fabian Coloma
##'@export
##'@examples
##'
##
fit_RI_ZI <- function( model_id_i, 
                    data_1, 
                    model_info, 
                    ipw_select,
                    fam_zi = "negbin"){

  mod_info <- model_info %>%
    filter(model_id == model_id_i)

  formula <- mod_info %>% pull(., "fml_complet") %>% as.formula(.)
  formula_ri <- mod_info %>% pull(., "RI_formula") %>% as.formula(.)

  ipw_mod <- mod_info %>% pull(., "has_ipw_def")

  ipw_select <- mod_info %>% pull(., "which_ipw")

  if(ipw_mod == FALSE){
    mod <- glmmTMB(formula,
                   ziformula = formula_ri,
                   family = "nbinom2",
                   data = data_1)

  }

  if(ipw_mod == TRUE){

    cat("at the moment it is not developed,maybe will, maybe not")
    # mod <- pscl::zeroinfl(
    #   formula,
    #   dist = fam_zi,
    #   data = data_1,
    #   weights = pull(data_1, ipw_select)
    # )
  }

  aic_mod <- AIC(mod)
  bic_mod <- BIC(mod)

  a <- summary(mod)$coefficients$zi %>%
    as.data.frame() %>%
    mutate(
      term = rownames(.),
      model_id = model_id_i,
      mod_info = "HRD zero"
    ) %>%
    `rownames<-`( NULL )

  b <- summary(mod)$coefficients$cond %>%
    as.data.frame() %>%
    mutate(
      term = rownames(.),
      model_id = model_id_i,
      mod_info = "HRD count"
    ) %>%
    `rownames<-`( NULL )

  rbind(a,b) %>%
  mutate(
      aic = aic_mod,
      bic = bic_mod
  )

}



# fit_hurdle <- function(model_id_i, data_1, model_info, ipw_select){

#   mod_info <- model_info %>%
#     filter(model_id == model_id_i)

#   formula <- mod_info %>% pull(., "fml_complet") %>% as.formula(.)

#   ipw_mod <- mod_info %>% pull(., "has_ipw_def")

#   ipw_select <- mod_info %>% pull(., "which_ipw")

#   if(ipw_mod == FALSE){
#     mod <- pscl::zeroinfl(
#       formula,
#       dist = "negbin",
#       data = data_1
#     )
#   }

#   if(ipw_mod == TRUE){
#     mod <- pscl::zeroinfl(
#       formula,
#       dist = "negbin",
#       data = data_1,
#       weights = pull(data_1, ipw_select)
#     )
#   }

#   aic_mod <- AIC(mod)
#   bic_mod <- BIC(mod)

#   a <- summary(mod)$coefficients$zero %>%
#     as.data.frame() %>%
#     mutate(
#       term = rownames(.),
#       model_id = model_id_i,
#       mod_info = "ZI zero"
#     ) %>%
#     `rownames<-`( NULL )

#   b <- summary(mod)$coefficients$count %>%
#     as.data.frame() %>%
#     mutate(
#       term = rownames(.),
#       model_id = model_id_i,
#       mod_info = "ZI count"
#     ) %>%
#     `rownames<-`( NULL )

#   rbind(a,b) %>%
#   mutate(
#       aic = aic_mod,
#       bic = bic_mod
#   )

# }

##' add function description
##'
##'
##' @title fit_logistic
##' @param data1 ADD_1rt_PARAMETER_DESCRIPTION
##' @param data2 ADD_2on_PARAMETER_DESCRIPTION
##' @param variable ADD_3r_PARAMETER_DESCRIPTION
##' @return data.frame / list / vector
##' @author Fabian Coloma
##' @export
##' @examples
##' 
##'
fit_logistic <- function(model_id_i, data_1, model_info, ipw_select){

  mod_info <- model_info %>%
    filter(model_id == model_id_i)

  formula <- mod_info %>% pull(., "fml_complet") %>% as.formula(.)

  ipw_mod <- mod_info %>% pull(., "has_ipw_def")

  ipw_select <- mod_info %>% pull(., "which_ipw")

  if(ipw_mod == FALSE){
    mod <- stats::glm(
      formula,
      data = data_1,
      family = binomial
    )
  }

  if(ipw_mod == TRUE){
    mod <- stats::glm(
      formula,
      data = data_1,
      family = binomial,
      weights = pull(data_1, ipw_select)
    )
  }

  aic_mod <- AIC(mod)
  bic_mod <- BIC(mod)

  summary(mod)$coefficients %>%
    as.data.frame() %>%
    mutate(
      term = rownames(.),
      model_id = model_id_i,
      mod_info = "binomial",
      aic = aic_mod,
      bic = bic_mod
    ) %>%
    `rownames<-`( NULL )

}

##' add function description
##'
##'
##' @title fit_quasipois
##' @param data1 ADD_1rt_PARAMETER_DESCRIPTION
##' @param data2 ADD_2on_PARAMETER_DESCRIPTION
##' @param variable ADD_3r_PARAMETER_DESCRIPTION
##' @return data.frame / list / vector
##' @author Fabian Coloma
##' @export
##' @examples
##' 
##'
fit_quasipois <- function(model_id_i, data_1, model_info, ipw_select){

  mod_info <- model_info %>%
    filter(model_id == model_id_i)

  formula <- mod_info %>% pull(., "fml_complet") %>% as.formula(.)

  ipw_mod <- mod_info %>% pull(., "has_ipw_def")

  ipw_select <- mod_info %>% pull(., "which_ipw")

  if(ipw_mod == FALSE){
    mod <- stats::glm(
      formula,
      data = data_1,
      family = quasipoisson
    )
  }

  if(ipw_mod == TRUE){
    mod <- stats::glm(
      formula,
      data = data_1,
      family = quasipoisson,
      weights = pull(data_1, ipw_select)
    )
  }

  aic_mod <- AIC(mod)
  bic_mod <- BIC(mod)

  vif_result <- vif_check(mod, check = 5)

  summary(mod)$coefficients %>%
    as.data.frame() %>%
    mutate(
      term = rownames(.),
      model_id = model_id_i,
      mod_info = "quasi poisson",
      aic = aic_mod,
      bic = bic_mod,
      vif_normal = vif_result[1],
      vif_std = vif_result[2]
    ) %>%
    `rownames<-`( NULL )

}


##' add function description
##'
##'
##' @title mapGeneralModelsCount
##' @param data1 ADD_1rt_PARAMETER_DESCRIPTION
##' @param data2 ADD_2on_PARAMETER_DESCRIPTION
##' @param variable ADD_3r_PARAMETER_DESCRIPTION
##' @return data.frame / list / vector
##' @author Fabian Coloma
##' @export
##' @examples
##' 
##'
mapGeneralModelsCount <- function(
  formulas_to_exe,
  data_mod,
  family_def = "negative binomial",
  fml_ri_hurd = NULL,
  ipw = FALSE,
  ipw_select = "ipw_ml_4",
  transformed_names = "default",
  path_save = ""){

  family_accepted <- c(
    "negative binomial",
    "poisson",
    "zero inflated",
    "zero inflated pois",
    "logistic",
    "quasipoisson",
    "ri-hurdle",
    "ri-zero inflated"
  )

  stopifnot(family_def %in% family_accepted)

  information_models <- desc_models(
    formulas_to_exe,
    has_ipw = ipw,
    ipw_selected = ipw_select
  )

  if(family_def %in%  c("ri-hurdle", "ri-zero inflated")){
    information_models$RI_formula <- fml_ri_hurd
  }

  if(family_def == family_accepted[1]){
    res <- purrr::map_df(
      information_models$model_id,
      function(x){
        fit_nb(x, data_1 = data_mod, model_info = information_models)
      }
    )
  }

  if(family_def == family_accepted[2]){
    res <- purrr::map_df(
      information_models$model_id,
      function(x){
        fit_pois(x, data_1 = data_mod, model_info = information_models)
      }
    )
  }

  if(family_def == family_accepted[3]){
    res <- purrr::map_df(
      information_models$model_id,
      function(x){
        fit_zi(x, data_1 = data_mod, model_info = information_models)
      }
    )
  }

  if(family_def == family_accepted[4]){
    res <- purrr::map_df(
      information_models$model_id,
      function(x){
        fit_zi(x, data_1 = data_mod, model_info = information_models, fam_zi = "poisson")
      }
    )
  }

  if(family_def == family_accepted[5]){
    res <- purrr::map_df(
      information_models$model_id,
      function(x){
        fit_logistic(x, data_1 = data_mod, model_info = information_models)
      }
    )
  }

  if(family_def == family_accepted[6]){
    res <- purrr::map_df(
      information_models$model_id,
      function(x){
        fit_quasipois(x, data_1 = data_mod, model_info = information_models)
      }
    )
  }

    if(family_def == family_accepted[7]){
    res <- purrr::map_df(
      information_models$model_id,
      function(x){
        fit_RI_hurdle(x, data_1 = data_mod, model_info = information_models)
      }
    )
  }

      if(family_def == family_accepted[8]){
    res <- purrr::map_df(
      information_models$model_id,
      function(x){
        fit_RI_ZI(x, data_1 = data_mod, model_info = information_models)
      }
    )
  }

  result <- list(
    coef_table = res,
    mod_info = information_models,
    transformed_names = transformed_names
  )

  if(!(path_save == "")){
    saveRDS(result, path_save)
  }

  result

}

##' add function description
##'
##'
##' @title report_generation
##' @param data1 ADD_1rt_PARAMETER_DESCRIPTION
##' @param data2 ADD_2on_PARAMETER_DESCRIPTION
##' @param variable ADD_3r_PARAMETER_DESCRIPTION
##' @return data.frame / list / vector
##' @author Fabian Coloma
##' @export
##' @examples
##' 
##'
report_generation <- function(
    data,
    outcome_variable = "h_respiratory_all_visits_v3",
    perc_max_visits = 1,
    perc_max_ipw = 0,
    list_posto_admnis = c("Manhica -Sede", "3 de Fevereiro", "Maluana",
        "Ilha Josina Machel", "Calanga", "Xinavane"),
    vExtra_info = "",
    path_to_work_initials = NULL,
    which_ses = "SES_var",
    source = "ilumination_fuel",
    cov_1 = NULL,
    cov_2 = NULL,
    cov_3 = NULL,
    cov_4 = NULL,
    cov_5 = NULL,
    cov_6 = NULL,
    models_definition = NULL
  ){
 
  extra_pot <- paste0(
    as.character(
        min(data$child_dob)
    ),
    " to ",
    as.character(
        max(data$child_dob)
    )
        )

  perc_vis <- sub(
    "[.]",
    "_",
    as.character(perc_max_visits)
    )

  perc_ipw <- sub(
    "[.]",
    "_",
    as.character(perc_max_ipw)
    )



  path_to_work <- paste0( path_to_work_initials,
                          "_",
                          extra_pot,
                          "-POSTOnum_",
                          length(list_posto_admnis),
                          "-OUTCOME_",
                          outcome_variable,
                          "-",
                          "FILTER_VISITS_",
                          perc_vis,
                          "-FILTER_IPW_",
                          perc_ipw,
                          "-",
                          vExtra_info )

    vExtra_info <- extra_pot

#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
#    creating the paths
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

  if(length(path_to_work) != 0){

    ##########
    ##########
    ## folder to save
    ##########
    ##########
      
    cat(
        paste0("folder_to_save <- '", path_to_work, "'"),
        file = here(
                # "FC-manhica_child_adversity_data_paper2_v2",
                "setting_files",
                "paths_ref.R"
            ),
        append = FALSE
    )

    ##########
    ##########
    ## folder to read
    ##########
    ##########

    cat(
        " ; path_from_read <- folder_to_save",
        file = here(
                # "FC-manhica_child_adversity_data_paper2_v2",
                "setting_files",
                "paths_ref.R"
            ),
        append = TRUE
    )

    }


  # set information

  source(
      here(
          # "FC-manhica_child_adversity_data_paper2_v2",
          "setting_files",
          "paths_ref.R"
      )
  )

    # # # dir.create(
    # # #     here(
    # # #         # "FC-manhica_child_adversity_data_paper2_v2",
    # # #         "reports",
    # # #         "model_results_visits",
    # # #         folder_to_save
    # # #     ),
    # # #     showWarnings = FALSE
    # # # )

    dir.create(
        here(
            # "FC-manhica_child_adversity_data_paper2_v2",
            "outputs",
            "paper_1_R_model_results",
            folder_to_save
        ),
        showWarnings = FALSE
    )
    
    # own functions 

    source(here::here("R/modelling-data.R"))

    # source("Y:/fcoloma/package_fabi/R/report_nan_by_var.R")

    # # # source(here::here(
    # # #   # "FC-manhica_child_adversity_data_paper2_v2",
    # # #   "R",
    # # #   "process-models.R"
    # # #   ))


    data$posto_admnistrativo <- factor(
        data$posto_admnistrativo,
        levels = c("Manhica -Sede", "3 de Fevereiro", "Maluana",
        "Ilha Josina Machel", "Calanga", "Xinavane")
    )

    data$posto_2 <- factor(
        data$posto_2,
        levels = c("Manhica -Sede", "3 de Fevereiro", "Maluana",
        "Ilha Josina Machel", "Xinavane")
    )
    
    data$year_birth <- factor(
        data$year_birth,
        levels = c(
            "2020",
            "2016",
            "2017",
            "2018",
            "2019"
            

        )
    )

    rows_of_base_data <- nrow(data)

    variable_out_selected <- outcome_variable

    data_2 <- data %>%
        filter(
            (posto_admnistrativo %in% list_posto_admnis)
        )
    
    
    list_posto_used <- list_posto_admnis

    n_limit_reg_remain <- nrow(data_2)


    data_pre_ipw <- data_2 %>%
        filter(
            get(outcome_variable) <= quantile(pull(data, outcome_variable), perc_max_visits)
        )

    perc_visits_deleted <- perc_max_visits
    
    n_limit_visits <- quantile(pull(data, outcome_variable), perc_max_visits)

    n_limit_visits_remain <- nrow(data_pre_ipw)
    
    ########################################################################
    ########################################################################
    #   PART 2 - we will create the ipw as variables of the 
    ########################################################################
    ########################################################################
    
    fml_4 <- "ilumination_fuel_v2_post_bn ~ offset(log(fu_months)) + gender + year_birth + mother_age_at_birth_of_child + posto_admnistrativo + healthdist_final_std + mother_education + SES_var"
    
    model_ipw_4 <- glm(
            fml_4,
            data = data_pre_ipw,
            family = binomial(link = "logit")
        )

    data_pre_ipw$prob_ml_4 <- predict(model_ipw_4, type = "response")

    data_ipw <- data_pre_ipw %>%
        mutate(

            ipw_ml_4 = 1 / prob_ml_4
        
        )


    data_filt_pre <- data_ipw %>% 
            left_join(
                data_ipw %>%
                dplyr::select(
                    posto_admnistrativo,
                    ipw_ml_4
                ) %>% 
                group_by(
                    posto_admnistrativo
                ) %>% 
                summarise(
                    quantile_99D_ipw = quantile(ipw_ml_4, 1 - perc_max_ipw)
                    # ,
                    # quantile_01D_ipw = quantile(ipw_ml_4, perc_max_ipw)
                ),
                by = "posto_admnistrativo"

        ) %>%
        filter(
            ipw_ml_4 <= quantile_99D_ipw
            # ,
            # ipw_ml_4 >= quantile_01D_ipw
        ) %>% 
        as.data.frame()


    perc_ipw_deleted <- perc_max_ipw

    n_limit_ipw_remain <- nrow(data_filt_pre)





    # data_filt <- data_filt_pre %>%
    #     filter(
    #         (posto_admnistrativo %in% list_posto_admnis)
    #     )
  
  data_filt <- data_filt_pre 

  dimension_of_data <- nrow(data_filt)

########################################################################
########################################################################
#   PART 1 - COVARIABLES
########################################################################
########################################################################

# that is the base from past versioning
outcome_v <- outcome_variable

# source(
#     here(
#         "FC-manhica_child_adversity_data_paper2_v2",
#         "setting_files",
#         "covariables_def_8.R"
#     )
# )

if(
  is.null(cov_1) |
  is.null(cov_2) |
  is.null(cov_3) |
  is.null(cov_4) |
  is.null(cov_5) |
  is.null(cov_6) 
){
  
  SES_def <- which_ses 

  cov_1 <- c(
      "ilumination_fuel_v2_post",
      "offset(log(fu_days + 1))",
      "gender"
  )

  cov_2 <- c(
      cov_1,
      SES_def
  )

  cov_3 <- c(
      cov_2,
      "healthdist_final_std"
  )

  cov_4 <- c(
      cov_3,
      "posto_admnistrativo"
  )

  cov_5 <- c(
      cov_4,
      "as.factor(year_birth)"

  )

  cov_6 <- c(
      cov_5,
      "mother_age_at_birth_of_child",
      "mother_education_cat"

  )

  data_mod_desc <- data.frame(
  model_id = c(
    "model_01",
    "model_02",
    "model_03",
    "model_04",
    "model_05",
    "model_06"
  ),
  model_desc =  c(
    "M1(sex)", 
    "M2(M1 + SES var)",
    "M3(M2 + Health distance)",
    "M4(M3 + posto admn)",
    "M5(M4 + year of birth)",
    "M6(M5 + moth. age at dob + moth. educ.)")
  )
} else {
  data_mod_desc <- data.frame(
  model_id = c(
    "model_01",
    "model_02",
    "model_03",
    "model_04",
    "model_05",
    "model_06"
  ),
  model_desc =  models_definition
  )
}





# ########################################################################
# ########################################################################
# #   PART 2 - Formulas creation
# ########################################################################
# ########################################################################

# source(
#     here(
#         "FC-manhica_child_adversity_data_paper2_v2",
#         "setting_files",
#         "formulas_def_8.R"
#     )
# )

# MODEL 1

fml_1 <- covariables_paste_fml(
    fml_anterior = "",
    vector_cov_current = cov_1,
    outcome_var = outcome_v
)

# MODEL 2

fml_2 <- covariables_paste_fml(
    fml_anterior = "",
    vector_cov_current = cov_2,
    outcome_var = outcome_v
)

# MODEL 3

fml_3 <- covariables_paste_fml(
    fml_anterior = "",
    vector_cov_current = cov_3,
    outcome_var = outcome_v
)

# MODEL 4

fml_4 <- covariables_paste_fml(
    fml_anterior = "",
    vector_cov_current = cov_4,
    outcome_var = outcome_v
)

# MODEL 5

fml_5 <- covariables_paste_fml(
    fml_anterior = "",
    vector_cov_current = cov_5,
    outcome_var = outcome_v
)

# MODEL 6

fml_6 <- covariables_paste_fml(
    fml_anterior = "",
    vector_cov_current = cov_6,
    outcome_var = outcome_v
)


fml <- c(
    fml_1,
    fml_2,
    fml_3,
    fml_4,
    fml_5,
    fml_6
    # ,
    # fml_7
    # ,
    # fml_8
    )


########################################################################
########################################################################
#   PART 2 - MODELS
########################################################################
########################################################################

###  summary_na_report(data_filt, names(data_filt), "h_respiratory_visists_v3") %>% View()
# --- negative binomial
###

modeRes <- mapGeneralModelsCount(
    formulas_to_exe = fml,
    data_mod = data_filt,
    family_def = "negative binomial",
    ipw = FALSE,
    # ipw_select = "ipw_ml_4",
    path_save = here(
        # "FC-manhica_child_adversity_data_paper2_v2",
        "outputs",
        "paper_1_R_model_results",
        folder_to_save,
        "negative_bn_no_IPW.rds"
    )
)

modeRes <- mapGeneralModelsCount(
    formulas_to_exe = fml,
    data_mod = data_filt,
    family_def = "negative binomial",
    ipw = TRUE,
    ipw_select = "ipw_ml_4",
    path_save = here(
        # "FC-manhica_child_adversity_data_paper2_v2",
        "outputs",
        "paper_1_R_model_results",
        folder_to_save,
        "negative_bn_IPW.rds"
    )
)

###
# --- poisson
###

modeRes <- mapGeneralModelsCount(
    formulas_to_exe = fml,
    data_mod = data_filt,
    family_def = "poisson",
    ipw = FALSE,
    ipw_select = "ipw_ml_4",
    path_save = here(
        # "FC-manhica_child_adversity_data_paper2_v2",
        "outputs",
        "paper_1_R_model_results",
        folder_to_save,
        "poisson_no_IPW.rds"
    )
)

modeRes <- mapGeneralModelsCount(
    formulas_to_exe = fml,
    data_mod = data_filt,
    family_def = "poisson",
    ipw = TRUE,
    ipw_select = "ipw_ml_4",
    path_save = here(
        # "FC-manhica_child_adversity_data_paper2_v2",
        "outputs",
        "paper_1_R_model_results",
        folder_to_save,
        "poisson_IPW.rds"
    )
)

###
# --- quasipoisson
###

modeRes <- mapGeneralModelsCount(
    formulas_to_exe = fml,
    data_mod = data_filt,
    family_def = "quasipoisson",
    ipw = FALSE,
    ipw_select = "ipw_ml_4",
    path_save = here(
        # "FC-manhica_child_adversity_data_paper2_v2",
        "outputs",
        "paper_1_R_model_results",
        folder_to_save,
        "quasipoisson_no_IPW.rds"
    )
)

modeRes <- mapGeneralModelsCount(
    formulas_to_exe = fml,
    data_mod = data_filt,
    family_def = "quasipoisson",
    ipw = TRUE,
    ipw_select = "ipw_ml_4",
    path_save = here(
        # "FC-manhica_child_adversity_data_paper2_v2",
        "outputs",
        "paper_1_R_model_results",
        folder_to_save,
        "quasipoisson_IPW.rds"
    )
)

###
# --- Zero inflated regresion
###

modeRes <- mapGeneralModelsCount(
    formulas_to_exe = fml,
    data_mod = data_filt,
    family_def = "zero inflated",
    ipw = FALSE,
    ipw_select = "ipw_ml_4",
    path_save = here(
        # "FC-manhica_child_adversity_data_paper2_v2",
        "outputs",
        "paper_1_R_model_results",
        folder_to_save,
        "ZI_no_IPW.rds"
    )
)

modeRes <- mapGeneralModelsCount(
    formulas_to_exe = fml,
    data_mod = data_filt,
    family_def = "zero inflated",
    ipw = TRUE,
    ipw_select = "ipw_ml_4",
    path_save = here(
        # "FC-manhica_child_adversity_data_paper2_v2",
        "outputs",
        "paper_1_R_model_results",
        folder_to_save,
        "ZI_IPW.rds"
    )
)


########################################################################
#   PART 3 - MADE REPORT part
########################################################################
########################################################################
vExtra_info <- vExtra_info

source_ilu <- source

number_of_models <- length(fml)

##########
##########
## adding visit and ipw distribution
##########
##########
# Change line color and fill color
# ggplot(data_filt, aes(x=h_visits))+
#   geom_histogram( color="darkblue", 
#                   fill="lightblue",
#                   bins = 150
#                 )

# ggplot(data_filt, aes(x=ipw_ml_4))+
#   geom_histogram( color="darkblue", 
#                   fill="lightblue",
#                   bins = 150
#                 )

# summary(data_filt$ipw_ml_4)

# outcome_variable_vector <- pull(data_filt, outcome_v)

# ipw_variable_vector <- pull(data_filt, "ipw_ml_4")
##########
##########
## ADD_YOUR_DESCRIPTION
##########
##########


    save(
      data_filt,
      source_ilu, 
      data_mod_desc,
      number_of_models,
      rows_of_base_data,
      variable_out_selected,
      perc_visits_deleted,
      n_limit_visits,
      n_limit_visits_remain,
      perc_ipw_deleted,
      n_limit_ipw_remain,
      list_posto_used,
      n_limit_reg_remain,
      vExtra_info,
      file = here(
          # "FC-manhica_child_adversity_data_paper2_v2",
          "outputs",
          "paper_1_R_model_results",
          folder_to_save,
          "info_accros.RData"
      )

    )



  ########################################################################
  #   PART 3 - MADE REPORT part
  ########################################################################
  ########################################################################

  rmarkdown::render(
      here::here(
          # "FC-manhica_child_adversity_data_paper2_v2",
          "reports",
          "Templates",
          "BASE_v2_paper_1",
          "index.Rmd"),
      output_format = "all",
      output_dir = here::here(
          # "FC-manhica_child_adversity_data_paper2_v2",
          "reports",
          "reports-results",
          "Paper_1_ses_char",
          folder_to_save
          )
  )

}

##' add function description
##'
##'
##' @title fml_vec_creator
##' @param data1 ADD_1rt_PARAMETER_DESCRIPTION
##' @param data2 ADD_2on_PARAMETER_DESCRIPTION
##' @param variable ADD_3r_PARAMETER_DESCRIPTION
##' @return data.frame / list / vector
##' @author Fabian Coloma
##' @export
##' @examples
##' 
##'
fml_vec_creator <- function(fml_base, vec_vars){
  fml_vec <- c()

  for(i in 1:length(vec_vars)){

    fml_2 <- stats::as.formula(paste(fml_base, vec_vars[i], sep = " + "))

    fml_vec <- c(fml_vec, fml_2)

  }

  fml_vec
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
data_prep_estimates <- function(data_1, data_2, especial_test = FALSE, data_mod_desct){

if(especial_test == FALSE){

  pre_plot <- data_1$coef_table %>% mutate(ipw_t = "Without IPW")  %>%
    rbind( ., data_2$coef_table %>% mutate(ipw_t = "With IPW")) %>%
    left_join(
      data_mod_desct,
      by = "model_id"
    ) %>%
  rename( vif_grt10 = "vif_normal",
          vif_std_grt10 = "vif_std" )

  to_plot <- pre_plot %>%
        mutate(Exp_Est = exp(Estimate)) %>% 
        hr.perincr(., 1) %>% 
        dplyr::select(
          model_desc,
          mod_info,
          has_IPW = ipw_t,
          Covariable = term,
          Coef = Estimate,
          beta_exp,
          Lower_exp,
          Upper_exp,
          `Std. Error`,
          `Pr(>|z|)`,
          aic,
          bic,
          vif_grt10,
          vif_std_grt10
        )

} else {

  if(especial_test == "zi"){

     pre_plot <- data_1$coef_table %>% mutate(ipw_t = "Without IPW")  %>%
      rbind( ., data_2$coef_table %>% mutate(ipw_t = "With IPW")) %>%
      left_join(
        data_mod_desct,
        by = "model_id"
      ) 
      # %>%
      # rename( vif_grt10 = "vif_normal",
      #         vif_std_grt10 = "vif_std" )

      to_plot <- pre_plot %>%
        mutate(Exp_Est = exp(Estimate)) %>% 
        hr.perincr(., 1) %>% 
        dplyr::select(
          model_desc,
          mod_info,
          has_IPW = ipw_t,
          Covariable = term,
          Coef = Estimate,
          beta_exp,
          Lower_exp,
          Upper_exp,
          `Std. Error`,
          `Pr(>|z|)`,
          aic,
          bic
        )

  } else {
      pre_plot <- data_1$coef_table %>% mutate(ipw_t = "Without IPW")  %>%
      rbind( ., data_2$coef_table %>% mutate(ipw_t = "With IPW")) %>%
      left_join(
        data_mod_desct,
        by = "model_id"
      ) %>%
    rename( vif_grt10 = "vif_normal",
            vif_std_grt10 = "vif_std" )

    to_plot <- pre_plot %>%
      mutate(Exp_Est = exp(Estimate)) %>%
      hr.perincr(., 1) %>%
      dplyr::select( model_desc,
              has_IPW = ipw_t,
              Covariable = term,
              Coef = Estimate,
              beta_exp,
              Lower_exp,
              Upper_exp,
              `Std. Error`,
              `Pr(>|t|)`,
              aic,
              bic,
              vif_grt10,
              vif_std_grt10  )
  }
  }




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
# mod_complete_sig_variables <- function( data,
#     covariables,
#     significative_variables,
#     outcome = "h_visits",
#     set_model = "zero inflated",
#     name_facility_in_order = FALSE,
#     STD_variable = FALSE,
#     add_decriptive = FALSE){

#   # hacer esta parte mejor!
#   what_exstract <- c("gender",  "healthdist_final_std", "healthdist_name_facility", "year_birth", "mother_age_at_birth_of_child_std")

#   # para el incremento

#   all_variables_incr <- c(what_exstract, significative_variables)

#   all_variables <- c(covariables, significative_variables)

#   fml_part_1 <- paste(all_variables, collapse = " + ")

#   # fml_final <- c(as.formula(paste0(outcome, " ~ ", fml_part_1)))

#   if(name_facility_in_order){

#     data$healthdist_name_facility <- relevel(as.factor(data$healthdist_name_facility), ref = "Manhica sede")

#   }

#   if(STD_variable == TRUE){

#     # Use lapply to transform column names based on class
#     transformed_names <- transform_names_by_colum_class(
#       data = data, 
#       significative_variables = significative_variables
#     )


#   fml_part_1 <- paste(c(covariables, transformed_names), collapse = " + ")
  
#   fml_final <- c(as.formula(paste0(outcome, " ~ ", fml_part_1)))
  
#   }else{
#     fml_part_1 <- paste(all_variables, collapse = " + ")
#     fml_final <- c(as.formula(paste0(outcome, " ~ ", fml_part_1)))

#   }

#   dt <- mapGeneralModelsCount(
#     formulas_to_exe = fml_final,
#     data_mod = data,
#     family_def = set_model,
#     ipw = FALSE,
#     # ipw_select = "ipw_ml_4",
#     transformed_names = "default",
#     path_save = ""
#   )


#   incre <-  get_incr_for_HR_by_colum_class(
#     data = data,
#     significative_variables = all_variables_incr
#   )

#   data_incr <- data.frame(
#     term2 = all_variables_incr,
#     increment = incre
#   ) %>% distinct()

# if(add_decriptive){

#     variables_name_to_m <- load_variables_names_to_match_from(path = here::here())

#       dt$coef_table <- dt$coef_table %>%
#     mutate(
#          term2 = str_extract(term, paste(all_variables_incr, collapse = "|"))
         
#     ) %>% left_join(
#       data_incr,
#       by = "term2"
#     ) %>%
#     mutate(increment = 1) %>% hr.perincr(., "increment") %>%
#     left_join(
#         variables_name_to_m,
#         by =  join_by("term" == "var_volcano")
#     )


# } else {
#     dt$coef_table <- dt$coef_table %>%
#         mutate(
#             term2 = str_extract(term, paste(all_variables_incr, collapse = "|"))
            
#         ) %>% left_join(
#         data_incr,
#         by = "term2"
#         ) %>%
#         mutate(increment = 1) %>% hr.perincr(., "increment") 
#         # mutate(increment = ifelse(is.na(increment), 1, increment)) %>% hr.perincr(., "increment")
# }


#   dt
# }



##' Fit a count model and compute per-increment effects for selected variables
##'
##' Builds a modeling formula from `covariables` and `significative_variables`,
##' optionally applies standardized/derived names via `transform_names_by_colum_class()`,
##' fits a count model (e.g., Poisson/Negative Binomial/Zero-inflated) using
##' `mapGeneralModelsCount()`, and augments the coefficient table with
##' per-increment effects via `hr.perincr()`. Optionally joins user-friendly
##' labels from `load_variables_names_to_match_from(path = here::here())`.
##'
##' @title Complete modeling pipeline for significant variables (count models)
##'
##' @param data A `data.frame` containing the outcome and all predictor variables.
##'   Must include (if `name_facility_in_order = TRUE`) the factor
##'   `healthdist_name_facility`.
##' @param covariables Character vector of baseline covariate names to always include.
##' @param significative_variables Character vector of variables considered significant
##'   (will be included in the model and in the per-increment summaries).
##' @param outcome Character scalar. Name of the outcome variable in `data`.
##'   Default is `"h_visits"`.
##' @param set_model Character scalar passed to `mapGeneralModelsCount()` defining
##'   the model family (e.g., `"poisson"`, `"negative binomial"`, `"zero inflated"`).
##'   Default is `"zero inflated"`.
##' @param name_facility_in_order Logical. If `TRUE`, relevels
##'   `healthdist_name_facility` so that `"Manhica sede"` is the reference level.
##'   Default is `FALSE`.
##' @param STD_variable Logical. If `TRUE`, uses `transform_names_by_colum_class()`
##'   to transform/standardize variable names for modeling. Default is `FALSE`.
##' @param add_decriptive Logical. If `TRUE`, joins friendly variable labels from
##'   `load_variables_names_to_match_from(path = here::here())` into the coefficient table. (Parameter name
##'   preserved as provided.) Default is `FALSE`.
##'
##' @return A list (as returned by `mapGeneralModelsCount()`), with the element
##'   `coef_table` augmented to include a `term2`, `increment`, and the per-increment
##'   effect columns returned by `hr.perincr()`. Other elements are passed through
##'   unchanged from `mapGeneralModelsCount()`.
##'
##' @details
##' **Pipeline overview**
##' 1. Assemble the modeling formula from `covariables` and `significative_variables`.
##' 2. Optionally standardize/transform variable names before modeling (`STD_variable = TRUE`).
##' 3. Optionally relevel the facility variable (`name_facility_in_order = TRUE`).
##' 4. Fit a count model via `mapGeneralModelsCount()`.
##' 5. Compute per-increment effects with `get_incr_for_HR_by_colum_class()` and `hr.perincr()`,
##'    and (optionally) join human-readable labels.
##'
##' **Note:** This function relies on helper functions available in your project:
##' `transform_names_by_colum_class()`, `mapGeneralModelsCount()`,
##' `get_incr_for_HR_by_colum_class()`, `hr.perincr()`, and
##' `load_variables_names_to_match_from(path = here::here())`.
##'
##' @author Fabian Coloma
##'
##' @seealso
##'  \code{\link{mapGeneralModelsCount}},
##'  \code{\link{transform_names_by_colum_class}},
##'  \code{\link{get_incr_for_HR_by_colum_class}},
##'  \code{\link{hr.perincr}},
##'  \code{\link{load_variables_names_to_match}}
##'
##' @importFrom dplyr mutate left_join distinct
##' @importFrom stringr str_extract
##' @importFrom stats as.formula relevel
##' @import magrittr
##'
##' @examples
##' \dontrun{
##' # Minimal example (assuming helper functions exist in your environment):
##' fit <- mod_complete_sig_variables(
##'   data = df,
##'   covariables = c("age", "gender"),
##'   significative_variables = c("year_birth", "mother_age_at_birth_of_child_std"),
##'   outcome = "h_visits",
##'   set_model = "zero inflated",
##'   name_facility_in_order = TRUE,
##'   STD_variable = FALSE,
##'   add_decriptive = TRUE
##' )
##'
##' # Inspect augmented coefficient table
##' fit$coef_table
##' }
mod_complete_sig_variables <- function(
  data,
  covariables,
  significative_variables,
  outcome = "h_visits",
  set_model = "zero inflated",
  name_facility_in_order = FALSE,
  STD_variable = FALSE,
  add_decriptive = FALSE
) {

  # ---- Input checks (lightweight, non-intrusive) ----------------------------
  stopifnot(is.data.frame(data))
  stopifnot(is.character(covariables), is.character(significative_variables))
  stopifnot(is.character(outcome), length(outcome) == 1L)
  stopifnot(is.logical(name_facility_in_order), length(name_facility_in_order) == 1L)
  stopifnot(is.logical(STD_variable), length(STD_variable) == 1L)
  stopifnot(is.logical(add_decriptive), length(add_decriptive) == 1L)

  # ---- Always-extracted variables (project-specific baseline set) -----------
  # NOTE: Names preserved as in the original function.
  what_exstract <- c(
    "gender",
    "healthdist_final_std",
    "healthdist_name_facility",
    "year_birth",
    "mother_age_at_birth_of_child_std"
  )

  # Variables used for per-increment summaries
  all_variables_incr <- c(what_exstract, significative_variables)

  # All variables to include in the model
  all_variables <- c(covariables, significative_variables)

  # ---- Optional releveling of facility name --------------------------------
  if (isTRUE(name_facility_in_order)) {
    if ("healthdist_name_facility" %in% names(data)) {
      data$healthdist_name_facility <- stats::relevel(
        as.factor(data$healthdist_name_facility),
        ref = "Manhica sede"
      )
    }
  }

  # ---- Build formula (optionally using standardized/transformed names) ------
  if (isTRUE(STD_variable)) {
    # Transform/standardize variable names for modeling
    transformed_names <- transform_names_by_colum_class(
      data = data,
      significative_variables = significative_variables
    )
    fml_part_1 <- paste(c(covariables, transformed_names), collapse = " + ")
    fml_final  <- stats::as.formula(paste0(outcome, " ~ ", fml_part_1))
  } else {
    fml_part_1 <- paste(all_variables, collapse = " + ")
    fml_final  <- stats::as.formula(paste0(outcome, " ~ ", fml_part_1))
  }

  # ---- Fit model via project helper (count family / zero-inflated, etc.) ----
  dt <- mapGeneralModelsCount(
    formulas_to_exe = c(as.formula(fml_final)), # al usar un map df para usar varias **
    data_mod        = data,
    family_def      = set_model,
    ipw             = FALSE,
    # ipw_select    = "ipw_ml_4",
    transformed_names = "default",
    path_save         = ""
  )

  # **formulas en lugar de una .. esto debe ir dentro de un vector

  # ---- Compute increments for per-increment effect summaries ----------------
  incre <- get_incr_for_HR_by_colum_class(
    data = data,
    significative_variables = all_variables_incr
  )

  data_incr <- data.frame(
    term2     = all_variables_incr,
    increment = incre
  ) %>%
    dplyr::distinct()

  # ---- Augment coefficient table with per-increment effects -----------------
  # NOTE: As in the original code, `increment` is set to 1 before calling
  # `hr.perincr()`. If you intended to use the computed `incre`, replace
  # `mutate(increment = 1)` with `mutate(increment = ifelse(is.na(increment), 1, increment))`.
  if (isTRUE(add_decriptive)) {

    variables_name_to_m <- load_variables_names_to_match_from(path = here::here())

    dt$coef_table <- dt$coef_table %>%
      dplyr::mutate(
        term2 = stringr::str_extract(term, paste(all_variables_incr, collapse = "|"))
      ) %>%
      dplyr::left_join(data_incr, by = "term2") %>%
      dplyr::mutate(increment = 1) %>%
      hr.perincr(., "increment") %>%
      dplyr::left_join(
        variables_name_to_m,
        dplyr::join_by("term" == "var_volcano")
      )

  } else {

    dt$coef_table <- dt$coef_table %>%
      dplyr::mutate(
        term2 = stringr::str_extract(term, paste(all_variables_incr, collapse = "|"))
      ) %>%
      dplyr::left_join(data_incr, by = "term2") %>%
      dplyr::mutate(increment = 1) %>%
      hr.perincr(., "increment")
    # Alternative (to use computed increments when available):
    # %>% dplyr::mutate(increment = ifelse(is.na(increment), 1, increment)) %>% hr.perincr(., "increment")
  }

  # ---- Return the model object with augmented coef_table --------------------
  dt
}



mod_complete_sig_variables_2 <- function(
  data,
  covariables,
  ri_zi = FALSE,
  significative_variables,
  outcome = "h_visits",
  set_model = "zero inflated",
  name_facility_in_order = FALSE,
  STD_variable = FALSE,
  add_decriptive = FALSE
) {

  # ---- Input checks (lightweight, non-intrusive) ----------------------------
  stopifnot(is.data.frame(data))
  stopifnot(is.character(covariables), is.character(significative_variables))
  stopifnot(is.character(outcome), length(outcome) == 1L)
  stopifnot(is.logical(name_facility_in_order), length(name_facility_in_order) == 1L)
  stopifnot(is.logical(STD_variable), length(STD_variable) == 1L)
  stopifnot(is.logical(add_decriptive), length(add_decriptive) == 1L)

  # ---- Always-extracted variables (project-specific baseline set) -----------
  # NOTE: Names preserved as in the original function.
  what_exstract <- c(
    "gender",
    "healthdist_final_std",
    "healthdist_name_facility",
    "year_birth",
    "mother_age_at_birth_of_child_std"
  )

  # Variables used for per-increment summaries
  all_variables_incr <- c(what_exstract, significative_variables)

  # All variables to include in the model
#   all_variables <- c(covariables, significative_variables)

  # ---- Optional releveling of facility name --------------------------------
  if (isTRUE(name_facility_in_order)) {
    if ("healthdist_name_facility" %in% names(data)) {
      data$healthdist_name_facility <- stats::relevel(
        as.factor(data$healthdist_name_facility),
        ref = "Manhica sede"
      )
    }
  }

  # ---- Build formula (optionally using standardized/transformed names) ------
  if (isTRUE(STD_variable)) {
    # Transform/standardize variable names for modeling
    transformed_names <- transform_names_by_colum_class(
      data = data,
      significative_variables = significative_variables
    )

    significative_vars_post <- transformed_names
  } else {
     significative_vars_post <- significative_variables
  }

  if(!ri_zi){
    covariables_post <- covariables[covariables != "(1 | house_number)" & covariables != "offset(log(fu_months))"]
  } else {
    covariables_post <- covariables
  }

    fml_part_1 <- paste(c(covariables, significative_vars_post), collapse = " + ")
    fml_final  <- stats::as.formula(paste0(outcome, " ~ ", fml_part_1))

    fml_part_2 <- paste(c(covariables_post, significative_vars_post), collapse = " + ")
    fml_final_zi  <- paste0(outcome, " ~ ", fml_part_2)


  # ---- Fit model via project helper (count family / zero-inflated, etc.) ----
  dt <- mapGeneralModelsCount(
    formulas_to_exe = c(as.formula(fml_final)), # al usar un map df para usar varias **
    data_mod        = data,
    fml_ri_hurd     = fml_final_zi,
    family_def      = set_model,
    ipw             = FALSE,
    # ipw_select    = "ipw_ml_4",
    transformed_names = "default",
    path_save         = ""
  )

  # **formulas en lugar de una .. esto debe ir dentro de un vector

  # ---- Compute increments for per-increment effect summaries ----------------
  incre <- get_incr_for_HR_by_colum_class(
    data = data,
    significative_variables = all_variables_incr
  )

  data_incr <- data.frame(
    term2     = all_variables_incr,
    increment = incre
  ) %>%
    dplyr::distinct()

  # ---- Augment coefficient table with per-increment effects -----------------
  # NOTE: As in the original code, `increment` is set to 1 before calling
  # `hr.perincr()`. If you intended to use the computed `incre`, replace
  # `mutate(increment = 1)` with `mutate(increment = ifelse(is.na(increment), 1, increment))`.
  if (isTRUE(add_decriptive)) {

    variables_name_to_m <- load_variables_names_to_match_from(path = here::here())

    dt$coef_table <- dt$coef_table %>%
      dplyr::mutate(
        term2 = stringr::str_extract(term, paste(all_variables_incr, collapse = "|"))
      ) %>%
      dplyr::left_join(data_incr, by = "term2") %>%
      dplyr::mutate(increment = 1) %>%
      hr.perincr(., "increment") %>%
      dplyr::left_join(
        variables_name_to_m,
        dplyr::join_by("term" == "var_volcano")
      )

  } else {

    dt$coef_table <- dt$coef_table %>%
      dplyr::mutate(
        term2 = stringr::str_extract(term, paste(all_variables_incr, collapse = "|"))
      ) %>%
      dplyr::left_join(data_incr, by = "term2") %>%
      dplyr::mutate(increment = 1) %>%
      hr.perincr(., "increment")
    # Alternative (to use computed increments when available):
    # %>% dplyr::mutate(increment = ifelse(is.na(increment), 1, increment)) %>% hr.perincr(., "increment")
  }

  # ---- Return the model object with augmented coef_table --------------------
  dt
}

