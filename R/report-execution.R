##'
##' This function will create a summary, from numerical variable and categorical,
##' plotting the plot to know a bit more the distribution
##' the dot out of the boxplto are outlaiers
##'
##'@title generator_boxplot_by_list
##'@param data XXX_xxx_XXX
##'@param list_names XXX_xxx_XXX_xxx
##'@param where_to_save xxx_XXX_xxx_XXX
##'@param prefix xxx_XXX_xxx_XXX
##'@return  list
##'@author Fabian Coloma
##'@export
##'@examples
##'
##
generator_boxplot_by_list <- function(
  data,
  list_names,
  where_to_save,
  prefix
){
  names_to_folder <- paste0(prefix, "_num_variables")

  path_to_save_pre <- here::here(where_to_save, names_to_folder)

  dir.create(path_to_save_pre, showWarnings = FALSE, recursive = TRUE)

  general_path <- list()

  for(var in list_names){

    name_to_legend <- toupper(gsub("_", " ", var))

    pat_to_save <- here::here(
      path_to_save_pre,
      paste0(prefix, "_", var, "_boxplot.png")
    )


    plot <- ggplot(data, aes(x = Cluster, y = .data[[var]], fill = Cluster)) +
              geom_boxplot(outlier.shape = NA, alpha = 0.7) +  # Boxplot for numeric variable
            #   geom_jitter(color = "#000000", width = 0.15, alpha = 0.10) +  # Optional: Adds points for better visualization
              labs(x = "Cluster", y = name_to_legend, fill = "Cluster") +
              theme_minimal()

    # Save the plot
    ggsave(
      filename = pat_to_save,
      plot = plot,
      dpi = 600,
      width = 10,
      height = 8,
      units = "in"
    )

    general_path[[name_to_legend]] <- pat_to_save
  }

  return(general_path)
}

##'
##' This function will create a summary, from two categorical variables,
##' Getting the count by category and hist percentage
##'
##'@title generator_bar_stacked_plot
##'@param data XXX_xxx_XXX
##'@param list_names XXX_xxx_XXX_xxx
##'@param where_to_save xxx_XXX_xxx_XXX
##'@param prefix xxx_XXX_xxx_XXX
##'@return  list
##'@author Fabian Coloma
##'@export
##'@examples
##'
##
generator_bar_stacked_plot <- function(
  data,
  list_names,
  where_to_save,
  prefix
){

  names_to_folder <- paste0(prefix, "_cat_variables")
  
  path_to_save_pre <- here::here(where_to_save, names_to_folder)

  dir.create(path_to_save_pre, showWarnings = FALSE, recursive = TRUE)

  general_path <- list()

  for(var in list_names){

    name_to_legend <- toupper(gsub("_", " ", var))
    # print(name_to_legend)

    pat_to_save <- here::here(
      path_to_save_pre,
      paste0(prefix, "_", var, "_bar_stack_plot.png")
    )
    
    plot <- ggplot(data, aes(x = Cluster, fill = .data[[var]])) +
              geom_bar(position = "stack") +  # Keeps counts on the y-axis
              geom_text(stat = "count", 
                        aes(label = paste0(after_stat(count), 
                          " (", 
                          round(after_stat(count / tapply(..count.., ..x.., sum)[as.character(..x..)] * 100), 1),
                          "%)")), 
                        position = position_stack(vjust = 0.5)) +
              labs(x = "Cluster", y = "Count", fill = name_to_legend) +
              theme_minimal()

    # Guardar el gráfico del clustering
    ggsave(
      filename = pat_to_save,
      plot = plot,
      dpi = 600,
      width = 10,
      height = 8,
      units = "in"
    )

    general_path[[name_to_legend]] <- pat_to_save

  }

  return(general_path)


}

# Función para renombrar clusters con descripciones más significativas
get_cluster_labels <- function(n_clusters) {
  default_labels <- paste0("C", 1:n_clusters)

  # Puedes personalizar los textos acá para los 4 clusters
  descriptions <- c(
   ""
    ,""
    ,""
  )

  # En caso de que haya más o menos de 4 clusters
  if (n_clusters <= length(descriptions)) {
    descriptions <- descriptions[1:n_clusters]
  } else {
    descriptions <- c(descriptions, paste("Cluster", (length(descriptions)+1):n_clusters))
  }

  paste0("C", 1:n_clusters, "", descriptions)
}


##'
##'XXXX____XXXXXXXXXXXXXXXXXXXXXX
##'
##'@title ADD_YOUR_FUNCTION_TITLE
##'@param data1 XXX_xxx_XXX
##'@param data2 XXX_xxx_XXX_xxx
##'@param variable xxx_XXX_xxx_XXX
##'@return data.frame / list / vector
##'@author Fabian Coloma
##'@export
##'@examples
##'
##'
creating_map_separate_by_class <- function(
    manhica
    , health_facilities
    , data 
    , var_to_split = "Cluster"
    , roads
    , where_to_save
    , pre_fix
    , palette_colors = NULL
    , buffer_by_facilities
    , zona
    , districts
){


  # Crear directorio de salida si no existe
  dir.create(where_to_save, showWarnings = FALSE, recursive = TRUE)


  
    var_to_sel <- c("gps_longitude", "gps_latitude", var_to_split)

    names_to_use_on_all <- paste0(pre_fix, "_cluster_on_geo")

    if ("ClusterLabel" %in% names(data)) {
        print("changing_clusterTOclusterlabel")
        data <- data %>%
            mutate(Cluster = ClusterLabel)
        }

    data <- data %>% select(all_of(var_to_sel)) %>% basic_transfor_to_sf_mozambique(.)

    plot_geo <-   mapa_by_cluster( manhica = manhica
        , health_facilities = health_facilities
        , participants = data 
        , roads = roads
        , var_to_split = var_to_split
        , palette_colors = palette_colors
        , buffer_by_facilities = buffer_by_facilities
        , title_plot = "Cluster membership"
        , districts = districts
        , zona = zona) 

    # Guardar el gráfico del clustering

    were_save_general <- here::here(where_to_save, paste0(names_to_use_on_all, ".png"))

    tmap_save(
        plot_geo, 
        filename = were_save_general, 
        width = 6000, 
        height = 6000, 
        dpi = 600
    )

  save_from <- list(
    all_class_geo = were_save_general
  )

  for(i in unique(data$Cluster)){

    plot_geo <-   mapa_by_cluster( manhica = manhica
    , health_facilities = health_facilities
    , participants = data %>%
        filter(.data[[var_to_split]] == i)
    , roads = roads
    , palette_colors = palette_colors 
    , var_to_split = var_to_split
    , buffer_by_facilities = buffer_by_facilities
    , districts = districts
    , zona = zona) 

    names_to_gp_pre <- paste0(names_to_use_on_all, "_",  substr(i, 1, 10) )

    names_to_gp <- paste0(names_to_gp_pre, ".png")

    # Guardar el gráfico del clustering
    tmap_save(
      plot_geo, 
      filename = here::here(where_to_save, names_to_gp), 
      width = 6000, 
      height = 6000, 
      dpi = 600
    )

    save_from[[names_to_gp_pre]] <- here::here(where_to_save, names_to_gp)
  }


    save_from

}
##'
##'XXXX____XXXXXXXXXXXXXXXXXXXXXX
##'
##'@title ADD_YOUR_FUNCTION_TITLE
##'@param data1 XXX_xxx_XXX
##'@param data2 XXX_xxx_XXX_xxx
##'@param variable xxx_XXX_xxx_XXX
##'@return data.frame / list / vector
##'@author Fabian Coloma
##'@export
##'@examples
##'
##'


mapa_by_cluster <- function(
    manhica
    , health_facilities
    , participants
    , roads
    , var_to_split
    , palette_colors = NULL
    , title_plot =  "Hospital Visits"
    , buffer_by_facilities
    , zona
    ,buff_alpha = 0.15
    , districts
    
){

    health_principal_vector <- c( "Manhica sede",
    "Maragra",
    "Ilha Josina Machel",
    "Taninga",
    "Nwamatibjana", # is palmeira - is the name of hospital
    "Malavele",
    "Xinavane",
    "Palmeira" 
    # Maluana - otros que no estan
    )


    health_facilities_w <- health_facilities %>%
        mutate(
            `Being surveilled` = ifelse(nome %in% health_principal_vector, 
                "yes",
                "no")
        )
if(is.null(palette_colors)){

plot<- tm_shape(districts, bbox = zona) +
        tm_polygons(col = "#ffffff", lwd = 0) +
        tm_shape(zona) +
        tm_polygons(col = "#ebebeb", border.col = "#3A3A3A", lwd = 0.25) +
        tm_shape(participants) +
        tm_dots(shape = 21,
            legend.show = TRUE,
            # shapes = c("Survilled" = 20, "Out" = 20),
            size = 0.3,
            col = var_to_split
            ,palette = c( "1" = "#0063ed", 
                          "2" = "#62e88a",
                          "3" = "#dff706"),
                title = title_plot,
            legend.shape.show = FALSE 
        ) +
    tm_shape(roads) +
    tm_lines(col = "#000000", 
            lwd = 1.2, 
            lty = "solid") +
    tm_shape(health_facilities_w) +
    tm_symbols(size = 0.5, 
                col = "#535353", 
                shape = 23) +
    tm_text("nome", 
            size = 0.8, 
            ymod = 0.9, fontface = "bold") + # Adjust size and position of labels as needed
#     tm_shape(buffer_by_facilities) +
#   tm_polygons(col = "Being surveilled",
#               border.col = "#3A3A3A",
#               alpha = buff_alpha, 
#               palette = c("no" = "#fff710", 
#                           "yes" = "#04ff32")) + 
    tm_add_legend(type = "symbol", 
                    labels = " Health facilities", 
                    col = "#535353", 
                    shape = 23) +
    tm_add_legend(type = "line", 
                labels = "Main road", 
                col = "#000000", 
                lwd = 2, 
                lty = "solid") +
    tm_layout(legend.position = c("right", "top"),
            legend.outside = TRUE,            # Moves legend outside of the plot
            legend.outside.position = "right") 
    
    } else {

    plot<- tm_shape(districts, bbox = zona) +
        tm_polygons(col = "#ffffff", lwd = 0) +
        tm_shape(zona) +
        tm_polygons(col = "#ebebeb", border.col = "#3A3A3A", lwd = 0.25) +
        tm_shape(participants) +
       tm_dots(shape = 21,
        legend.show = TRUE,
        size = 0.3,
        col = var_to_split,
        palette = palette_colors,
        title = title_plot,
        # shape.legend = tm_legend_hide(),
        auto.palette.mapping = FALSE) +
        # tm_check_fix() +
    tm_shape(roads) +
    tm_lines(col = "#000000", 
            lwd = 1.2, 
            lty = "solid") +
    tm_shape(health_facilities_w) +
    tm_symbols(size = 0.5, 
                col = "#535353", 
                shape = 23) +
    tm_text("nome", 
            size = 0.8, 
            ymod = 0.9, fontface = "bold") + # Adjust size and position of labels as needed
#     tm_shape(buffer_by_facilities) +
#   tm_polygons(col = "Being surveilled",
#               border.col = "#3A3A3A",
#               alpha = buff_alpha, 
#               palette = c("no" = "#fff710", 
#                           "yes" = "#04ff32")) + 
    tm_add_legend(type = "symbol", 
                    labels = " Health facilities", 
                    col = "#535353", 
                    shape = 23) +
    tm_add_legend(type = "line", 
                labels = "Main road", 
                col = "#000000", 
                lwd = 2, 
                lty = "solid") +
    tm_layout(legend.position = c("right", "top"),
            legend.outside = TRUE,            # Moves legend outside of the plot
            legend.outside.position = "right")
            }

    plot


}


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
map_by_all_facilities <- function(
    districts
    ,manhica
    ,zona
    ,participants
    ,health_facilities_w
    ,buffer_by_facilities
    ,roads
    ,buff_alpha = 0.15
    ,id_survilled = NULL
){

if(!is.null(id_survilled)){
    participants <- participants %>%
        dplyr::mutate(
            selected_participant = ifelse(perm_id %in% id_survilled, "Survilled", "Out")
        )

    participants$dot_size <- ifelse(participants$selected_participant == "Survilled", 0.1, 0.02)
    participants$has_vis2 <- ifelse(participants$with_some_hospital_visit == "HAS visits", "count > 0", "count = 0")

}


tm_shape(districts, bbox = zona) +
  tm_polygons(col = "#ffffff", lwd = 0) +
tm_shape(zona) +
  tm_polygons(col = "#ffffff", border.col = "#3A3A3A", lwd = 0.15) +
tm_shape(participants) +
  tm_dots(col = "healthdist_name_facility_all"
          ,legend.show = T
          ,title = "Health facility"
          
    ) + 
tm_shape(health_facilities_w) +
  tm_symbols(size = 0.75, 
             col = "#535353", 
             shape = 23) +
  tm_text("nome", 
          size = 0.8, 
          ymod = 0.5) + # Adjust size and position of labels as needed
tm_shape(buffer_by_facilities) +
  tm_polygons(col = "Being surveilled",
              border.col = "#3A3A3A",
              alpha = buff_alpha, 
              palette = c("no" = "#fff710", 
                          "yes" = "#04ff32")) + 
  tm_shape(roads) +
  tm_lines(col = "#00316d", 
           lwd = 4, 
           lty = "dotted") +
tm_layout(legend.position = c("right", "top"),
          legend.outside = TRUE,            # Moves legend outside of the plot
          legend.outside.position = "right")
}

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
map_by_7_facilities <- function(
    districts
    ,manhica
    ,zona
    ,participants
    ,health_facilities_w
    ,buffer_by_facilities
    ,roads
    ,buff_alpha = 0.15
    , id_survilled = NULL
){

if(!is.null(id_survilled)){
    participants <- participants %>%
        dplyr::mutate(
            selected_participant = ifelse(perm_id %in% id_survilled, "Survilled", "Out")
        )

    participants$dot_size <- ifelse(participants$selected_participant == "Survilled", 0.1, 0.02)
    participants$has_vis2 <- ifelse(participants$with_some_hospital_visit == "HAS visits", "count > 0", "count = 0")

}

tm_shape(districts, bbox = zona) +
  tm_polygons(col = "#ffffff", lwd = 0) +
tm_shape(zona) +
  tm_polygons(col = "#ebebeb", border.col = "#3A3A3A", lwd = 0.25) +
tm_shape(participants) +
  tm_dots(col = "healthdist_name_facility"
          ,legend.show = T
          ,title = "Health facility"
          
    ) + 
tm_shape(health_facilities_w) +
  tm_symbols(size = 0.75, 
             col = "#535353", 
             shape = 23) +
  tm_text("nome", 
          size = 0.8, 
          ymod = 0.5) + # Adjust size and position of labels as needed
tm_shape(buffer_by_facilities) +
  tm_polygons(col = "Being surveilled",
              border.col = "#3A3A3A",
              alpha = buff_alpha, 
              palette = c("no" = "#fff710", 
                          "yes" = "#04ff32")) + 
  tm_shape(roads) +
  tm_lines(col = "#00316d", 
           lwd = 4, 
           lty = "dotted") +
tm_layout(legend.position = c("right", "top"),
          legend.outside = TRUE,            # Moves legend outside of the plot
          legend.outside.position = "right")
}

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
map_by_efective_data <- function(
    districts
    ,manhica
    ,zona
    ,participants
    ,health_facilities_w
    ,buffer_by_facilities
    ,roads
    ,buff_alpha = 0.1
    , id_survilled = NULL
){

if(!is.null(id_survilled)){
    participants <- participants %>%
        dplyr::mutate(
            selected_participant = ifelse(perm_id %in% id_survilled, "Survilled", "Out")
        )

    participants$dot_size <- ifelse(participants$selected_participant == "Survilled", 0.1, 0.02)
    participants$has_vis2 <- ifelse(participants$with_some_hospital_visit == "HAS visits", "count > 0", "count = 0")

}

tm_shape(districts, bbox = zona) +
  tm_polygons(col = "#ffffff", lwd = 0) +
tm_shape(zona) +
  tm_polygons(col = "#ebebeb", border.col = "#3A3A3A", lwd = 0.25)  +
tm_shape(buffer_by_facilities) +
  tm_polygons(col = "Being surveilled",
             title = "In surveillance system",
              border.col = "#3A3A3A",
              alpha = buff_alpha, 
              palette = c("no" = "#fff710", 
                          "yes" = "#69d6ef")) + 
tm_shape(participants) +
  tm_dots(shape = "selected_participant",
          legend.show = TRUE,
          shapes = c("Survilled" = 20, "Out" = 20),
          size = 0.1,
          col = "has_vis2"
          ,palette = c("count > 0" = "#0063ed", 
                        "count = 0" = "#e86262"),
            title = "Hospital visits",
          legend.shape.show = FALSE 
    ) +
tm_shape(roads) +
  tm_lines(col = "#000000", 
           lwd = 1, 
           lty = "solid") +
tm_shape(health_facilities_w) +
  tm_symbols(size = 0.5, 
             col = "#535353", 
             shape = 23) +
  tm_text("nome", 
          size = 0.8, 
          ymod = 0.5, fontface = "bold") + # Adjust size and position of labels as needed
  tm_add_legend(type = "symbol", 
                labels = " Health facilities", 
                col = "#535353", 
                shape = 23) +
tm_add_legend(type = "line", 
              labels = "Main road", 
              col = "#000000", 
              lwd = 2, 
              lty = "solid") +
tm_layout(legend.position = c("right", "top"),
          legend.outside = TRUE,            # Moves legend outside of the plot
          legend.outside.position = "right")
}


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
generating_buffer_limit <- function(data,
                                       limit){
    data %>%
        left_join(
            limit,
            ,by = join_by("nome" == "get(group_field)")
        ) %>%
        mutate(
            health_distance_m2 = ifelse(is.na(health_distance_limit),
                                        10, 
                                        health_distance_limit),
            geometry = st_buffer(geometry, dist = health_distance_m2)
        )
}



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
limit_by_geo_and_metric <- function(data, 
                                    group_field,
                                    metric, 
                                    percentage_limit = 1){

    data %>% 
        dplyr::select(
            all_of(
                c(
                    metric,
                    group_field
                )
            )
        ) %>%
        group_by(
            get(group_field)
        ) %>%
        summarise(
            health_distance_limit = quantile(get(metric), 
                                            percentage_limit, 
                                            na.rm = TRUE)
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
calc_table_comp_1 <- function(   d1 = data_est_zi
                                ,d2 = data_est_nb
                                ,d3 = data_est_pois
                                ,d4 = data_est_qs
                                ,model_i = 6  ){

    detect_mod <- paste0("^M", model_i)

    d1 %>%
    filter(
        stringr::str_detect(model_desc, detect_mod)
    ) %>%
    dplyr::select(
        # mod_info,
        has_IPW,
        zero_infl_aic = aic,
        zero_infl_bic = bic
    ) %>% 
    distinct() %>%
    left_join(
        d2 %>%
            filter(
                stringr::str_detect(model_desc, detect_mod)
            ) %>%
            dplyr::select(
                # mod_info,
                has_IPW,
                nb_aic = aic,
                nb_bic = bic
            ) %>% 
            distinct(),
        by = "has_IPW"
    ) %>%
    left_join(
        d3 %>%
            filter(
                stringr::str_detect(model_desc, detect_mod)
            ) %>%
            dplyr::select(
                # mod_info,
                has_IPW,
                pois_aic = aic,
                pois_bic = bic
            ) %>% 
            distinct(),
        by = "has_IPW"
    ) 
    # %>%
    # left_join(
    #     data_est_qs %>%
    #         filter(
    #             stringr::str_detect(model_desc, detect_mod)
    #         ) %>%
    #         dplyr::select(
    #             # mod_info,
    #             has_IPW,
    #             qs_aic = aic,
    #             qs__bic = bic
    #         ) %>% 
    #         distinct(),
    #     by = "has_IPW"
    # ) 


}


##'
##' Choicing between outcome, to define the description
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
choice_des_outcome <- function(char){
    if(stringr::str_detect(char, "respiratory")){
       out <-  "all-cause visits"
    } else {
       out <- "respiratory-linked visits"
    }

    out
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
plot_by_two_results_save_on <- function(
    path_base,
    result_1,
    result_2,
    vColor = "Outcome",
    path_save = here::here(
            "scratch",
            "Final_plot_sensitivity_analisys",
            "figure_2_v2_hvisits_respvist.png"
        ),
    y_lim_sup = 1.1,
    y_lim_inf = 0.75,
    source_ch = "ilumination_fuel_v2_postPolluting"
){
    load(paste0(path_base, result_1, "/info_accros.RData"))

    d1 <- readRDS(paste0(path_base, result_1, "/ZI_no_IPW.rds"))

    d2 <- readRDS(paste0(path_base, result_2, "/ZI_no_IPW.rds"))

    outcome1 <- unique(d1$mod_info$outcome)[1]
    outcome2 <-  unique(d2$mod_info$outcome)[1]

    source_concat <- source_ch

    # any("Calanga" %in% unique(data_filt$posto_admnistrativo))
    grp_1 = "nothing"
    grp_2 = "nothing"
    
    if(outcome1 == outcome2){
        vColor <- "group"

        if( any("Calanga" %in% unique(data_filt$posto_admnistrativo))){
         grp_1 <- "South"
         grp_2 <- "North" } else {
            grp_2 <- "South"
            grp_1 <- "North"
         }
    }

    data_to_ploteo <- d1$coef_table %>%
        filter( term == source_concat,
                mod_info == "ZI count" ) %>%
        mutate(
            Outcome = choice_des_outcome(outcome1),
            Covariable = term,
            group = grp_1
        ) %>%
        rbind( . ,
            d2$coef_table %>%
            filter( term == source_concat,
                    mod_info == "ZI count" ) %>%
            mutate(
                Outcome = choice_des_outcome(outcome2),
                Covariable = term,
                group = grp_2
            )
        ) %>%
        left_join(
            data_mod_desc,
            by = "model_id"
        ) %>%
        hr.perincr(., 1)


    plot <- plot_hr_ic( data_to_ploteo,
                        source = source_concat,
                        xlim_inf = y_lim_inf,
                        xlim_upper = y_lim_sup,
                        vColor = vColor)
    
    ggsave(
        path_save,
        plot = plot,
        height = 10, 
        width = 12
    )

    

}

##' Create a table with multiple options like
##' show "n" entries, filter and search
##'
##' @title data table improve htmls reports
##' @param data any table that you wanna show on html report.
##' @return A data table.
##' @author Fabian Coloma
format_out_table <- function(data, round_table = FALSE, which_length = c(10, 25, 50)){
  # coerce matrix -> data frame
  data_df <- as.data.frame(data, stringsAsFactors = FALSE)
  
  if (isFALSE(round_table)) {
    data_new <- data_df
  } else {
    data_new <- data_df %>%
      mutate_if(
        is.numeric,
        ~round(.x, digits = round_table)
      )
  }

    # DT::datatable(
    # data_new,
    # filter = list(position = "top", plain = TRUE),
    # selection = "multiple",
    # rownames = TRUE,
    # extensions = c("Buttons"),  # Enables export/download options
    # options = list(
    #   dom = "Bfrtip",  # Includes Buttons, Filtering, Table, Pagination
    #   buttons = c("copy", "csv", "excel", "pdf"),  # Export options
    #   lengthMenu = list(c(10, 25, 50, 100, -1), c("10", "25", "50", "100", "All")),  # Adjustable row display
    #   scrollX = TRUE,  # Enables horizontal scrolling if needed
    #   pageLength = 10,  # Default rows per page
    #   searchHighlight = TRUE,  # Highlights search terms
    #   autoWidth = TRUE  # Adjusts column width automatically
    # ),
    # class = "display nowrap compact"  # Enhances table appearance
    # )

    DT::datatable(
  data_new,
  filter = list(position = "top", plain = TRUE),  # Filters on top
  selection = "multiple",
  rownames = TRUE,
  extensions = c("Buttons"),  # Enable download buttons
  options = list(
    dom = "Blfrtip",  # B = Buttons, f = Filter, r = Processing info, t = Table, i = Info, p = Pagination
    buttons = list(
      "copy", 
      list(
        extend = "csv",
        filename = "data_export"
      ),
      list(
        extend = "excel",
        filename = "data_export"
      ),
      list(
        extend = "pdf",
        filename = "data_export",
        title = "Exported Data",
        orientation = "landscape",
        pageSize = "A4"
      )
    ),
    lengthMenu = list(c(10, 25, 50, 100, -1), c("10", "25", "50", "100", "All")),  # Adjustable row display
    pageLength = 10,  # Default rows per page
    scrollX = TRUE,  # Enable horizontal scrolling
    searchHighlight = TRUE,  # Highlights search terms
    autoWidth = TRUE  # Adjusts column width automatically
  ),
  class = "display nowrap compact"  # Enhances appearance
)

}
# ##' Create a table with multiple options like
# ##' show "n" entries, filter and search
# ##'
# ##' @title data table improve htmls reports
# ##' @param data any table that you wanna show on html report.
# ##' @return A data table.
# ##' @author Fabian Coloma
# format_out_table <- function(data, round_table = FALSE, which_length = c(10, 25, 50)){
#     if(!(round_table == FALSE)){
#         data_new <- data %>% as.data.frame() %>%
#             mutate_if(
#                 is.numeric,
#                 round,
#                 digit = round_table 
#             )
#     } else {
#         data_new <- data %>% as.data.frame() 
#     }

#     DT::datatable(
#     data_new,
#     filter = list(position = "top", plain = TRUE),
#     selection = "multiple",
#     rownames = TRUE,
#     # extensions = c("Buttons"),
#     options = list(
#         # dom = "Bfrtip",
#         # buttons = c("copy", "csv", "excel", "pdf"),  # Show Entries dropdown
#         lengthMenu = which_length,
#         scrollX = TRUE
#     )
#     )
# }

##' Creating a diferents plots
##' 
##'
##' @title multi plot
##' @param type. 
##' @return A data frame.
##' @author Fabian Coloma
multi_plot_choice <- function(type, data, legend_T){

    if(!(type %in% c("boxplot", "densityplot"))){
        print("not in on plot availables ")
    }

    if(type == "boxplot"){

      fig <-   plotly::plot_ly(
            x = data$value,
            color = as.factor(data$variable),
            type = "box"
        ) 
        # %>% 
        # layout(
        #     showlegend = legend_T
        # )

    }

    if(type == "densityplot"){
       fig <-  ggplotly(
                    data %>% ggplot(., aes(x = value, fill = variable)) + geom_density(alpha=.3)
                )

    }

    if(type == "boxplot2"){
        fig <-  ggplot2::ggplot(data, ggplot2::aes(x = as.factor(variable), y = value)) +
  ggplot2::geom_boxplot(fill = "lightblue", color = "black") +
  ggplot2::labs(title = "Boxplot of Numeric Variable by Category",
                x = "Category",
                y = "Value") +
  ggplot2::theme_minimal()
    }

 fig 

}

##' Creating a density plot per all information
##' 
##'
##' @title simple density plot 
##' @param data Table with the information.
##' @param variable_list List of variable to made the boxplot.
##' @param identifyer id variable
##' @param legend boolean. TRUE or FALSE to add the legend
##' @return A data frame.
##' @author Fabian Coloma
dens_ly_vars <- function(
    data,
    variable_list,
    identifyer = "perm_id",
    legend = FALSE
){

    data_pre_plot <- data %>%
        dplyr::select(
            all_of(identifyer),
            all_of(variable_list)
        )
    
    to_plot <- reshape2::melt(data_pre_plot, id = identifyer)

   
    plotly::ggplotly(
    to_plot %>% ggplot(., aes(x=value, fill=variable)) + geom_density(alpha=.3)
    )


}

##' Creating a boxplot per all information
##' 
##'
##' @title simple boxplot 
##' @param data Table with the information.
##' @param variable_list List of variable to made the boxplot.
##' @param identifyer id variable
##' @return A data frame.
##' @author Fabian Coloma
box_ly_vars <- function(
    data,
    variable_list,
    identifyer = "perm_id",
    legend = FALSE
){

    data_pre_plot <- data %>%
        dplyr::select(
            all_of(identifyer),
            all_of(variable_list)
        )
    
    to_plot <- reshape2::melt(data_pre_plot, id = identifyer)

    plotly::plot_ly(
        x = to_plot$value,
        color = to_plot$variable,
        type = "box"
    ) %>% 
    layout(
        showlegend = legend
    )

}


# 'bar', 'barpolar', 'box', 'candlestick', 'carpet', 
# 'choropleth', 'choroplethmapbox', 'cone', 'contour',
#  'contourcarpet', 'densitymapbox', 'funnel', 'funnelarea', 
#  'heatmap', 'heatmapgl', 'histogram', 'histogram2d', 'histogram2dcontour',
#   'icicle', 'image', 'indicator', 'isosurface', 'mesh3d', 'ohlc', 'parcats',
#    'parcoords', 'pie', 'pointcloud', 'sankey', 'scatter', 'scatter3d', 
#    'scattercarpet', 'scattergeo', 'scattergl', 'scattermapbox', 'scatterpolar',
#     'scatterpolargl', 'scattersmith', 'scatterternary', 'splom', 'streamtube', 
#     'sunburst', 'surface', 'table', 'treemap', 'violin', 'volume', 'waterfall'


##' Creating a boxplot per posto administrativo with multiple tabs on rmarkdow.
##' 
##'
##' @title  boxplot with multiple tabs
##' @param data Table with the information.
##' @param variable_list List of variable to made the boxplot.
##' @param variable_tabs Variable to made the tabs
##' @param legend boolean. include or not the legend
##' @param type_plot chartacter. choice a type of the 
##' plot for now c("boxplot", "densityplot")
##' @return A data frame.
##' @author Fabian Coloma
plot_ly_tabs_vars <- function(
    data,
    variable_list,
    variable_tabs,
    identifyer = "perm_id",
    legend = FALSE,
    type_plot = "boxplot" 
){

    # if(length(variable_list) == 1){
    #     tabs <- unique(variable_tabs)
    # } else  {
    #     tabs <- as.vector(
    #         unique(
    #             pull(
    #                 data,
    #                 variable_tabs
    #             )))
    # }

    tabs <- unique(variable_list)

# i = 1
for(i  in 1:length(tabs)) {

  cat('\n')

  cat('#### ', tabs[i], '   \n')

  cat('\n')


#   plot_values <- diamonds %>% filter(cut == tab)

    #   filter(get(variable_tabs) == tabs[i]) %>%

  to_plot <- data %>%
      dplyr::select(
          all_of(variable_tabs),
          all_of(tabs[i])
      )
    
  print(paste(as.character(dim(to_plot)), collapse = " - "))

  cat('\n')

  names(to_plot) <- c("variable", "value") 
#   to_plot <- reshape2::melt(data_pre_plot, id = identifyer)
#   to_plot <- 

  fig <- multi_plot_choice(
    type = type_plot,
    data = to_plot,
    legend_T = legend
  )

#   print(
        
#         #    htmltools::tagList(plotly::subplot(fig, fig, nrows=2, heights = c(0.1, 0.9)))
#        htmltools::tagList(fig)
        
#     )
    print(fig)
    
  cat('\n')

}

}



# crear formula dos .. cambiando la distribucion de tabs y esas movidas
# es mas facil que hacerlo condicional
 

#  plot_ly_tabs_vars <- function(
#     data,
#     variable_list,
#     variable_tabs,
#     identifyer = "perm_id",
#     legend = FALSE,
#     type_plot = "boxplot" 
# ){

#     if(length(variable_list) == 1){
#         tabs <- unique(variable_tabs)
#     } else  {
#         tabs <- as.vector(
#             unique(
#                 pull(
#                     data,
#                     variable_tabs
#                 )))
#     }

# # i = 1
# for(i  in 1:length(tabs)) {

#   cat('\n')

#   cat('#### ', tabs[i], '   \n')

#   cat('\n')


# #   plot_values <- diamonds %>% filter(cut == tab)

#   data_pre_plot <- data %>%
#     #   filter(get(variable_tabs) == tabs[i]) %>%
#       select(
#           all_of(identifyer),
#           all_of(variable_list)
#       )
    
#   print(paste(as.character(dim(data_pre_plot)), collapse = " - "))

#   cat('\n')


#   to_plot <- reshape2::melt(data_pre_plot, id = identifyer)

#   fig <- multi_plot_choice(
#     type = type_plot,
#     data = to_plot,
#     legend_T = legend
#   )

#   print(
#         #    htmltools::tagList(plotly::subplot(fig, fig, nrows=2, heights = c(0.1, 0.9)))
#        htmltools::tagList(fig)
        
#     )

#   cat('\n')

# }

# }

create_buffers_pre <- function(health_facilities, health_principal_vector){

    health_facilities %>%
    mutate(
        `Being surveilled` = ifelse(nome %in% health_principal_vector, 
            "yes",
            "no"),
        
    )

}

generate_buffers <- function(data_to_model_and_analise ,
                            percentage_of_block ,
                            health_facilities_w ){

    limit_by_geo <- limit_by_geo_and_metric(data_to_model_and_analise
                                ,group_field = "healthdist_name_facility_all"
                                ,metric = "healthdist_all"
                                ,percentage_limit = percentage_of_block)

    generating_buffer_limit(health_facilities_w,
                                                limit_by_geo)


                            }







################################################################################################################
# funciones para el diagnostico

make_dx_long <- function(df,
                         diag_cols   = paste0("diag", 1:4),
                         lab_cols    = paste0("labdiag", 1:4),
                         id_cols     = c("perm_id"),
                         keep_cols   = NULL) {

  df <- df %>% ungroup()

  # Chequeo mínimo
  stopifnot(all(diag_cols %in% names(df)))
  stopifnot(all(lab_cols  %in% names(df)))
  stopifnot(all(id_cols   %in% names(df)))

  # Creamos un índice de fila para poder “emparejar” diag1 con labdiag1, etc.
  df2 <- df %>%
    mutate(.row_id = row_number())

  # Long por diag
  dx <- df2 %>%
    select(all_of(c(id_cols, keep_cols, ".row_id", diag_cols))) %>%
    pivot_longer(
      cols = all_of(diag_cols),
      names_to = "diag_slot",
      values_to = "diag"
    )

  # Long por labdiag
  lb <- df2 %>%
    select(all_of(c(id_cols, keep_cols, ".row_id", lab_cols))) %>%
    pivot_longer(
      cols = all_of(lab_cols),
      names_to = "lab_slot",
      values_to = "labdiag"
    ) %>%
    mutate(diag_slot = str_replace(lab_slot, "labdiag", "diag")) %>%
    select(-lab_slot)

  # Join por fila + slot para alinear diag1<->labdiag1, etc.
  out <- dx %>%
    left_join(lb, by = c(id_cols, keep_cols, ".row_id", "diag_slot")) %>%
    mutate(
      diag = str_trim(diag),
      labdiag = str_trim(labdiag)
    ) %>%
    filter(!is.na(diag), diag != "") %>%
    select(-.row_id)

  diag_agrupation <- out %>%
    select(
        icd10_code = diag
    ) %>% 
    distinct() %>%
    arrange(icd10_code)  %>%
  mutate(
    icd_chapter = case_when(
      icd10_code >= "A00" & icd10_code <= "B99" ~ "Infectious",
      icd10_code >= "C00" & icd10_code <= "D48" ~ "Neoplasms",
      icd10_code >= "D50" & icd10_code <= "D89" ~ "Blood/immune",
      icd10_code >= "E00" & icd10_code <= "E90" ~ "Endocrine/metabolic",
      icd10_code >= "F00" & icd10_code <= "F99" ~ "Mental",
      icd10_code >= "G00" & icd10_code <= "G99" ~ "Nervous system",
      icd10_code >= "H00" & icd10_code <= "H59" ~ "Eye",
      icd10_code >= "H60" & icd10_code <= "H95" ~ "Ear",
      icd10_code >= "I00" & icd10_code <= "I99" ~ "Circulatory",
      icd10_code >= "J00" & icd10_code <= "J99" ~ "Respiratory",
      icd10_code >= "K00" & icd10_code <= "K93" ~ "Digestive",
      icd10_code >= "L00" & icd10_code <= "L99" ~ "Skin",
      icd10_code >= "M00" & icd10_code <= "M99" ~ "Musculoskeletal",
      icd10_code >= "N00" & icd10_code <= "N99" ~ "Genitourinary",
      icd10_code >= "O00" & icd10_code <= "O99" ~ "Pregnancy",
      icd10_code >= "P00" & icd10_code <= "P96" ~ "Perinatal",
      icd10_code >= "Q00" & icd10_code <= "Q99" ~ "Congenital",
      icd10_code >= "R00" & icd10_code <= "R99" ~ "Symptoms",
      icd10_code >= "S00" & icd10_code <= "T98" ~ "Injury",
      icd10_code >= "V01" & icd10_code <= "Y98" ~ "External causes",
      icd10_code >= "Z00" & icd10_code <= "Z99" ~ "Health services",
      TRUE ~ "Other"
    ),
    respiratory_icd_chapter = case_when(
        icd10_code >= "J00" & icd10_code <= "J06" ~ "Infectious",
        icd10_code >= "J30" & icd10_code <= "J39" ~ "Infectious",
        icd10_code >= "J10" & icd10_code <= "J18" ~ "Infectious",
        icd10_code >= "J20" & icd10_code <= "J22" ~ "Infectious",
        icd10_code == "A15"                       ~ "Infectious",
        icd10_code >= "J40" & icd10_code <= "J47" ~ "Chronic",
        icd10_code >= "J60" & icd10_code <= "J70" ~ "Environmental/Interstitial",
        icd10_code >= " J80" & icd10_code <= "J84" ~ "Environmental/Interstitial",
        # icd10_code >= "" & icd10_code <= "" ~ "",
        TRUE ~ "Other")
  ) %>%
  dplyr::rename(diag = icd10_code)


  out %>%
    left_join(diag_agrupation, by = "diag")


}



count_dx <- function(dx_long,
                     flag_col = NULL,
                     top_n = NULL) {

  if (!is.null(flag_col)) {
    stopifnot(flag_col %in% names(dx_long))

    tab <- dx_long %>%
      mutate(flag = case_when(
        isTRUE(.data[[flag_col]])  ~ "TRUE",
        isFALSE(.data[[flag_col]]) ~ "FALSE",
        TRUE                       ~ "NA"
      )) %>%
      count(flag, diag, sort = TRUE) %>%
      group_by(flag) %>%
      mutate(pct = n / sum(n) * 100) %>%
      ungroup()

  } else {
    tab <- dx_long %>%
      count(diag, sort = TRUE) %>%
      mutate(pct = n / sum(n) * 100)
  }

  if (!is.null(top_n)) tab <- tab %>% slice_head(n = top_n)
  tab
}


word_freq_from_labdiag <- function(dx_long,
                                   text_col = "labdiag",
                                   flag_col = NULL,
                                   language = "en",
                                   min_freq = 3) {

  stopifnot(text_col %in% names(dx_long))

  data_txt <- dx_long %>%
    filter(!is.na(.data[[text_col]]), .data[[text_col]] != "")

  if (!is.null(flag_col)) {
    stopifnot(flag_col %in% names(dx_long))
    data_txt <- data_txt %>%
      mutate(flag = case_when(
        isTRUE(.data[[flag_col]])  ~ "TRUE",
        isFALSE(.data[[flag_col]]) ~ "FALSE",
        TRUE                       ~ "NA"
      ))
  }

  # Stopwords
  sw <- tibble(word = stopwords::stopwords(language))

  # Tokenización + limpieza ligera
  freq <- data_txt %>%
    # select(any_of("flag"), text = all_of(text_col)) %>%
    select(text = all_of(text_col)) %>%
    mutate(text = str_to_lower(text)) %>%
    # quita paréntesis y signos típicos
    mutate(text = str_replace_all(text, "[\\(\\)\\[\\],;:]", " ")) %>%
    tidytext::unnest_tokens(word, text) %>%
    filter(!str_detect(word, "^[0-9]+$")) %>%     # fuera números puros
    filter(str_length(word) >= 3) %>%             # evita tokens muy cortos
    anti_join(sw, by = "word") %>%
    count(word, sort = TRUE) %>%
    filter(n >= min_freq)

  freq
}

plot_wordcloud <- function(freq_tbl,
                           max_words = 150,
                           seed = 123) {

  set.seed(seed)

  # Si viene estratificado por flag, aquí se espera que filtres antes (ver ejemplo)
  wordcloud::wordcloud(
    words = freq_tbl$word,
    freq  = freq_tbl$n,
    max.words = max_words,
    random.order = FALSE
  )
}
