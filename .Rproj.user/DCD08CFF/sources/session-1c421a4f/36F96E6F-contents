rm(list=ls())

source("loadpath.R")

library(haven)

weights_data <- read.csv(file.path(dhsDir,"nigeria/kano_ibadan_epi/new_field_data/Kano Wet Season Data Sept. 2024/kano_weights_data.csv"))

wet_season_kano_data_without_rdt <- read_dta(file.path(dhsDir,"nigeria/kano_ibadan_epi/new_field_data/Kano wet Season Data_latest_Nov2024/Kano wet season survey data/wet Season Data inclusive of wrong coordiates.dta"))[, -c(2,3)]

wet_season_kano_data_rdt <- read_dta(file.path(dhsDir,"nigeria/kano_ibadan_epi/new_field_data/Kano wet Season Data_latest_Nov2024/Kano wet season survey data/KN wet hhold with RDT_131124.dta"))




wetdata <- read_dta("/Users/macbookpro/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi/new_field_data/Kano Wet Season Data Sept. 2024/KN wet season hhold  RDT results_290924.dta")
wethh <- read_dta("/Users/macbookpro/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi/new_field_data/Kano Wet Season Data Sept. 2024/KN Wet season household data_edited_290924.dta")

wet_season_kano <- wethh %>% 
  inner_join(wetdata, by = "sn")

kano_weights <- weights_data %>% 
  transmute(longitude, latitude, ward, enumeration_area, ea_serial_number, 
            hh_serial_number, structure_serial_number, 
            ward_weight = 1/prob_selected_ward, 
            ea_settlement_weight = 1/prob_selected_eas_settlement, 
            hhs_weights = 1/prob_selected_hh_structure)





combined_data_corrected_eas <- wet_season_kano %>% 
  mutate(line_number00 = hl1, 
         age_calc = hl5,
         unique_id = paste0(sn, "_", hl1), 
         agebin = cut(age_calc, c(0, 5, 10, 17, 30, 100), include.lowest = T),
         ea_cluster = ea,
         ward = as_factor(ward)
         ) %>%
  # inner_join(Kano_data_malaria_screening_cleaned, by = c("serial_number", "unique_id")) %>% 
  group_by(ward) %>%
  mutate(ward_total = n()) %>% 
  ungroup() %>% 
  group_by(ward, ea_cluster) %>% 
  mutate(ea_total = n()) %>%
  ungroup() %>% 
  group_by(ward, ea_cluster, sn) %>% 
  mutate(hh_total = n()) %>%
  ungroup() %>% 
  mutate(agebin = cut(age_calc, c(0,5,10,17,30, 122), include.lowest = T)) %>% 
  group_by(ward, ea_cluster, sn, agebin) %>%
  # mutate(age_total = n(), 
  #        longitude = as.numeric(bi7_long), 
  #        latitude = case_match("e+01" %in% bi7_long ~ bi7_long/1e1
  #                              "e+08" %in% bi7_long ~ bi7_long/1e8
  #                             
  #                              ))
  mutate(
    age_total = n(),
    longitude = as.numeric(bi7_long),
    latitude = case_when(
      bi7_lat >= 1e7 ~ bi7_lat / 1e7,  # If values are in E7 format
      bi7_lat >= 1e1 & bi7_lat < 1e7 ~ bi7_lat / 1e1,  # If values are in E1 format
      TRUE ~ as.numeric(bi7_lat)  # Default case: keep as is
    ),
    latitude_n = ifelse(latitude < 11 , latitude *10, latitude)
  )



add_totals <- combined_data_corrected_eas %>% 
  mutate(longitude = as.numeric(longitude), 
         latitude = as.numeric(latitude_n)) %>%  
  tidyr::drop_na(longitude,latitude_n) 



add_totals_sf <- sf::st_as_sf(add_totals, coords = c("longitude", "latitude"), crs = 4326)
kano_weight_sf <- sf::st_as_sf(kano_weights, coords = c("longitude", "latitude"), crs = 4326) 

merged_dataset <- sf::st_join(add_totals_sf, kano_weight_sf) 

nearest_indices <- sf::st_nearest_feature(add_totals_sf, kano_weight_sf)
merged_dataset <- cbind(add_totals_sf, kano_weight_sf[nearest_indices, ])



missing_weights <- merged_dataset %>%
  filter(is.na(hhs_weights))



modified_merged_dataset <- merged_dataset %>% 
  mutate(enumeration_area = enumeration_area) %>% 
  group_by(ward) %>%
  mutate(ward_weight = ifelse(is.na(ward_weight)==T, mean(ward_weight, na.rm = T), ward_weight)) %>% 
  ungroup() %>% 
  group_by(ward, bi3 , ea_settlement_weight) %>% 
  mutate(ea_settlement_weight = ifelse(is.na(ea_settlement_weight)==T, mean(ea_settlement_weight, na.rm = T), ea_settlement_weight)) %>% 
  ungroup() %>% 
  group_by(ward, enumeration_area ,bi3) %>% 
  mutate(hhs_weights = ifelse(is.na(hhs_weights)==T, 
                              mean(hhs_weights, na.rm = T), 
                              hhs_weights)) 



modified_merged_dataset_updated <- modified_merged_dataset %>% 
  group_by(ward, bi3, enumeration_area, hh_total, agebin) %>% 
  mutate(ind_total = n(),
         ward_weight = ifelse(ward =="Fagge", 1, ward_weight ),
         prob_ind_hh = 1/ind_total, 
         ind_weights_hh = 1/prob_ind_hh, 
         overall_hh_weight  = ind_weights_hh * ward_weight *
           ea_settlement_weight * hhs_weights) %>% 
  ungroup() 


missing_weights_hh <- modified_merged_dataset_updated %>%
  filter(is.na(overall_hh_weight))


coords <- sf::st_coordinates(modified_merged_dataset_updated)


modified_merged_dataset_updated$longitude <- coords[, 'X']
modified_merged_dataset_updated$latitude <- coords[, 'Y']

modified_merged_dataset_mod <- modified_merged_dataset_updated %>% 
  sf::st_drop_geometry() %>% 
  sf::st_drop_geometry(geometry.1) %>% 
  dplyr::select(-geometry.1)

modified_merged_dataset_mod <- modified_merged_dataset_mod[!duplicated(modified_merged_dataset_mod$unique_id, fromLast = TRUE), ] 


stata_format <- function(name) {
  
  name <- gsub("[\\(\\)]", "", name)  # Remove parentheses
  name <- gsub("[^[:alnum:]_]", "_", name)  # Replace special characters with underscores
  name <- tolower(name)  # Convert to lowercase
  
  # Ensure names are no longer than 32 characters
  name <- substr(name, 1, 32)
  
  # Make sure the name starts with a letter or underscore
  if (!grepl("^[a-zA-Z_]", name)) {
    name <- paste0("v_", name)  
  }
  
  return(name)
}


names(modified_merged_dataset_mod) <- sapply(names(modified_merged_dataset_mod), stata_format)




write_dta(modified_merged_dataset_mod, file.path(dhsDir,"nigeria/kano_ibadan_epi/new_field_data/kano_malaria_weighted_information_final_wet_season_new.dta"))

write_csv(modified_merged_dataset_mod, file.path(dhsDir,"nigeria/kano_ibadan_epi/new_field_data/kano_malaria_weighted_information_final_wet_season_new.csv"))


