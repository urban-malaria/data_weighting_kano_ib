rm(list=ls())

source("loadpath.R")


weights_data <- read.csv(file.path(dhsDir,"nigeria/kano_ibadan_epi/new_field_data/Kano Wet Season Data Sept. 2024/kano_weights_data.csv"))

dry_season_kano_data_without_rdt <- read_dta("C:/Users/laure/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi/new_field_data/Kano Dry Season Data_latest_Nov2024/Kano dry season survey data/Dry Season Data inclusive of wrong coordiates.dta")[, -c(2,3)]

dry_season_kano_data_rdt <- read_dta("C:/Users/laure/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi/new_field_data/Kano Dry Season Data_latest_Nov2024/Kano dry season survey data/KN dry hhold with RDT_131124.dta")


dry_season_kano <- dry_season_kano_data_without_rdt %>% 
  inner_join(dry_season_kano_data_rdt)




ib_weights <- weights_data %>% 
  transmute(longitude, latitude, ward, enumeration_area.x, cluster_number.x, 
            hh_serial_no_in_ea.y, hh_serial_no_in_structure.y, 
            ward_weight = 1/prob_selected_ward, 
            ea_settlement_weight = 1/prob_selected_eas_settlement, 
            hhs_weights = 1/prob_selected_hh_structure)



ibadan_data$Ward <- factor(ibadan_data$bi2, 
                           levels = c(1, 2, 3, 4), 
                           labels = c("Agugu", "Basorun", "Challenge", "Olopomewa"))

ibadan_data$ea_cluster <- ibadan_data$bi5

combined_data_corrected_eas <- ibadan_data %>% 
  mutate(line_number00 = hl1, 
         age_calc = hl5,
         unique_id = paste0(sn, "_", hl1), 
         agebin = cut(age_calc, c(0, 5, 10, 17, 30, 100), include.lowest = T)) %>%
  # inner_join(Kano_data_malaria_screening_cleaned, by = c("serial_number", "unique_id")) %>% 
  group_by(Ward) %>%
  mutate(ward_total = n()) %>% 
  ungroup() %>% 
  group_by(Ward, ea_cluster) %>% 
  mutate(ea_total = n()) %>%
  ungroup() %>% 
  group_by(Ward, ea_cluster, sn) %>% 
  mutate(hh_total = n()) %>%
  ungroup() %>% 
  mutate(agebin = cut(age_calc, c(0,5,10,17,30, 122), include.lowest = T)) %>% 
  group_by(Ward, ea_cluster, sn, agebin) %>%
  mutate(age_total = n(), 
         longitude = as.numeric(bi7_long), 
         latitude = as.numeric(bi7_lat)) 




add_totals <- combined_data_corrected_eas %>% 
  mutate(longitude = as.numeric(longitude), 
         latitude = as.numeric(latitude)) %>%  
  tidyr::drop_na(longitude,latitude) 



add_totals_sf <- sf::st_as_sf(add_totals, coords = c("longitude", "latitude"), crs = 4326)
ib_weight_sf <- sf::st_as_sf(ib_weights, coords = c("longitude", "latitude"), crs = 4326) 

merged_dataset <- sf::st_join(add_totals_sf, ib_weight_sf) 

nearest_indices <- sf::st_nearest_feature(add_totals_sf, ib_weight_sf)
merged_dataset <- cbind(add_totals_sf, ib_weight_sf[nearest_indices, ])



missing_weights <- merged_dataset %>%
  filter(is.na(hhs_weights))



# modified_merged_dataset <- merged_dataset %>% 
#   mutate(enumeration_area = enumeration_area) %>% 
#   group_by(ward) %>%
#   mutate(ward_weight = ifelse(is.na(ward_weight)==T, mean(ward_weight, na.rm = T), ward_weight)) %>% 
#   ungroup() %>% 
#   group_by(ward, bi3 , ea_settlement_weight) %>% 
#   mutate(ea_settlement_weight = ifelse(is.na(ea_settlement_weight)==T, mean(ea_settlement_weight, na.rm = T), ea_settlement_weight)) %>% 
#   ungroup() %>% 
#   group_by(ward, enumeration_area ,bi3) %>% 
#   mutate(hhs_weights = ifelse(is.na(hhs_weights)==T, 
#                               mean(hhs_weights, na.rm = T), 
#                               hhs_weights)) 



modified_merged_dataset_updated <- merged_dataset %>% 
  group_by(ward, bi3, ea_cluster, hh_total, agebin) %>% 
  mutate(ind_total = n(),
         prob_ind_hh = 1/ind_total, 
         ind_weights_hh = 1/prob_ind_hh, 
         overall_hh_weight  = ind_weights_hh * ward_weight *
           ea_settlement_weight * hhs_weights) %>% 
  ungroup() 


# coords <- sf::st_coordinates(modified_merged_dataset_updated)
# 
# 
# modified_merged_dataset_updated$longitude <- coords[, 'X']
# modified_merged_dataset_updated$latitude <- coords[, 'Y']

modified_merged_dataset_mod <- modified_merged_dataset_updated %>% 
  sf::st_drop_geometry() %>% 
  sf::st_drop_geometry(geometry.1) %>% 
  dplyr::select(-geometry.1)

modified_merged_dataset_mod <- modified_merged_dataset_mod[!duplicated(modified_merged_dataset_mod$unique_id, fromLast = TRUE), ] 

write.csv(modified_merged_dataset_mod, file.path(cleaned_data_path, metropolis_name,"kano_malaria_weighted_information_dry_season.csv"), row.names = FALSE) 

# write_dta(modified_merged_dataset_mod, file.path(dhsDir,"nigeria/kano_ibadan_epi/new_field_data/Kano Dry Season Data Sept. 2024/kano_malaria_weighted_information_final_dry_season.dta"))

finalweightdta <- janitor::clean_names(modified_merged_dataset_mod)

write_csv(modified_merged_dataset_mod, file.path(dhsDir,"nigeria/kano_ibadan_epi/new_field_data/ibadan_malaria_weighted_information_dry_season.csv"))

write_dta(finalweightdta, file.path(dhsDir,"nigeria/kano_ibadan_epi/new_field_data/ibadan_malaria_weighted_information_dry_season.dta"))
