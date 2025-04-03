
rm(list=ls())

source("loadpath.R")

###Household Listed into 
ib_hh_listed_00<- readxl::read_excel(file.path(dhsDir,"nigeria/kano_ibadan_epi/EA_data/Ibadan_HH_Listing_2023-12-01 (1).xlsx"))

ib_hh_listed <- ib_hh_listed_00 %>%
  mutate(ea_serial_number_new = Cluster_Number 
  )



#Reading the sampled HH file

ib_hh_sampled <- read_csv(file.path(dhsDir,"nigeria/kano_ibadan_epi/Sampled HHs/Ib_sampled_list_final.csv"))
ib_hh_sampled <- ib_hh_sampled %>%
  mutate(ea_serial_number_new = cluster_number 
  )

# 
# kano_hh_sampled <- kano_hh_sampled %>% 
#   mutate(enumeration_area = paste0(toupper(ea_name_new), "/", ea_serial_number_new), 
#          enumeration_area = str_replace_all(enumeration_area, "[ ,]", ""))

geosampled <- ib_hh_listed_00 %>%
  dplyr::select("_index",`_Enter_GPS_Location_latitude`, `_Enter_GPS_Location_longitude`)

ib_hh_sampled00 <- left_join(ib_hh_sampled, geosampled, by = c("_index"="_index"))

ib_hh_sampled <- ib_hh_sampled00 %>%
  group_by(enumeration_area) %>%  # Group by the Enumeration Area
  mutate(
    # Assign total households in the same EA to 'tota_hh_ea', only if 'tota_hh_ea' is NA
    tota_hh_ea = n(),  
    
    # Assign serial number within the EA, only if 'hh_serial_no_in_ea' is NA
    hh_serial_no_in_ea =  row_number()
  ) %>%
  ungroup()


names(ib_hh_sampled)

# #Reading corrected EA names
# newcorrected_EAS <- read_xlsx(file.path(dhsDir,"nigeria/kano_ibadan_epi/EA_data/Kano Selected EAs.xlsx"))%>%
#   mutate(capital_ea_name = toupper(name), 
#          ea_name = paste0(capital_ea_name,"/",eas),
#         ea_name = gsub(" ","",ea_name)
#          )

# 
# information <- datahhkn %>%
#  dplyr::select(1,6,121,122,10,120,12,13,14,15 )
# write_dta(information, "/Users/macbookpro/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi/new_field_data/Kano Wet Season Data Sept. 2024/HH_Details_request.dta")
# 
# 
# rdtkn_data <- read_dta(file.path(dhsDir,"nigeria/kano_ibadan_epi/new_field_data/Kano Wet Season Data Sept. 2024/Kano_data_with_EA_names_cleaned_to_114.dta") )
# 
# gpsmissing <- haven::read_dta(file.path(dhsDir,"nigeria/kano_ibadan_epi/new_field_data/Kano_missing_lon_lat_add_totals.dta"))%>%
#   left_join(rdtkn_data, by=c("serial_number" = "sn"),relationship = "many-to-many")%>%
#   dplyr::select(1:34,46,47,153:155)


###Household Sampled
ib_hh_listed <- as.data.frame(ib_hh_listed)

names(ib_hh_listed) <- c("start", "end", "location", "latitude", "longitude", 
                           "altitude", "precision", "date_time", "city", "ward","PSU",
                           "enumeration_area", "ea_serial_number", "settlement", 
                           "lister_name", "phone_number", "structure_serial_number", 
                           "address", "residential_structure", "hh_serial_number", "Status_of_dwelling",
                            "notes_one", "save_one", "visit_two", 
                           "notes_two", "save_two", "visit_three", "hh_gender", "number_in_hh", 
                           "final_completion", "gratitude", "id", "uuid", "submission",
                           "validation_status", "notes_three", "status", "submitted",
                           "version",  "tags", "index","ea_serial_number_new" )

 



listed_households <- ib_hh_listed %>%
  mutate(prob_selected_ward= case_when(ward == "Agugu"~ 1/14,
                                       ward == "Challenge"~ 1/24,
                                       ward == "Bashorun"~ 1/15,
                                       ward == "Olopomewa"~ 1/6)) %>%
  group_by(enumeration_area,structure_serial_number) %>%
  mutate(total_hh_listed_structure = n())


ib_hh_sampled_ <- ib_hh_sampled %>%
  mutate(
    settlement = case_when(
      enumeration_area %in% c("olopomewa_004", "olopomewa_006", "olopomewa_016") ~ "formal",
      enumeration_area %in% c("olopomewa_010", "olopomewa_044", "olopomewa_051", "olopomewa_019") ~ "informal",
      TRUE ~ settlement  # This handles any unmatched cases
    )
  )


selected_household <- ib_hh_sampled_ %>%
  mutate(
    prob_selected_eas_settlement = case_when(
      ward== "Agugu" & settlement == "informal" ~ 0.56,
      ward== "Agugu" & settlement == "slum" ~ 0.57,
      ward== "Bashorun" & settlement == "formal" ~ 0.43,
      ward== "Bashorun" & settlement == "informal" ~ 0.47,
      ward== "Bashorun" & settlement == "slum" ~ 0.5,
      ward== "Challenge" & settlement == "formal" ~ 0.72,
      ward== "Challenge" & settlement == "informal" ~ 0.7,
      ward== "Challenge" & settlement == "slum" ~ 1,
      ward== "Olopomewa" & settlement == "formal" ~ 0.7,
      ward== "Olopomewa" & settlement == "informal" ~ 0.68,
      #TRUE ~ Value  # If none of the conditions match, keep the original 'Value'
    )
  ) %>%
  group_by(ward, enumeration_area,`_001_serial_number_of_structure`, `_004_serial_number_o_old_in_the_`) %>%
  mutate(total_hh_selected_structure = n()) %>%
  ungroup()%>%
  group_by(ward, enumeration_area,`_001_serial_number_of_structure`)%>%
  mutate(
    total_hh_structure = n()
  )

missingsettlements <- filter(selected_household, is.na(prob_selected_eas_settlement))
write_csv(missingsettlements, file.path(dhsDir,"nigeria/kano_ibadan_epi/Sampled HHs/Missing_settlements_ib.csv"))







# listed_households <- kano_hh_listed %>% 
#   mutate(
#     enumeration_area = paste0(toupper(ea_name_new), "/", ea_serial_number_new), 
#     enumeration_area = str_replace_all(enumeration_area, "[ ,]", ""),
#     prob_selected_ward = case_when(ward == "Zango"~ 1,
#                                    ward == "Dorayi"~ 1/2,
#                                    ward == "Tudun Wazurchi"~ 1,
#                                    ward == "Gobirawa"~ 1, 
#                                    ward == "Giginyu"~ 1,
#                                    ward == "Fagge"~ 1)) %>%
#   group_by(ea_serial_number_new, ward, hh_serial_number, structure_serial_number) %>%
#   distinct() %>% 
#   mutate(total_hh_listed_structure = n())


# selected_household <- kano_hh_sampled %>% 
#   mutate(enumeration_area  = enumeration_area,
#     prob_selected_eas_settlement = case_when(
#       ward == "Zango" & settlement == "Informal" ~ 1,
#       ward == "Zango" & settlement == "Slum" ~ 1,
#       ward == "Zango" & settlement == "Formal" ~ 1,
#       
#       ward == "Gobirawa" & settlement == "Formal" ~ 3/14,
#       ward == "Gobirawa" & settlement == "Informal" ~ 37/228,
#       ward == "Gobirawa" & settlement == "Slum" ~ 0,
#       
#       ward == "Giginyu" & settlement == "Formal" ~ 1,
#       ward == "Giginyu" & settlement == "Informal" ~ 1,
#       ward == "Giginyu" & settlement == "Slum" ~ 0,
#       
#       ward == "Fagge" & settlement == "Formal" ~ 1,
#       ward == "Fagge" & settlement == "Informal" ~ 1,
#       ward == "Fagge" & settlement == "Slum" ~ 0,
#       
#       ward == "Dorayi" & settlement == "Formal" ~ 1,
#       ward == "Dorayi" & settlement == "Informal" ~ 27/29,
#       ward == "Dorayi" & settlement == "Slum" ~ 0,
#       
#       TRUE ~ NA_real_  # If none of the conditions match, keep the original 'Value'
#     )
#   ) %>%
#   group_by(enumeration_area, ea_serial_number_new, ward, ea_name_new ,structure_serial_number) %>% 
#   dplyr::distinct() %>% 
#   mutate(total_hh_selected_structure = n(), ) %>% 
#   ungroup()


naeas <- selected_household %>% filter(is.na(prob_selected_eas_settlement))




all_eas <- selected_household %>% 
  dplyr::select(ward, enumeration_area, cluster_number , settlement) %>% 
  group_by(ward, enumeration_area, cluster_number, settlement) %>% 
  summarise(total = n(), 
            ea_name = paste0(enumeration_area[1], "/", cluster_number[1]))



all_selected_hh <- inner_join(selected_household, 
                             ib_hh_sampled , 
                              by = c( "_index" = "_index"), relationship = "many-to-many"
                             ) 


all_selected_hh_ <- all_selected_hh  %>% 
  mutate(prob_selected_hh_structure = total_hh_selected_structure/total_hh_structure) 

all_selected_hh_ <- all_selected_hh_ %>%
  mutate(prob_selected_ward= case_when(ward.x == "Agugu"~ 1/14,
                                       ward.x == "Challenge"~ 1/24,
                                       ward.x == "Bashorun"~ 1/15,
                                       ward.x == "Olopomewa"~ 1/6)) 



weights_data <- all_selected_hh_ %>% 
  dplyr::select(longitude=`_Enter_GPS_Location_longitude.y`, latitude=`_Enter_GPS_Location_latitude.y`, settlement.x, ward = ward.x, `_index`,
                enumeration_area.x, cluster_number.x,
                hh_serial_no_in_ea.y,
                hh_serial_no_in_structure.y,prob_selected_ward,
                prob_selected_eas_settlement,
                prob_selected_hh_structure)%>%
  distinct()

finalweight <- weights_data %>%
  mutate(latitude = as.double(latitude),
         longitude = as.double(longitude)
         )

write.csv(weights_data, file.path(dhsDir,"nigeria/kano_ibadan_epi/new_field_data/Ibadan Dry Season data_latest_Nov24/ib_weights_data.csv"))
