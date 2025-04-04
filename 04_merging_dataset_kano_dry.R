LuDir <- file.path(dhsDir,"nigeria/kano_ibadan_epi/new_field_data/Kano Dry Season Data_latest_Nov2024/Kano dry season survey data")
#Maldir <- file.path(dhsDir,"nigeria/kano_ibadan_epi/new_field_data/Kano dry season survey data/")
# IbDir <- file.path(dhsDir,"nigeria/kano_ibadan_epi/new_field_data/240922_Ibadan_latest_data/")  


#write_dta(modified_merged_dataset_mod, file.path(dhsDir,"nigeria/kano_ibadan_epi/new_field_data/kano_malaria_weighted_information_final_dry_season_new.dta"))


# data paths 
kano_household_listdry <- read_dta(file.path(dhsDir,"nigeria/kano_ibadan_epi/new_field_data/kano_malaria_weighted_information_final_dry_season_new.dta"))

#kano_household_list <- read.csv(file.path(cleaned_data_path, metropolis_name,"kano_malaria_weighted_information_dry_season.csv"))
dryseasonhhdata <- read_dta(file.path(LuDir, "KN dry hhold data_edited_131124.dta" ))

 kn_household_net_insp <- read_dta(file.path(LuDir, "KN dry hhold net inspectn.dta" ))
 kn_household_travelers <- read_dta(file.path(LuDir, "KN dry hhold travellers.dta" ))
 kn_household_visitors <- read_dta(file.path(LuDir, "KN dry hhold visitors.dta" ))
kn_women_survey <- read_dta(file.path(LuDir, "KN dry- women edited_131124.dta" ))
kn_womenmal_survey <- read_dta(file.path(LuDir, "KN dry- women malaria hx_081124.dta" ))
kn_men_survey <- read_dta(file.path(LuDir, "KN dry -men edited 131124.dta" ))
kn_womenmob_survey <- read_dta(file.path(LuDir, "KN dry- women mobility_081124.dta" )) 
#




wide_kano_data <- kano_household_listdry %>%
  pivot_wider(
    names_from = hl1, 
    values_from = c(hl2:q304),
    id_cols = sn
  )


wide_kano_datadetails <- kano_household_list %>%
  pivot_wider(
    names_from = hl1, 
    values_from = c(hl2:q304)
  ) %>%
  distinct(sn, .keep_all = TRUE)%>%
  dplyr::select(1:143)


dry <- dryseasonhhdata %>%
  dplyr::select(1:112) 

wide_ <- wide_kano_datadetails %>%
  dplyr::select(113:143)  

widedata_hh <- left_join(dry, wide_kano_data, wide_, by ="sn") 



wide_kn_household_visitors <- kn_household_visitors %>%
  dplyr::select(-c(redcap_repeat_instrument)) %>% 
  pivot_wider(names_from = redcap_repeat_instance, 
              values_from = c(v1:visitors_complete))

wide_kn_household_travelers <- kn_household_travelers %>%
  dplyr::select(-c(redcap_repeat_instrument)) %>% 
  pivot_wider(names_from = c(redcap_repeat_instance, c1ii ), 
              values_from = c(c1iii:tfhi_complete))


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


names(wide_kn_household_travelers) <- sapply(names(wide_kn_household_travelers), stata_format)




wide_kn_household_net_insp <- kn_household_net_insp %>%
  dplyr::select(-c(redcap_repeat_instrument)) %>% 
  pivot_wider(names_from = c(redcap_repeat_instance), 
              values_from = c(nh108:net_inspection_complete))

widedata_hh %>% count(sn) %>% filter(n > 1)


wide_data_together <- widedata_hh %>% 
  left_join(kn_men_survey, by = "sn", suffix = c("", "_men")) %>% 
  left_join(kn_women_survey, by = "sn", suffix = c("", "_women")) %>% 
  left_join(wide_kn_household_net_insp, by = "sn", suffix = c("", "_net_insp")) %>% 
  left_join(wide_kn_household_travelers, by = "sn", suffix = c("", "_travelers")) %>% 
  left_join(wide_kn_household_visitors, by = "sn", suffix = c("", "_visitors"))

names(wide_data_together) <- sapply(names(wide_data_together), stata_format)
#cleaning names for saving 
wide_data_together_filtered <- wide_data_together[, !grepl("\\.y", names(wide_data_together))]

names(wide_data_together_filtered)[grepl("\\.x", names(wide_data_together_filtered))] <- gsub("\\.x", "", names(wide_data_together_filtered)[grepl("\\.x", names(wide_data_together_filtered))])

names(wide_data_together_filtered) <- gsub("[^[:alnum:]]", "_", names(wide_data_together_filtered))



write_dta(wide_data_together_filtered, "/Users/macbookpro/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi/Combined Working Data/Kano/Dry Season Data/Wide Data/kano_dryseason_wide_data.dta")

write_csv(wide_data_together_filtered, "/Users/macbookpro/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi/Combined Working Data/Kano/Dry Season Data/Wide Data/kano_dryseason_wide_data.csv")


##################################
#long data 
##################################
allnames_householdlist <- names(kano_household_list)



# removing duplicated columns before merging 
women_common_names <- which(names(kn_women_survey) %in% allnames_householdlist)[-1]

# kn_women_survey <- kn_women_survey %>% 
  
kn_women_survey <- kn_women_survey %>% 
  dplyr::select(-c(women_common_names))



men_common_names <- which(names(kn_men_survey) %in% allnames_householdlist)[-1]

kn_men_survey <- kn_men_survey %>% 
  dplyr::select(-c(men_common_names))


 net_insp_common_names <- which(names(kn_household_net_insp) %in% allnames_householdlist)[-c(1, 3)]
 
 kn_household_net_insp <- kn_household_net_insp %>% 
   dplyr::select(-c(net_insp_common_names))



 kn_household_net_insp_mod_hh <- kn_household_net_insp %>% 
   group_by(sn) %>% 
mutate(number_nets = max(nh108)) %>%
  mutate(number_nets = max(redcap_repeat_instance )) %>%
  transmute(number_nets,  redcap_repeat_instance,nh108,
            nh108a_yes = sum(ifelse(nh108a == 1, 1, 0), na.rm = T),
            nh108a_no = sum(ifelse(nh108a == 2, 1, 0), na.rm = T),
            number_nets = ifelse((nh108a_yes + nh108a_no)!= number_nets,
                                 (nh108a_yes + nh108a_no), number_nets ),
            nh109_yes = sum(ifelse(nh109 == 1, 1, 0), na.rm = T),
            nh109_no = sum(ifelse(nh109 == 2, 1, 0), na.rm = T),
            nh110_yes = sum(ifelse(nh110 == 1, 1, 0), na.rm = T),
            nh110_no = sum(ifelse(nh110 == 2, 1, 0), na.rm = T),
            nh111_cotton = sum(ifelse(nh111 == 1, 1, 0), na.rm = T),
            nh111_synthetic = sum(ifelse(nh111 == 2, 1, 0), na.rm = T),
            nh111_others= sum(ifelse(nh111 == 3, 1, 0), na.rm = T),
            nh113_yes = sum(ifelse(nh113 == 1, 1, 0), na.rm = T),
            nh113_no = sum(ifelse(nh113== 2, 1, 0), na.rm = T), ) %>%
   pivot_wider(names_from = nh108,
               values_from = c(nh108a_yes:nh113_no ,number_nets
                               )) %>%
  distinct() %>%
  mutate(count = n())

 
 
 
 
 
 
 ib_household_net_insp_mod_ind <- kn_household_net_insp %>% 
#   # to be corrected later after adding unique ids 
   group_by(sn) %>% 
   mutate(unique_id = paste0(sn,"/",)) %>% 
   pivot_wider(names_from = nh108, 
               values_from = c(nh108a:n113, n115:number_nets))



travelers_common_names <- which(names(kn_household_travelers) %in% allnames_householdlist)[-1]

kn_household_travelers <- kn_household_travelers %>%
  dplyr::select(-c(travelers_common_names))






visitors_common_names <- which(names(kn_household_visitors) %in% allnames_householdlist)[-1]

kn_household_visitors <- kn_household_visitors %>%
  dplyr::select(-c(visitors_common_names))

kn_household_visitors_mod <- kn_household_visitors  %>%
  group_by(sn) %>%
  mutate(number_visitors = n()) %>%
  transmute(redcap_repeat_instance, number_visitors,
            v1) %>%
  distinct()





names(kn_women_survey) <- c( "sn", paste0("women_", names(kn_women_survey)[-1]))
names(kn_men_survey) <- c("sn", paste0("men_", names(kn_men_survey)[-1]))



kn_household_common_names <- which(names(ib_household) %in% allnames_householdlist)[-1]

kn_household_mod <- kn_household %>%
  dplyr::select(-c(kn_household_common_names))

ib_household <- ib_household%>%
  dplyr::select(-c(redcap_repeat_instrument,
         redcap_repeat_instance, hl4, ))



long_data_together <-kano_household_list %>% 
  dplyr::left_join(kn_men_survey, by = ("sn")) %>% 
  left_join(kn_women_survey, by = ("sn" ))  %>% 
  left_join(kn_household_net_insp, by = c("sn", "redcap_repeat_instance"), relationship = "many-to-many") %>% 
  left_join(kn_household_net_insp_mod_hh, by =c("sn", "redcap_repeat_instance"), relationship = "many-to-many") %>% 
  left_join(kn_household_travelers, by =c("sn", "redcap_repeat_instance"), relationship = "many-to-many") %>%  #Individual and should be put at the end
  left_join(kn_household_visitors, by =c("sn", "redcap_repeat_instance"), relationship = "many-to-many")%>%
  left_join(kn_household_visitors_mod, by =c("sn", "redcap_repeat_instance"), relationship = "many-to-many")%>%
  distinct()


long_data_together <- long_data_together

# %>%
#   dplyr::select(-c(463:485,162:184))

write_dta(long_data_together, file.path(LuDir,"long_dryseason_household_membersV00.dta")) #no nets information 



#cleaning names for saving 
long_data_together <- long_data_together[, !grepl("\\.y", names(long_data_together))]

names(long_data_together)[grepl("\\.x", names(long_data_together))] <- gsub("\\.x", "", names(long_data_together)[grepl("\\.x", names(long_data_together))])

names(long_data_together) <- gsub("[^[:alnum:]]", "_", names(long_data_together))

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


names(long_data_together) <- sapply(names(long_data_together), stata_format)




write_dta(long_data_together, "/Users/macbookpro/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi/Combined Working Data/Kano/Dry Season Data/Long Data/kano_dryseason_long_data.dta")

write_csv(long_data_together, "/Users/macbookpro/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi/Combined Working Data/Kano/Dry Season Data/Long Data/kano_dryseason_long_data.csv")
