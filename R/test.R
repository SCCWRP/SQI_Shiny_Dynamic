# # Script pulling the SQI dataset from the SMC and reformatting it to align with Shiny dashboard script
# # This is what makes the dataset "Dynamic"
# 
# # by Annie Holt 4/13/2022
# # Generally have to prep/rename columns and clean dataset a bit 
# # Then run through SQI function to calculate
# 
# # Data Assembly notes (this was all done in a database query):
# # Max Bio sample (ASCI, CSCI) was taken per masterid/sampledate
# # Average Chemistry sample (TN, TP, Cond) was taken per masterid/sampledate
# # Chemistry Data (TN, TP, Cond) was joined to Bio Data (ASCI, CSCI) by masterid and sampledate
# # Habitat Data (PHAB, CRAM) was joined to Bio/Chem Data by masterid and year
# 
# # If there were multiple samples in one year, just keep most recent row of data; we just want one row per masterid
# # Also only kept 'complete' data rows (have all Chem, Bio, Hab metrics per site/year)
# 
# # required libraries
# library(tidyverse)
# library(sf)
# library(SQI)
# 
# # Import new/dynamic data
# # this dataset is created as a database View in SCCWRP's SMC database (vw_sqi_dat)
# # to change the dataset/how it is assembled, need to edit the View ... though some changes can be made below as well if don't need to restructure
# # should be in-house accessible only
# sqi_raw <- read_csv("https://nexus.sccwrp.org/smcchecker/sqi_rawdata")
# 
# # # load underlying shapefiles
# # # do this in app instead
# data(sheds)
# data(cntys)
# data(rwqbs)
# data(cnstr)
# # # old data for reference
# # # data("sqidat")
# 
# sqidat_fordash <- sqi_raw %>% 
#   # column renaming to match with names in index.Rmd 
#   rename(MasterID = masterid, COMID = comid, 
#          
#          yr = year, ASCI = d_asci_max, CSCI = csci_max, IPI = ipi,
#          # stream class, scape categories
#          strcls = ref10, lower = qt10, meds = qt50, upper = qt90,
#          # phab metrics
#          Ev_FlowHab = ev_flowhab_score, H_AqHab = h_aqhab_score,H_SubNat = h_subnat_score, PCT_SAFN = pct_safn_score, XCMG = xcmg_score,
#          # cram score and metrics
#          indexscore_cram = cram_score,
#          ps = cram_physicalstructure, hy = cram_hydrology, blc = cram_bufferandladscapecontext, bs = cram_bioticstructure,
#          # biostim analytes
#          Cond = cond, TN = total_n_all, TP = total_p_all) %>% 
#   mutate(Ev_FlowHab_raw = ev_flowhab, H_AqHab_raw = h_aqhab,H_SubNat_raw = h_subnat, PCT_SAFN_raw = pct_safn, XCMG_raw = xcmg) %>%
#   # assign levels to stream class
#   mutate(strcls  = factor(strcls, levels = c("likely unconstrained", "possibly unconstrained", "possibly constrained",
#                                              "likely constrained"))) %>% 
#   
#   # final columns needed for Shiny
#   select(MasterID, COMID, latitude, longitude, yr, csci_sampledate, ASCI, CSCI, IPI, Ev_FlowHab, Ev_FlowHab_raw, H_AqHab, H_AqHab_raw, 
#          H_SubNat, H_SubNat_raw,  PCT_SAFN, PCT_SAFN_raw,  XCMG, XCMG_raw,
#          indexscore_cram, ps, hy, blc, bs, Cond, TN, TP, strcls, lower, meds, upper)  %>%
#   
#   #just drop if missing any ASCI or CSCI
#   drop_na(ASCI, CSCI) %>%
#   
#   # Add data_status column based on missing data conditions
#   mutate(data_status = case_when(
#     (is.na(IPI) | is.na(Ev_FlowHab) | is.na(H_AqHab) | is.na(H_SubNat) | 
#        is.na(PCT_SAFN) | is.na(XCMG) | is.na(indexscore_cram) | is.na(ps) | 
#        is.na(hy) | is.na(blc) | is.na(bs)) & 
#       !(is.na(Cond) | is.na(TN) | is.na(TP)) ~ "missing_habitat",
#     
#     (is.na(Cond) | is.na(TN) | is.na(TP)) & 
#       !(is.na(IPI) | is.na(Ev_FlowHab) | is.na(H_AqHab) | is.na(H_SubNat) | 
#           is.na(PCT_SAFN) | is.na(XCMG) | is.na(indexscore_cram) | is.na(ps) | 
#           is.na(hy) | is.na(blc) | is.na(bs)) ~ "missing_chemistry",
#     
#     (is.na(Cond) | is.na(TN) | is.na(TP)) & 
#       (is.na(IPI) | is.na(Ev_FlowHab) | is.na(H_AqHab) | is.na(H_SubNat) | 
#          is.na(PCT_SAFN) | is.na(XCMG) | is.na(indexscore_cram) | is.na(ps) | 
#          is.na(hy) | is.na(blc) | is.na(bs)) ~ "missing_habitat_chemistry",
#     
#     TRUE ~ "complete"
#   ))
# 
# 
# complete_data <- sqidat_fordash %>%
#   filter(data_status == "complete")
# 
# missing_habitat_data <- sqidat_fordash %>%
#   filter(data_status == "missing_habitat")
# 
# missing_chemistry_data <- sqidat_fordash %>%
#   filter(data_status == "missing_chemistry")
# 
# missing_habitat_chemistry_data <- sqidat_fordash %>%
#   filter(data_status == "missing_habitat_chemistry")
# 
# # Step 2: Run the same processing pipeline on each subset (without `data_status`)
# process_data <- function(df, status) {
#   df %>%
#     select(-data_status) %>%  # Drop the `data_status` column
#     # Continue with your pipeline
#     st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
#     st_intersection(cntys) %>%
#     st_intersection(sheds) %>%
#     st_intersection(rwqbs) %>%
#     arrange(desc(csci_sampledate)) %>%
#     distinct(MasterID, yr, .keep_all = TRUE) %>%
#     sqi() %>% 
#     mutate(
#       data_status = status
#       
#     )
# }
# 
# # Step 3: Apply the process to each subset
# processed_complete_data <- process_data(complete_data, "complete")
# processed_missing_habitat_data <- process_data(missing_habitat_data, "missing_habitat")
# processed_missing_chemistry_data <- process_data(missing_chemistry_data, "missing_chemistry")
# processed_missing_habitat_chemistry_data <- process_data(missing_habitat_chemistry_data, "missing_habitat_chemistry")
# 
# 
# 
# library(writexl)
# 
# data_to_write <- list(
#   "Complete Data" = processed_complete_data,
#   "Missing Habitat Data" = processed_missing_habitat_data,
#   "Missing Chemistry Data" = processed_missing_chemistry_data,
#   "Missing Habitat & Chemistry Data" = processed_missing_habitat_chemistry_data
# )
# 
# # Write all sheets to an Excel file
# write_xlsx(data_to_write, "processed_data.xlsx")
# 
# 
# # Define the output file path
# output_file <- "processed_data.xlsx"
# 
# # Write the dataframes to different sheets in the Excel file
# write.xlsx(processed_complete_data, file = output_file, sheetName = "Complete Data")
# write.xlsx(processed_missing_habitat_data, file = output_file, sheetName = "Missing Habitat Data", append = TRUE, row.names = FALSE)
# write.xlsx(processed_missing_chemistry_data, file = output_file, sheetName = "Missing Chemistry Data", append = TRUE, row.names = FALSE)
# write.xlsx(processed_missing_habitat_chemistry_data, file = output_file, sheetName = "Missing Habitat & Chemistry Data", append = TRUE, row.names = FALSE)
# 
# 
# # Step 4: Combine the processed data back together
# sqidat_fordash <- bind_rows(
#   processed_complete_data,
#   processed_missing_habitat_data,
#   processed_missing_chemistry_data,
#   processed_missing_habitat_chemistry_data
# )
# 
# # Step 5: Export the data
# save(sqidat_fordash, file = 'data/sqidat_fordash.RData', compress = 'xz')
# 
# 
