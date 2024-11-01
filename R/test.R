# library(tidyverse)
# library(sf)
# library(SQI)
# library(dplyr)
# 
# data(sheds)
# data(cntys)
# data(rwqbs)
# data(cnstr)
# 
# pool <- pool::dbPool(
#   drv = RPostgreSQL::PostgreSQL(),
#   dbname = Sys.getenv("dbname"),
#   host = Sys.getenv("host"),
#   user = Sys.getenv("user"),
#   password = Sys.getenv("password")
# )
# 
# sqi_dat_raw <- pool::dbGetQuery(pool, "SELECT * FROM vw_sqi_dat_missing_filled;")
# 
# sqi_dat <- sqi_dat_raw %>%
#   rename(
#     MasterID = masterid,
#     COMID = comid,
#     yr = year,
#     ASCI = d_asci_max,
#     CSCI = csci_max,
#     IPI = ipi,
#     # stream class, scape categories
#     strcls = ref10,
#     lower = qt10,
#     meds = qt50,
#     upper = qt90,
#     # phab metrics
#     Ev_FlowHab = ev_flowhab_score,
#     H_AqHab = h_aqhab_score,
#     H_SubNat = h_subnat_score,
#     PCT_SAFN = pct_safn_score,
#     XCMG = xcmg_score,
#     # cram score and metrics
#     indexscore_cram = cram_score,
#     ps = cram_physicalstructure,
#     hy = cram_hydrology,
#     blc = cram_bufferandladscapecontext,
#     bs = cram_bioticstructure,
#     # biostim analytes
#     Cond = cond,
#     TN = total_n_all,
#     TP = total_p_all
#   ) %>%
#   mutate(
#     Ev_FlowHab_raw = ev_flowhab,
#     H_AqHab_raw = h_aqhab,
#     H_SubNat_raw = h_subnat,
#     PCT_SAFN_raw = pct_safn,
#     XCMG_raw = xcmg
#   ) %>%
#   # assign levels to stream class
#   mutate(
#     strcls = factor(
#       strcls,
#       levels = c("likely unconstrained", "possibly unconstrained", "possibly constrained", "likely constrained"))) %>%
#   filter(!is.na(ASCI) & !is.na(CSCI)) %>%
#   sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)  %>%
#   
#   sf::st_intersection(cntys) %>%
#   sf::st_intersection(sheds) %>%
#   sf::st_intersection(rwqbs) %>%
#   arrange(desc(csci_sampledate)) %>%
#   distinct(MasterID, yr, .keep_all = TRUE)
# 
# #Run it through SQI function
# sqi_dat_final <- sqi_dat %>% SQI::sqi()
# 
# 
# 
# 
# 
