# Script pulling the SQI dataset from the SMC and reformatting it to align with Shiny dashboard script
# This is what makes the dataset "Dynamic"

# by Annie Holt 4/13/2022
# Generally have to prep/rename columns and clean dataset a bit 
# Then run through SQI function to calculate

# Data Assembly notes (this was all done in a database query):
# Max Bio sample (ASCI, CSCI) was taken per masterid/sampledate
# Average Chemistry sample (TN, TP, Cond) was taken per masterid/sampledate
# Chemistry Data (TN, TP, Cond) was joined to Bio Data (ASCI, CSCI) by masterid and sampledate
# Habitat Data (PHAB, CRAM) was joined to Bio/Chem Data by masterid and year

# If there were multiple samples in one year, just keep most recent row of data; we just want one row per masterid
# Also only kept 'complete' data rows (have all Chem, Bio, Hab metrics per site/year)

# required libraries
library(tidyverse)
library(sf)
library(SQI)

# Import new/dynamic data
# this dataset is created as a database View in SCCWRP's SMC database (vw_sqi_dat)
# to change the dataset/how it is assembled, need to edit the View ... though some changes can be made below as well if don't need to restructure
# should be in-house accessible only
sqi_raw <- read_csv("https://nexus.sccwrp.org/smcchecker/sqi_rawdata")

# # load underlying shapefiles
# # do this in app instead
# data(sheds)
# data(cntys)
# data(rwqbs)
# data(cnstr)
# # old data for reference
# # data("sqidat")

sqidat_fordash <- sqi_raw %>% 
  # column renaming to match with names in index.Rmd
  rename(MasterID = masterid, COMID = comid, 
         
         yr = year, ASCI = d_asci_max, CSCI = csci_max, IPI = ipi,
         # stream class, scape categories
         strcls = ref10, lower = qt10, meds = qt50, upper = qt90,
         # phab metrics
         Ev_FlowHab = ev_flowhab, H_AqHab = h_aqhab,H_SubNat = h_subnat, PCT_SAFN = pct_safn, XCMG = xcmg,
         # cram score and metrics
         indexscore_cram = cram_score,
         ps = cram_physicalstructure, hy = cram_hydrology, blc = cram_bufferandladscapecontext, bs = cram_bioticstructure,
         # biostim analytes
         Cond = cond, TN = total_n_all, TP = total_p_all) %>% 
  # assign levels to stream class
  mutate(strcls  = factor(strcls, levels = c("likely unconstrained", "possibly unconstrained", "possibly constrained",
                                             "likely constrained"))) %>% 
  
  # final columns needed for Shiny
  select(MasterID, COMID, latitude, longitude, yr, csci_sampledate, ASCI, CSCI, IPI, Ev_FlowHab, H_AqHab, H_SubNat, PCT_SAFN, XCMG,
         indexscore_cram, ps, hy, blc, bs, Cond, TN, TP, strcls, lower, meds, upper) %>% 
  
  # only keep data that has complete data across below metrics (No NAs) 
  drop_na(ASCI, CSCI, IPI, Ev_FlowHab, H_AqHab, H_SubNat, PCT_SAFN, XCMG, indexscore_cram, ps, hy, blc, bs, Cond, TN, TP) %>% 
  
  # create into shapefile
  st_as_sf(coords = c("longitude", "latitude"), 
            crs = 4326) %>%   # geographic wgs84
  # spatial join with other shapefiles for added info like county, watershed name, regional board
  st_intersection(cntys) %>% 
  st_intersection(sheds) %>% 
  st_intersection(rwqbs) %>% 
  
  # have to define unique sample, don't want multiple rows per masterid/year
  arrange(desc(csci_sampledate)) %>% 
  distinct(MasterID, yr, .keep_all = TRUE) %>% 
  
  # now run through SQI function to calculate various SQI metrics

  sqi()
  
# export
# save(sqidat_fordash, file = 'data/sqidat_fordash.RData', compress = 'xz')


# exploring differences between new dataset and previous version (data/sqidat.RData)

# 
# dat_explore <- sqidat_fordash %>%
#   select(MasterID, yr, csci_sampledate, CSCI) %>%
#   rename(csci_sampledate_new = csci_sampledate, CSCI_new = CSCI) %>%
#   full_join(sqidat %>% as.data.frame() %>% select(MasterID, yr, CSCI)) 
# 
# write_csv(dat_explore, "C:/Downloads/sqi_explore.csv")

