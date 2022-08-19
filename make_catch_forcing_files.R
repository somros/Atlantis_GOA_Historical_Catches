# Alberto Rovellini 8/19/2022
# Run this script to run all the scripts that make the catch forcing files
# "Forcing_1_AKFIN.R", "Forcing_2_DFO.R", "Forcing_3_IPHC.R", "Forcing_4_salmon.R", "Forcing_5_nontarget.R", "Forcing_6_crabs.R"

catch_scripts <- list("Forcing_1_AKFIN.R", "Forcing_2_DFO.R", "Forcing_3_IPHC.R", "Forcing_4_salmon.R", "Forcing_5_nontarget.R", "Forcing_6_crabs.R")
sapply(catch_scripts,source) # this will take a few minutes
 