library(tidyverse)
if("devtools" %in% rownames(installed.packages()) == F){install.packages("devtools")}
library(devtools)
if("gcamdata" %in% rownames(installed.packages()) == F){install_github(repo="JGCRI/gcamdata")}
library(dplyr)
library(tidyr)
library(foreach)
library(gcamdata)

# This script creates a single .xml file to improve commercial and residential building shell efficiency in Colombia. 

# The way shell efficiencies are handled in GCAM is as follows. The values for USA are set in
# inst/extdata/energy/A44.USA_TechChange.csv, and the multipliers to go from those assumptions to other regions are in:
# inst/extdata/energy/A44.shell_eff_mult_RG3.csv. Note that it was implemented using GCAM 3 regions 
# (e.g., Latin America). These files need to be downloaded from the GCAM LAC branch, not the base values from gcamdata.

# These files above then feed into L244.ShellConductance_bld.csv. It is this file that ultimately feeds into
# building_det.xml--the only relevant XML in this case. So the code below is going to read in L244.ShellConductance_bld,
# then change the values consistent with Colombia's policy, then use gcamdata to just create the
# xml so it resembles building_det.xml. This requires a unique header, which is in colombia_policy/headers_rdm.txt.

base_directory <- c('C:/Users/twild/all_git_repositories/IDB_RDM_Colombia/colombia_policy/BuildingShellEfficiency/')

# Read in base data (taken from Gcam data system after building). We will modify the efficiencies in the base data to
# create a new set of assumptions and associated file.
L244.ShellConductance_bld <- read.csv(paste0(base_directory,'L244.ShellConductance_bld.csv'), skip=1)

L244.ShellConductance_bld.Colombia <- L244.ShellConductance_bld %>% 
  filter(region=='Colombia') %>% 
  select(-floor.to.surface.ratio, -shell.year)

# Get 2015 values since policy defined in terms of percentage decline from 2015
shell.cond.comm.2015.Col <- (L244.ShellConductance_bld.Colombia %>% filter(year==2015, nodeInput=='comm'))$shell.conductance
shell.cond.resid.2015.Col <- (L244.ShellConductance_bld.Colombia %>% filter(year==2015, nodeInput=='resid'))$shell.conductance
improvement.rate.2030 <- 0.22/(2030-2015)
improvement.rate.2050 <- (0.26-0.22)/(2050-2030)
# Deal first with portion through 2030
L244.ShellConductance_bld.Colombia <- L244.ShellConductance_bld.Colombia %>% 
  mutate(shell.conductance.new = if_else(year<=2015, shell.conductance,
                                         if_else(year<=2030, if_else(nodeInput=='comm', (1-(year-2015)*improvement.rate.2030)*shell.cond.comm.2015.Col, 
                                                                    (1-(year-2015)*improvement.rate.2030)*shell.cond.resid.2015.Col
                                                                    ),
                                                          shell.conductance
                                                )
                                         )
         )
# Grab 2030 values for comm and resid, then project through 2050 using that.
shell.cond.comm.2030.Col <- (L244.ShellConductance_bld.Colombia %>% filter(year==2030, nodeInput=='comm'))$shell.conductance.new
shell.cond.resid.2030.Col <- (L244.ShellConductance_bld.Colombia %>% filter(year==2030, nodeInput=='resid'))$shell.conductance.new
L244.ShellConductance_bld.Colombia <- L244.ShellConductance_bld.Colombia %>% 
  mutate(shell.conductance.new = if_else(year<=2030, shell.conductance.new,
                                         if_else(nodeInput=='comm', (1-(year-2030)*improvement.rate.2050)*shell.cond.comm.2030.Col, 
                                                                     (1-(year-2030)*improvement.rate.2050)*shell.cond.resid.2030.Col
                                                )
                                        )
        )
L244.ShellConductance_bld.Colombia <- L244.ShellConductance_bld.Colombia %>% 
  select(-shell.conductance) %>% 
  rename(shell.conductance=shell.conductance.new)
# Auto-produce XML from CSV
gcamdata_variable <- "Bldg_ShellEff" #  "AgProdChange"
imported_data <- tibble::as.tibble(L244.ShellConductance_bld.Colombia)
xmlpath <- paste0(base_directory, 'Bldg_ShellEff.xml')
mi_header <- 'C:/Users/twild/all_git_repositories/IDB_RDM_Colombia/colombia_policy/headers_rdm.txt'
gcamdata::create_xml(xmlpath, mi_header = mi_header) %>%
  gcamdata::add_xml_data(imported_data, gcamdata_variable, NULL) %>%
  gcamdata::run_xml_conversion()
