library(tidyverse)
if("devtools" %in% rownames(installed.packages()) == F){install.packages("devtools")}
library(devtools)
if("gcamdata" %in% rownames(installed.packages()) == F){install_github(repo="JGCRI/gcamdata")}
library(dplyr)
library(tidyr)
library(foreach)
library(gcamdata)

# This script creates a single .xml file to eliminate nuclear deployment in all future time periods in Colombia. The
# resulting xml is designed to resemble the structure that appears in electricity_water.xml. Producing the
# xml requires a standard electricity subsector header, which appears in colombia_policy/headers_rdm.txt.

base_directory <- c('C:/Users/twild/all_git_repositories/IDB_RDM_Colombia/uncertainty/uncertain_factor_files/')
# Delete First Segment (2010-2015) of Existing Nuclear Interpolation Rule. Must Delete it to overwrite.
Nuclear_SW_Interp1 <- data.frame('region' = 'Colombia', 'supplysector' = 'electricity', 'subsector' = 'nuclear',
                                 'apply-to' = 'share-weight', 'delete' = 1, 'from-year' = 2010, 'to-year' = 2015,
                                 'to-value' = 0.5, 'interpolation-function' = 'linear')
# Delete First Segment (2010-2015) of Existing Nuclear Interpolation Rule. Must Delete it to overwrite.
Nuclear_SW_Interp2 <- data.frame('region' = 'Colombia', 'supplysector' = 'electricity', 'subsector' = 'nuclear',
                                 'apply-to' = 'share-weight', 'delete' = 1, 'from-year' = 2020, 'to-year' = 2035,
                                 'to-value' = 0.5, 'interpolation-function' = 'linear')
# Delete First Segment (2010-2015) of Existing Nuclear Interpolation Rule. Must Delete it to overwrite.
Nuclear_SW_Interp3 <- data.frame('region' = 'Colombia', 'supplysector' = 'electricity', 'subsector' = 'nuclear',
                                 'apply-to' = 'share-weight', 'delete' = 1, 'from-year' = 2035, 'to-year' = 2050,
                                 'to-value' = 0.5, 'interpolation-function' = 'linear')
# Delete First Segment (2010-2015) of Existing Nuclear Interpolation Rule. Must Delete it to overwrite.
Nuclear_SW_Interp4 <- data.frame('region' = 'Colombia', 'supplysector' = 'electricity', 'subsector' = 'nuclear',
                                 'apply-to' = 'share-weight', 'delete' = 1, 'from-year' = 2050, 'to-year' = 2300,
                                 'to-value' = 0.5, 'interpolation-function' = 's-curve')

# Auto-produce XML from CSV
gcamdata_variable <- "TradBiomass_SW" #  "AgProdChange"
imported_data <- tibble::as.tibble(TradBiomass_SW)
xmlpath <- paste0(base_directory, 'TradBiomass_SW.xml')
mi_header <- 'C:/Users/twild/all_git_repositories/IDB_RDM_Colombia/colombia_policy/headers_rdm.txt'
gcamdata::create_xml(xmlpath, mi_header = mi_header) %>%
  gcamdata::add_xml_data(imported_data, gcamdata_variable, NULL) %>%
  gcamdata::run_xml_conversion()
