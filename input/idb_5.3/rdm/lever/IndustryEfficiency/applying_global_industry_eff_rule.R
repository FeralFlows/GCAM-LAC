library(tidyverse)
if("devtools" %in% rownames(installed.packages()) == F){install.packages("devtools")}
library(devtools)
if("gcamdata" %in% rownames(installed.packages()) == F){install_github(repo="JGCRI/gcamdata")}
library(dplyr)
library(tidyr)
library(foreach)
library(gcamdata)


# This script creates a single .csv file to increase industrial efficiency. It applies a simple function that improves
# industrial efficiency over time across sectors/subsectors/stub techs. It needs to be run through
# the GCAM model interface to create a XML file. Need to figure out if these xml files can actually be kept separate,
# in which case we can automate the XML creation process

base_directory <- c('C:/Users/twild/all_git_repositories/IDB_RDM_Colombia/colombia_policy/IndustryEfficiency/')

# Read in base data (taken from Gcam data system after building). We will modify the efficiencies in the base data to
# create a new set of assumptions and associated file.
data <- read.csv(paste0(base_directory,'IndustrialStubTechEff_Base.csv'), skip=4)

# Rate of increase per year of efficiency improvement. Begins at 30% in 2030 and continues to 2030. 
# Will backcast to 2020 from 2030, and project from 2030 to 2050.
eff_rate_policy <- (0.5-0.3)/(2050-2030)
official_policy_begin_yr <- 2030
policy_application_begin_yr <- 2020

desired_output_column_order <- names(data)
#Modify the efficiency using rate of change rule
modified_efficiency <- data %>% 
  filter(period>=policy_application_begin_yr) %>% 
  mutate(efficiency_improvement = 0.3 + (period-official_policy_begin_yr)*eff_rate_policy) %>% 
  mutate(new_efficiency = efficiency*(1 + efficiency_improvement)) %>% 
  mutate(new_efficiency = if_else(new_efficiency>1, 1, new_efficiency))
#merge new efficiencies (which only apply to future years) with historical efficiencies
output <- data %>% 
  left_join(modified_efficiency %>% select(-efficiency_improvement, -efficiency), 
            by=c('region', 'supplysector', 'subsector', 'stub.technology', 'period', 'minicam.energy.input', 
                 'market.name')) %>%
  mutate(final_efficiency = if_else(is.na(new_efficiency)==TRUE, efficiency, new_efficiency)) %>% 
  select(-efficiency, -new_efficiency) %>% 
  rename(efficiency=final_efficiency)
output <- output[desired_output_column_order]  # Reorder coluns to order required by header

# Write top column labels required as proper formatting to create XML
output_file <- paste0(base_directory, 'IndustrialStubTechEff.csv')
if(file.exists(output_file)){
  file.remove(output_file)
}
write.table('INPUT_TABLE,,,,,,,', 
            output_file,
            append=TRUE,
            sep=',',
            row.names=FALSE,
            col.names=FALSE,
            quote=FALSE)
write.table('Variable ID,,,,,,,', 
            output_file, 
            append=TRUE, 
            sep = ',',
            row.names=FALSE,
            col.names=FALSE,
            quote=FALSE)
write.table('StubTechEff,,,,,,,', 
            output_file, 
            append=TRUE, 
            sep = ',',
            row.names=FALSE,
            quote=FALSE,
            col.names=FALSE)
write.table(',,,,,,,', 
            output_file, 
            append=TRUE, 
            sep = ',',
            row.names=FALSE,
            col.names=FALSE,
            quote=FALSE)
# Export data
write.table(output, output_file, 
            row.names=FALSE,
            sep = ',',
            append=TRUE,
            quote=FALSE)

# Auto-produce XML from CSV
gcamdata_variable <- "StubTechEff" #  "AgProdChange"
imported_data <- tibble::as.tibble(read.csv(output_file, skip = 4, stringsAsFactors = F)) %>% 
  rename(year=period)
xmlpath <- paste0(base_directory, 'IndustrialStubTechEff.xml')
mi_header <- 'C:/Users/twild/Downloads/gcam-v5.1.3-Windows-Release-Package/input/gcamdata/inst/extdata/mi_headers/ModelInterface_headers.txt'

gcamdata::create_xml(xmlpath, mi_header = mi_header) %>%
  gcamdata::add_xml_data(imported_data, gcamdata_variable) %>%
  gcamdata::run_xml_conversion()
