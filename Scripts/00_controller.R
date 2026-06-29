# Load in required packages
library(rmarkdown)
library(tidyverse)
library(broom)

#Determine which scripts should be run
process_data = F #Runs data analysis 
make_report = F #Runs project summary
knit_manuscript = F #Compiles manuscript draft

############################
### Read in the RAW data ###
############################

if(process_data == T){
  source(file = "Scripts/01_data_processing.R")
}

##################################
### Read in the PROCESSED data ###
##################################

logger_16c = read.csv("Raw_data/temp_loggers/2026_06_12_16c.csv") %>% 
  janitor::clean_names() %>% 
  select("datetime" = date_time_edt, "temp_c" = temperature_c) %>% 
  mutate(datetime = mdy_hms(datetime), 
         time_point = row_number(), 
         start_temp = "16", 
         ten_min_int = ceiling(time_point / 20))

logger_22c = read.csv("Raw_data/temp_loggers/2026_06_12_22c.csv") %>% 
  janitor::clean_names() %>% 
  select("datetime" = date_time_edt, "temp_c" = temperature_c) %>% 
  mutate(datetime = mdy_hms(datetime), 
         time_point = row_number(), 
         start_temp = "22", 
         ten_min_int = ceiling(time_point / 20))

comb_data = bind_rows(logger_16c, logger_22c)


trait_data = readr::read_csv(list.files(path = "Raw_data/ctmax_data/", 
                                        pattern = "*.csv", 
                                        full.names = TRUE),
                             show_col_types = FALSE)

if(make_report == T){
  render(input = "Output/Reports/report.Rmd", #Input the path to your .Rmd file here
         #output_file = "report", #Name your file here if you want it to have a different name; leave off the .html, .md, etc. - it will add the correct one automatically
         output_format = "all")
}

##################################
### Read in the PROCESSED data ###
##################################

if(knit_manuscript == T){
  render(input = "Manuscript/manuscript_name.Rmd", #Input the path to your .Rmd file here
         output_file = paste("dev_draft_", Sys.Date(), sep = ""), #Name your file here; as it is, this line will create reports named with the date
                                                                  #NOTE: Any file with the dev_ prefix in the Drafts directory will be ignored. Remove "dev_" if you want to include draft files in the GitHub repo
         output_dir = "Output/Drafts/", #Set the path to the desired output directory here
         output_format = "all",
         clean = T)
}
