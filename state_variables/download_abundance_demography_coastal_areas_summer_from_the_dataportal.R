##----------------------------------------------------------------------------##
####          COAT SV UNGULATE COASTAL AREAS DATA PORTAL DOWNLOAD           ####
##----------------------------------------------------------------------------##

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
####                         Description                                    ####
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

## This script downloads available datasets from the annual census of the     ##
## reindeer population in coastal areas of western Spitsbergen                ##
## (Brøggerhalvøya, Sarsøyra, and Kaffiøyra) from the COAT Data Portal        ##
## and generates a summary table showing the number of individuals in each    ##
## age and sex category.                                                      ##
##                                                                            ##
## To ensure the script works properly, you need to manually insert the URL   ##
## from the COAT Data Portal where the datasets are located.                  ##
## This should be done in the section titled:                                 ## 
## "Link to the latest available data from the COAT data portal" below.       ##
##                                                                            ##
## The script does not save the final summary table. If you want to save it   ##
## to your personal workspace, you need add the appropriate code at the end.  ##
##                                                                            ##
## Script created by Vegard Bang Fjeldheim for COAT Svalbard.                 ##
## Last updated: 12.03.2026                                                   ##
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
####                Required library and prepare workspace                  ####
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

if (!requireNamespace("tidyverse", quietly = TRUE)) {install.packages("tidyverse")};library("tidyverse")
if (!requireNamespace("rvest", quietly = TRUE)) {install.packages("rvest")};library("rvest")
if (!requireNamespace("tools", quietly = TRUE)) {install.packages("tools")};library("tools")
if (!requireNamespace("lubridate", quietly = TRUE)) {install.packages("lubridate")};library("lubridate")

#clean workspace
rm(list=ls())

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
####      Link to the latest available data from the COAT data portal       ####
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

# manually insert the URL to the latest available dataset version from the 
# COAT data portal. Make sure you select the newest version that is not 
# under embargo.

url_data <- "INSERT URL TO LATEST VERSION HERE"

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
####               Load datasets from the COAT Data Portal                  ####
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

# read the HTML page
page <- read_html(url_data)

# find all links to .txt files
txt_links <- page %>%
  html_nodes("a") %>%
  html_attr("href") %>%
  grep("\\.txt$", ., value = TRUE)

# construct complete URLs for each .txt file
full_urls <- ifelse(grepl("^https?://", txt_links),
                    txt_links,
                    paste0("https://data.coat.no", txt_links))

# read and store each dataset as a separate object
for (file_url in full_urls) {
  file_name <- file_path_sans_ext(basename(file_url))  # use filename as objectname
  message("load: ", file_url)
  
  tryCatch({
    assign(
      file_name,
      read.table(file_url, header = TRUE, sep = ";", stringsAsFactors = FALSE),
      envir = .GlobalEnv
    )
  }, error = function(e) {
    message("error while loading dataset: ", file_url)
  })
}

# check which datasets have been loaded
dataset_names <- ls(pattern = "^s_ungulates_")
cat("The following datasets have been successfully loaded:\n", paste(dataset_names, collapse = "\n"))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
####           Vectors used for data validation and table formatting        ####
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

# vector for potential locations
pot.localities <- c("broggerhalvoya","sarsoyra","kaffioyra")

# vector for potenial animal categories
pot.categories <- c("male_adult","female_adult","unknown_adult","male_yearling",
                    "female_yearling","unknown_yearling","unknown_calf","unknown")

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
####           Merge, check, and generate a summary table                   ####
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

# make a vector with the names of relevant datasets
data_in <- ls(pattern = "^s_ungulates_abundance_")

# make a list with the names of relevant datasets
data_list <- lapply(data_in, get)

# create a merged dataset for all years
merged_data <- bind_rows(data_list)

# check the spelling of locality names
if(all(merged_data$sn_locality %in% pot.localities)){
  print("all localities are correct. continue")
} else {
  print("some localities are spelled wrong. please check: ")
  merged_data[!(merged_data %in% pot.localities),]
}

# check the spelling of animal category names
if(all(merged_data$v_animal_category %in% pot.categories)){
  print("all categories are correct. continue")
} else {
  print("some categories are spelled wrong. please check: ")
  merged_data[!(merged_data %in% pot.categories),]
}

#create a year column for data from 2012 and onwards
merged_data <- merged_data %>%
  mutate(
    t_date = as.Date(t_date),                     
    t_year = ifelse(!is.na(t_date),
                    as.integer(format(t_date, "%Y")),
                    t_year))

# check that all rows have a year
if(all(!is.na(merged_data$t_year))) {
  print("all rows have a year. continue")
} else {
  print("some rows are missing year. please check: ")
  merged_data[is.na(merged_data$t_year), ]
}  

# generate a summary table
final.table <- merged_data %>%
  group_by(t_year, sn_locality, v_animal_category) %>%
  summarise(total = sum(v_abundance, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = v_animal_category, values_from = total, values_fill = 0) %>%
  mutate(total_animals = rowSums(select(., intersect(pot.categories, colnames(.))))) %>%
  select(t_year, sn_locality, all_of(pot.categories))

# check first rows of table
head(final.table, 6)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
####                             DONE                                       ####
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
