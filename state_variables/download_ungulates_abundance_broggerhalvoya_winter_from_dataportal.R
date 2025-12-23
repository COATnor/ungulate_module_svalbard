##----------------------------------------------------------------------------##
####      COAT SV UNGULATE BROGGERHALVOYA WINTER DATA PORTAL DOWNLOAD       ####
##----------------------------------------------------------------------------##

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
####                         Description                                    ####
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

## This script downloads available datasets from the annual winter census of  ##
## the reindeer population at Brøggerhalvøya in coastal areas of western      ##
## Spitsbergen from the COAT Data Portal and generates a summary table        ##
## showing the number of individuals in each age and sex category.            ##            ##
##                                                                            ##
## Since the data from 2000–2013 is compiled into a single file and formatted ##
## somewhat differently than the annual files from 2014 onwards, the script   ##
## first creates a summary table for the years 2014 and later, followed by a  ##
## separate table for 2000–2013. These two tables are then merged into one    ##
## final summary table.                                                       ##
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
## Last updated: 23.12.2025                                                   ##
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
pot.localities <- c("broggerhalvoya")

# vector for potenial animal categories
pot.categories <- c("male_adult","female_adult","unknown_adult","male_yearling",
                    "female_yearling","unknown_yearling","male_calf",
                    "female_calf","unknown_calf","unknown")

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
####        Merge, check, and generate a summary table for the              ####
####                    years 2014 to present                               ####
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

# make a vector with the names of relevant datasets
data_in <- ls(pattern = "^s_ungulates_abundance_")

# make a list with the names of relevant datasets
data_list <- lapply(data_in, get)

# create a merged dataset for the years 2014 to present
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

# check that all rows have a date
if(all(!is.na(merged_data$t_date))) {
  print("all rows have a date. continue")
} else {
  print("some rows are missing a date. please check: ")
  merged_data[is.na(merged_data$t_date), ]
}  

# generate a summary table for the years 2014 to present
summary_new <- merged_data %>%
  mutate(t_year = year(ymd(t_date))) %>% #Add year to the dataset 
  group_by(t_year, sn_locality, v_animal_category) %>%
  summarise(total = sum(v_abundance, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = v_animal_category, values_from = total, values_fill = 0) %>%
  mutate(total_animals = rowSums(select(., intersect(pot.categories, colnames(.)))))

# check first rows of table
head(summary_new, 6)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
####    Check and generate a summary table for the years 2000 to 2013       ####
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

# rename dataset and filter out unwanted rows
data_old <- s_ungulates_summary_broggerhalvoya_winter_2000_2013 %>%
  filter(v_animal_category != "total")
  
# check the spelling of locality names
if(all(data_old$sn_locality %in% pot.localities)){
  print("all localities are correct. continue")
} else {
  print("some localities are spelled wrong. please check: ")
  data_old[!(data_old %in% pot.localities),]
}

# check the spelling of animal category names
if(all(data_old$v_animal_category %in% pot.categories)){
  print("all categories are correct. continue")
} else {
  print("some categories are spelled wrong. please check: ")
  data_old[!(data_old %in% pot.categories),]
}

# check that all rows have a year
if(all(!is.na(data_old$t_year))) {
  print("all rows have a year. continue")
} else {
  print("some rows are missing a year please check: ")
  data_old[is.na(data_old$t_year), ]
}  

# generate a summary table for the years 2000-2013
summary_old <- data_old %>%
  group_by(t_year, sn_locality, v_animal_category) %>%
  summarise(total = sum(v_abundance, na.rm=TRUE), .groups = "drop") %>%
  pivot_wider(names_from = v_animal_category, values_from = total, values_fill = NA) %>%
  mutate(total_animals = rowSums(select(., intersect(pot.categories, colnames(.)))))


# add columns with animal categories missing in the 2000–2013 dataset
summary_old <- summary_old %>%
  mutate(unknown_adult = 0,
         male_yearling = 0,
         female_yearling = 0,
         unknown_yearling = 0,
         male_calf = 0,
         female_calf = 0
         )

# reorder columns to match the structure of the 2014–present summary table
summary_old <- summary_old %>%
  select(t_year,
         sn_locality, 
         male_adult,
         female_adult,
         unknown_adult,
         male_yearling,
         female_yearling,
         unknown_yearling,
         male_calf,
         female_calf,
         unknown_calf,
         unknown,
         total_animals)

# check first rows of table
head(summary_old, 6)


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
####            Make a final summary table for reindeer abundance in        ####
####                  the coastal areas for all year                        ####
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

final.table <- rbind(summary_old, summary_new)

# check first rows of table
head(final.table, 6)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
####                             DONE                                       ####
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
