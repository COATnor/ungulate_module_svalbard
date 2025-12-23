##----------------------------------------------------------------------------##
####   COAT SV UNGULATE CARCASSES SUMMER ADVENTDALEN DATA PORTAL DOWNLOAD   ####
##----------------------------------------------------------------------------##

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
####                         Description                                    ####
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

## This script downloads available datasets of observed reindeer carcasses    ##
## from the COAT Data Portal for Adventdalen, its side valleys, and the       ## 
## adjacent mountain plateau from. It then generates a summary table showing  ##
## the number of carcasses in each age and sex category from 2012 onwards.    ##
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
  message("loading: ", file_url)
  
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
pot.localities <- c("adventdalen")

#vector for potential sex categories
pot.sex <- c("male","female",NA)

#vector for potential age categories
pot.age <- c("adult","yearling","calf",NA)

# vector for potential age_sex categories
pot.categories <- c("male_adult","female_adult","unknown_adult","male_yearling",
                    "female_yearling","unknown_yearling","male_calf",
                    "female_calf","unknown_calf","male_unknown",
                    "female_unknown","unknown")

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
####        Merge, check, and generate a summary table for the              ####
####                    years 2012 to present                               ####
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

# make a vector with the names of relevant datasets
data_in <- ls(pattern = "^s_ungulates_carcasses_")

# make a list with the names of relevant datasets
data_list <- lapply(data_in, get)

# create a merged dataset for the years 2012 to present
merged_data <- bind_rows(data_list)

# check the spelling of localities
if(all(merged_data$sn_locality %in% pot.localities)){
  print("all localities are correct. continue")
} else {
  print("some localities are spelled wrong. please check: ")
  merged_data[!(merged_data %in% pot.localities),]
}

# check the spelling of animal sex category
if(all(merged_data$v_sex %in% pot.sex)){
  print("all sex categories are correct. continue")
} else {
  print("some sex categories are spelled wrong. please check: ")
  merged_data[!(merged_data %in% pot.categories),]
}

# check the spelling of animal age category
if(all(merged_data$v_age_class %in% pot.age)){
  print("all age classes are correct. continue")
} else {
  print("some age classes are spelled wrong. please check: ")
  merged_data[!(merged_data %in% pot.age),]
}

# check that all rows have a year
if(all(!is.na(merged_data$t_year))) {
  print("all rows have a year. continue")
} else {
  print("some rows are missing a year. please check: ")
  merged_data[is.na(merged_data$t_year), ]
}  

# generate a summary table for the years 2012 to present
summary_new <- merged_data %>%
  mutate(v_category =case_when(
    v_sex == "male" & v_age_class == "adult"  ~ "male_adult",
    v_sex == "female" & v_age_class == "adult" ~ "female_adult",
    is.na(v_sex) & v_age_class == "adult" ~ "unknown_adult",
    v_sex == "male" & v_age_class == "yearling" ~ "male_yearling",
    v_sex == "female" & v_age_class == "yearling" ~ "female_yearling",
    is.na(v_sex) & v_age_class == "yearling" ~ "unknown_yearling",
    v_sex == "male" & v_age_class == "calf" ~ "male_calf",
    v_sex == "female" & v_age_class == "calf" ~ "female_calf",
    is.na(v_sex) & v_age_class == "calf" ~ "unknown_calf",
    v_sex == 'male' & is.na(v_age_class) ~ "male_unknown",
    v_sex == 'female' & is.na(v_age_class) ~ "female_unknown",
    is.na(v_sex) & is.na(v_age_class) ~ "unknown")) %>%
  group_by(t_year,sn_locality, v_category) %>%
  summarise(antall = n(), .groups = "drop") %>%
  pivot_wider(names_from = v_category, values_from=antall, values_fill = 0) %>%
  mutate(total_carcasses = rowSums(select(., intersect(pot.categories, colnames(.)))))


# check that all columns are included, and insert 0 if missing
for (miss.categories in pot.categories){
  if(!(miss.categories %in% colnames(summary_new))){
    summary_new[[miss.categories]] <-0
  }
}

# check that all carcasses is included in the summary table
for (i in unique(merged_data$t_year)){
  expected <- sum(merged_data$t_year==i)
  actual <- summary_new$total_carcasses[summary_new$t_year == i]
  if(expected != actual){
    print(paste("error found in year", i, ". expected: ", expected,". found: ", actual, ". please check data."))
  } else {
    print(paste("summary table is correct for year", i))
  }
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
####            Make a final summary table for reindeer carcasses in        ####
####         Adventdalen with its adjacent valles and mountain plateaus     ####
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

# reorder columns and make a final table
final.table <- summary_new %>%
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
         male_unknown,
         female_unknown,
         unknown,
         total_carcasses)

# check first rows of table
head(final.table, 6)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
####                             DONE                                       ####
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
