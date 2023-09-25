####################################################################
##                           ASSIGNMENT                           ##
##                   Data Cleaning and Standards                  ##
##                          submitted for                         ##
## Productivity and Reproducibility for Ecology and Evolution     ##
####################################################################
## Author: Ming Qiu
## Date_updated: 2023-09-15
## Data_source: Bromeliad Working Group (BWG)


##---------------------------------------------------------------
##                       Script Overview                       --
##---------------------------------------------------------------

###  The overall goal of this script is to tidy the data regarding dates and coordinates in the BWG database. 
###  Specific tasks include:
###  (A) convert all dates to ISO standards (yyyy-mm-dd);
###  (B) join datasets for comparison of dates;
###  (C) assign a coordinate reference system to all coordinates, and then project geographic coordinates.

###  To achieve these tasks, this script provides solutions to the issues that may arise in the process of data cleaning.
###  The issues include:
###  (1) import incomplete datasets due to potential faulty uncompression
###  (2) the data type of the columns were not correctly specified in the original datasets, so filtering with class()
###  may not completely identify the date columns;
###  (3) the dates were originally recorded in different formats, hindering a universal parsing of dates;


##---------------------------------------------------------------
##                    Environment Set-up                       --
##---------------------------------------------------------------

### .
### └── LDP_MiniProject
###     ├── 00_rawdata
###             └── BWG_database
###     ├── 01_script
###             └── data_clean.R
###     ├── renv
###     ├── renv.lock
###     ├── LDP_MoniProject.Rproj
###     ├── README.md


### All the path arguments were specified with relative paths. If this script is correctly
### stored in Folder "02_script", no modifications on the paths are required.


### ----------- load or install the required packages ------------
### Note that I also used renv for the package control. 
### This section is provided as an alternative.


list_packages <- c("tidyverse","dplyr","lubridate","sf")

invisible(lapply(list_packages, function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package)
    library(package, character.only = TRUE)
  }
}))

### -----------------  create output folder  --------------------
### check the work directory
getwd()

### create a folder named "02_outdata" if it doesn't exist 

if(!file.exists("./02_outdata")){dir.create("./02_outdata")}



##---------------------------------------------------------------
##                      Import Raw Data                        --
##---------------------------------------------------------------


### --------------------  read datasets  ------------------------

### list all the files in the BWG database
list_FilePaths <- list.files(path='00_rawdata/BWG_database',pattern="*.csv",full.names = TRUE)
### get the names of all the datasets
list_FileNames <- make.names(gsub(".*1_","",tools::file_path_sans_ext(list_FilePaths)),unique=TRUE)

### read all the datasets and load them into the global environment
list2env(lapply(setNames(list_FilePaths,list_FileNames),
                read_csv)
         ,envir = .GlobalEnv)

### ----------------  Solution to ISSUE 1  ----------------------

### check if all the seven datasets have been imported correctly
list_Required_datasets <- c("abundance","bromeliads","datasets","owners","ownership","traits","visits")
if (!all(list_Required_datasets %in% ls())){
  missed_file <- list_Required_datasets[!list_Required_datasets %in% ls()]
  stop(paste("DATASET",missed_file,"LOSTED",collapse = " "))
}

### check if the seven datasets contains all the records

Required_n <- c(711,76,4,3,6,838,5) # standard number of records
Need_To_Check_n <- c(nrow(abundance),nrow(bromeliads),nrow(datasets),nrow(owners),nrow(ownership),nrow(traits),nrow(visits))
for (i in 1:7){  
  if(!Required_n[i]==Need_To_Check_n[i]){
  stop(paste("DATASET",list_Required_datasets[i],"LOSTED RECORDS",collapse = " "))}
  }


##---------------------------------------------------------------
##                       Task A Execution                      --
##---------------------------------------------------------------

### -------------------  Solution to ISSUE 2  -------------------
### -------------------  Inspect Data Type  ---------------------


### list the structures of all the seven datasets
str(lapply((list_FileNames),get),give.attr=FALSE) # Let Argument "give.attr" off to avoid massive details

### inspect the column names and data types, and write down the columns related to dates and coordinates
### Date-columns:
###    * "bromeliads$collection_date"
###    * "datasets$bwg_release"
###    * "datasets$public_release"
###    * "visits$date"

###

### Coordinate-columns:
###    * "datasets$lat", "datasets$lng"
###    * "visits$latitude","visits$longitude"


### -------------------  Solution to ISSUE 3  -------------------
### ------------------- Targeted Date Parsing -------------------

### Careful inspection of Date-coloumns suggested dates in Datasets "bromeliads" and "visits" were formatted
### in "year-month-day", whereas Dataset "datasets" was formatted in "day-month-year". Noteworthy is that dates in
### "datasets" were in type of character rather than date.Therefore, they will be parsed separately using different 
### Arguments "orders" in lubridate::parse_date_time()
### Note that we are uncertain about the time zones of the raw data, dates will be parsed in Coordinated Universal Time (UTC) in default.


### format: 'y-m-d'
bromeliads_clean <- bromeliads # avoid processing on the raw objects
bromeliads_clean$collection_date <- parse_date_time(bromeliads_clean$collection_date,orders = 'y-m-d',truncated=3)
visits_clean <- visits
visits_clean$date <- parse_date_time(visits_clean$date,orders = 'y-m-d',truncated=3)

### format: 'd/m/y'
datasets_clean <- datasets
datasets_clean$public_release <- parse_date_time(datasets_clean$public_release,orders = 'd/m/y',truncated=3)
datasets_clean$bwg_release <- parse_date_time(datasets_clean$bwg_release,orders = 'd/m/y',truncated=3)


### ---------------------  Check ------------------------------

### inspect if all the dates have been converted to ISO standards
str(bromeliads_clean$collection_date)
str(visits_clean$date)
str(datasets_clean$public_release)
str(datasets_clean$bwg_release)


##---------------------------------------------------------------
##                       Task B Execution                      --
##---------------------------------------------------------------

### --------------------- Data join ------------------------------
### join cleanded 'visits' and 'bromeliads' for comparison of the dates
bromeliads_clean <- visits_clean %>% 
  select(visit_id,date) %>% 
  right_join(.,bromeliads_clean,by="visit_id") %>%
  add_column(visit_date=.$date,.after = "collection_date") %>%  # add a new 'date' column immediately after Column 'collection_date'
  select(-date)

visits_clean <- visits_clean %>% 
  rename(visit_date=date) %>%  # change the name 'date' to 'visit_date',
  add_column(year = year(.$visit_date), # add columns for day, month, and year of each visit
             month = month(.$visit_date),
             day= day(.$visit_date),
             .after = "visit_date") # Place the new columns immediately after the date column


### --------------- Extension of Thinking --------------------
### compare 'visit_date' and 'collection_date'
View(bromeliads_clean[c('visit_date','collection_date')])
### Does the visit_date match with the 'collection_date' in the bromeliads dataframe?
### The results suggest that it is not necessary, and collection dates cannot be earlier than visit dates.


### ---------------------  Check ------------------------------
str(bromeliads_clean,give.attr=FALSE)
str(visits_clean,give.attr=FALSE)


##---------------------------------------------------------------
##                       Task C Execution                      --
##---------------------------------------------------------------

### -------- Assignment of coordinate reference system  ---- ----
### only 'datasets' and 'visits' have coordinate data.

xy_datasets<- datasets %>% 
  rename(longitude=lng,latitude=lat) 
xy_datasets <-  st_as_sf(xy_datasets,    # The whole dataset was imported, so other columns work as attributes of the spatial points
               coords = c("longitude", "latitude"),
               crs = "+proj=longlat +datum=WGS84")

xy_visits <-  st_as_sf(visits, 
                       coords = c("longitude", "latitude"),
                       crs = "+proj=longlat +datum=WGS84")

### ------------ Projection to geographic coordinates ------------
xy_datasets_utm <- st_transform(xy_datasets, crs = "+proj=utm +zone=16 +datum=WGS84")
xy_visits_utm <- st_transform(xy_visits, crs = "+proj=utm +zone=16 +datum=WGS84")

### ------------ Plot and Check ------------
plot(st_geometry(xy_visits_utm), col = "red", pch = 16, cex = 1.5,main='DATASETS visits')
plot(st_geometry(xy_datasets_utm), col = "blue", pch = 16, cex = 1.5,main='DATASETS datasets')
#### Both can be plotted successfully, indicating geographic coordinates have been assigned correctly.


##---------------------------------------------------------------
##                    Export Cleaned Data                      --
##---------------------------------------------------------------

### export tidy csv data
write.csv(bromeliads_clean,'02_outdata/bromeliads_clean.csv')
write.csv(datasets_clean,'02_outdata/datasets_clean.csv')
write.csv(visits_clean,'02_outdata/visits_clean.csv')

### export sf object as GeoJSON for GIS
st_write(xy_datasets_utm, "02_outdata/xy_datasets_utm.geojson", driver = "GeoJSON",append=FALSE) # use append=FALSE to overwrite layer
st_write(xy_visits_utm, "02_outdata/xy_visits_utm.geojson", driver = "GeoJSON",append=FALSE)

print("Tasks Completed")


