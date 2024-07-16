setwd('D:\\R Studio')
# Function to install and load libraries
install_and_load <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
    library(package, character.only = TRUE)
  }
}
# Load required libraries
libraries <- c("dplyr", "readr", "readxl", "tidyr", "ggplot2", "BSDA")
lapply(libraries, install_and_load)
data=read.csv('NSSO68.csv')

#Filtering the data for UP
df=data%>%
  filter(state_1=='UP')

#Dataset info
missing_info <- colSums(is.na(df))
cat("Missing Values Information:\n")
print(missing_info)

#Subsetting the data
UPnew=df%>%
  select(state_1, District, Region, Sector, State_Region, Meals_At_Home, ricepds_v, Wheatpds_q, chicken_q, pulsep_q, wheatos_q, No_of_Meals_per_day)
# Impute missing values with mean for specific columns
impute_with_mean <- function(column) {
  if (any(is.na(column))) {
    column[is.na(column)] <- mean(column, na.rm = TRUE)
  }
  return(column)
}
UPnew$Meals_At_Home <- impute_with_mean(UPnew$Meals_At_Home)

# Finding outliers and removing them
remove_outliers <- function(df, column_name) {
  Q1 <- quantile(df[[column_name]], 0.25)
  Q3 <- quantile(df[[column_name]], 0.75)
  IQR <- Q3 - Q1
  lower_threshold <- Q1 - (1.5 * IQR)
  upper_threshold <- Q3 + (1.5 * IQR)
  df <- subset(df, df[[column_name]] >= lower_threshold & df[[column_name]] <= upper_threshold)
  return(df)
}
outlier_columns <- c("ricepds_v", "chicken_q")
for (col in outlier_columns) {
  apnew <- remove_outliers(UPnew, col)
}
# Summarize consumption
UPnew$total_consumption=rowSums(UPnew[, c("ricepds_v", "Wheatpds_q", "chicken_q", "pulsep_q", "wheatos_q")], na.rm = TRUE)

# Summarize and display top consuming districts and regions
summarize_consumption=function(group_col) {
  summary <- UPnew %>%
    group_by(across(all_of(group_col))) %>%
    summarise(total = sum(total_consumption)) %>%
    arrange(desc(total))
  return(summary)
}

district_summary=summarize_consumption("District")
region_summary=summarize_consumption("Region")

cat("Top Consuming Districts:\n")
print(head(district_summary, 4))
cat("Region Consumption Summary:\n")
print(region_summary)

#Renaming the districts and the sectors
sector_mapping=c("2" = "URBAN", "1" = "RURAL")
district_mapping=c('01'='Saharanpur','02'='Muzaffarnagar','03'='Bijnor','04'='Moradabad','05'='Rampur','06'='Jyotiba Phule Nagar *','07'='Meerut','08'='Baghpat *','09'='Ghaziabad','10'='Gautam Buddha Nagar *','11'='Bulandshahar','12'='Aligarh','13'='Hathras *','14'='Mathura','15'='Agra','16'='Firozabad','17'='Etah','18'='Mainpuri','19'='Budaun','20'='Bareilly','21'='Pilibhit','22'='Shahjahanpur','23'='Kheri','24'='Sitapur','25'='Hardoi','26'='Unnao','27'='Lucknow','28'='Rae Bareli','29'='Farrukhabad','30'='Kannauj *','31'='Etawah','32'='Auraiya *','33'='Kanpur Dehat','34'='Kanpur Nagar','35'='Jalaun','36'='Jhansi','37'='Lalitpur','38'='Hamirpur','39'='Mahoba *','40'='Banda','41'='Chitrakoot *','42'='Fatehpur','43'='Pratapgarh','44'='Kaushambi *','45'='Allahabad','46'='Barabanki','47'='Faizabad','48'='Ambedkar Nagar *','49'='Sultanpur','50'='Bahraich','51'='Shrawasti *','52'='Balrampur *','53'='Gonda','54'='Siddharthnagar','55'='Basti','56'='Sant Kabir Nagar *','57'='Mahrajganj','58'='Gorakhpur','59'='Kushinagar *','60'='Deoria','61'='Azamgarh','62'='Mau','63'='Ballia','64'='Jaunpur','65'='Ghazipur','66'='Chandauli *','67'='Varanasi','68'='Sant Ravidas Nagar Bhadohi *','69'='Mirzapur','70'='Sonbhadra','71'='Kashiramnagar'
)

UPnew$District <- as.character(UPnew$District)
UPnew$Sector <- as.character(UPnew$Sector)
UPnew$District <- ifelse(UPnew$District %in% names(district_mapping), district_mapping[UPnew$District], UPnew$District)
UPnew$Sector <- ifelse(UPnew$Sector %in% names(sector_mapping), sector_mapping[UPnew$Sector], UPnew$Sector)

View(UPnew)

hist(UPnew$total_consumption, breaks = 10, col = 'red', border = 'black', 
     xlab = "Consumption", ylab = "Frequency", main = "Consumption Distribution in Uttar Pradesh")

UP_consumption <- aggregate(total_consumption ~ District, data = UPnew, sum) 
View(UP_consumption)
barplot(UP_consumption$total_consumption, 
        names.arg = UP_consumption$District, 
        las = 2, # Makes the district names vertical
        col = 'red', 
        border = 'black', 
        xlab = "District", 
        ylab = "Total Consumption", 
        main = "Total Consumption per District",
        cex.names = 0.7) # Adjust the size of district names if needed

#Plotting on the Uttarpradesh state map using NSSO68.csv data
install.packages('sf')
library(ggplot2) 
library(sf) # mapping
library(dplyr) 
Sys.setenv("SHAPE_RESTORE_SHX" = "YES") 

data_map=st_read('C:\\Users\\SAMPREETH\\Downloads\\UTTAR PRADESH_DISTRICTS.geojson') 
View(data_map)

data_map=data_map %>% 
  rename(District = dtname) 
colnames(data_map) 
data_map_data=merge(UP_consumption,data_map,by = "District") 
View(data_map_data)
ggplot(data_map_data) + 
  geom_sf(aes(fill =total_consumption, geometry = geometry)) + 
  scale_fill_gradient(low = "blue", high = "green") + 
  ggtitle("Total Consumption_by_District") 

ggplot(data_map_data) + 
  geom_sf(aes(fill = total_consumption, geometry = geometry)) + 
  scale_fill_gradient(low = "yellow", high = "red") + 
  ggtitle("Total Consumption by District") +
  geom_sf_text(aes(label = District, geometry = geometry), size = 3, color = "black")
