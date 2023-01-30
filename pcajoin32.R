s<-Sys.time()
# Load necessary packages 
pacman::p_load(dplyr, readxl, writexl, janitor, purrr,plm,testthat) 

# Set working directory to location of data files
setwd("C:/Users/HP/Desktop/Paneloriginal/panel18")

# Create a vector of file names to be read in
filelist<-c("1951.xlsx","1961.xlsx","1971.xlsx","1981.xlsx","1991.xlsx","2001.xlsx","2011.xlsx")

# Read in each file and store as a list
df_list <- map(filelist,~read_excel(.))
# Test
expect_equal(length(df_list),7)

# Name the list elements with the corresponding file names
names(df_list)<-filelist

# Read in variable names
varnames<-read_excel("varnames.xlsx")

# Assign variable names to data frames in the list
df_list <- lapply(df_list, setNames, varnames$varnames)
# Test
expect_equal(varnames$varnames,names(df_list[[sample(1:7,1)]]))

# Create a vector of selected variable names
l<-c("district_mdds_name","sub_district_mdds_name","name_of_mdds_village_town",
     "village_town_mdds","male_population","female_population","male_0_6_years",
     "female_0_6_years","male_literate_population","female_literate_population",
     "male_sc_population","female_sc_population","male_st_population","female_st_population")

# Create a vector of variable names to keep
m<-c("district_mdds_name","sub_district_mdds_name","name_of_mdds_village_town")

# Function to remove first three rows and select variables of interest
fee<-function(x){
  x<-x[-1:-3,]
  x<-x[,l]
  x[,-which(names(x) %in% m)]<-as.data.frame(lapply(x[,-which(names(x) %in% m)],as.integer))
  x<-x[complete.cases(x),] |> print()
}

# Function to group by village_town_mdds and sum population variables
fei<-function(x) {
  x %>% group_by(village_town_mdds) %>% summarise(across(contains("population"),sum))
}

# Apply the fee and fei functions to each data frame in the list
df_list<-df_list  |> map(fee) |> map(fei)

#expect_equal(df_list[[sample(1:7,1)]][sample(1:10,1)])

# Create a vector of years
year<-substr(filelist,1,4)

# Add a "year" variable to each data frame in the list
for( i in seq_along(df_list)){
  df_list  [[i]]$year <- rep(year[i],nrow( df_list  [[i]]))
}

# Bind the data frames together
df_list<-bind_rows(df_list)


#Join all data with Master List
df_list<-left_join(read_excel("mrMDDS.xls"),df_list,"village_town_mdds",all = T)


