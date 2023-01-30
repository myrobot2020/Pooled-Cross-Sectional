s<-Sys.time()
pacman::p_load(dplyr, readxl, writexl, janitor, purrr) 
setwd("C:/Users/HP/Desktop/Paneloriginal/panel18")
filelist<-c("1951.xlsx","1961.xlsx","1971.xlsx","1981.xlsx","1991.xlsx","2001.xlsx","2011.xlsx")
year_list<-substr(filelist,1,4)
df_list <- map(filelist,~read_excel(.))
names(df_list)<-filelist
varnames<-read_excel("varnames.xlsx")
df_list <- lapply(df_list, setNames, varnames$varnames)

l<-c("district_mdds_name","sub_district_mdds_name","name_of_mdds_village_town",
     "village_town_mdds","male_population","female_population","male_0_6_years",
     "female_0_6_years","male_literate_population","female_literate_population",
     "male_sc_population","female_sc_population","male_st_population","female_st_population")
m<-c("district_mdds_name","sub_district_mdds_name","name_of_mdds_village_town")


fee<-function(x){
  x<-x[-1:-3,]
  x<-x[,l]
  x[,-which(names(x) %in% m)]<-as.data.frame(lapply(x[,-which(names(x) %in% m)],as.integer))
  x<-x[complete.cases(x),] |> print()
}

fei<-function(x) {
  x %>% group_by(village_town_mdds) %>% summarise(across(contains("population"),sum))
}

df_list<-df_list  |> map(fee) |> map(fei)

year<-c(1951,1961,1971,1981,1991,2001,2011)
for( i in seq_along(df_list)){
  df_list  [[i]]$year <- rep(year[i],nrow( df_list  [[i]]))
}

df_list<-bind_rows(df_list)
df_list<-left_join(read_excel("mrMDDS.xls"),df_list1,"village_town_mdds",all = T)

df# Create a data frame with the original data
df <- data.frame(
  country = c("india", "us", "chin"),
  gdp.a = c(99, 100, 101),
  year.1 = c(2000, 2000, 2000),
  gdp.b = c(89, 79, 69),
  year.2 = c(2001, 2001, 2001),
  gdp.c = c(1, 2, 3),
  year.3 = c(2002, 2002, 2002)
)

# Convert the data frame to a data table
dt <- data.table(df)
# Use melt() to reshape the data
dt_melted <- dcast(melt(dt, id.vars = "country"), country ~ variable, value.var = "value")

# Create a new dataframe with the desired format
df_final <- data.frame(country=rep(dt_melted$country,3),gdp=c(dt_melted$gdp.a,dt_melted$gdp.b,dt_melted$gdp.c),year=c(dt_melted$year.1,dt_melted$year.2,dt_melted$year.3))

# Print the reshaped data
print(df_final)


dt1<-data.table(df_list1)
dt_melted1 <- dcast(melt(dt1, id.vars = "village_town_mdds"), village_town_mdds ~ variable, value.var = "value")
df_final <- data.frame(country=rep(dt_melted$country,3),
                       gdp=c(dt_melted$gdp.a,dt_melted$gdp.b,dt_melted$gdp.c),
                       year=c(dt_melted$year.1,dt_melted$year.2,dt_melted$year.3))
