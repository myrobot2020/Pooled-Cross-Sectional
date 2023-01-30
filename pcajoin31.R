s<-Sys.time()
pacman::p_load(dplyr, readxl, writexl, janitor, purrr,plm) 
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
df_list<-left_join(read_excel("mrMDDS.xls"),df_list,"village_town_mdds",all = T)
