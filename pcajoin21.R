  s<-Sys.time()
library(readxl)
library(writexl)
library(janitor)
library(dplyr)
library(purrr)
#############################################
setwd("C:/Users/HP/Desktop/Paneloriginal/panel18")
#filelist<-c("1951.xlsx","1961.xlsx")#,"1971.xlsx","1981.xlsx","1991.xlsx","2001.xlsx")#,"2011.xlsx")
filelist<-c("1951.xlsx","1961.xlsx","1971.xlsx","1981.xlsx","1991.xlsx","2001.xlsx","2011.xlsx")
df_list <- map(filelist,~read_excel(.))
names(df_list)<-filelist
############################################
varnames <- read_excel("varnames.xlsx")
for (i in seq_along(df_list)){
  colnames(df_list[[i]]) <- varnames$varnames
}
#year<-c(1951,1961)#,1971,1981,1991,2001)#,2011)
year<-c(1951,1961,1971,1981,1991,2001,2011)
for( i in seq_along(df_list)){
  df_list  [[i]]$year <- rep(year[i],nrow( df_list  [[i]]))
}
###############################################
v<-c("male_population","female_population","male_0_6_years",
     "female_0_6_years","male_literate_population","female_literate_population",
     "male_sc_population","female_sc_population","male_st_population","female_st_population")
l<-c("district_mdds_name","sub_district_mdds_name","name_of_mdds_village_town",
     "village_town_mdds",v,"year")
m<-c("district_mdds_name","sub_district_mdds_name","name_of_mdds_village_town")

###############################################
fee<-function(x){
  x<-x[-1:-3,]
  x<-x[,l]
  x[,-which(names(x) %in% m)]<-as.data.frame(lapply(x[,-which(names(x) %in% m)],as.integer))
  x<-x[complete.cases(x),]
  print(x)
}
df_list<-map(df_list,fee)
###############################################
fei<-function(x) {
  x %>% group_by(village_town_mdds) %>% summarise(across(contains("population"),sum))
}
df_list<-map(df_list,fei)
##############################################

mrMDDS <- read_excel("mrMDDS.xls")
d<-df_list |> reduce(full_join,by="village_town_mdds")

#####################################
t<-Sys.time()
s-t