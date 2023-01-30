library(readxl)
library(writexl)
library(janitor)
library(dplyr)
library(purrr)
#############################################
setwd("C:/Users/HP/Desktop/Paneloriginal/panel18")
filelist<-c("1951.xlsx","1961.xlsx")#,"1971.xlsx","1981.xlsx","1991.xlsx","2001.xlsx")#,"2011.xlsx")
df_list <- map(filelist,~read_excel(.))
df_list1<-df_list
df_list<-df_list1
names(df_list)<-filelist
a<-df_list[[1]]
############################################
varnames <- read_excel("varnames.xlsx")
for (i in seq_along(df_list)){
  colnames(df_list[[i]]) <- varnames$varnames
}
year<-c(1951,1961)#,1971,1981,1991,2001)#,2011)
for( i in seq_along(df_list)){
  df_list  [[i]]$year <- rep(year[i],nrow( df_list  [[i]]))
}
###############################################
fee<-function(x){
  l<-c("district_mdds_name","sub_district_mdds_name","name_of_mdds_village_town",
       "village_town_mdds","male_population","female_population","year")
  m<-c("district_mdds_name","sub_district_mdds_name","name_of_mdds_village_town")
  
  x<-x[-1:-3,]
  x<-x[,l]
  x[,-which(names(x) %in% m)]<-as.data.frame(lapply(x[,-which(names(x) %in% m)],as.integer))
  #x<-x[x$district_mdds_name=="Ramanagara",]
  x<-x[complete.cases(x),]
  #x<-aggregate(cbind(male_population,female_population)~sub_district_mdds_name+year,x,sum,na.rm=T)
  #colnames(x)[-1]<-paste(colnames(x)[-1],x$year[1],sep = "-")
  # x[2]<-NULL
  print(x)}

df_list<-map(df_list,fee)
write_xlsx(x = df_list,"out1.xlsx")

# View(df_list[[1]])
# a<-df_list[[1]]
df_listz<-df_list %>% reduce(full_join,by='village_town_mdds')
mrMDDS <- read_excel("mrMDDS.xls")
df_list2<-merge(mrMDDS,df_list2,"village_town_mdds")






a<-left_join(mrMDDS,df_list2,"village_town_mdds")

# View(df_list)
write_xlsx(x = df_list2,"out2.xlsx")
write_xlsx(x = a,"out3.xlsx")
