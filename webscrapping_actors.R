#
# Set Data location / available paths
#
getwd()
setwd("C:/Users/r083485/OneDrive - Volvo Group/R/Projet")
locdata<-c("C:/Users/r083485/OneDrive - Volvo Group/R/Projet/Data/ml-latest/")
fplinks<-file.path(paste0(locdata,c("links.csv")))
#
# Read Data
#
library(tidyverse)
library(dplyr)
library(stringr)
library(rvest)
Donlinks<-read.csv(fplinks)
#
# Get selected Actors / Directors
#
final_df <- readRDS('user_summary_v3_no_ts')
Var1<-tibble(VAR = colnames(final_df))
misc<- Var1[1:5,1]
Var1<-Var1 %>% filter(!(VAR %in% misc$VAR))%>% 
  mutate(Type = str_split(VAR,"\\_",simplify = TRUE)[,1]) %>% 
  mutate(Car = str_split(VAR,"\\_",simplify = TRUE)[,2]) %>% 
  mutate(Name = str_split(VAR,"\\_",simplify = TRUE)[,3]) 
Directors <- Var1 %>% select(Type,Car,Name) %>% filter(Car=="dir") %>% 
  pivot_wider(names_from = Type, values_from = Car) %>%  select(Name)
Actors<-Var1 %>% select(Type,Car,Name) %>% filter(Car=="act") %>% 
  pivot_wider(names_from = Type, values_from = Car) %>%  select(Name)
#
# Webscrapping on themoviedb
#
res0<-tibble()
for (ii in 1:500){
  Xurl<-paste0("https://www.themoviedb.org/person?page=",ii)
  Xurl
  data_html<- read_html(Xurl)
  res1<-data_html %>% 
    html_nodes(".fifty_square") %>% html_elements("a") %>% html_attr(c("href"))
  res2<-data_html %>% 
    html_nodes(".fifty_square") %>% html_elements("a") %>% html_attr(c("alt"))
  res<-tibble(res1,res2) %>% filter(!is.na(res2))
  res0<-rbind(res0,res)
}
persons<- res0 %>% filter(res2 %in% c(all_of(Actors$Name),all_of(Directors$Name))) %>% 
  mutate(link = paste0("https://www.themoviedb.org/",res1))
nbper<- dim(persons)[1]
#
res0<-tibble()
for (ii in 1:nbper){
  Xurl<- persons$link[ii]
  data_html<- read_html(Xurl)
  res<-  data_html %>% 
    html_nodes(".credit_group")  %>% html_elements("a") %>% html_attr(c("href"))
  res<-as_tibble(res)
  res<- res %>% mutate(tmdbId = str_split(value,"\\/",simplify = TRUE)[,3]) %>% 
    mutate(type = str_split(value,"\\/",simplify = TRUE)[,2]) %>% filter(type =="movie") %>% 
    mutate(person = persons$res2[ii])
  res0<-rbind(res0,res)
}
Doncast_large_focus<-res0 %>% mutate(tmdbId=as.integer(tmdbId)) %>% left_join(Donlinks, by = "tmdbId")
saveRDS(Doncast_large_focus,"C:/Users/r083485/OneDrive - Volvo Group/R/Projet/Doncast_large_focus")
view(Doncast_large_focus)
all_of(Doncast_large_focus %>% filter(movieId=="1") %>% select(person))


