#
# Set Data location / available paths
#
getwd()
setwd("C:/Users/r083485/OneDrive - Volvo Group/R/Projet")
locdata<-c("C:/Users/r083485/OneDrive - Volvo Group/R/Projet/Data/ml-latest-small/")
fplinks<-file.path(paste0(locdata,c("links.csv")))
#
# Read Data
#
library(tidyverse)
library(dplyr)
library(stringr)
Donlinks<-read.csv(fplinks)
view(Donlinks)
#
#
#
#################################################
#
# LINKS TABLE - Search for Cast&Crew (webscraping)
# To be done for all films
#
#################################################
#
#
# 1)Rebuild internet url based on readme instructions
#
# 7 digits required for imdbId - see id 1094
Donlinks<-Donlinks %>% 
  mutate(linkIMDb=as.character(imdbId+10000000)) %>% 
  mutate(linkIMDb=substr(linkIMDb,2,8)) %>% 
  mutate(linkIMDb=paste0(c("http://www.imdb.com/title/tt"),linkIMDb,c("/fullcredits?ref_=tt_cl_wr_sm"))) %>% 
  mutate(linkTM=paste0(c("https://www.themoviedb.org/movie/"),tmdbId))
view(Donlinks)
#
# 2) Webscrapping on themoviedb
#
library(rvest)
#
# Main actors
#
resu1act<- function (Xurl){
# nact main Actors
  nact<-5
  test<-try(read_html(Xurl))
  if(class(test[1])=="list"){
   data_html<- read_html(Xurl)
   res<-data_html %>% html_nodes(".cast_list > tr > td:nth-of-type(2) > a") %>% html_text2() %>% head(nact)
#  print(Xurl)
  res<-c(res,rep("no more actors",times=nact-length(res)))
  }
  else {
    res<-rep("url error",times=5)
  }
#  print(res)
  return(res)
}
#
# Director
#
resu1dir<-function (Xurl){
  test<-try(read_html(Xurl))
  if(class(test[1])=="list"){
    data_html<- read_html(Xurl)
    res<-data_html %>% html_nodes("#fullcredits_content > table:nth-of-type(1) > tbody > tr > td:nth-of-type(1)") %>% 
      html_text2() %>% head(1)}
  else {
    res<-rep("url error",times=1)
  }
#  print(res)
  return(res)
}
#
# Build Cast and Crew table
#
lact<-c("Act1" , "Act2", "Act3", "Act4", "Act5")
ltokeep<- c("movieId","DirIMdb" , lact)
nlines<-length(Donlinks$movieId)
nlines
int<-c(seq(from = 0, to = nlines, by= 5),nlines)
nbouc<-length(int)
nbouc
for(ii in 1888:(nbouc-1)){
  print(ii)
  ii1<-int[ii]+1
  ii2<-int[ii+1]
  DonDistri1<- Donlinks %>% filter(!is.na(imdbId)) %>% slice (ii1:ii2) %>% mutate(ActIMdb=t(sapply(linkIMDb,resu1act))) %>% 
  mutate(DirIMdb=sapply(linkIMDb,resu1dir))
  XX<-DonDistri1 %>% mutate(Act1= ActIMdb[,1])  %>% mutate(Act2= ActIMdb[,2]) %>% 
  mutate(Act3= ActIMdb[,3]) %>% mutate(Act4= ActIMdb[,4]) %>% mutate(Act5= ActIMdb[,5]) %>% 
  select(all_of(ltokeep))
  if(ii==1){
  Doncast<-XX
  }
  else {
  Doncast<-rbind(Doncast,XX)
  }
  saveRDS(Doncast,"Doncast")
}
saveRDS(Doncast,"Doncast_small")
Doncast<-readRDS("Doncast_small")
#
###############################################
# Correct errors linkked to connection issues
###############################################
#
#
# Calculated twice
#
ltwice<-Doncast %>% group_by(movieId, Act1) %>% summarise(n = n(), .groups = "drop") %>% filter(n > 1L) %>% select(movieId)
Doncast[Doncast$movieId %in% ltwice$movieId,]
Doncast<-Doncast[-(4566:4570),]
#
# Missing films
#
lmissing<-Donlinks[!(Donlinks$movieId %in% Doncast$movieId),]$movieId
lmissing
DonDistri1<- Donlinks %>% filter(!is.na(imdbId)) %>% filter(movieId %in% lmissing) %>% mutate(ActIMdb=t(sapply(linkIMDb,resu1act))) %>% 
  mutate(DirIMdb=sapply(linkIMDb,resu1dir))
XX<-DonDistri1 %>% mutate(Act1= ActIMdb[,1])  %>% mutate(Act2= ActIMdb[,2]) %>% 
  mutate(Act3= ActIMdb[,3]) %>% mutate(Act4= ActIMdb[,4]) %>% mutate(Act5= ActIMdb[,5]) %>% 
  select(all_of(ltokeep))
Doncast<-rbind(XX,Doncast)
Doncast<-Doncast %>% arrange(movieId)
#
# Try again on url issue
#
lup<-Doncast %>% filter((Act1=="url error")|(DirIMdb=="url error")) %>% select(movieId)
DonDistri1<- Donlinks %>% filter(!is.na(imdbId)) %>% filter(movieId %in% lup[,c("movieId")]) %>% mutate(ActIMdb=t(sapply(linkIMDb,resu1act))) %>% 
    mutate(DirIMdb=sapply(linkIMDb,resu1dir))
XX<-DonDistri1 %>% mutate(Act1= ActIMdb[,1])  %>% mutate(Act2= ActIMdb[,2]) %>% 
    mutate(Act3= ActIMdb[,3]) %>% mutate(Act4= ActIMdb[,4]) %>% mutate(Act5= ActIMdb[,5]) %>% 
    select(all_of(ltokeep))
Doncast[Doncast$movieId %in% lup$movieId,]<-XX
#
saveRDS(Doncast,"Doncast_small_final")
#
