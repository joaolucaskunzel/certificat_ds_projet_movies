#
# Set Data location / available paths
#
getwd()
setwd("C:/Users/r083485/OneDrive - Volvo Group/R/Projet")
locdata<-c("C:/Users/r083485/OneDrive - Volvo Group/R/Projet/Data/ml-latest-small/")
fplinks<-file.path(paste0(locdata,c("links.csv")))
fpmovies<-file.path(paste0(locdata,c("movies.csv")))
fpratings<-file.path(paste0(locdata,c("ratings.csv")))
fptags<-file.path(paste0(locdata,c("tags.csv")))
#
# Read Data
#
library(tidyverse)
library(dplyr)
library(stringr)
Donlinks<-read.csv(fplinks)
Donmovies<-read.csv(fpmovies)
Donratings<-read.csv(fpratings)
Dontags<-read.csv(fptags)
#
# Optionnal - view tables
view(Donlinks)
view(Donmovies)
view(Donratings)
view(Dontags)
#
#################################################
# LINKS TABLE
# Rebuild internet url based on readme instructions
#################################################
#
# 7 digits required for imdbId - see id 1094
Donlinks<-Donlinks %>% 
  mutate(linkIMDb=as.character(imdbId+10000000)) %>% 
  mutate(linkIMDb=substr(linkIMDb,2,8)) %>% 
  mutate(linkIMDb=paste0(c("http://www.imdb.com/title/tt"),linkIMDb)) %>% 
  mutate(linkTM=paste0(c("https://www.themoviedb.org/movie/"),tmdbId))
view(Donlinks)
#
#
###########################
# MOVIES TABLE
###########################
#
# 1) Set genres in columns
#
#liste of available "genre" based on readme info + IMAX
lgenre<-c("Action","Adventure","Animation","Children","Comedy","Crime","Documentary","Drama","Fantasy","Film-Noir",
    "Horror","Musical","Mystery","Romance","Sci-Fi","Thriller","War","Western","(no genres listed)","IMAX")
ng<-length(lgenre)
cmov<-colnames(Donmovies)
for (ii in 1:ng){
  genii<-lgenre[ii]
  cmov<-c(cmov,c(genii))
  Donmovies<-Donmovies %>% mutate(ii=str_detect(genres,genii))
  colnames(Donmovies)<-cmov
}
summary(Donmovies)
#Donmovies %>% slice(8518)
#
# 2) Split Title and date
#
Donmovies<-Donmovies %>% mutate(Name=unlist(str_split(title,"\\("))[1]) %>% 
  mutate(Sortie=as.integer(str_extract(title, "(?<=\\()\\d+")))
#
#

