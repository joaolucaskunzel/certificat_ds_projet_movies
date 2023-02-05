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
#Dontags<-read.csv(fptags)
#
# Optionnal - view tables
view(Donlinks)
view(Donmovies)
view(Donratings)
#view(Dontags)
#
#
################################################
#
# Select films to estimate and users to advice
# get film to estimate rating
# remove films to estimate from Donratings
#
################################################
#
# Parameters for filtering
#
# Films to estimate must have at least Nfpu ratings
Nfpu<-100
# Nfilms are selected for been estimated (sample)
Nfilm<-5
# 
Nusers<-50
set.seed(4321)
SelMov<-Donratings %>% group_by(movieId) %>% summarise(Nrate=n()) %>% filter(Nrate>Nfpu) %>% select(movieId)
SelMov<-sample(SelMov$movieId,size = Nfilm, replace=FALSE)
Yval<-Donratings %>% filter(movieId %in% SelMov) %>% select(!c(timestamp )) %>% pivot_wider(names_from="movieId",values_from = "rating")
Seltitle <- Donmovies %>% filter(movieId %in% SelMov) %>% select(c("title"))
# Only users who rated all selected films are used in the list to advise
SelUsers<-Donratings %>% filter(movieId %in% SelMov) %>% select(!c(timestamp )) %>% group_by(userId) %>% summarise(N=n()) %>% 
  filter(N==Nfilm)
SelUsers<-sample(SelUsers$userId,size = min(Nusers,nrow(SelUsers)), replace=FALSE)
#
# selected films to estimate ID = SelMov
SelMov
# with title 
Seltitle
# Y rating for these films - /!\ all users = Yval
summary(Yval)
# selected users to advice = Selusers
SelUsers
#
# Remove films to estimate from Donratings
Donratings<-Donratings %>% filter(!(movieId %in% SelMov))
#
#
#################################################
#
# LINKS TABLE - Search for Cast&Crew (webscraping)
# To be done for all films
#
#################################################
#
# Done in webscrapping file
Doncast<-readRDS("Doncast_small_final")
#
#############################################
#
# Identify Top Actors and Top DIrectors
# Use non Y films only 
# Use all users
#
############################################
#
Donrate<-Donratings %>% select(movieId,userId,rating)
#
# Top Directors
NDirTop<-100
#
DirclasN<-Doncast %>% left_join (Donrate, by = "movieId") %>% group_by(DirIMdb) %>% summarize(N=n())
#
DirclasR<-Doncast %>% left_join (Donrate, by = "movieId") %>% group_by(DirIMdb) %>% summarize(R=mean(rating,na.rm=TRUE))
#
TopDir <-DirclasR %>% inner_join(DirclasN, by = "DirIMdb")%>% arrange(desc(N)) %>% head(NDirTop) %>% select(DirIMdb)
view(TopDir)
#
# Top Actors
NActTop<-120
#
ActclasN<-Doncast %>% full_join (Donrate, by = "movieId")%>% select(c(movieId,all_of(lact),rating))     %>% 
  pivot_longer(cols = all_of(lact), names_to = "Actid",values_to = "Actors") %>% 
  group_by(Actors)%>% summarize(N=n()) %>% filter(!is.na(Actors))
#
ActclasR<-Doncast %>% full_join (Donrate, by = "movieId")%>% select(c(movieId,all_of(lact),rating))     %>% 
  pivot_longer(cols = all_of(lact), names_to = "Actid",values_to = "Actors") %>% 
  group_by(Actors)%>% summarize(R=mean(rating,na.rm=TRUE)) %>% filter(!is.na(Actors))
#
TopActors<- ActclasR %>% inner_join(ActclasN, by = "Actors") %>% arrange(desc(N))%>% head(NActTop) %>% select(Actors) %>% filter(!(Actors=="no more actors"))
view(TopActors)
#
#
# Identify movies with top directors
# 
DoncastT<-Doncast %>% filter(DirIMdb %in% TopDir$DirIMdb) %>% mutate(Dir=DirIMdb) %>% mutate(Dirvalue = TRUE) %>% select(c(movieId,Dir,Dirvalue))
Doncast<-Doncast %>% left_join(DoncastT,by="movieId") %>% 
  pivot_wider(names_from = Dir, values_from = Dirvalue,values_fill=FALSE,names_prefix="Dir_") %>% 
  select(starts_with("movie")|starts_with("Act")|starts_with("Dir_"))
#
# Identify movies with top actors and select relevant columns
#
Doncast<-Doncast %>% pivot_longer(cols = all_of(lact), names_to = "Actid",values_to = "Actors")
DoncastT<-Doncast %>% filter(Actors %in% TopActors$Actors) %>% mutate(TopAct=Actors) %>% mutate(Actvalue = TRUE) %>% select(c(movieId,TopAct,Actvalue))
Doncast<-Doncast %>% pivot_wider(names_from = Actid, values_from = Actors) %>% 
  left_join(DoncastT,by="movieId") %>% 
  pivot_wider(names_from = TopAct, values_from = Actvalue,values_fill=FALSE,names_prefix="Act_")%>% 
  select(movieId|starts_with("Dir_")|starts_with("Act_"))
lDir<-colnames(Doncast %>% select(starts_with("Dir_")))
lAct<-colnames(Doncast %>% select(starts_with("Act_")))
#Doncast %>% group_by(movieId, TopAct) %>% summarise(n = n(), .groups = "drop") %>% filter(n > 1L)
#
################################
#
# MOVIES TABLE - Arrange data
#
###############################
#
# 1) Set genres in columns
#
#liste of available "genre" based on readme info + IMAX
lgenre<-c("Action","Adventure","Animation","Children","Comedy","Crime","Documentary","Drama","Fantasy","Film-Noir",
    "Horror","Musical","Mystery","Romance","Sci-Fi","Thriller","War","Western","(no genres listed)","IMAX")
lgenre2<-c("Action","Adventure","Animation","Children","Comedy","Crime","Documentary","Drama","Fantasy","Film-Noir",
          "Horror","Musical","Mystery","Romance","SciFi","Thriller","War","Western","NoGenre","IMAX")
ng<-length(lgenre)
cmov<-colnames(Donmovies)
for (ii in 1:ng){
  cmov<-c(cmov,c(lgenre2[ii]))
  Donmovies<-Donmovies %>% mutate(ii=str_detect(genres,lgenre[ii]))
  colnames(Donmovies)<-cmov
}
#
# 2) Split Title and date
#
#Donmovies<-Donmovies %>% mutate(Name=unlist(str_split(title,"\\("))[1]) %>% 
#  mutate(Year =as.integer(str_extract(title, "(?<=\\()\\d+")))
#view(Donmovies)
#
# 3)Donmovies cleansing
#
Donmovies<-Donmovies %>% select(movieId, title ,all_of(lgenre2))
#
# 4) Add Top Director and Top Actors
#
Donmovies<-Donmovies %>% left_join(Doncast,by="movieId")
lcar<-c(lgenre2,lDir,lAct)
# film caractéristics are 
# 1) Does film include Top director and Top actor
# 2) Does film belong to genre
#
#
#########################
#
# users and caracteristics
#
#########################
#
# Total number of films seen
#
DonNtot<-Donratings %>% group_by(userId) %>% summarise(Ntot=n())
#
# Global mean
#
DonMeantot<-Donratings %>% group_by(userId) %>% summarise(Meantot=mean(rating))
#
# Global sd
#
Donsdtot<-Donratings %>% group_by(userId) %>% summarise(Sdtot=sd(rating))
#
# Mean per caracteristic - NA set to 0 + calculate %
#
Donmean<-Donratings %>% left_join(Donmovies,by="movieId") %>% pivot_longer(cols=lcar,names_to = "Caracteristic", values_to = "YN") %>% 
  mutate(Ttype=YN*rating) %>%
  group_by(userId,Caracteristic) %>% filter(YN==TRUE) %>% summarise(Avrating=mean(Ttype)) %>% 
  pivot_wider(names_from = Caracteristic, values_from = Avrating,values_fill=0,names_prefix="Mean_")
lcol<-colnames(Donmean)[-1]
Donmean<- DonMeantot %>% left_join(Donmean,by="userId")%>% pivot_longer(cols=all_of(lcol),names_to = "Caracteristic", values_to = "Value") %>% 
  mutate(Invtot = Meantot**(-1)) %>% mutate(pp=Invtot*Value) %>%  mutate(Value=NULL) %>% mutate(Invtot=NULL) %>% 
  pivot_wider(names_from = Caracteristic, values_from = pp,names_prefix="PPR_") %>% left_join(Donmean,by="userId") %>% 
  mutate(Meantot=NULL)
#
# SD per caracteristic - NA set to high value compare other ? (ex 20) or 0 ? + No % calculation (meaning ?)
#
Donsd<-Donratings %>% left_join(Donmovies,by="movieId") %>% pivot_longer(cols=lcar,names_to = "Caracteristic", values_to = "YN") %>% 
  mutate(Ttype=YN*rating) %>%
  group_by(userId,Caracteristic) %>% filter(YN==TRUE) %>% summarise(sdrating=sd(Ttype)) %>% filter(!is.na(sdrating)) %>% 
  pivot_wider(names_from = Caracteristic, values_from = sdrating,names_prefix="SD_",values_fill=0)
#
# Number per caracteristic - NA set to 0 + calculate %
#
Donnum <-Donratings %>% left_join(Donmovies,by="movieId") %>% pivot_longer(cols=lcar,names_to = "Caracteristic", values_to = "YN") %>% 
  group_by(userId,Caracteristic) %>% filter(YN==TRUE) %>% summarise(n=n()) %>% 
  pivot_wider(names_from = Caracteristic, values_from = n,values_fill=0,names_prefix="N_")
lcol<-colnames(Donnum)[-1]
Donnum<- DonNtot %>% left_join(Donnum,by="userId")%>% pivot_longer(cols=all_of(lcol),names_to = "Caracteristic", values_to = "Value") %>% 
  mutate(Invtot = Ntot**(-1)) %>% mutate(pp=Invtot*Value) %>%  mutate(Value=NULL) %>% mutate(Invtot=NULL) %>% 
  pivot_wider(names_from = Caracteristic, values_from = pp,names_prefix="PPN_") %>% left_join(Donnum,by="userId") %>% 
  mutate(Ntot=NULL)
#
# Assembly
#
Doncar<-DonNtot %>% left_join(DonMeantot,by="userId") %>% left_join(Donsdtot,by="userId") %>% 
  left_join(Donnum,by="userId")%>% left_join(Donmean,by="userId")%>% left_join(Donsd,by="userId") 
summary(Doncar)
view(Doncar)
saveRDS(Doncar,"Doncar")
#
