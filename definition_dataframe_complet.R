library(tidyverse)

inp_movies_df<-read.csv("ml-latest/movies.csv")
inp_ratings_df<-read.csv("ml-latest/ratings.csv")
#inp_tags_df<-read.csv("ml-latest/tags.csv")
#inp_links_df<-read.csv("ml-latest/links.csv")
####################################################
inp_cast_df<-readRDS("Doncast_small_final")
####################################################

inp_movies_df <- inp_movies_df %>% mutate(year = as.numeric(str_extract(title, "(?<=\\()[0-9]{4}(?=\\))")))
inp_movies_df <- inp_movies_df %>% mutate(title_str = str_extract(title, ".*(?= \\()"))

user_movie_df <- inp_ratings_df %>% merge(inp_movies_df, by='movieId') %>% mutate(date_watch = as.Date(as.POSIXct(timestamp, origin="1970-01-01")))

rm(inp_ratings_df)
rm(inp_movies_df)
####################################################
# Select movies for validation
# Nfpu<-100 #min requested ratings
# Nfilm<-5 # number of film to select
# set.seed(4321)
# SelMov<-user_movie_df %>% group_by(movieId) %>% 
#   summarise(Nrate=n()) %>% filter(Nrate>Nfpu)%>% arrange(desc(Nrate))%>%
#   select(movieId)
# SelMov<-sample(SelMov$movieId,size = Nfilm, replace=FALSE)
# Yval<-user_movie_df %>% filter(movieId %in% SelMov)
# user_movie_df<-user_movie_df %>% filter(!(movieId %in% SelMov))
####################################################
# exclude toy Story (test impacte agregation)
user_movie_df<-user_movie_df %>% filter(movieId != '1')

####################################################
# Add casting & Crew in table
user_movie_df<- user_movie_df %>% merge(inp_cast_df, by='movieId')
####################################################

nbr_vues_film <- user_movie_df %>% group_by(movieId, title) %>% summarise(total_views = n()) %>% arrange(-total_views)
nbr_vues_film_50 <- nbr_vues_film %>% filter (total_views>=3000)

nbr_vues_user <- user_movie_df %>% group_by(userId) %>% summarise(total_views = n()) %>% arrange(-total_views)
nbr_vues_user_50 <- nbr_vues_user %>% filter (total_views>=50)

films_populaire <- nbr_vues_film %>% head(100)

#==== characterize user ================
genre_by_user <- user_movie_df %>% 
  select(userId,movieId, rating, genres) %>% 
  separate(genres, sep = "\\|", c('genre1','genre2','genre3','genre4','genre5','genre6')) %>%
  pivot_longer(c('genre1','genre2','genre3','genre4','genre5','genre6'), names_to = 'genres_all') %>%
  drop_na()%>%
  group_by(userId, value) %>% 
  summarise(
    avg_genre = mean(rating), 
    #sd = sd(rating), 
    count_gen = n()) %>%
  #filter(count_gen >= 5 ) %>% ## il faut au moins avoir vu 5 films d'une categorie
  filter(value!="(no genres listed)")%>%
  pivot_wider(values_from = c(avg_genre,count_gen), names_from = value, values_fill=0)

decedes_by_user <- user_movie_df %>% 
  select(userId,movieId, rating, year) %>% 
  mutate(dec_film = (floor(year/10)*10)) %>%
  mutate(dec_film = case_when(
    dec_film < 1950 ~ "<40",
    dec_film >= 1950 & dec_film <= 1969 ~ "50-60",
    dec_film >= 1970 & dec_film <= 1989 ~ "70-80",
    dec_film >= 1990 & dec_film <= 2009 ~ "90-10",
    dec_film >= 2010 ~ ">10",
  )) %>% drop_na() %>% 
  group_by(userId, dec_film) %>% 
  summarise(
    avg_dec = mean(rating), 
    count_dec = n()) %>%
  #filter(count_dec >= 5 ) %>% ## il faut au moins avoir vu 5 films d'une decennie
  pivot_wider(values_from = c(avg_dec,count_dec), names_from = dec_film, values_fill=0)
#mutate_at(vars("count_dec_<40","count_dec_50-60","count_dec_70-80","count_dec_90-10","count_dec_>10"), ~replace_na(., 0))

####################################################
#Actors by user
NActTop<-20
ActclasN<-user_movie_df %>% 
  select(-c(timestamp,title,genres,year,title,genres,year,title_str,date_watch,DirIMdb)) %>% 
  pivot_longer(cols = starts_with("Act"), names_to = "IAct",values_to = "Actors") %>% 
  group_by(Actors)%>% summarize(N=n()) %>% filter(!(Actors=="no more actors")) %>% arrange(desc(N))%>% head(NActTop) %>% 
  select(Actors)

actors_by_user <-  user_movie_df %>% 
  select(-c(timestamp,title,genres,year,title,genres,year,title_str,date_watch,DirIMdb)) %>%
  pivot_longer(cols = starts_with("Act"), names_to = "IAct",values_to = "Actors") %>% 
  select(userId, rating, starts_with("Act")) %>% 
  filter(Actors %in% ActclasN$Actors) %>% 
  group_by(userId, Actors)  %>% 
  summarise(
    avg_act = mean(rating), 
    #sd_act = sd(rating), 
    count_act = n()) %>% 
  #mutate_at(vars("sd_act"), ~replace_na(., 0)) %>% 
  pivot_wider(values_from = c(avg_act,count_act), names_from = Actors, values_fill=0)
####################################################
#Director by user
NDirTop<-20
DirclasN<-user_movie_df %>%
  select(c("movieId","userId", "rating", 'DirIMdb')) %>% 
  group_by(DirIMdb)%>% 
  summarize(N=n()) %>% filter(!(DirIMdb=="no director")) %>% 
  arrange(desc(N))%>% 
  head(NDirTop) %>% 
  select(DirIMdb)

directors_by_user <-  user_movie_df %>%
  select(userId, rating, DirIMdb) %>% 
  filter(DirIMdb %in% DirclasN$DirIMdb) %>% 
  group_by(userId, DirIMdb)  %>% 
  summarise(
    avg_act = mean(rating), 
    #sd_act = sd(rating), 
    count_act = n()) %>% 
  #mutate_at(vars("sd_act"), ~replace_na(., 0)) %>% 
  pivot_wider(values_from = c(avg_act,count_act), names_from = DirIMdb, values_fill=0)


####################################################

user_summary <- user_movie_df %>% 
  group_by(userId) %>% 
  summarise(
    nbr_films_watched = n_distinct(movieId),
    average_rating = mean(rating),
    sd_user = sd(rating),
    #oldest_film_watched = min(date_watch),
    #last_film_watched_ts = min(timestamp),
    oldest_film_watched_ts = min(year)
  ) %>% 
  merge(genre_by_user) %>%
  merge(decedes_by_user) %>% 
  merge(actors_by_user) %>% 
  merge(directors_by_user) 

####################################################

user_movie_df %>% summarise(tot_users = n_distinct(userId), tot_films = n_distinct(movieId))

Y_ts =  inp_ratings_df %>% filter(movieId == '1') %>% select(userId,rating) 

saveRDS(user_summary, "user_summary_v2_no_ts")
saveRDS(Y_ts, "Y_ts")


#favorite_genre_by_user %>% ungroup() %>% summarise(nbr_genres = n_distinct(fav_genre))

# 
# #========== create pivoted movies lsit dataframe ===============
# movies_pivoted <- user_movie_df %>% select(movieId,userId,rating) %>% filter(movieId %in% as.list(films_populaire['movieId'])$movieId) %>%
#   filter(userId %in% as.list(nbr_vues_user_50['userId'])$userId) %>%
#   pivot_wider(values_from = rating, names_from =movieId)
# 
# saveRDS(movies_pivoted, "movies_pivoted")
# 
# final_df <- user_summary
# final_df_w_films <- user_summary %>% inner_join(movies_pivoted, by = 'userId')
# 
# 
# mov_id_name <- inp_movies_df %>% select(movieId,title = title_str)
# df_colmuns = (colnames(final_df_w_films))
# 
# user_cols <- df_colmuns[2:72]
# film_cols <-df_colmuns[73:577]

