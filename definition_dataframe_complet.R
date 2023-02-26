library(tidyverse)

print('reading input files')

inp_movies_df<-read.csv("ml-latest/movies.csv")
inp_ratings_df<-read.csv("ml-latest/ratings.csv")
#inp_tags_df<-read.csv("ml-latest/tags.csv")
#inp_links_df<-read.csv("ml-latest/links.csv")
####################################################
inp_cast_df<-readRDS("Doncast_small_final")
####################################################

inp_movies_df <- inp_movies_df %>% mutate(year = as.numeric(str_extract(title, "(?<=\\()[0-9]{4}(?=\\))")))
inp_movies_df <- inp_movies_df %>% mutate(title_str = str_extract(title, ".*(?= \\()"))

print('Creating user_movie_df')
user_movie_df <- inp_ratings_df %>% merge(inp_movies_df, by='movieId') %>% mutate(date_watch = as.Date(as.POSIXct(timestamp, origin="1970-01-01")))

####################################################
# exclude target films 
target_films <- c('1','318','356','296','593','260','527','2858','1721','4878','4848','1298','2381','164909','3462','616','1219','8154','899','3435','2010','2683')
user_movie_df<-user_movie_df %>% filter(!movieId %in% target_films)

####################################################
# Add casting & Crew in table
print('Adding Cast to user_movie_df')
user_movie_df<- user_movie_df %>% left_join(inp_cast_df, by='movieId')
####################################################

nbr_vues_film <- user_movie_df %>% group_by(movieId, title) %>% summarise(total_views = n()) %>% arrange(-total_views)
nbr_vues_film_50 <- nbr_vues_film %>% filter (total_views>=3000)

nbr_vues_user <- user_movie_df %>% group_by(userId) %>% summarise(total_views = n()) %>% arrange(-total_views)
nbr_vues_user_50 <- nbr_vues_user %>% filter (total_views>=50)

films_populaire <- nbr_vues_film %>% head(100)


ids_users <- user_movie_df %>% select(userId) %>% group_by(userId) %>% summarise(id = max(userId))%>% arrange(userId) %>% select(id)
step <- floor(max(ids_users) / 100)

print('characterize user: genre, decades')

for (i in c(0:100)) {
  print(i)
  batch <- user_movie_df %>% filter(userId %in% c((1+step*i):(step*(i+1))))
  
  #==== characterize user ================
  genre_by_user_temp <- batch %>% 
    select(userId,movieId, rating, genres) %>% 
    separate(genres, sep = "\\|", c('genre1','genre2','genre3','genre4','genre5','genre6')) %>%
    pivot_longer(c('genre1','genre2','genre3','genre4','genre5','genre6'), names_to = 'genres_all') %>%
    drop_na()%>%
    group_by(userId, value) %>% 
    summarise(
      avg_genre = mean(rating), 
      sd_genre = sd(rating), 
      count_gen = n()) %>%
    #filter(count_gen >= 5 ) %>% ## il faut au moins avoir vu 5 films d'une categorie
    filter(value!="(no genres listed)")%>%
    pivot_wider(values_from = c(avg_genre,sd_genre,count_gen), names_from = value, values_fill=0)
  
  decedes_by_user_temp <- batch %>% 
    select(userId,movieId, rating, year) %>% 
    mutate(dec_film = (floor(year/10)*10)) %>%
    mutate(dec_film = case_when(
      dec_film < 1950 ~ "<50",
      dec_film >= 1950 & dec_film <= 1969 ~ "50-60",
      dec_film >= 1970 & dec_film <= 1989 ~ "70-80",
      dec_film >= 1990 & dec_film <= 2009 ~ "90-10",
      dec_film >= 2010 ~ ">10",
    )) %>% drop_na() %>% 
    group_by(userId, dec_film) %>% 
    summarise(
      avg_dec = mean(rating),
      sd_dec = sd(rating), 
      count_dec = n()) %>%
    #filter(count_dec >= 5 ) %>% ## il faut au moins avoir vu 5 films d'une decennie
    pivot_wider(values_from = c(avg_dec,sd_dec,count_dec), names_from = dec_film, values_fill=0)
  #mutate_at(vars("count_dec_<40","count_dec_50-60","count_dec_70-80","count_dec_90-10","count_dec_>10"), ~replace_na(., 0))
  if(i==0) {
    genre_by_user <- genre_by_user_temp
    decedes_by_user <- decedes_by_user_temp
  }else{
    genre_by_user <- rbind(genre_by_user, genre_by_user_temp)
    decedes_by_user <- rbind(decedes_by_user, decedes_by_user_temp)
  }
  
}

print('characterize user: Actors, Directors')

NActTop<-20
ActclasN<-user_movie_df %>% 
  select(-c(timestamp,title,genres,year,title,genres,year,title_str,date_watch,DirIMdb)) %>% 
  pivot_longer(cols = starts_with("Act"), names_to = "IAct",values_to = "Actors") %>% 
  group_by(Actors)%>% summarize(N=n()) %>% filter(!(Actors=="no more actors")) %>% arrange(desc(N))%>% head(NActTop) %>% 
  select(Actors)

NDirTop<-20
DirclasN<-user_movie_df %>%
  select(c("movieId","userId", "rating", 'DirIMdb')) %>% 
  group_by(DirIMdb)%>% 
  summarize(N=n()) %>% filter(!(DirIMdb=="no director")) %>% 
  arrange(desc(N))%>% 
  head(NDirTop) %>% 
  select(DirIMdb)

for (i in c(0:100)) {
  print(i)
  batch <- user_movie_df %>% filter(userId %in% c((1+step*i):(step*(i+1))))
  
  ####################################################
  # Actors by user
  
  actors_by_user_temp <-  batch %>% 
    select(-c(timestamp,title,genres,year,title,genres,year,title_str,date_watch,DirIMdb)) %>%
    pivot_longer(cols = starts_with("Act"), names_to = "IAct",values_to = "Actors") %>% 
    select(userId, rating, starts_with("Act")) %>% 
    filter(Actors %in% ActclasN$Actors) %>% 
    group_by(userId, Actors)  %>% 
    summarise(
      avg_act = mean(rating), 
      sd_act = sd(rating), 
      count_act = n()) %>% 
    #mutate_at(vars("sd_act"), ~replace_na(., 0)) %>% 
    pivot_wider(values_from = c(avg_act,sd_act,count_act), names_from = Actors, values_fill=0)
  ####################################################
  #Director by user
  
  directors_by_user_temp <-  batch %>%
    select(userId, rating, DirIMdb) %>% 
    filter(DirIMdb %in% DirclasN$DirIMdb) %>% 
    group_by(userId, DirIMdb)  %>% 
    summarise(
      avg_dir = mean(rating), 
      sd_dir = sd(rating), 
      count_dir = n()) %>% 
    #mutate_at(vars("sd_act"), ~replace_na(., 0)) %>% 
    pivot_wider(values_from = c(avg_dir,sd_dir,count_dir), names_from = DirIMdb, values_fill=0)
  
  if(i==0) {
    actors_by_user <- actors_by_user_temp
    directors_by_user <- directors_by_user_temp
  }else{
    actors_by_user <- rbind(actors_by_user, actors_by_user_temp)
    directors_by_user <- rbind(directors_by_user, directors_by_user_temp)
  }
  
}

####################################################
print('Creating user_summary')

user_summary <- user_movie_df %>% 
  group_by(userId) %>% 
  summarise(
    nbr_films_watched = n_distinct(movieId),
    average_rating = mean(rating),
    sd_user = sd(rating),
    oldest_film_watched_ts = min(year)
  ) %>% 
  left_join(genre_by_user) %>%
  left_join(decedes_by_user) %>% 
  left_join(actors_by_user) %>% 
  left_join(directors_by_user) %>%
  mutate_all(~replace_na(.,0))

####################################################

user_movie_df %>% summarise(tot_users = n_distinct(userId), tot_films = n_distinct(movieId))

Y_films =  inp_ratings_df %>% filter(movieId %in% target_films) %>% select(userId,movieId,rating) 

saveRDS(user_summary, "user_summary_v4_no_target_films")
saveRDS(Y_films, "Y_films")

