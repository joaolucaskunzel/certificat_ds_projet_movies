### fichier travail Joao
library(tidyverse)
library(corrplot)

inp_movies_df<-read.csv("ml-latest-small/movies.csv")
inp_ratings_df<-read.csv("ml-latest-small/ratings.csv")
inp_tags_df<-read.csv("ml-latest-small/tags.csv")
inp_links_df<-read.csv("ml-latest-small/links.csv")


inp_movies_df <- inp_movies_df %>% mutate(year = as.numeric(str_extract(title, "(?<=\\()[0-9]{4}(?=\\))")))

user_movie_df <- inp_ratings_df %>% merge(inp_movies_df, by='movieId') %>% mutate(date_watch = as.Date(as.POSIXct(timestamp, origin="1970-01-01")))

nbr_vues_film <- user_movie_df %>% group_by(movieId, title) %>% summarise(total_views = n()) %>% arrange(-total_views)
nbr_vues_film_500 <- nbr_vues_film %>% filter (total_views>=46)

nbr_vues_user <- user_movie_df %>% group_by(userId) %>% summarise(total_views = n()) %>% arrange(-total_views)
nbr_vues_user_50 <- nbr_vues_user %>% filter (total_views>=50)

#==== characterize user ================
genre_by_user <- user_movie_df %>% 
  select(userId,movieId, rating, genres) %>% 
  separate(genres, sep = "\\|", c('genre1','genre2','genre3','genre4','genre5','genre6')) %>%
  pivot_longer(c('genre1','genre2','genre3','genre4','genre5','genre6'), names_to = 'genres_all') %>%
  drop_na()%>%
  group_by(userId, value) %>% 
  summarise(
    avg_genre = mean(rating), 
    sd = sd(rating), 
    count_gen = n()) %>%
  filter(count_gen >= 5 ) %>% ## il faut au moins avoir vu 5 films d'une categorie
  filter(value!="(no genres listed)")%>%
  pivot_wider(values_from = c(avg_genre,sd,count_gen), names_from = value)

decedes_by_user <- user_movie_df %>% 
  select(userId,movieId, rating, year) %>% 
  mutate(dec_film = (floor(year/10)*10)) %>%
  mutate(dec_film = case_when(
    dec_film < 1950 ~ "<40",
    dec_film >= 1950 & dec_film <= 1969 ~ "50-60",
    dec_film >= 1970 & dec_film <= 1989 ~ "70-80",
    dec_film >= 1990 & dec_film <= 2009 ~ "90-10",
    dec_film >= 2010 ~ ">10",
  )) %>%
  group_by(userId, dec_film) %>% 
  summarise(
    avg_dec = mean(rating), 
    count_dec = n()) %>%
  filter(count_dec >= 5 ) %>% ## il faut au moins avoir vu 5 films d'une decennie
  pivot_wider(values_from = c(avg_dec,count_dec), names_from = dec_film) %>%
  mutate_at(vars("count_dec_<40","count_dec_50-60","count_dec_70-80","count_dec_90-10","count_dec_>10"), ~replace_na(., 0))

user_summary <- user_movie_df %>% 
  group_by(userId) %>% 
  summarise(
    #nbr_films_watched = n_distinct(movieId),
    #average_rating = mean(rating),
    #oldest_film_watched = min(date_watch),
    oldest_film_watched_ts = min(timestamp)
  ) %>% 
  merge(genre_by_user) %>%
  merge(decedes_by_user)

# columns sum of NAs
result = (data.frame(user_summary %>% map( ~sum(is.na(.))))*100/dim(user_summary)[1])



user_movie_df %>% summarise(tot_users = n_distinct(userId), tot_films = n_distinct(movieId))

#favorite_genre_by_user %>% ungroup() %>% summarise(nbr_genres = n_distinct(fav_genre))


#========== create pivoted movies lsit dataframe ===============
movies_pivoted <- user_movie_df %>% select(movieId,userId,rating) %>% filter(movieId %in% as.list(nbr_vues_film_500['movieId'])$movieId) %>%
  filter(userId %in% as.list(nbr_vues_user_50['userId'])$userId) %>%
  pivot_wider(values_from = rating, names_from =movieId)



final_df <- user_summary %>% inner_join(movies_pivoted, by = 'userId')





#========= Exploration descriptive données ==============
cor_mat_films <- cor(movies_pivoted%>% select(-userId), use = 'pairwise.complete.obs')
cor_mat_user <- cor(user_summary%>% select(-userId), use = 'pairwise.complete.obs')
cor_mat_user_ts <- cor(user_summary %>%  inner_join(movies_pivoted%>% select(userId,'1'), by = 'userId') %>% select(-userId,), use = 'pairwise.complete.obs')

user_summary

corrplot(cor_mat_user_ts)



user_movie_df %>% 
  select(userId,movieId, rating, genres) %>% 
  separate(genres, sep = "\\|", c('genre1','genre2','genre3','genre4','genre5','genre6')) %>%
  pivot_longer(c('genre1','genre2','genre3','genre4','genre5','genre6'), names_to = 'genres_all') %>%
  drop_na()%>%
  group_by(value) %>% 
  summarise(
    avg_genre = mean(rating), 
    sd = sd(rating), 
    count_gen = n())



