### fichier travail Joao
library(tidyverse)
library(corrplot)

inp_movies_df<-read.csv("ml-latest-small/movies.csv")
inp_ratings_df<-read.csv("ml-latest-small/ratings.csv")
inp_tags_df<-read.csv("ml-latest-small/tags.csv")
inp_links_df<-read.csv("ml-latest-small/links.csv")

user_movie_df <- inp_ratings_df %>% merge(inp_movies_df, by='movieId') %>% mutate(date_watch = as.Date(as.POSIXct(timestamp, origin="1970-01-01")))

#==== characterize user ================
favorite_genre_by_user <- user_movie_df %>% 
  select(userId,movieId, rating, genres) %>%
  separate(genres, sep = "\\|", c('genre1','genre2','genre3','genre4','genre5','genre6')) %>%
  pivot_longer(c('genre1','genre2','genre3','genre4','genre5','genre6'), names_to = 'genres_all') %>%
  drop_na() %>% group_by(userId,value) %>% summarise(count_gen=n()) %>% arrange(userId,-count_gen, by_group = TRUE) %>%
  filter(row_number()==1) %>% select (userId, fav_genre=value)


favorite_genre_by_user <- user_movie_df %>% 
  select(userId,movieId, rating, genres) %>% 
  separate(genres, sep = "\\|", c('genre1','genre2','genre3','genre4','genre5','genre6')) %>%
  pivot_longer(c('genre1','genre2','genre3','genre4','genre5','genre6'), names_to = 'genres_all') %>%
  drop_na()%>%
  group_by(userId, value) %>% summarise(avg_genre = mean(rating), sd = sd(rating), count_gen = n()) %>%
  filter(count_gen >= 5 ) %>%
  pivot_wider(values_from = c(avg_genre,sd,count_gen), names_from = value)


user_summary <- user_movie_df %>% group_by(userId) %>% summarise(nbr_films_watched = n_distinct(movieId),
                                                                 average_rating = mean(rating),
                                                                 oldest_film_watched = min(date_watch),
                                                                 oldest_film_watched_ts = min(timestamp)
) %>%
  merge(favorite_genre_by_user)


nbr_vues_film <- user_movie_df %>% group_by(movieId, title) %>% summarise(total_views = n()) %>% arrange(-total_views)
nbr_vues_film_500 <- nbr_vues_film %>% filter (total_views>=46)

nbr_vues_user <- user_movie_df %>% group_by(userId) %>% summarise(total_views = n()) %>% arrange(-total_views)
nbr_vues_user_50 <- nbr_vues_user %>% filter (total_views>=50)


user_movie_df %>% summarise(tot_users = n_distinct(userId), tot_films = n_distinct(movieId))

favorite_genre_by_user %>% ungroup() %>% summarise(nbr_genres = n_distinct(fav_genre))


#========== create pivoted movies lsit dataframe ===============
user_mobie_pivoted_df <- user_movie_df %>% select(movieId,userId,rating) %>% filter(movieId %in% as.list(nbr_vues_film_500['movieId'])$movieId) %>%
  filter(userId %in% as.list(nbr_vues_user_50['userId'])$userId) %>%
  pivot_wider(values_from = rating, names_from =movieId)


user_mobie_pivoted_df


cor_mat <- cor(user_mobie_pivoted_df%>% select(-userId), use = 'pairwise.complete.obs')


#corrplot(cor_mat)


