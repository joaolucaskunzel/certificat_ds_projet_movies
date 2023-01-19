#https://towardsdatascience.com/xgboost-is-not-black-magic-56ca013144b4


library(tidyverse)
library(corrplot)

library(xgboost)
library(caret)     
library(imputeTS)

inp_movies_df<-read.csv("ml-latest-small/movies.csv")
inp_ratings_df<-read.csv("ml-latest-small/ratings.csv")
inp_tags_df<-read.csv("ml-latest-small/tags.csv")
inp_links_df<-read.csv("ml-latest-small/links.csv")

user_movie_df <- inp_ratings_df %>% merge(inp_movies_df, by='movieId') %>% mutate(date_watch = as.Date(as.POSIXct(timestamp, origin="1970-01-01")))

#==== characterize user ================
favorite_genre_by_user <- user_movie_df %>% select(userId,movieId, genres) %>% separate(genres,c('genre1','genre2','genre3','genre4','genre5','genre6')) %>%
  pivot_longer(c('genre1','genre2','genre3','genre4','genre5','genre6'), names_to = 'genres_all') %>%
  drop_na() %>%
  group_by(userId,value) %>% summarise(count_gen=n()) %>% arrange(userId,-count_gen, by_group = TRUE) %>%
  filter(row_number()==1) %>% select (userId, fav_genre=value)


user_summary <- user_movie_df %>% group_by(userId) %>% summarise(nbr_films_watched = n_distinct(movieId),
                                                                 average_rating = mean(rating),
                                                                 oldest_film_watched = min(date_watch),
                                                                 oldest_film_watched_ts = min(timestamp)
) %>%
  merge(favorite_genre_by_user)


nbr_vues_film <- user_movie_df %>% group_by(movieId, title) %>% summarise(total_views = n()) %>% arrange(-total_views)
nbr_vues_film_500 <- nbr_vues_film %>% filter (total_views>=46)

nbr_vues_user <- user_movie_df %>% group_by(userId) %>% summarise(total_views = n()) %>% arrange(-total_views)
nbr_vues_user_40 <- nbr_vues_user %>% filter (total_views>=40)

most_seen_films <- nbr_vues_film %>% head(50)

user_movie_df %>% summarise(tot_users = n_distinct(userId), tot_films = n_distinct(movieId))

favorite_genre_by_user %>% ungroup() %>% summarise(nbr_genres = n_distinct(fav_genre))


#========== create pivoted movies list data frame ===============
user_mobie_pivoted_df <- user_movie_df %>% select(movieId,userId,rating) %>% filter(movieId %in% as.list(nbr_vues_film_500['movieId'])$movieId) %>%
  filter(userId %in% as.list(nbr_vues_user_40['userId'])$userId) %>%
  pivot_wider(values_from = rating, names_from =movieId)


final_df <- user_summary %>% inner_join(user_mobie_pivoted_df, by = 'userId')


cor_mat <- cor(user_mobie_pivoted_df%>% select(-userId), use = 'pairwise.complete.obs')
corrplot(cor_mat[50:80,50:80])


plot(user_mobie_pivoted_df%>% select('293','350',-userId)  %>% drop_na())



#====================XGBOOST================

set.seed(1)

target_film = '2'

cor_film <- cor_mat[,target_film] 
films_high_cor<-names(cor_film[abs(cor_film)>0.1])
tage_inc<-c("userId","nbr_films_watched","average_rating","oldest_film_watched","oldest_film_watched_ts","fav_genre")
final_df_filt <- final_df %>% select(tage_inc,films_high_cor)

#use 70% of dataset as training set and 30% as test set 
train <- final_df_filt %>% drop_na(target_film) %>% sample_frac(0.70)  %>% replace(is.na(.), 0)
test  <- final_df_filt %>% drop_na(target_film) %>% anti_join(train, by = 'userId') %>% replace(is.na(.), 0)

train <- na_mean(train)
test <- na_mean(test)

data_train <- data.matrix(train %>% select(-userId,-oldest_film_watched_ts, -target_film))
target_train <- (train %>%  select(target_film))[,1]

data_test <- data.matrix(test %>% select(-userId,-oldest_film_watched_ts, -target_film))
target_test <- (test %>%  select(target_film))[,1]

xgb_train = xgb.DMatrix(data = data_train, label = target_train)
xgb_test = xgb.DMatrix(data = data_test, label = target_test)

#===========TRAIN============
watchlist = list(train=xgb_train, test=xgb_test)

model = xgb.train(data = xgb_train, max.depth = 2, eta = 0.3, watchlist=watchlist, nrounds = 50, early_stopping_rounds = 1)
nround_cv <- model$best_iteration
bstSparse <- xgboost(data = data_train, label = target_train, max.depth = 2, eta = 0.3, nrounds = nround_cv, objective = "reg:squarederror")

#================TEST=============
pred_y = predict(bstSparse, data_test, target_test)

pred_y_round = round(pred_y*2)/2

sqrt(mean((target_test - pred_y)^2)) #rmse - Root Mean Squared Error

plot(target_test, pred_y)

plot(target_test, pred_y - target_test)

colnames(final_df)
