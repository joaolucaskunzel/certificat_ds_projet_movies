library(tidyverse)
library(data.table)
# ======= READ INPUT DATA ============
inp_movies_df<-read.csv("ml-latest-small/movies.csv")
inp_ratings_df<-read.csv("ml-latest-small/ratings.csv")

# ========= filter data ==========
user_movie_df <- inp_ratings_df %>% merge(inp_movies_df, by='movieId') %>% mutate(date_watch = as.Date(as.POSIXct(timestamp, origin="1970-01-01")))

# == Remove films and users with few views
nbr_vues_film <- user_movie_df %>% group_by(movieId, title) %>% summarise(total_views = n()) %>% arrange(-total_views)
nbr_vues_film_100 <- nbr_vues_film %>% filter (total_views>=100)

nbr_vues_user <- user_movie_df %>% group_by(userId) %>% summarise(total_views = n()) %>% arrange(-total_views)
nbr_vues_user_50 <- nbr_vues_user %>% filter (total_views>=50)

#===========================================
# ===== Pivot table => sparse matrix========
movies_pivoted <- user_movie_df %>% select(movieId,userId,rating) %>% 
  filter(movieId %in% as.list(nbr_vues_film_100['movieId'])$movieId) %>%
  filter(userId %in% as.list(nbr_vues_user_50['userId'])$userId) %>%
  pivot_wider(values_from = rating, names_from =movieId)

films_watch <- movies_pivoted
films_watch[!is.na(movies_pivoted)] <- 1

rownames(movies_pivoted) <- movies_pivoted$userId

mov_id_name <- inp_movies_df %>% select(movieId,title)
df_colmuns = (colnames(movies_pivoted))

# === transpose data frame for users ====
movies_pivoted_tran <- transpose(movies_pivoted)
#     redefine row and column names
rownames(movies_pivoted_tran) <- colnames(movies_pivoted)
colnames(movies_pivoted_tran) <- movies_pivoted$userId

films_watch_tran <- transpose(films_watch)
rownames(films_watch_tran) <- colnames(films_watch)
colnames(films_watch_tran) <- movies_pivoted$userId

# ==== calculate correlations matrices
cor_films = cor(movies_pivoted %>% select(-userId), method = 'pearson', use = 'pairwise.complete.obs')
cor_users = cor(movies_pivoted_tran %>% filter(!row_number() %in% c(1)), method = 'pearson', use = 'pairwise.complete.obs')

#ignore films own correlation
diag(cor_films) <- NA 
diag(cor_users) <- NA

#===========================================
# ============ Film based filtering =========
# == one movie and one user prediction
sum(cor_films[,1]*movies_pivoted[1,-1], na.rm=T)/sum(cor_films[!is.na(movies_pivoted[1,-1]),1],na.rm=T) # user 1 , film 1 
sum(cor_films[,1]*movies_pivoted[2,-1], na.rm=T)/sum(cor_films[!is.na(movies_pivoted[2,-1]),1],na.rm=T) # user 2 , film 1 
sum(cor_films[,2]*movies_pivoted[1,-1], na.rm=T)/sum(cor_films[!is.na(movies_pivoted[1,-1]),2],na.rm=T) # user 1 , film 2 
sum(cor_films[,2]*movies_pivoted[2,-1], na.rm=T)/sum(cor_films[!is.na(movies_pivoted[2,-1]),2],na.rm=T) # user 2 , film 2

# == all users prediction for one film
cor_matrix_film <- sweep(movies_pivoted[,-1], MARGIN=2, cor_films[,1], `*`)
cor_matrix_weights <- sweep(films_watch[,-1], MARGIN=2, cor_films[,1], `*`)

pred_note_film = rowSums(cor_matrix_film, na.rm = T) / rowSums(cor_matrix_weights, na.rm = T)


## ===Matrix completion ==:
pred_note_mat = movies_pivoted$userId
for (film in colnames(movies_pivoted %>% select(-userId)) ){

  cor_matrix_film <- sweep(movies_pivoted[,-1], MARGIN=2, cor_films[,film], `*`)
  cor_matrix_weights <- sweep(films_watch[,-1], MARGIN=2, cor_films[,film], `*`)
  
  pred_note_film = rowSums(cor_matrix_film, na.rm = T) / rowSums(cor_matrix_weights, na.rm = T)
  
  pred_note_mat <- cbind(pred_note_mat,pred_note_film)
}
colnames(pred_note_mat) <- colnames(movies_pivoted)


# ===  Test Results =====
nbr_users <- dim(movies_pivoted)[1]
avg_note_film <- colMeans(movies_pivoted%>% select(-userId),  na.rm = T)

error_mat <- movies_pivoted[,-1] - pred_note_mat[,-1]
error_dum <- sweep(movies_pivoted[,-1], MARGIN=2, avg_note_film, `-`)

rmse <- sqrt(colMeans(error_mat**2,  na.rm = T))
dummy_rmse <- sqrt(colMeans(error_dum**2,  na.rm = T))
R2 <- 1 - rmse**2/dummy_rmse**2

perf_ib<-data.frame(rmse,dummy_rmse,R2)

#===========================================
# ============ USER based filtering =========
# == one movie and one user prediction
sum(cor_users[,1]*movies_pivoted_tran[2,], na.rm=T)/sum(cor_users[!is.na(movies_pivoted_tran[2,]),1],na.rm=T) # user 1 , film 1 
sum(cor_users[,1]*movies_pivoted_tran[3,], na.rm=T)/sum(cor_users[!is.na(movies_pivoted_tran[3,]),1],na.rm=T) # user 1 , film 2 
sum(cor_users[,2]*movies_pivoted_tran[2,], na.rm=T)/sum(cor_users[!is.na(movies_pivoted_tran[2,]),2],na.rm=T) # user 2 , film 1 
sum(cor_users[,2]*movies_pivoted_tran[3,], na.rm=T)/sum(cor_users[!is.na(movies_pivoted_tran[3,]),2],na.rm=T) # user 2 , film 2

# == all films prediction for one user
cor_matrix_film <- sweep(movies_pivoted_tran[-1,], MARGIN=2, cor_users[,1], `*`)
cor_matrix_weights <- sweep(films_watch_tran[-1,], MARGIN=2, cor_users[,1], `*`)

pred_note_user = rowSums(cor_matrix_film, na.rm = T) / rowSums(cor_matrix_weights, na.rm = T)
pred_note_user

## ===Matrix completion ==:
pred_note_mat_user = c() #movies_pivoted$userId
for (user in colnames(movies_pivoted_tran) ){
  
  cor_matrix_film <- sweep(movies_pivoted_tran[-1,], MARGIN=2, cor_users[,user], `*`)
  cor_matrix_weights <- sweep(films_watch_tran[-1,], MARGIN=2, cor_users[,user], `*`)
  
  pred_note_user = rowSums(cor_matrix_film, na.rm = T) / rowSums(cor_matrix_weights, na.rm = T)
  
  pred_note_mat_user <- rbind(pred_note_mat_user,pred_note_user)
}
rownames(pred_note_mat_user) <- movies_pivoted$userId
pred_note_mat_user <- cbind(userId = movies_pivoted$userId, pred_note_mat_user)

# ===  Test Results =====
nbr_users <- dim(movies_pivoted)[1]
avg_note_film <- colMeans(movies_pivoted%>% select(-userId),  na.rm = T)

error_mat <- movies_pivoted[,-1] - pred_note_mat_user[,-1]
error_dum <- sweep(movies_pivoted[,-1], MARGIN=2, avg_note_film, `-`)

rmse <- sqrt(colMeans(error_mat**2,  na.rm = T))
dummy_rmse <- sqrt(colMeans(error_dum**2,  na.rm = T))
R2 <- 1 - rmse**2/dummy_rmse**2

perf_ub <- data.frame(rmse,dummy_rmse,R2)




sqrt(colMeans((movies_pivoted%>% select('1') - colMeans(movies_pivoted%>% select('1'),na.rm =T) )^2, ,na.rm =T) ) #rmse - Root Mean Squared Error

     