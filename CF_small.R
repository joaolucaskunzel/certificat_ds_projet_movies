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

target_films <- c('1','318','356','296','593','260','527','2858','1721','4878','4848','1298','2381','164909','3462','616','1219','8154','899','3435','2010','2683')

target_films_present <- intersect(nbr_vues_film_100$movieId, target_films)
#===========================================
# ===== Pivot table => sparse matrix========
movies_pivoted <- user_movie_df %>% select(movieId,userId,rating) %>% 
  filter(movieId %in% as.list(nbr_vues_film_100['movieId'])$movieId) %>%
  filter(userId %in% as.list(nbr_vues_user_50['userId'])$userId) %>%
  pivot_wider(values_from = rating, names_from =movieId)

films_watch <- movies_pivoted
films_watch[!is.na(movies_pivoted)] <- 1

rownames(movies_pivoted) <- movies_pivoted$userId

mov_id_name <- inp_movies_df %>% select(movieId,title) %>% mutate(movieId=as.character(movieId))

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
for (film in  target_films_present){ #colnames(movies_pivoted %>% select(-userId))

  cor_matrix_film <- sweep(movies_pivoted[,-1], MARGIN=2, cor_films[,film], `*`)
  cor_matrix_weights <- sweep(films_watch[,-1], MARGIN=2, cor_films[,film], `*`)
  
  pred_note_film = rowSums(cor_matrix_film, na.rm = T) / rowSums(cor_matrix_weights, na.rm = T)
  
  pred_note_mat <- cbind(pred_note_mat,pred_note_film)
}
colnames(pred_note_mat) <- c('userId',target_films_present)


# ===  Test Results =====
nbr_users <- dim(movies_pivoted)[1]
movies_pivoted_filt <- movies_pivoted %>% select(all_of(target_films_present))

avg_note_film <- colMeans(movies_pivoted_filt, na.rm = T)

error_mat_film <- movies_pivoted_filt - pred_note_mat[,-1]
error_dum <- sweep(movies_pivoted_filt, MARGIN=2, avg_note_film, `-`)

rmse <- sqrt(colMeans(error_mat_film**2,  na.rm = T))
dummy_rmse <- sqrt(colMeans(error_dum**2,  na.rm = T))
R2 <- 1 - rmse**2/dummy_rmse**2

rownames(rmse)

perf_ib<-data.frame(target_films_present,rmse,dummy_rmse,R2) %>% left_join(mov_id_name, by=c("target_films_present" = "movieId"))

#===========================================
# ============ USER based filtering =========
movies_pivoted_tran_filt <- movies_pivoted_tran[target_films_present,]
films_watch_tran_filt <- films_watch_tran[target_films_present,]

# == one movie and one user prediction
sum(cor_users[,1]*movies_pivoted_tran_filt[1,], na.rm=T)/sum(cor_users[!is.na(movies_pivoted_tran_filt[1,]),1],na.rm=T) # user 1 , film 1 
sum(cor_users[,1]*movies_pivoted_tran_filt[2,], na.rm=T)/sum(cor_users[!is.na(movies_pivoted_tran_filt[2,]),1],na.rm=T) # user 1 , film 2 
sum(cor_users[,2]*movies_pivoted_tran_filt[1,], na.rm=T)/sum(cor_users[!is.na(movies_pivoted_tran_filt[1,]),2],na.rm=T) # user 2 , film 1 
sum(cor_users[,2]*movies_pivoted_tran_filt[2,], na.rm=T)/sum(cor_users[!is.na(movies_pivoted_tran_filt[2,]),2],na.rm=T) # user 2 , film 2

sum(cor_users[,1]*movies_pivoted_tran_filt[7,], na.rm=T)/sum(cor_users[!is.na(movies_pivoted_tran_filt[7,]),1],na.rm=T) # user 1 , film 7
sum(cor_users[,2]*movies_pivoted_tran_filt[7,], na.rm=T)/sum(cor_users[!is.na(movies_pivoted_tran_filt[7,]),2],na.rm=T) # user 2 , film 7

# == all films prediction for one user
cor_matrix_film <- sweep(movies_pivoted_tran_filt, MARGIN=2, cor_users[,1], `*`)
cor_matrix_weights <- sweep(films_watch_tran_filt, MARGIN=2, cor_users[,1], `*`)

pred_note_user = rowSums(cor_matrix_film, na.rm = T) / rowSums(cor_matrix_weights, na.rm = T)
pred_note_user

## ===Matrix completion ==:
pred_note_mat_user = c() #movies_pivoted$userId
for (user in colnames(movies_pivoted_tran) ){
  
  cor_matrix_film <- sweep(movies_pivoted_tran_filt, MARGIN=2, cor_users[,user], `*`)
  cor_matrix_weights <- sweep(films_watch_tran_filt, MARGIN=2, cor_users[,user], `*`)
  
  pred_note_user = rowSums(cor_matrix_film, na.rm = T) / rowSums(cor_matrix_weights, na.rm = T)
  
  pred_note_mat_user <- rbind(pred_note_mat_user,pred_note_user)
}
rownames(pred_note_mat_user) <- movies_pivoted$userId
pred_note_mat_user <- cbind(userId = movies_pivoted$userId, pred_note_mat_user)

# ===  Test Results =====
nbr_users <- dim(movies_pivoted)[1]

error_mat_user <- movies_pivoted_filt - pred_note_mat_user[,-1]

rmse_user <- sqrt(colMeans(error_mat_user**2,  na.rm = T))
R2_user <- 1 - rmse_user**2/dummy_rmse**2

perf_ub <- data.frame(target_films_present,rmse = rmse_user,dummy_rmse,R2 = R2_user) %>% left_join(mov_id_name, by=c("target_films_present" = "movieId"))




#sqrt(colMeans((movies_pivoted%>% select('1') - colMeans(movies_pivoted%>% select('1'),na.rm =T) )^2, ,na.rm =T) ) #rmse - Root Mean Squared Error

     