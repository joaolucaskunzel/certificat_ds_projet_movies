#https://towardsdatascience.com/xgboost-is-not-black-magic-56ca013144b4
#source("definition_dataframe.R")
#inp_ratings_df<-read.csv("ml-latest/ratings.csv")

# user_summary_no_ts<-readRDS("user_summary_v2_no_ts")
user_summary<-readRDS("data/user_summary_v4_no_target_films")

# test_dif=user_summary %>% filter(userId %in% user_summary_no_ts$userId)-user_summary_no_ts

Y_ts<-readRDS("data/Y_ts")
Y_films<-readRDS("data/Y_films")


library(tidyverse)
library(corrplot)

library(xgboost)
library(caret)     

library(rAmCharts)

#====================XGBOOST================

set.seed(1)

target_film = '1'
# Y =  inp_ratings_df %>% filter(movieId == target_film) %>% select(userId,rating) 

final_df_filt <- user_summary %>% #select(-starts_with('avg_act'),-starts_with('count_act'),-starts_with('sd_act'),-starts_with('avg_dir'),-starts_with('count_dir'),-starts_with('sd_dir')) %>% 
  left_join(Y_ts)%>% 
  drop_na(rating)

# final_df_filt <- final_df_filt %>%  filter(nbr_films_watched>=20) %>% 
#   mutate(across(.cols = starts_with('avg_'), .fns = ~ (. - average_rating)/sd_user)) %>% 
#   mutate(across(.cols = starts_with('count_'), .fns = ~ (.)/nbr_films_watched)) %>% 
#   drop_na()

#[,colnames(user_summary) %in% sc]
final_df_filt<- final_df_filt%>%
                mutate_all(~replace_na(.,0)) %>% 
                filter(nbr_films_watched>=1)
#final_df_filt <- completed_user_movie_df %>% select(tage_inc,starts_with('avg_'),starts_with('count_'),films_high_cor, target_film) #,films_high_cor



train <- final_df_filt %>% sample_frac(0.7)
test  <- final_df_filt %>% anti_join(train, by = 'userId')

data_train <- data.matrix(train %>% select(-userId,-rating))
target_train <- (train %>%  select(rating))[,1]$rating

data_test <- data.matrix(test %>% select(-userId,-rating))
target_test <- (test %>%  select(rating))[,1]$rating

xgb_train = xgb.DMatrix(data = data_train, label = target_train)
xgb_test = xgb.DMatrix(data = data_test, label = target_test)

#===========TRAIN============
watchlist = list(train=xgb_train, test=xgb_test)

model = xgb.train(data = xgb_train, max.depth = 2, eta = 0.3, watchlist=watchlist, nrounds = 500, early_stopping_rounds = 1)
nround_cv <- model$best_iteration
bstSparse <- xgboost(data = data_train, label = target_train, max.depth = 4, eta = 0.3, nrounds = nround_cv, objective = "reg:squarederror")


#================TEST=============
pred_y = predict(bstSparse, data_test)
#pred_y_round = round(pred_y*2)/2

rmse_pred = sqrt(mean((target_test - pred_y)^2)) #rmse - Root Mean Squared Error
print(paste0('predicted RMSE: ',rmse_pred))

rmse_mean = sqrt(mean((target_test - mean(target_train))^2)) #rmse - Root Mean Squared Error
print(paste0('Dummy RMSE: ', rmse_mean))

print(paste0('improvement: ', (rmse_mean - rmse_pred), ' (', 100*(rmse_mean - rmse_pred)/rmse_mean, ' %)'))

print(paste0('R2: ',1 - sum((target_test - pred_y)^2)/sum((target_test - mean(target_train))^2)))

R2_nfilms <- data.frame(cbind(nbr_films_watched=test$nbr_films_watched,target_test,pred_y)) %>% 
  mutate(error = target_test - pred_y) %>% 
  mutate(films_watch_bin = cut(nbr_films_watched, breaks=c(1,20,50,150,500,100000))) %>% 
  group_by(films_watch_bin) %>% summarise(RMSE = sqrt(mean((error)^2)), R2 = 1 - (mean((error)^2))/(rmse_mean**2) )

# 
 plot(target_test, pred_y, lwd = 0.1, type= "p")
 
 target_pred_df <- data.frame(target_test, pred_y) %>% group_by(target_test) %>% 
   summarise(pred_y = median(pred_y))
 
 target_pred_df %>% ggplot(aes(x=target_test, y=pred_y)) +
   geom_point() +
   geom_smooth(method=lm, se=FALSE, col='green', size=0.5)+
   xlim(0,5)+
   ylim(0,5)+
   geom_abline(intercept = 0, slope = 1, color="blue", 
               linetype="dashed", size=0.5)+
 
 
 abline(a=0,b=1, col = "blue", lwd = 2)
 
 
   plot(ylim=c(0,5),xlim=c(0,5))
 lines(predict(lm(pred_y~target_test)),col='green')

  
  
  R2_nfilms %>% drop_na() %>%
   gather('metric','value',-films_watch_bin ) %>% 
   ggplot() +
   aes(x = films_watch_bin, y = value, fill=metric)+
   geom_col(position="dodge", stat="identity")+
   labs(
     title = 'Toy Story  -  Test RMSE and R2 by number of films watched'
   )
   
# qqnorm(pred_y, pch = 1, frame = FALSE)
# qqline(pred_y, col = "steelblue", lwd = 2)
# 
# 
# qqplot(target_test,pred_y)


# # 
#plot(target_test, pred_y - target_test)
# amPlot(target_test, pred_y)
# boxplot(pred_y~target_test)
# boxplot((pred_y - target_test)~target_test)
# #colnames(final_df)
# 
importance <- xgb.importance(feature_names = colnames(data_train), model = bstSparse)
amBarplot(data= head(importance,15), x='Feature',y='Gain', horiz =TRUE)


#========= RF ==============

library(ranger)
library(janitor)

train_rf= janitor::clean_names(train)

rf <- ranger(rating ~ . - user_id, data = train_rf, importance = 'impurity')

pred_y_rf = predict(rf, janitor::clean_names(test))

print(paste0('R2: ',1 - sum((target_test - pred_y_rf$predictions)^2)/sum((target_test - mean(target_train))^2)))

setdiff(colnames(janitor::clean_names(train)), colnames((train))) 

importance_rf <- importance(rf)
amBarplot(data= head(data.frame(Gain = sort(importance_rf, decreasing = TRUE)),15), y='Gain', horiz =TRUE)

qsd = data.frame(value = sort(importance_rf, decreasing = TRUE))

# # soft impute test
# 
# svd_mat_y <- softImpute(final_df[c('userId',film_cols)]  %>% inner_join(train['userId']), trace=TRUE, type = "svd")
# 
# df_svd_to_test <- final_df[c('userId',film_cols)]  %>% inner_join(test['userId'])
# df_svd_to_test[target_film] <- NA
# inp_matrix_y_svd <- softImpute::complete(df_svd_to_test, svd_mat_y)
# 
# pred_y_svd <- inp_matrix_y_svd[[target_film]]
# 
# rmse_pred_svd = sqrt(mean((target_test - pred_y_svd)^2)) #rmse - Root Mean Squared Error
# print(paste0('predicted RMSE: ',rmse_pred_svd))
# 
# print(paste0('improvement: ', (rmse_mean - rmse_pred_svd), ' (', 100*(rmse_mean - rmse_pred_svd)/rmse_mean, ' %)'))

