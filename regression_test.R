#https://towardsdatascience.com/xgboost-is-not-black-magic-56ca013144b4
#source("definition_dataframe.R")
inp_ratings_df<-read.csv("ml-latest/ratings.csv")

# user_summary_no_ts<-readRDS("user_summary_v2_no_ts")
user_summary<-readRDS("user_summary_v3_no_ts")

# test_dif=user_summary %>% filter(userId %in% user_summary_no_ts$userId)-user_summary_no_ts

Y_ts<-readRDS("Y_ts")


library(tidyverse)
library(corrplot)

library(xgboost)
library(caret)     

library(rAmCharts)

#====================XGBOOST================

set.seed(1)

target_film = '1'
Y =  inp_ratings_df %>% filter(movieId == target_film) %>% select(userId,rating) 

final_df_filt <- user_summary %>% #select(-starts_with('avg_act'),-starts_with('count_act'),-starts_with('sd_act'),-starts_with('avg_dir'),-starts_with('count_dir'),-starts_with('sd_dir')) %>% 
  left_join(Y_ts)%>% 
  drop_na(rating)
#[,colnames(user_summary) %in% sc]
final_df_filt<- final_df_filt%>%
                mutate_all(~replace_na(.,0)) %>% 
                filter(nbr_films_watched>=20)
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
bstSparse <- xgboost(data = data_train, label = target_train, max.depth = 5, eta = 0.3, nrounds = nround_cv, objective = "reg:squarederror")


#================TEST=============
pred_y = predict(bstSparse, data_test)
#pred_y_round = round(pred_y*2)/2

rmse_pred = sqrt(mean((target_test - pred_y)^2)) #rmse - Root Mean Squared Error
print(paste0('predicted RMSE: ',rmse_pred))

rmse_mean = sqrt(mean((target_test - mean(target_train))^2)) #rmse - Root Mean Squared Error
print(paste0('Dummy RMSE: ', rmse_mean))

print(paste0('improvement: ', (rmse_mean - rmse_pred), ' (', 100*(rmse_mean - rmse_pred)/rmse_mean, ' %)'))

print(paste0('R2: ',1 - sum((target_test - pred_y)^2)/sum((target_test - mean(target_train))^2)))

# 
 plot(target_test, pred_y, lwd = 0.1, type= "p")
# # 
#plot(target_test, pred_y - target_test)
# 
# amPlot(target_test, pred_y)
#  
# boxplot(pred_y~target_test)
# 
# boxplot((pred_y - target_test)~target_test)
# 
# 
# #colnames(final_df)
# 
importance <- xgb.importance(feature_names = colnames(data_train), model = bstSparse)
head(importance,20)
# 
# barplot(importance$Gain, names.arg=importance$Feature, horiz)
# 
# 
# 
# 
# 
 amBarplot(data= head(importance,15), x='Feature',y='Gain', horiz =TRUE)
# 
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

