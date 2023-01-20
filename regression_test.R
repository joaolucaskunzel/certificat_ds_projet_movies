#https://towardsdatascience.com/xgboost-is-not-black-magic-56ca013144b4
source("definition_dataframe.R")

library(tidyverse)
library(corrplot)

library(xgboost)
library(caret)     
library(imputeTS)

library("softImpute")

library(rAmCharts)

cor_mat <- cor(movies_pivoted%>% select(-userId), use = 'pairwise.complete.obs')
corrplot(cor_mat[1:30,1:30])


#plot(movies_pivoted%>% select('293','350',-userId)  %>% drop_na())




#====================XGBOOST================

set.seed(1)

target_film = '1'

cor_film <- cor_mat[,target_film] 
films_high_cor<-names(cor_film[abs(cor_film)>=0.0])

#====mat complete=====
svd_mat <- softImpute(final_df[film_cols], trace=TRUE, type = "svd")

xs <- as(data.matrix(final_df[film_cols]), "Incomplete")
lam0 <- lambda0(xs)

fit0 <- softImpute(xs, lambda = 1 + .2)

inp_matrix <- softImpute::complete(final_df[film_cols], svd_mat)
inp_matrix_reg <- softImpute::complete(final_df[film_cols], fit0)

completed_user_movie_df <- cbind(final_df[c('userId',user_cols)], inp_matrix) %>%
      select(-target_film) %>% 
      cbind(final_df[target_film]) %>% 
      drop_na(target_film)

tage_inc<-c("userId","oldest_film_watched_ts")

#use 70% of dataset as training set and 30% as test set 
# 
# final_df_filt <- final_df %>% select(tage_inc,starts_with('avg_'), starts_with('count_'),films_high_cor)
# train <- final_df_filt %>% drop_na(target_film) %>% sample_frac(0.70)  %>% replace(is.na(.), 0)
# test  <- final_df_filt %>% drop_na(target_film) %>% anti_join(train, by = 'userId') %>% replace(is.na(.), 0)
# 
# train <- na_mean(train)
# test <- na_mean(test)

final_df_filt <- completed_user_movie_df %>% select(tage_inc,starts_with('avg_'),starts_with('count_'),films_high_cor, target_film) #,films_high_cor

train <- final_df_filt %>% sample_frac(0.9)
test  <- final_df_filt %>% anti_join(train, by = 'userId')

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

rmse_pred = sqrt(mean((target_test - pred_y)^2)) #rmse - Root Mean Squared Error
print(paste0('predicted RMSE: ',rmse_pred))

rmse_mean = sqrt(mean((target_test - mean(target_train))^2)) #rmse - Root Mean Squared Error
print(paste0('Dummy RMSE: ', rmse_mean))

print(paste0('improvement: ', (rmse_mean - rmse_pred), ' (', 100*(rmse_mean - rmse_pred)/rmse_mean, ' %)'))

print(paste0('R2: ',1 - sum((target_test - pred_y)^2)/sum((target_test - mean(target_train))^2)))


plot(target_test, pred_y)
# 
 plot(target_test, pred_y - target_test)

#colnames(final_df)

importance <- xgb.importance(feature_names = colnames(data_train), model = model)
head(importance)

barplot(importance$Gain, names.arg=importance$Feature, horiz)





amBarplot(data= importance, x='Feature',y='Gain', horiz =TRUE)

# soft impute test

svd_mat_y <- softImpute(final_df[c('userId',film_cols)]  %>% inner_join(train['userId']), trace=TRUE, type = "svd")

df_svd_to_test <- final_df[c('userId',film_cols)]  %>% inner_join(test['userId'])
df_svd_to_test[target_film] <- NA
inp_matrix_y_svd <- softImpute::complete(df_svd_to_test, svd_mat_y)

pred_y_svd <- inp_matrix_y_svd[[target_film]]

rmse_pred_svd = sqrt(mean((target_test - pred_y_svd)^2)) #rmse - Root Mean Squared Error
print(paste0('predicted RMSE: ',rmse_pred_svd))

print(paste0('improvement: ', (rmse_mean - rmse_pred_svd), ' (', 100*(rmse_mean - rmse_pred_svd)/rmse_mean, ' %)'))

