# Import des packages #
library(tidyverse)
library(tidymodels)
library(kernlab)
library(kknn)
library(sqldf)
library(reshape2)
library(doParallel)
library(glmnet)

Y_films<-as.data.frame(readRDS("data/Y_films"))

Y_films <- dcast(Y_films,userId ~ movieId , value.var = 'rating')
colnames(Y_films) <- c("userId", paste("movie", colnames(Y_films[-c(1)]), sep = "_"))

# import des data frames
X_ts <- readRDS('data/user_summary_v4_no_target_films')

# Fusion en une table
Table_initiale <- merge(X_ts, Y_films, by='userId', all.y = T)
#Table réduite à utiliser pour tester si le code fonctionne
#Table_initiale_reduite <- Table_initiale[1:1000,]


# Definition training et testing set
final_df_split <- initial_split(Table_initiale, prop = 0.8) 
final_df_train_base <- training(final_df_split)
final_df_test_base <- testing(final_df_split)

# Table erreur de prévision
Var_names <- c('film','Erreur_lm', 'Erreur_lasso', 'Erreur_rf', 'Erreur_xg', #'Erreur_svm',
               'Erreur_knn')
Prevision <- data.frame()
for (k in Var_names) Prevision[[k]] <- as.character()

# Liste de film #
Film <- grep("movie", names(Table_initiale), value=TRUE)

# Boucle pour tuner les hyper-paramètres par film
for (i in Film){
  try({  
    final_df_train <- final_df_train_base
    final_df_test <- final_df_test_base
    
    final_df_train$Y <- eval(parse(text=(paste0("final_df_train[,c('",i,"')]")))) 
    final_df_train <- final_df_train[ !is.na(final_df_train$Y) , !(names(final_df_train) %in% Film)]
    
    final_df_test$Y <- eval(parse(text=(paste0("final_df_test[,'",i,"']")))) 
    final_df_test <- final_df_test[ !is.na(final_df_test$Y), !(names(final_df_test) %in% Film)]
    
    ## Recette de base ##
    rec_basic <- 
      recipe(Y ~., data = final_df_train) %>% 
      step_impute_mean(all_numeric_predictors()) %>%
      step_impute_mode(all_nominal_predictors()) %>% 
      step_normalize(all_numeric_predictors()) %>% 
      #step_other(trajet, threshold = 0.05) %>%
      step_zv(all_nominal_predictors()) %>% 
      step_dummy(all_nominal_predictors()) %>%
      #step_naomit() %>% 
      prep()
    
    ## Validation croisée
    
    ames_cv_folds <- 
      recipes::bake(
        rec_basic, 
        new_data = final_df_train
      ) %>%  
      rsample::vfold_cv(v = 4)
    
    ## Regréssion linéaire ##
    
    log_model <- linear_reg() %>% 
      set_engine("glm") %>% 
      set_mode("regression")
    
    log_wflow <- 
      workflow() %>% 
      add_recipe(rec_basic) %>%
      # add_recipe(rec_interaction) %>%
      # add_recipe(rec_spline) %>%
      add_model(log_model)
    
    log_fit <- fit(log_wflow, final_df_train)
    A <- predict(log_fit, final_df_test)
    final_df_test$predict_lm <- as.numeric(A$.pred)
    Erreur_lm <- (1 - sum(( final_df_test$predict_lm[!is.na(final_df_test$Y)] - final_df_test$Y[!is.na(final_df_test$Y)])^2)/sum((final_df_test$Y[!is.na(final_df_test$Y)] - mean(final_df_test$Y, na.rm=T))^2))
    
    print(paste0("Fait pour LR et film ",i))
    
    ## Lasso ##
    
   lasso_spec <- linear_reg(penalty = tune(), mixture = 1) %>%
      set_engine("glmnet")%>% 
      set_mode("regression")
    
    lasso_wflow <- 
      workflow() %>% 
      add_recipe(rec_basic) 
    
    lambda_grid <- grid_regular(penalty(), levels = 50)
    
    lasso_grid <- tune_grid(
      lasso_wflow %>% add_model(lasso_spec),
      resamples = ames_cv_folds,
      grid = lambda_grid  )
  
    lasso_grid %>%
      collect_metrics()
    
    lowest_rmse <- lasso_grid %>%
      select_best("rmse", maximize = FALSE)
    
    
    lasso_fit <- fit(final_lasso, final_df_train)
    A <- predict(lasso_fit, final_df_test)
    final_df_test$predict_lasso <- as.numeric(A$.pred)
    Erreur_lasso <- (1 - sum(( final_df_test$predict_lasso[!is.na(final_df_test$Y)] - final_df_test$Y[!is.na(final_df_test$Y)])^2)/sum((final_df_test$Y[!is.na(final_df_test$Y)] - mean(final_df_test$Y, na.rm=T))^2))
    
    print(paste0("Fait pour LASSO et film ",i))  
  
    
    ## Random forest ##
    rf_model <- 
      rand_forest() %>%
      #rand_forest(trees = tune()) %>% 
      set_engine("ranger") %>% 
      set_mode("regression")
    
    rf_model <- 
      parsnip::rand_forest(
        mode = "regression",
        trees = 500,
        min_n = tune(),
        mtry = tune(),
      ) %>%
      set_engine("ranger")
    
    rf_workflow <- 
      workflow() %>% 
      add_recipe(recipe = rec_basic) %>%
      add_model(rf_model)
    
    tune_res <- tune_grid(
      rf_workflow,
      resamples = ames_cv_folds,
      grid = 2
    )
    
    rf_grid <- grid_regular(
      mtry(range = c(10, 30)),
      min_n(range = c(2, 8)),
      levels = 2
    )
    
    regular_res <- tune_grid(
      rf_workflow,
      resamples = ames_cv_folds,
      grid = rf_grid
    )
    
    best_auc <- select_best(regular_res, "rmse")
    final_rf <- finalize_model(
      rf_model,
      best_auc
    )
    
    
    final_rf <- workflow() %>%
      add_recipe(rec_basic) %>%
      add_model(final_rf)
    
    
    rf_fit <- 
      fit(final_rf, final_df_train[!is.na(final_df_train$Y),])
    
    A <- predict(rf_fit, final_df_test)
    
    final_df_test$predict_rf <- as.numeric(A$.pred)
    Erreur_rf <- (1 - sum(( final_df_test$predict_rf[!is.na(final_df_test$Y)] - final_df_test$Y[!is.na(final_df_test$Y)])^2)/sum((final_df_test$Y[!is.na(final_df_test$Y)] - mean(final_df_test$Y, na.rm=T))^2))
    
    print(paste0("Fait pour RF et film ",i))
    
  
    
    ### Gradient Boosting
    
    xgboost_model <- 
      parsnip::boost_tree(
        mode = "regression",
        trees = 1000,
        min_n = tune(),
        tree_depth = tune(),
        learn_rate = tune(),
        loss_reduction = tune()
      ) %>%
      set_engine("xgboost", objective = "reg:squarederror")
    
    xgboost_params <- 
      dials::parameters(
        min_n(),
        tree_depth(),
        learn_rate(),
        loss_reduction()
      )
    
    xgboost_grid <- 
      dials::grid_max_entropy(
        xgboost_params, 
        size = 5
      )
    
    xgboost_workflow <- 
      workflow() %>% 
      add_recipe(rec_basic) %>% # rec_interaction, rec_spline
      # add_recipe(rec_interaction) %>%
      # add_recipe(rec_spline) %>%
      add_model(xgboost_model)
    
    xgboost_tuned <- tune::tune_grid(
      object = xgboost_workflow,
      resamples = ames_cv_folds,
      grid = xgboost_grid,
      metrics = yardstick::metric_set(rmse, rsq, mae),
      control = tune::control_grid(verbose = TRUE)
    )
    
    
    xgboost_tuned %>%
      tune::show_best(metric = "rmse") %>%
      knitr::kable()
    
    xgboost_best_params <- xgboost_tuned %>%
      tune::select_best("rmse")
    knitr::kable(xgboost_best_params)
    
    xgboost_model_final <- xgboost_model %>% 
      finalize_model(xgboost_best_params)
  
    train_processed <- bake(rec_basic,  new_data = final_df_train)
    
    train_prediction <- xgboost_model_final %>%
      # fit the model on all the training data
      fit(
        formula = Y ~ ., 
        data    = final_df_train
      ) %>%
      # predict the Y for the training data
      predict(new_data = train_processed) %>%
      bind_cols(final_df_train)
    
#   Erreur_xg_train <- (1 - sum(( train_prediction$Y - train_prediction$.pred)^2)/sum((train_prediction$Y - mean(train_prediction$Y, na.rm=T))^2))
    
    test_processed  <- bake(rec_basic, new_data = final_df_test)
    test_prediction <- xgboost_model_final %>%
      # fit the model on all the training data
      fit(
        formula = Y ~ ., 
        data    = train_processed
      ) %>%
      # use the training model fit to predict the test data
      predict(new_data = test_processed) %>%
      bind_cols(final_df_test)
    
    Erreur_xg <- (1 - sum(( test_prediction$Y - test_prediction$.pred)^2)/sum((test_prediction$Y - mean(train_prediction$Y, na.rm=T))^2))
   # RMSE_xg <- sqrt()(1 - sum(( test_prediction$Y - test_prediction$.pred)^2)/sum((test_prediction$Y - mean(train_prediction$Y, na.rm=T))^2))
    
    print(paste0("Fait pour XG et film ",i))   
    
   
    
    ### Finalement pas utilisé car trop long SVM
#    svm <- 
#      svm_poly(degree = tune(), cost = tune(), scale_factor=tune()) %>%
#      set_engine('kernlab') %>% 
#      set_mode("regression")
    
#    svm_workflow <- 
#      workflow() %>% 
#      add_recipe(rec_basic) %>% # rec_interaction, rec_spline
#      # add_recipe(rec_interaction) %>%
#      # add_recipe(rec_spline) %>%
#      add_model(svm)
    
#    tune_res <- tune_grid(
#      svm_workflow,
#      resamples = ames_cv_folds,
#      grid = 20    )
    
#    svm_grid <- grid_regular(
#      degree(),
#      cost(),
#      scale_factor(),
#      levels = 10
#    )
    
#    regular_res <- tune_grid(
#      svm_workflow,
#      resamples = ames_cv_folds,
#      grid = svm_grid
#    )
    
#    best_auc <- select_best(regular_res, "rmse")
#    final_svm <- finalize_model(
#      svm,
#      best_auc
#    )
    
    
#    final_svm <- workflow() %>%
#      add_recipe(rec_basic) %>%
#      add_model(final_svm)
    
    
#    svm_fit <- 
#      fit(final_svm, final_df_train[!is.na(final_df_train$Y),])
#    
#    A <- predict(svm_fit, final_df_test)
    
#    final_df_test$predict_svm <- as.numeric(A$.pred)
#    Erreur_svm <- (1 - sum(( final_df_test$predict_svm[!is.na(final_df_test$Y)] - final_df_test$Y[!is.na(final_df_test$Y)])^2)/sum((final_df_test$Y[!is.na(final_df_test$Y)] - mean(final_df_test$Y, na.rm=T))^2))
    
#    print(paste0("Fait pour SVM et film ",i))    
    
   
    ### Plus proches voisins ###
    
    knn <- 
      nearest_neighbor(neighbors = tune(), dist_power = tune()) %>%
      set_engine('kknn') %>% 
      set_mode("regression")
    
    knn_workflow <- 
      workflow() %>% 
      add_recipe(rec_basic) %>% # rec_interaction, rec_spline
      # add_recipe(rec_interaction) %>%
      # add_recipe(rec_spline) %>%
      add_model(knn)
    
    tune_res <- tune_grid(
      knn_workflow,
      resamples = ames_cv_folds,
      grid = 20    )
    
    knn_grid <- grid_regular(
      neighbors(),
      dist_power(),
      levels = 10
    )
    
    regular_res <- tune_grid(
      knn_workflow,
      resamples = ames_cv_folds,
      grid = knn_grid
    )
    
    best_auc <- select_best(regular_res, "rmse")
    final_knn <- finalize_model(
      knn,
      best_auc
    )
    
    final_knn <- workflow() %>%
      add_recipe(rec_basic) %>%
      add_model(final_knn)
    
    knn_fit <-   fit(final_knn, final_df_train)
    
    A <- predict(knn_fit, final_df_test)
    
    final_df_test$predict_knn <- as.numeric(A$.pred)
    Erreur_knn <- (1 - sum(( final_df_test$predict_knn[!is.na(final_df_test$Y)] - final_df_test$Y[!is.na(final_df_test$Y)])^2)/sum((final_df_test$Y[!is.na(final_df_test$Y)] - mean(final_df_test$Y, na.rm=T))^2))
    
    print(paste0("Fait pour KNN et film ",i))
      
    Prevision_new <- as.data.frame(t(rbind(i, Erreur_lm, 
                                           Erreur_lasso,
                                           Erreur_rf, 
                                           Erreur_xg, 
                                           #Erreur_svm, 
                                           Erreur_knn)))
    
    Prevision <- rbind(Prevision, Prevision_new)
    
    #J'enregistr les résultats de la prévision
    write.csv(Prevision,"C:/Users/gabri/Documents/Projet_data/Y_films (1)/Prevision.csv")
    print(paste0("Fait pour ",i))
  }, silent=TRUE)      # end of try function
  
  
}

# Je sélectionne la meilleure méthode
Prevision$best <- colnames(Prevision)[apply(Prevision,1,which.max)]

# Pour chaque film, je fais tourner le meilleur modèle sur toute la base pour estimer les notes manquantes

for (i in Prevision$i){
  Table_initiale$Y <- eval(parse(text=(paste0("Table_initiale[,c('",i,"')]")))) 
  Table_estimation <- Table_initiale[ , !(names(Table_initiale) %in% Film)]
  print(Prevision$best[Prevision$i == i])
  
  if (Prevision$best[Prevision$i == i] == 'Erreur_rf'){
    
    rf_model <- 
      rand_forest() %>%
      #rand_forest(trees = tune()) %>% 
      set_engine("ranger") %>% 
      set_mode("regression")
    
    rf_model <- 
      parsnip::rand_forest(
        mode = "regression",
        trees = 500,
        min_n = tune(),
        mtry = tune(),
      ) %>%
      set_engine("ranger",importance = "impurity")
    
    rf_workflow <- 
      workflow() %>% 
      add_recipe(recipe = rec_basic) %>%
      add_model(rf_model)
    
    tune_res <- tune_grid(
      rf_workflow,
      resamples = ames_cv_folds,
      grid = 2
    )
    
    rf_grid <- grid_regular(
      mtry(range = c(10, 30)),
      min_n(range = c(2, 8)),
      levels = 2
    )
    
    regular_res <- tune_grid(
      rf_workflow,
      resamples = ames_cv_folds,
      grid = rf_grid
    )
    
    best_auc <- select_best(regular_res, "rmse")
    final_rf <- finalize_model(
      rf_model,
      best_auc
    )
    
    
    final_rf <- workflow() %>%
      add_recipe(rec_basic) %>%
      add_model(final_rf)
    
    
    rf_fit <- 
      fit(final_rf, Table_estimation[!is.na(Table_estimation$Y),])
    
    A <- predict(rf_fit, Table_estimation[!is.na(Table_estimation$Y),])
    
    eval(parse(text = (paste0("colnames(A) <- '",i,"'"))))
    
  } else if(Prevision$best[Prevision$i == i] == 'Erreur_xg') {
    
    xgboost_model <- 
      parsnip::boost_tree(
        mode = "regression",
        trees = 1000,
        min_n = tune(),
        tree_depth = tune(),
        learn_rate = tune(),
        loss_reduction = tune()
      ) %>%
      set_engine("xgboost", objective = "reg:squarederror")
    
    xgboost_params <- 
      dials::parameters(
        min_n(),
        tree_depth(),
        learn_rate(),
        loss_reduction()
      )
    
    xgboost_grid <- 
      dials::grid_max_entropy(
        xgboost_params, 
        size = 5
      )
    
    xgboost_workflow <- 
      workflow() %>% 
      add_recipe(rec_basic) %>% # rec_interaction, rec_spline
      # add_recipe(rec_interaction) %>%
      # add_recipe(rec_spline) %>%
      add_model(xgboost_model)
    
    xgboost_tuned <- tune::tune_grid(
      object = xgboost_workflow,
      resamples = ames_cv_folds,
      grid = xgboost_grid,
      metrics = yardstick::metric_set(rmse, rsq, mae),
      control = tune::control_grid(verbose = TRUE)
    )
    
    
    xgboost_tuned %>%
      tune::show_best(metric = "rmse") %>%
      knitr::kable()
    
    xgboost_best_params <- xgboost_tuned %>%
      tune::select_best("rmse")
    knitr::kable(xgboost_best_params)
    
    xgboost_model_final <- xgboost_model %>% 
      finalize_model(xgboost_best_params)
    
    train_processed <- bake(rec_basic,  new_data = Table_estimation)
    
    train_prediction <- xgboost_model_final %>%
      # fit the model on all the training data
      fit(
        formula = Y ~ ., 
        data    = Table_estimation[!is.na(Table_estimation$Y),]
      ) %>%
      # predict the Y for the training data
      predict(new_data = Table_estimation[!is.na(Table_estimation$Y),]) %>%
      bind_cols(final_df_train)
    
    
    eval(parse(text = (paste0("colnames(A) <- '",i,"'"))))
    
  }  else if(Prevision$best[Prevision$i == i] == 'Erreur_knn'){
    
    knn <- 
      nearest_neighbor(neighbors = tune(), dist_power = tune()) %>%
      set_engine('kknn') %>% 
      set_mode("regression")
    
    knn_workflow <- 
      workflow() %>% 
      add_recipe(rec_basic) %>% # rec_interaction, rec_spline
      # add_recipe(rec_interaction) %>%
      # add_recipe(rec_spline) %>%
      add_model(knn)
    
    tune_res <- tune_grid(
      knn_workflow,
      resamples = ames_cv_folds,
      grid = 20    )
    
    knn_grid <- grid_regular(
      neighbors(),
      dist_power(),
      levels = 10
    )
    
    regular_res <- tune_grid(
      knn_workflow,
      resamples = ames_cv_folds,
      grid = knn_grid
    )
    
    best_auc <- select_best(regular_res, "rmse")
    final_knn <- finalize_model(
      knn,
      best_auc
    )
    
    final_knn <- workflow() %>%
      add_recipe(rec_basic) %>%
      add_model(final_knn)
    
    knn_fit <-   fit(final_knn, final_df_train)
    
    A <- predict(knn_fit, Table_estimation)
    eval(parse(text = (paste0("colnames(A) <- '",i,"'"))))
    
  } else if(Prevision$best[Prevision$i == i] == 'Erreur_lasso'){
    
    lasso_spec <- linear_reg(penalty = tune(), mixture = 1) %>%
      set_engine("glmnet")%>% 
      set_mode("regression")
    
    lasso_wflow <- 
      workflow() %>% 
      add_recipe(rec_basic) 
    
    lambda_grid <- grid_regular(penalty(), levels = 50)
    
    lasso_grid <- tune_grid(
      lasso_wflow %>% add_model(lasso_spec),
      resamples = ames_cv_folds,
      grid = lambda_grid  )
    
    lasso_grid %>%
      collect_metrics()
    
    lowest_rmse <- lasso_grid %>%
      select_best("rmse", maximize = FALSE)
    
    
    lasso_fit <- fit(final_lasso, final_df_train)
   
    A <- predict(lasso_fit, Table_estimation)
    eval(parse(text = (paste0("colnames(A) <- '",i,"'"))))
    
  }
  
  Estimation <- cbind(Estimation,A)
  print(paste0("Fait pour ",i))
}


## Je supprime les notes pour les films déja regardés
for (i in Prevision$i){
  A <- Y_films[,c('userId',i)]
  Estimation[!(Estimation$userId %in% A$userId[is.na(A[c(2)])]) ,c(i)] <- ''
}

Estimation$best <- colnames(Estimation[-c(1)])[apply(Estimation[-c(1)],1,which.max)]
table(Estimation$best)



write.csv(Estimation,"C:/Users/gabri/Documents/Projet_data/Reco.csv")

