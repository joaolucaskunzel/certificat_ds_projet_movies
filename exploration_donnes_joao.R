library(corrplot)
library(scales)
library(softImpute)
library(FactoMineR)
library("plotly")

#========= Exploration descriptive données ==============
# imort data frame
source("definition_dataframe.R")

cor_mat_films <- cor(movies_pivoted%>% select(-userId), use = 'pairwise.complete.obs')
cor_mat_user <- cor(user_summary%>% select(-userId), use = 'pairwise.complete.obs')
cor_mat_user_toy_story <- cor(user_summary %>%  inner_join(movies_pivoted%>% select(userId,'1'), by = 'userId') %>% select(-userId,), use = 'pairwise.complete.obs')


final_df %>% select(starts_with('avg_')) %>% pivot_longer(cols = everything())  %>%  mutate(name = gsub("_", " ", name)) %>%
  ggplot()+
  aes(x=name, y=value)+
  geom_boxplot()+
  scale_x_discrete(labels = wrap_format(10))


final_df %>% select(starts_with('count_')) %>% pivot_longer(cols = everything())  %>%  mutate(name = gsub("_", " ", name)) %>%
  ggplot()+
  aes(x=name, y=value)+
  geom_boxplot()+
  scale_x_discrete(labels = wrap_format(10))

corrplot(cor_mat_user_toy_story)

plot(final_df %>% select(starts_with('avg_')))

user_movie_df %>% 
  select(userId,movieId, rating, genres) %>% 
  separate(genres, sep = "\\|", c('genre1','genre2','genre3','genre4','genre5','genre6')) %>%
  pivot_longer(c('genre1','genre2','genre3','genre4','genre5','genre6'), names_to = 'genres_all') %>%
  drop_na()  %>% mutate(rating = as.integer(rating)) %>% ggplot()+
    aes(x=value, y=rating)+
    geom_raster()+
    scale_x_discrete(labels = wrap_format(10))

user_movie_df %>% 
  select(userId,movieId, rating, genres) %>% ggplot()+
    aes(x=rating)+
    geom_histogram()

user_movie_df %>% 
  select(userId,movieId, rating, genres) %>% 
  separate(genres, sep = "\\|", c('genre1','genre2','genre3','genre4','genre5','genre6')) %>%
  pivot_longer(c('genre1','genre2','genre3','genre4','genre5','genre6'), names_to = 'genres_all') %>%
  drop_na()  %>% mutate(rating = as.integer(rating)) %>% ggplot()+
    aes(x=rating, fill = value)+
    geom_histogram(aes(y=stat(density*width),fill = value),position = 'dodge')

#==== Matrix completion algorithm

set.seed(1011)

svd_mat <- softImpute(final_df[film_cols], trace=TRUE, type = "svd")

xs <- as(data.matrix(final_df[film_cols]), "Incomplete")
lam0 <- lambda0(xs)

fit0 <- softImpute(xs, lambda = 1 + .2)

inp_matrix <- softImpute::complete(final_df[film_cols], svd_mat)
inp_matrix_reg <- softImpute::complete(final_df[film_cols], fit0)

colnames(inp_matrix) <- (mov_id_name %>% filter(movieId %in% colnames(inp_matrix)) %>% select(title))$title

#=== Clustering ====
# PCA
pca_films <- prcomp(inp_matrix, scale = TRUE)
plot(pca_films)

res.pca=PCA(inp_matrix)
names(res.pca)
barplot(res.pca$eig[,1],
          names=paste("Dim",1:nrow(res.pca$eig)),
          main="inertie expliquée")
names(res.pca$ind)

plot(res.pca,choix="var",habillage=13,cex=.7)


plot(res.pca$ind$coord[,c(1,2)])


#Kmeans
ratio=list()
for (k in 1:12) {
  clusters <- kmeans(t(inp_matrix),centers = k)
  
  #plot(df, col=clusters$cluster+1)
  
  #print(clusters$withinss)
  #print(clusters$tot.withinss)
  
  ratio[k]=clusters$betweenss/clusters$totss
  
}
plot(1:k,ratio)

clust_user_km <-  kmeans(inp_matrix,centers = 4)
clusters_users_km <- clust_user_km$cluster
clust_film_km <-  kmeans(t(inp_matrix),centers = 4)
clusters_films_km <- clust_film_km$cluster

plot(res.pca$ind$coord[,c(1,2)], col=clusters_users_km)
plot(res.pca$var$coord[,c(1,2)], col=clusters_films_km)

# ACH
cah_u <- hclust(dist(inp_matrix), method="ward.D2")
plot(as.dendrogram(cah_u))
plot(sort(cah_u$height,dec=T),type="h")
gpcah <- cutree(cah_u,k=4)
plot(res.pca$ind$coord[,c(1,2)], col=gpcah)

cah_f <- hclust(dist(t(inp_matrix)), method="ward.D2")
plot(as.dendrogram(cah_f))
plot(sort(cah$height,dec=T),type="h")
gpcah <- cutree(cah,k=4)
plot(res.pca$ind$coord[,c(1,2)], col=gpcah)


#==== characterize cluster ================

user_clust <- cbind(final_df['userId'],clusters_users_km)
cluster_movie_df <- user_movie_df %>% inner_join(user_clust, by = 'userId') %>% 
  group_by(clusters_users_km) %>%  mutate(nbr_users_clust =  n_distinct(userId)) %>% 
  ungroup()

genre_by_cluster <- cluster_movie_df  %>% 
  select(userId,clusters_users_km,movieId, rating, genres, nbr_users_clust) %>% 
  separate(genres, sep = "\\|", c('genre1','genre2','genre3','genre4','genre5','genre6')) %>%
  pivot_longer(c('genre1','genre2','genre3','genre4','genre5','genre6'), names_to = 'genres_all') %>%
  drop_na()%>%
  group_by(clusters_users_km, value) %>% 
  summarise(
    avg_genre = mean(rating), 
    #sd = sd(rating), 
    avg_count_gen = n()/max(nbr_users_clust) ) %>%
  filter(value!="(no genres listed)")%>%
  pivot_wider(values_from = c(avg_genre,avg_count_gen), names_from = value)

decedes_by_cluster <- cluster_movie_df %>% 
  select(clusters_users_km,movieId, rating, year, nbr_users_clust) %>% 
  mutate(dec_film = (floor(year/10)*10)) %>%
  mutate(dec_film = case_when(
    dec_film < 1950 ~ "<40",
    dec_film >= 1950 & dec_film <= 1969 ~ "50-60",
    dec_film >= 1970 & dec_film <= 1989 ~ "70-80",
    dec_film >= 1990 & dec_film <= 2009 ~ "90-10",
    dec_film >= 2010 ~ ">10",
  )) %>%
  group_by(clusters_users_km, dec_film) %>% 
  summarise(
    avg_dec = mean(rating), 
    count_dec = n()/max(nbr_users_clust)) %>%
  #filter(count_dec >= 5 ) %>% ## il faut au moins avoir vu 5 films d'une decennie
  pivot_wider(values_from = c(avg_dec,count_dec), names_from = dec_film) %>%
  mutate_at(vars("count_dec_<40","count_dec_50-60","count_dec_70-80","count_dec_90-10","count_dec_>10"), ~replace_na(., 0))

cluster_summary <- cluster_movie_df %>% 
  group_by(clusters_users_km) %>% 
  summarise(
    #nbr_films_watched = n_distinct(movieId),
    #average_rating = mean(rating),
    #oldest_film_watched = min(date_watch),
    #last_film_watched_ts = min(timestamp),
    oldest_film_watched_ts = mean(year)-1900
  ) %>% 
  merge(genre_by_cluster) %>%
  merge(decedes_by_cluster)

cluster_summary$clusters_users_km<- as.integer(cluster_summary$clusters_users_km)


cluster_summary %>% pivot_longer(cols = -clusters_users_km, names_to = 'names') %>%  
  ggplot() +
  aes(x=clusters_users_km, y=value, fill=factor(clusters_users_km)) +
  geom_col(stat='sum', position = 'dodge2') +
  facet_wrap(vars(names), scales = 'free_y')
