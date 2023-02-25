library(corrplot)
library(scales)
library(FactoMineR)
library("plotly")
library(tidyverse)
library(ggridges)

# imort data frames
# user_movie_df <- readRDS('user_movie_df')

final_df <- readRDS('user_summary_v4_no_target_films')
Y_ts <- readRDS('Y_ts') 
Y_films <- readRDS('Y_films')


final_df<-final_df %>% mutate(across(.cols = starts_with('avg_'), .fns = ~ na_if(.x,0)))

final_df_norm <- final_df %>% 
                  mutate(across(.cols = starts_with('avg_'), .fns = ~ (. - average_rating))) %>% 
                  mutate(across(.cols = starts_with('count_'), .fns = ~ (.)/nbr_films_watched)) 


#========= Exploration descriptive données ==============
# distribution Nbr films watched by user
final_df %>% filter(nbr_films_watched>=25 & nbr_films_watched<=500) %>% select(nbr_films_watched) %>% 
  ggplot()+
  aes(x=nbr_films_watched)+
  geom_histogram(bins=250)

# distribution of users avg rating
final_df %>% filter(nbr_films_watched>=25 & nbr_films_watched<=500) %>% select(average_rating) %>%  
  ggplot()+
  aes(x=average_rating)+
  geom_histogram(bins=250)

# distribution of users avg rating and nbr_films
final_df %>% filter(nbr_films_watched>=25 & nbr_films_watched<=750) %>% select(average_rating, nbr_films_watched) %>%  
  ggplot()+
  aes(y=average_rating, x=nbr_films_watched)+
  geom_bin2d(bins=125)

# scatter plot of users avg rating and nbr_films
final_df %>% filter(nbr_films_watched>=25 & nbr_films_watched<=750) %>% 
  select(average_rating, nbr_films_watched) %>%
  sample_frac(size = 0.3) %>% 
  ggplot()+
  aes(y=average_rating, x=nbr_films_watched)+
  geom_point()

# distribution plot of users avg rating by binned nbr_films
final_df %>% filter(nbr_films_watched>=5 & nbr_films_watched<=30000) %>% 
  select(average_rating, nbr_films_watched) %>%
  mutate(nb_films_watch = cut(nbr_films_watched, breaks=c(0,20, 200, 750, 30000))) %>% 
  ggplot()+
  aes(x=average_rating, fill=nb_films_watch)+
  geom_histogram(aes(y = ..density..),position='identity',bins=125, alpha=0.5)+
  facet_wrap(~nb_films_watch, nrow=4)

# dist. oldest film watched
final_df %>% filter(nbr_films_watched>=25 & nbr_films_watched<=500) %>% 
  filter(oldest_film_watched_ts>1910 & oldest_film_watched_ts<2000) %>% 
  select(oldest_film_watched_ts) %>%  
  ggplot()+
  aes(x=oldest_film_watched_ts)+
  stat_ecdf()+
  scale_x_continuous(breaks=c(seq(1910,2000,10)))+
  scale_y_continuous(breaks=c(seq(0,1,0.125)))


#==notes et vues par genre
final_df %>% filter(nbr_films_watched>=25) %>%  select(starts_with('avg_genre')) %>% 
  na_if(0) %>% 
  pivot_longer(cols = everything())  %>%  mutate(name = gsub("_", " ", name)) %>%
  ggplot()+
  aes(x=name, y=value)+
  geom_boxplot()+
  scale_x_discrete(labels = wrap_format(10))

final_df %>% filter(nbr_films_watched>=25 & nbr_films_watched<=150) %>% select(starts_with('count_gen')) %>% 
  pivot_longer(cols = everything())  %>%  mutate(name = gsub("_", " ", name)) %>%
  ggplot()+
  aes(x=name, y=value)+
  geom_boxplot()+
  scale_x_discrete(labels = wrap_format(10))


#==notes et vues par decenie
final_df %>% filter(nbr_films_watched>=25) %>%  select(starts_with('avg_dec')) %>% 
  na_if(0) %>% 
  pivot_longer(cols = everything())  %>%  mutate(name = gsub("_", " ", name)) %>%
  ggplot()+
  aes(x=name, y=value)+
  geom_boxplot()+
  scale_x_discrete(labels = wrap_format(10))

final_df %>% filter(nbr_films_watched>=25) %>% select(starts_with('count_dec')) %>% 
  pivot_longer(cols = everything())  %>%  mutate(dec = gsub("_", " ", name)) %>%
  ggplot(aes(x=value, y=dec, group = dec))+
  geom_density_ridges(alpha=0.6) +
  theme_ridges() + 
  theme(legend.position = "none")+
  xlim(c(0,150))
  # scale_x_discrete(labels = wrap_format(10))

# Correlation plots for X
cor_mat_films_gen <- cor(final_df_norm%>%select(nbr_films_watched,average_rating,starts_with('avg_gen'),
                                           starts_with('count_gen')), use = 'pairwise.complete.obs')
corrplot(cor_mat_films_gen[2:21,c(2:21)])

cor_mat_films_dec <- cor(final_df_norm%>%select(nbr_films_watched,average_rating,starts_with('avg_dec'),
                                           starts_with('count_dec')), use = 'pairwise.complete.obs')
corrplot(cor_mat_films_dec[2:7,]) #

cor_mat_films_dec_gen <- cor(final_df_norm%>%select(nbr_films_watched,average_rating,starts_with('avg_gen'),
                                               starts_with('count_gen'),starts_with('avg_dec'),
                                               starts_with('count_dec')), use = 'pairwise.complete.obs')
corrplot(cor_mat_films_dec_gen[2:21,c(1,22:50)]) #
corrplot(cor_mat_films_dec_gen[21:50,c(1,22:50)]) #

cor_mat_films_gen_dir_act <- cor(final_df_norm%>%select(nbr_films_watched,average_rating,
                                                   starts_with('avg_gen'),starts_with('count_gen'),
                                                   starts_with('avg_dir'),starts_with('count_dir'),
                                                   starts_with('avg_dir'),starts_with('count_dir'),
                                                   starts_with('avg_act'),starts_with('count_act')),
                                 use = 'pairwise.complete.obs')

corrplot(cor_mat_films_gen_dir_act[2:21,c(41:120)])

#======== Toy Story =============

df_ts_cor <- final_df_norm %>% inner_join(Y_ts) %>% rename(Toy_Story = 'rating') %>% select(-starts_with('sd_'))


cor_ts <- cor(df_ts_cor, use = 'pairwise.complete.obs')

corrplot(cor_ts[c(3,133),1:42])

corrplot(cor_ts[c(3,133),43:52])

corrplot(cor_ts[c(3,133),53:132])

df_ts <- final_df %>% inner_join(Y_ts) %>% rename(Toy_Story = 'rating')
df_ts_norm <- final_df_norm %>% inner_join(Y_ts) %>% rename(Toy_Story = 'rating')


df_ts %>%  select(Toy_Story) %>% 
  ggplot()+
  aes(Toy_Story)+
  geom_histogram()


df_ts %>% sample_frac(1) %>% 
  select(Toy_Story,average_rating) %>% 
  ggplot()+
  aes(x = average_rating, y = Toy_Story)+
  geom_point()+
  theme_bw()


df_ts %>% sample_frac(1) %>% 
  select(Toy_Story,average_rating) %>% 
  group_by(Toy_Story) %>% summarise(average_rating=mean(average_rating)) %>% 
  ggplot()+
  aes(x = average_rating, y = Toy_Story)+
  geom_point()+
  theme_bw()

df_ts_norm %>% sample_frac(0.05) %>% 
  select(Toy_Story,average_rating, sd_user, starts_with('avg_genre')) %>% 
  gather(starts_with('avg_genre'), key = "var", value = "value") %>% 
  ggplot()+
  aes(x = value, y = Toy_Story)+
    geom_point()+
    facet_wrap(~ var, scales = "free") +
    theme_bw()

df_ts%>% sample_frac(1) %>% 
  select(Toy_Story,average_rating, sd_user, starts_with('avg_genre')) %>% 
  group_by(Toy_Story) %>% summarise_all(mean, na.rm = TRUE) %>% 
  gather(starts_with('avg_genre'), key = "var", value = "value") %>% 
  ggplot()+
  aes(x = value, y = Toy_Story)+
  geom_point()+
  facet_wrap(~ var, scales = "free") +
  theme_bw()

df_ts_norm %>% sample_frac(1) %>% 
  select(Toy_Story,average_rating, sd_user, starts_with('avg_genre')) %>% 
  group_by(Toy_Story) %>% summarise_all(mean, na.rm = TRUE) %>% 
  gather(starts_with('avg_genre'), key = "var", value = "value") %>% 
  ggplot()+
    aes(x = value, y = Toy_Story)+
    geom_point()+
    facet_wrap(~ var, scales = "free") +
    theme_bw()



df_ts_norm %>% sample_frac(0.5) %>% 
  select(Toy_Story,average_rating, starts_with('avg_genre')) %>% 
  gather(starts_with('avg_genre'), key = "var", value = "value") %>% 
  ggplot()+
    aes(x = value, y = Toy_Story)+
    geom_bin2d(bins=9)+
    facet_wrap(~ var, scales = "free") +
    theme_bw()

df_ts_norm %>% sample_frac(1) %>% 
  select(Toy_Story,nbr_films_watched, starts_with('count_gen')) %>% 
  group_by(Toy_Story) %>% summarise_all(mean, na.rm = TRUE) %>% 
  gather(starts_with('count_gen'), key = "var", value = "value") %>% 
  ggplot()+
    aes(x = value, y = Toy_Story)+
    geom_point()+
    facet_wrap(~ var, scales = "free") +
    theme_bw()



df_ts_norm %>% sample_frac(0.5) %>% 
  select(Toy_Story,starts_with('avg_dir')) %>% 
  group_by(Toy_Story) %>% summarise_all(mean, na.rm = TRUE) %>% 
  gather(starts_with('avg_dir'), key = "var", value = "value") %>% 
  ggplot()+
  aes(x = value, y = Toy_Story)+
    geom_point()+
    facet_wrap(~ var, scales = "free") +
    theme_bw()


df_ts %>% filter(nbr_films_watched>=20) %>%  select(Toy_Story, starts_with('avg_genre_Children')) %>% 
  mutate(avg_genre_Children = cut(avg_genre_Children, breaks=c(0,2.5, 3.5, 4, 4.5, 5))) %>% 
  drop_na() %>% 
  ggplot()+
    aes(x=Toy_Story, fill=avg_genre_Children)+
    geom_histogram(aes(y = ..density..),position='identity',bins=125, alpha=0.5)+
    facet_wrap(~avg_genre_Children, nrow=5)


df_ts %>% filter(nbr_films_watched>=20) %>%  select(Toy_Story, starts_with('avg_genre_Horror')) %>% 
  mutate(avg_genre_Horror = cut(avg_genre_Horror, breaks=c(0,2.5, 3.5, 4, 4.5, 5))) %>% 
  drop_na() %>% 
  ggplot()+
  aes(x=Toy_Story, fill=avg_genre_Horror)+
  geom_histogram(aes(y = ..density..),position='identity',bins=125, alpha=0.5)+
  facet_wrap(~avg_genre_Horror, nrow=5)

df_ts %>% filter(nbr_films_watched>=20) %>%  select(Toy_Story, starts_with('count_gen_Children')) %>% 
  mutate(count_gen_Children = cut(count_gen_Children, breaks=c(0,5, 20, 50, 100, 500))) %>% 
  drop_na() %>% 
  ggplot()+
  aes(x=Toy_Story, fill=count_gen_Children)+
  geom_histogram(aes(y = ..density..),position='identity',bins=125, alpha=0.5)+
  facet_wrap(~count_gen_Children, nrow=5)

  











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

#=== Clustering ====
# PCA
final_df_mean_col <- na_mean(final_df)


pca_films <- prcomp(final_df_mean_col, scale = TRUE)
plot(pca_films)

res.pca=PCA(final_df)
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
  clusters <- kmeans(final_df_mean_col,centers = k)
  
  #plot(df, col=clusters$cluster+1)
  
  #print(clusters$withinss)
  #print(clusters$tot.withinss)
  
  ratio[k]=clusters$betweenss/clusters$totss
  
}
plot(1:k,ratio)

clust_user_km <-  kmeans(final_df_mean_col,centers = 4)
clusters_users_km <- clust_user_km$cluster
# clust_film_km <-  kmeans(t(inp_matrix),centers = 4)
# clusters_films_km <- clust_film_km$cluster

plot(res.pca$ind$coord[,c(1,2)], col=clusters_users_km)
# plot(res.pca$var$coord[,c(1,2)], col=clusters_films_km)

# ACH
# cah_u <- hclust(dist(inp_matrix), method="ward.D2")
# plot(as.dendrogram(cah_u))
# plot(sort(cah_u$height,dec=T),type="h")
# gpcah <- cutree(cah_u,k=4)
# plot(res.pca$ind$coord[,c(1,2)], col=gpcah)
# 
# cah_f <- hclust(dist(t(inp_matrix)), method="ward.D2")
# plot(as.dendrogram(cah_f))
# plot(sort(cah$height,dec=T),type="h")
# gpcah <- cutree(cah,k=4)
# plot(res.pca$ind$coord[,c(1,2)], col=gpcah)


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
