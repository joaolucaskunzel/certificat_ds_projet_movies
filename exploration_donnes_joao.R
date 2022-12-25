library(corrplot)

#========= Exploration descriptive données ==============
cor_mat_films <- cor(movies_pivoted%>% select(-userId), use = 'pairwise.complete.obs')
cor_mat_user <- cor(user_summary%>% select(-userId), use = 'pairwise.complete.obs')
cor_mat_user_ts <- cor(user_summary %>%  inner_join(movies_pivoted%>% select(userId,'1'), by = 'userId') %>% select(-userId,), use = 'pairwise.complete.obs')

user_summary

corrplot(cor_mat_user_ts)



user_movie_df %>% 
  select(userId,movieId, rating, genres) %>% 
  separate(genres, sep = "\\|", c('genre1','genre2','genre3','genre4','genre5','genre6')) %>%
  pivot_longer(c('genre1','genre2','genre3','genre4','genre5','genre6'), names_to = 'genres_all') %>%
  drop_na()%>%
  group_by(value) %>% 
  summarise(
    avg_genre = mean(rating), 
    sd = sd(rating), 
    count_gen = n())



