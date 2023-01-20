readRDS("user_summary")
readRDS("movies_pivoted")

inp_movies_df<-read.csv("ml-latest/movies.csv")

final_df <- user_summary
final_df_w_films <- user_summary %>% inner_join(movies_pivoted, by = 'userId')

mov_id_name <- inp_movies_df %>% select(movieId,title = title_str)
df_colmuns = (colnames(final_df_w_films))

user_cols <- df_colmuns[2:72]
film_cols <-df_colmuns[73:577]
