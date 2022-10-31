#EbT 29-10-2022
### twitter_data_100 means LAST 100 tweets per policitian
twitter_data_1000 <- df_filtered

twitter_data_200 <- as.data.frame(twitter_data_200)
twitter_data_500 <- as.data.frame(twitter_data_500)
twitter_data_1000 <- as.data.frame(twitter_data_1000)
# twitter_data_100 <- as.data.frame(twitter_data_100)

## Selected period
selected_twitter_data <- twitter_data_1000

selected_twitter_data <- selected_twitter_data[selected_twitter_data$created_at > '2022-06-30',]

# write.csv(selected_twitter_data, "filtered_tweets.csv")

## START
politicians <- list()
for (row_num in 1:nrow(selected_twitter_data))
{
  single_tweet <- selected_twitter_data[row_num,]
  politicians <- c(politicians, toString(unlist(single_tweet['Politician_name'])))
}
unique_politicians <- unique(unlist(politicians))


list_hashtags_all_politicians <- list()
for (name_politician in unique_politicians){
  list_hashtags_single <- list()
  for (row_num in 1:nrow(selected_twitter_data)){
    if (selected_twitter_data[row_num,'Politician_name'] == name_politician){
      if (selected_twitter_data[row_num,'hashtags'] != 'NaN') {
        list_hashtags_single <- append(list_hashtags_single, unlist(strsplit(selected_twitter_data[row_num,'hashtags'], ","))) 
      }
    }
  }
  list_hashtags_single <- sapply(list_hashtags_single, trimws)
  list_hashtags_single <- sapply(list_hashtags_single, tolower)
  unique_single_politician <- unique(unlist(list_hashtags_single))
  

  print('-------------')
  name_and_hashtags <- append(name_politician, unique_single_politician)
  print(name_and_hashtags)
  list_hashtags_all_politicians <- append(list_hashtags_all_politicians, list(name_and_hashtags))
}

## create matrix
adjacency.matrix <- data.frame(matrix(ncol = length(unique_politicians), nrow = length(unique_politicians)))
rownames(adjacency.matrix) <- unique_politicians
colnames(adjacency.matrix) <- unique_politicians

## start comparing hashtags and fill in matrix
for (first_politician in list_hashtags_all_politicians){
  
  for (second_politician in list_hashtags_all_politicians){
    # create list with all hashtags for SECOND
    second_to_compare <- list()
    for (second_cmpr in 2:length(second_politician)){
      second_to_compare <- append(second_to_compare, second_politician[second_cmpr])  
      second_to_compare <- unlist(second_to_compare)
    }
    
    count_same_hashtag <- 0
    for (first_cmpr in 2:length(first_politician)){
      if (first_politician[first_cmpr] %in% second_to_compare){
        count_same_hashtag <- count_same_hashtag + 1
      }
    }
    adjacency.matrix[first_politician[1], second_politician[1]] <- count_same_hashtag
  }
  
}

# save matrix with XX days
matrix_1000 <- adjacency.matrix

#create graph and matrix
adjacency.matrix <- data.matrix(adjacency.matrix)
diag(adjacency.matrix) <- 0

hashtag.net <- snafun::to_igraph(adjacency.matrix, bipartite = FALSE, vertices = NULL)
plot(hashtag.net)

print(hashtag.net)

install.packages("writexl")
writexl::write_xlsx(twitter_data_1000, "twitter_data_1000.xlsx")
writexl::write_xlsx(selected_twitter_data, "filtered_data_20220630.xlsx")

