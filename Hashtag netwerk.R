twitter_data <- readxl::read_excel("twitter_data.xlsx")

table(twitter_data$hashtags)
table(twitter_data$mentions)

nodes <- unique(twitter_data$Politician_name)
hashtags <- data.frame()

for (politician in nodes){
  tweets <- twitter_data[twitter_data$Politician_name==politician,]
  hashtags_per_politician <- c()
  for (row in 1:nrow(tweets)){
    if (grepl(",", tweets[row, 'hashtags'], fixed = TRUE)){
      hashtags_per_politician <- c(hashtags_per_politician, unlist(strsplit(tweets[row, 'hashtags'][[1]], ",")))
    } else if (!gbutils::isNA(tweets[row, 'hashtags'][[1]])){
      hashtags_per_politician <- append(hashtags_per_politician, tweets[row, 'hashtags'][[1]])
    }
  }
  if (length(hashtags_per_politician)==0){
    next
  }
  hashtags_per_politician <- sapply(hashtags_per_politician, trimws)
  hashtags_per_politician <- sapply(hashtags_per_politician, tolower)
  hashtags_processed <- c(politician, unique(hashtags_per_politician))
  length(hashtags_processed) <- 20
  hashtags <- rbind(hashtags, hashtags_processed)
}

rownames(hashtags) <- hashtags[[1]]
hashtags[[1]] <- NULL

adjacency.matrix <- data.frame(matrix(ncol = 18, nrow = 18))
rownames(adjacency.matrix) <- rownames(hashtags)
colnames(adjacency.matrix) <- rownames(hashtags)

for (politician1 in rownames(hashtags)){
  for (politician2 in rownames(hashtags)){
    combined.hashtags <- c(hashtags[politician1,], hashtags[politician2,])
    combined.hashtags <- combined.hashtags[!is.na(combined.hashtags)]
    if (any(duplicated(combined.hashtags))){
      adjacency.matrix[politician1, politician2] = 1
    } else {
      adjacency.matrix[politician1, politician2] = 0
    }
    
  }
}

adjacency.matrix <- data.matrix(adjacency.matrix)
diag(adjacency.matrix) <- 0

hashtag.net <- snafun::to_igraph(adjacency.matrix, bipartite = FALSE, vertices = NULL)
plot(hashtag.net)
