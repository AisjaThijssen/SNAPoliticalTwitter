twitter_data <- readxl::read_excel("twitter_data.xlsx")

table(twitter_data$hashtags)
table(twitter_data$mentions)

nodes <- unique(twitter_data$Politician_name)
mentions <- data.frame()

for (politician in nodes){
  tweets <- twitter_data[twitter_data$Politician_name==politician,]
  mentions_per_politician <- c()
  for (row in 1:nrow(tweets)){
    if (grepl(",", tweets[row, 'mentions'], fixed = TRUE)){
      mentions_per_politician <- c(mentions_per_politician, unlist(strsplit(tweets[row, 'mentions'][[1]], ",")))
    } else if (!gbutils::isNA(tweets[row, 'mentions'][[1]])){
      mentions_per_politician <- append(mentions_per_politician, tweets[row, 'mentions'][[1]])
    }
  }
  if (length(mentions_per_politician)==0){
    next
  }
  mentions_per_politician <- sapply(mentions_per_politician, trimws)
  mentions_per_politician <- sapply(mentions_per_politician, tolower)
  mentions_processed <- c(politician, unique(mentions_per_politician))
  length(mentions_processed) <- 20
  mentions <- rbind(mentions, mentions_processed)
}

rownames(mentions) <- mentions[[1]]
mentions[[1]] <- NULL

adjacency.matrix <- data.frame(matrix(ncol = 25, nrow = 25))
rownames(adjacency.matrix) <- rownames(mentions)
colnames(adjacency.matrix) <- rownames(mentions)

for (politician1 in rownames(mentions)){
  for (politician2 in rownames(mentions)){
    combined.mentions <- c(mentions[politician1,], mentions[politician2,])
    combined.mentions <- combined.mentions[!is.na(combined.mentions)]
    if (any(duplicated(combined.mentions))){
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
