
cal_weight <- function(data,method){
  
  if(method == 'pearson'){
    result <- as.data.frame(cor(t(data),method = 'pearson'))
  }else if(method == 'spearman'){
    result <- as.data.frame(cor(t(data),method = 'spearman'))
  }else if(method == 'vector'){
    result <- as.data.frame(cosine(t(movie_train)))
  }
  
  
  
}



########### Movie Data #############

##### Transform data
movie_train <- read.csv('./data_sample/eachmovie_sample/data_train.csv')
Transformer <- function(data.set){
  columns.data <- sort(unique(data.set$Movie))
  rows.data <- sort(unique(data.set$User))
  Table_ <- matrix(0,nrow = length(rows.data), ncol = length(columns.data))
  for(i in 1:length(columns.data)){
    col.name <- columns.data[i]
    index <- which(data.set$Movie == col.name)
    scores <- data.set[index,4] #Scores
    users <- data.set[index,3] #Users
    index2 <- which(rows.data %in% users)
    Table_[index2,i] = scores
    Table_[!index2,i] = 0
  }
  colnames(Table_) <- columns.data
  rownames(Table_) <- rows.data
  return(Table_)
}

movie_train <- Transformer(movie_train)
dim(movie_train)

########## calculate weight matrix 

pearson_weight <- cal_weight(movie_train,'pearson')
spearman_weight <- cal_weight(movie_train,'spearman')
vector_weight <- cal_weight(movie_train,'vector')

write.csv('pearson_weight',pearson_weight)
save(pearson_weight,file = './pearson_weight.RData')
save(spearman_weight,file='./spearman_weight.RData')
save(vector_weight,file = './vector_weight.RData')




