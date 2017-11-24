



######## function to calculate similarity weight ##########
cal_weight <- function(data,method){
  ## calculate similarity weight
  ## input: data - movie data or MS data in wide form
  ##        method - 'pearson' or 'spaerman' or 'vector'
  ## output: similarity weight matrix
  library(lsa)
  data <- as.matrix(data)
  weight_mat <- matrix(NA,nrow=nrow(data),ncol=nrow(data))
  for(i in 1:nrow(data)){
    
    #index <- t(apply(data,1,function(x){return((!is.na(x))&(!is.na(data[i,])))}))
    weight_mat[i,] <- apply(data,1,function(x){
      index <- (!is.na(x))&(!is.na(data[i,]))
      if(sum(index)==0){
        return(0)
      }else{
        if(method == 'pearson'){
          return(cor(data[i,index],x[index],method='pearson'))
        }else if(method == 'spearman'){
          return(cor(data[i,index],x[index],method='spearman'))
        }else if(method == 'vector'){
          return(cosine(data[i,index],x[index]))
        }
      }
      
      
    })
    
  }
  return(round(weight_mat,4))
}



##### Transform data function
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
    Table_[!index2,i] = NA
  }
  colnames(Table_) <- columns.data
  rownames(Table_) <- rows.data
  return(Table_)
}
Transformer2 <- function(data){
  user_num <- which(data$V1 == 'C')
  user_id <- data$V2[user_num]
  page_id <- unique(data$V2[which(data$V1 == 'V')])
  Table_2 <- matrix(0, nrow=length(user_id), ncol=length(page_id))
  rownames(Table_2) <- as.character(user_id)
  colnames(Table_2) <- as.character(page_id)
  for (i in 1:length(user_num)){
    start_num <- user_num[i]
    if (i != length(user_num)){
      end_num <- user_num[i+1]
    }
    else {
      end_num <- nrow(data)+1
    }
    
    user_id_mat <- as.character(user_id[i])
    
    for (j in (start_num+1):(end_num-1)){
      page_id_mat <- as.character(data$V2[j])
      Table_2[user_id_mat, page_id_mat] <- 1
    }
  }
  return(Table_2)
}


########### Read Data #############
movie_train <- read.csv('./data_sample/eachmovie_sample/data_train.csv')
movie_train <- Transformer(movie_train)

MS_train <- read.csv('./data_sample/MS_sample/data_train.csv')
MS_train <- Transformer2(MS_train)



########## calculate weight matrix 
pearson_weight <- cal_weight(movie_train,'pearson')
spearman_weight <- cal_weight(movie_train,'spearman')
vector_weight <- cal_weight(movie_train,'vector')

save(pearson_weight,file = './pearson_weight.RData')
save(spearman_weight,file='./spearman_weight.RData')
save(vector_weight,file = './vector_weight.RData')


pearson_weight_MS <- cal_weight(MS_train,'pearson')
spearman_weight_MS <- cal_weight(MS_train,'spearman')
vector_weight_MS <- cal_weight(MS_train,'vector')

save(pearson_weight_MS,file = './pearson_weight_MS.RData')
save(spearman_weight_MS,file='./spearman_weight_MS.RData')
save(vector_weight_MS,file = './vector_weight_MS.RData')

