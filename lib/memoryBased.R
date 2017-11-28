## For Dataset Movie
Transformer <- function(data.set){
  columns.data <- sort(unique(data.set$Movie))
  rows.data <- sort(unique(data.set$User))
  Table_ <- matrix(NA,nrow = length(rows.data), ncol = length(columns.data))
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

## For Dataset MS
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

## Similarity Weighting
cal_weight <- function(data,method){
  ## calculate similarity weight
  ## input: data - movie data or MS data in wide form
  ##        method - 'pearson' or 'spaerman' or 'vector'
  ## output: similarity weight matrix
  library(lsa)
  data <- as.matrix(data)
  weight_mat <- matrix(NA,nrow=nrow(data),ncol=nrow(data))
  for(i in 1:nrow(data)){
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

## Significance Weighting
significance_weight_assign <- function(i, j, mat=movie_train, not_rate = 0 ){
  r_i <- mat[i, ]
  r_j <- mat[j, ]
  n <- length(intersect(which(r_i != 0), which(r_j != 0)))
  if (n >= 50){
    return(1)
  }
  else {
    return(n/50)
  }
}

significance_weight_matrix <- function(mat_dim_1, mat = movie_train){
  mat_weight = matrix(1, nrow=mat_dim_1, ncol=mat_dim_1)
  for (i in 1:(mat_dim_1-1)){
    for (j in (i+1):mat_dim_1){
      weight <- significance_weight_assign(i, j, mat=mat)
      mat_weight[i, j] <- weight
      mat_weight[j, i] <- weight
    }
    
  }
  return(mat_weight)
}

## Variance Weighting
find_var <- function(mat=movie_train){
  var_vector <- apply(movie_train, 2, var)
  var_max <- max(var_vector)
  var_min <- min(var_vector)
  v <- (var_vector - var_min)/var_max
  return(v)
}

variance_weight_assign <- function(i, j, v, mat=movie_train){
  r_i <- scale(mat[i, ])
  r_j <- scale(mat[j, ])
  weight <- sum(v*r_i*r_j)/sum(v)
  return(weight)
}

variance_weight_matrix <- function(mat_dim_1, mat = movie_train){
  mat_weight = matrix(1, nrow=mat_dim_1, ncol=mat_dim_1)
  v <- find_var(mat = mat)
  for (i in 1:(mat_dim_1-1)){
    print(i)
    print(Sys.time())
    for (j in (i+1):mat_dim_1){
      weight <- variance_weight_assign(i, j, v, mat = movie_train)
      mat_weight[i, j] <- weight
      mat_weight[j, i] <- weight
    }
  }
  return(mat_weight)
}

## Selecting Neighborhoods - correlation_thresholding
neighbors.select <- function(simweights, thresholding = 0.5){
  ## Correlation Thresholding
  ## Input: Simweights: similarity weights matrix
  ##        Thresholding: min-abs-cor
  ## Output: A list of neighbors's index for each users
  top.neighbors = list()
  coverage = 0
  for(i in 1:nrow(simweights)){
    vec1 = simweights[i,]
    index_ = which((abs(vec1) > thresholding) & (vec1 != 1))
    top.neighbors[[i]] = index_
    coverage = coverage + length(top.neighbors[[i]])
  }
  return(top.neighbors)
}

## Prediction
pred.matrix.movie <- function(testdat = movie_test, traindat = movie_train, simweights, top.neighbors){
  prediction.matrix = matrix(NA, ncol = ncol(testdat), nrow = nrow(testdat))
  test.loc = is.na(testdat)
  avg.ratings = apply(traindat, 1, mean, na.rm = T)
  neighbor.avg <- apply(traindat,1,mean,na.rm=T)
  # neighbor.weights = try[user,top.neighbors]
  for(i in 1:nrow(traindat)){
    train.col = colnames(testdat)[which(!is.na(testdat[i,]))]
    #train.loc = traindat[i,train.col]
    neighbor.weights = simweights[i,top.neighbors[[i]]]
    neighbor.ratings = traindat[top.neighbors[[i]], train.col]
    if(length(top.neighbors[[i]]) <2){
      if(length(top.neighbors[[i]]) <1){
        prediction.matrix[i,!test.loc[i,]] = round(avg.ratings[i] + (neighbor.ratings-neighbor.avg[top.neighbors[[i]]]) * neighbor.weights / sum(neighbor.weights, na.rm = T),0)
      }
      else{
        prediction.matrix[i,!test.loc[i,]] = round(avg.ratings[i],0) 
      }
    }
    else{
      prediction.matrix[i,!test.loc[i,]] = round(avg.ratings[i] + apply((neighbor.ratings-neighbor.avg[top.neighbors[[i]]]) * neighbor.weights, 2, sum, na.rm=T) / sum(neighbor.weights, na.rm = T), 0)
    }
    
  }
  #prediction1 = prediction.matrix[,1:ncol(testdat)]
  return(prediction.matrix)
}

pred.matrix.ms <- function(testdat = MS_test, traindat = MS_train, simweights, top.neighbors){
  #testdat = MS_test
  #traindat = MS_train
  #simweights = pearson_weight_MS
  #top.neighbors = results_MS
  prediction.matrix = matrix(0, ncol = ncol(traindat), nrow = nrow(traindat))
  avg.ratings = apply(traindat, 1, mean, na.rm = T)
  test.col = colnames(testdat)
  test.row = rownames(testdat)
  # neighbor.avg <- apply(traindat,1,mean,na.rm=T)
  # neighbor.weights = try[user,top.neighbors]
  for(i in 1:nrow(traindat)){
    #train.col = colnames(testdat)[which(!is.na(testdat[i,]))]
    #train.loc = traindat[i,train.col]
    neighbor.weights = simweights[i,top.neighbors[[i]]]
    neighbor.ratings = traindat[top.neighbors[[i]], ]
    # neighbor.avg = ifelse(length(top.neighbors[[i]]) <2, mean(traindat[top.neighbors[[i]], !test.loc[i,]]),apply(traindat[top.neighbors[[i]], ],1,mean,na.rm= T))
    if(length(top.neighbors[[i]]) <2){
      if(length(top.neighbors[[i]]) < 1){
        prediction.matrix[i,] = avg.ratings[i]
      }
      else{prediction.matrix[i,] = avg.ratings[i] + (neighbor.ratings-avg.ratings[top.neighbors[[i]]]) * neighbor.weights / sum(neighbor.weights, na.rm = T)}
    }
    else{
      prediction.matrix[i,] = avg.ratings[i] + apply((neighbor.ratings-avg.ratings[top.neighbors[[i]]]) * neighbor.weights, 2, sum, na.rm=T) / sum(neighbor.weights, na.rm = T)
    }
    
  }
  colnames(prediction.matrix) = colnames(traindat)
  rownames(prediction.matrix) = rownames(traindat)
  prediction1 = prediction.matrix[test.row,test.col]
  return(prediction1)
}

## Rank Score
rank_score <- function(predicted_test,true_test){
  ## function to calculate rank score of predicted value
  ## input: predicted_test - predicted value matrix of test data
  ##        true_test - test data matrix
  ## output: rank score
  d <- 0.02
  rank_mat_pred <- ncol(predicted_test)+1-t(apply(predicted_test,1,function(x){return(rank(x,ties.method = 'first'))}))
  rank_mat_test <- ncol(true_test)+1-t(apply(true_test,1,function(x){return(rank(x,ties.method = 'first'))}))
  vec = ifelse(true_test - d > 0, true_test - d, 0)
  R_a <- apply(1/(2^((rank_mat_pred-1)/4)) * vec,1,sum)
  R_a_max <- apply(1/(2^((rank_mat_test-1)/4)) * vec,1,sum)
  
  R <- 100*sum(R_a)/sum(R_a_max)
  return(R)
}

## ROC
evaluation.roc <- function(roc_value, mat, mat.true){
  mat.criterion <- matrix(roc_value, nrow = nrow(mat), ncol = ncol(mat))
  same_num <- sum((mat >= mat.criterion) == (mat.true >= mat.criterion))
  n <- sum(mat != matrix(0, nrow=nrow(mat), ncol=ncol(mat)))
  return(same_num/n)
}

## MAE
evaluation.mae <- function(pred.val, true.val){
  ## function to calculate mean absolute error of predicted value
  ## Input: pred.val - predicted value
  ##        true.val - test data matrix
  ## Output: MAE
  mae <- mean(abs(pred.val - true.val), na.rm = T)
  return(mae)
}