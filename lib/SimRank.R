simrank <- function(train_data, C){
  ###
  # this part is left for later 
  # trasform of train_data
  # if like, not NA
  # if not like, NA
  
  ###
  
  num_user <- nrow(train_data)
  num_item <- ncol(train_data)
  
  ## initialize
  user_sim <- diag(1,nrow = num_user)
  item_sim <- diag(1,nrow = num_item)
  
  for(iter in 1:8){
    ###### update user_sim
    for(i in 1:(num_user-1)){
      for(j in (i+1):num_user){
        index_i <- !is.na(train_data[i,])
        index_j <- !is.na(train_data[j,])
        user_sim[i,j] <- C * mean(item_sim[index_i,index_j])
        user_sim[j,i] <- user_sim[i,j]
        
      }
    }
    ###### update item_sim
    for(i in 1:(num_item-1)){
      for(j in (i+1):num_item){
        index_i <- !is.na(train_data[,i])
        index_j <- !is.na(train_data[,j])
        item_sim[i,j] <- C * mean(user_sim[index_i,index_j])
        item_sim[j,i] <- item_sim[i,j]
        
      }
    }
    
    
  }
  
  return(list(user_sim,item_sim))
  
}

example <- matrix(c(1,1,1,NA,NA,1,1,1),nrow=2,byrow = T)
example

round(simrank(example,C=0.8)[[1]],3)
round(simrank(example,C=0.8)[[2]],3)
