
rank_score <- function(predicted_test,true_test){
  ## function to calculate rank score of predicted value
  ## input: predicted_test - predicted value matrix of test data
  ##        true_test - test data matrix
  ## output: rank score
  predicted_test[is.na(predicted_test)] <- 0
  true_test[is.na(true_test)] <- 0
  
  rank_mat <- ncol(true_test)+1-t(apply(true_test,1,function(x){return(rank(x,ties.method = 'first'))}))
  
  #R_a_denominator <- 1/(2^((rank_mat-1)/4))
  #R_a_nominator <- max(predicted_test-apply(predicted_test,1,mean))
                    
  R_a <- apply(1/(2^((rank_mat-1)/4)) * predicted_test,1,sum)
  R_a_max <- apply(1/(2^((rank_mat-1)/4)) * true_test,1,sum)
  R <- 100*sum(R_a)/sum(R_a_max)
  return(R)
  
}