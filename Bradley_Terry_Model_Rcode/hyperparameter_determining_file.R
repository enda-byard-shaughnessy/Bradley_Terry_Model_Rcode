
library(plotly)

df <- read.csv("atp_matches_2024_2025_clean_1.csv", stringsAsFactors = FALSE)
matches <- subset(df,won == TRUE) #since data contain both player's 'perspective'.


#this normalise() function is technically not necessary when tuning the model. It is used
#for when we want to see how well our model predicts on unseen 2026 data as there
#is some discrepancies in the strings for some players' names.
normalise <- function(x) {
  x |>
    tolower() |>                          # no capitals
    gsub("[[:space:]]+", "", x = _) |>    # remove all whitespace
    gsub("-", "", x = _)                  # remove hyphens
}

players_chron <- unique(c(normalise(matches$player_name),normalise(matches$opp_name)))
players <- sort(players_chron)    


#this associates an index with each player as oppose to a string
matches_p1 <- match(normalise(matches$player_name),players)
matches_p2 <- match(normalise(matches$opp_name),players)       


#the winner of a given match is p1 in every instance of the data
matches_players <- data.frame(training_p1 = matches_p1, 
                              training_p2 = matches_p2)


#initialisation of player ability scores
abilities <- rep(0,length(players))
names(abilities) <- players  


#This function helps us perform cross validation by splitting the data into k_fold
#sections and we use the section of data indexed by test_index as the test set.
#So: 1 <= test_index <= k_folds
data_splitter <- function(matches_played,k_folds,test_index){
  match_count <- nrow(matches_played)
  
  groups <- cut(seq_len(match_count),breaks=k_folds)
  grouped_indexes <- split(seq_len(match_count),groups)
  
  training_index <- -test_index
  
  training1 <- matches_players[unlist(grouped_indexes[training_index]),]
  test1     <- matches_players[grouped_indexes[[test_index]],]
  colnames(test1) <- c("test_p1","test_p2")
  return(list(training = training1,test = test1))}


#The purpose of this function is to aggregrate our data so we have all unique
#matchups that occured and the win count of each player in each given matchup.
#A matchup being defined as a non-ordered unique pair of two players.
#This cuts down our data from every match that occured to just player matchups.

#Something important to note is that the dataframe inputted into this function must
#have its columns named training_p1,training_p2.
data_aggregrator <- function(training){
  training_amount <- nrow(training)
  
  results <- data.frame(player1 = integer(training_amount), 
                        player2 = integer(training_amount),
                        wins1 = integer(training_amount),
                        wins2 = integer(training_amount))
  
  count = 0
  for (i in 1:length(training$training_p1)){
    row_index <- which(((training$training_p1[i] == results$player1) & 
                          (training$training_p2[i] == results$player2))|
                         ((training$training_p2[i] == results$player1) & 
                            (training$training_p1[i] == results$player2)))
    if (length(row_index) > 0){
      if (training$training_p1[i] == results$player1[row_index]){
        results$wins1[row_index] = results$wins1[row_index] + 1
      }else {
        results$wins2[row_index] = results$wins2[row_index] + 1
      }
    }else{
      count = count + 1
      results$player1[count] <- training$training_p1[i]
      results$player2[count] <- training$training_p2[i]
      results$wins1[count] <- 1
    }
  }
  results <- results[seq_len(count), ]
  return(results)}


#This function determines which shrinkage value a player gets.
#Less than 5 games played requires greater shrinkage, the bigger the number the
#stronger the shrinkage here as we have lambda = 1/sigma, where sigma comes from
#the pdf of the associated gaussian prior on our player abilities.
regularisation_assigner <- function(results,lambda_val_1,lambda_val_2){
  games_played_t <- rowsum(c(results$wins1,results$wins2), c(results$player1,results$player2))
  
  lambda_vals <- c(lambda_val_1,lambda_val_2)
  
  games_played <- integer(length(abilities))
  games_played[as.integer(rownames(games_played_t))] <- games_played_t
  lambda <- integer(length(abilities))
  lambda[games_played<=5] <- lambda_vals[1]                  
  lambda[games_played>5] <- lambda_vals[2]    
  
  return(lambda)}


#Negative log-likelihood function, this takes in the player abilities and their shrinkage
#values. It is negative because optim finds the minimum. So optim's results will then be 
#the maximum of the log_likelihood which has not been negated.
neg_log_likelihood <- function(theta,lambda){
  
  ab1 <- theta[results$player1]
  ab2 <- theta[results$player2]
  w1 <- results$wins1
  w2 <- results$wins2
  
  matchup_result <- sum(w1*(log(exp(ab1)+exp(ab2))-ab1)+w2*(log(exp(ab1)+exp(ab2))-ab2))
  neg_LL = matchup_result
  
  neg_posterior = neg_LL + 0.5*sum((theta*lambda)^2) #1/sqrt(2pi)sigma doesnt affect argmax
  return(neg_posterior)
}


#this is the gradient of the negative log_likelihood
gradient <- function(theta,lambda){
  
  ab1 <- theta[results$player1]
  ab2 <- theta[results$player2]
  w1  <- results$wins1
  w2  <- results$wins2
  
  n  <- w1 + w2
  p1 <- plogis(ab1 - ab2)         
  
  g1 <- n * p1 - w1
  g2 <- -g1
  grad_vector <- rowsum(c(g1,g2),c(results$player1,results$player2))
  grad <- numeric(length(theta))
  grad[as.integer(rownames(grad_vector))] <- grad_vector
  
  grad <- grad + theta * lambda^2    
  return(grad)}


#This function produces the estimated player abilities given the data and prior, it 
#also produces a hessian matrix for these player abilities
solver <- function(abilities,likelihood,gradient,lambda,compute_hessian = FALSE){
  vector <- optim(par = abilities, fn = neg_log_likelihood, gr = gradient,
                  lambda = lambda, method = "BFGS", hessian = compute_hessian)
  
  player_scores <- vector$par
  out <- list(player_scores= player_scores)
  if (compute_hessian){
    
    hessian <- vector$hessian
    out$SE_vector <- sqrt(diag(solve(hessian)))
  }
  return(out)}

se <- function(x){return( sd(x) / sqrt(length(x)))}

#This is the for loop that actually runs all the functions above.
#k_folds represents the number of sections we are split our data into, the for loop
#iterates through each value so that we have a new test set, the value 'j' of the for 
#loop at each iteration represents test_index which is used in data_splitter().


fold_data <- vector("list", k_folds)

for (j in 1:k_folds) {
  split <- data_splitter(matches_players, k_folds, j)
  fold_data[[j]] <- list(
    results = data_aggregrator(split$training),
    test    = split$test
  )
}


index_seq_l1 <- seq(1,5, by = 0.1) #this is the strong shrikage parameter loop values
index_seq_l2 <- seq(0.5,1.5, by = 0.01) #this is the weak shrikage parameter loop values

n_col <- length(index_seq_l1)       
n_row <- length(index_seq_l2)

log_loss_matrix <- matrix(NA_real_, nrow = n_row, ncol = n_col)
se_log_loss_matrix <- matrix(NA_real_, nrow = n_row, ncol = n_col)


#two outer for loops are for looping through the shrinkage parameter values
for (h in 1:length(index_seq_l1)){
  log_losses <- numeric(length(index_seq_l2))
  for (m in 1:length(index_seq_l2)){
    k_folds <- 10
    total_LL <- 0
    total_SE <- 0
    total_ab <- 0
    log_loss_per_fold <- 0
    initial_ab <- abilities
    
    for (j in 1:k_folds){
      results <- fold_data[[j]]$results   
      test_fold    <- fold_data[[j]]$test
      lambda <- regularisation_assigner(results,lambda_val_1 = index_seq_l1[h], 
                                        lambda_val_2 = index_seq_l2[m])
      optim_results <- solver(initial_ab,neg_log_likelihood,gradient,lambda)
      player_scores <- optim_results$player_scores
      
      test_results <- cbind(test_fold,integer(length(split$test)/2))
      
      test_results[,1] <- player_scores[test_results[,1]]
      test_results[,2] <- player_scores[test_results[,2]]
      #test_results[,3] <- ifelse(test_results[,1] >= test_results[,2],1,0)#this line doesn't need to be here if we are optimising for log_loss
      
      probabilities <- exp(test_results[,1])/(exp(test_results[,1])+exp(test_results[,2]))
      
      log_loss <- (-1/length(test_results[,1]))*sum(log(probabilities))
      total_LL <- total_LL + log_loss
      #print(log_loss)#print(log_loss) technically isn't necessary
      
      #these 4 lines here in particular are subject to change/update because of hessian.
      total_SE <- total_SE + optim_results$SE_vector
      total_ab <- total_ab + player_scores
      log_loss_per_fold[j] <- log_loss
      initial_ab <- player_scores
    }
    se_log_loss <- se(log_loss_per_fold)
    log_losses[m] <- total_LL/k_folds    #log loss average
    SE_vector_avg <- total_SE/k_folds #standard error of estimated player abilities average (not doing anythign here sice the hessian matrix isnt being computed)
    ab_vector_avg <- total_ab/k_folds #estimated player abilities average
    cat("log loss average:", log_losses[m], "\n", "lambda_val_1:",index_seq_l1[h],"\n"
        ,"lambda_val_2:", index_seq_l2[m],"\n", "Standard error of log loss:",se_log_loss,"\n")
  }
  log_loss_matrix[,h] <- log_losses
  se_log_loss_matrix[,h] <- se_log_loss
}


#Here is where the process outlined in section 3.3 of the report is performed
min_idx <- which(log_loss_matrix == min(log_loss_matrix),arr.ind = TRUE)
threshold  <- log_loss_matrix[min_idx] * 1.001        # within 0.1%
candidates <- which(log_loss_matrix <= threshold, arr.ind = TRUE)
#pick <- candidates[which.min(candidates[, "col"]), ]
#approx_minimum <- log_loss_matrix[pick["row"], pick["col"]]

min_col_cand <- min(candidates[, "col"])
group_cand   <- candidates[candidates[, "col"] == min_col_cand, , drop = FALSE]
min_row_cand <- which(log_loss_matrix[group_cand] == min(log_loss_matrix[group_cand]),arr.ind = TRUE)

optimal_idx <- group_cand[min_row_cand,]

approx_minimum <- log_loss_matrix[optimal_idx]