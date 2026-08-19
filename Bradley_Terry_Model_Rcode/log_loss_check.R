df <- read.csv("atp_matches_2024_2025_clean_1.csv", stringsAsFactors = FALSE)
matches <- subset(df,won == TRUE)

players_chron <- unique(c(matches$player_name,matches$opp_name))
players <- sort(players_chron)

#this associates an index with each player as oppose to a string
matches_p1 <- match(matches$player_name,players)
matches_p2 <- match(matches$opp_name,players)

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
regularisation_assigner <- function(results){
  games_played_t <- rowsum(c(results$wins1,results$wins2), c(results$player1,results$player2))
  
  lambda_vals <- c(2.7,0.96)
  
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
solver <- function(abilities,likelihood,gradient,lambda){
  vector <- optim(par = abilities, fn = neg_log_likelihood, gr = gradient,
                  lambda = lambda, method = "BFGS")
  
  player_scores <- vector$par
  best_scores <- sort(vector$par)
  return(player_scores)}


#loop through each hold-out set as part of cross validation
k_folds <- 10
total_LL <- 0
for (j in 1:k_folds){
  split <- data_splitter(matches_players,k_folds,j)
  results <- data_aggregrator(split$training)
  lambda <- regularisation_assigner(results)
  player_scores <- solver(abilities,neg_log_likelihood,gradient,lambda)
  
  test_results <- cbind(split$test,integer(length(split$test)/2))
  
  test_results[,1] <- player_scores[test_results[,1]]
  test_results[,2] <- player_scores[test_results[,2]]
  
  
  probabilities <- exp(test_results[,1])/(exp(test_results[,1])+exp(test_results[,2]))
  
  log_loss <- (-1/length(test_results[,1]))*sum(log(probabilities))
  total_LL <- total_LL + log_loss
  print(log_loss)
}

#in this file we are checking accuracy (which is not the metric we optimise for)
#the hyperparameter file has the code for computing log loss
log_loss_score <- total_LL/k_folds
cat("log loss score average:", log_loss_score)

#we need this here to be used in the prediction file on 2026 data
write.csv(data.frame(ab_vector_avg = ab_vector_avg), "player_abilities.csv", row.names = FALSE)
