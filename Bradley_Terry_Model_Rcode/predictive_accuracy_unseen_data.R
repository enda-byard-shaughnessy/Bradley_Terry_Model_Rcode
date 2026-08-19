df_2026 <- read.csv("atp_data_2026_2.csv", stringsAsFactors = FALSE)

atp_ranking_data <- read.csv("atp_rankings_2026_start.csv", stringsAsFactors = FALSE)

#reversing rankings for prediction (i.e 1st ranked player now has score 600 from top
#600 players by ATP rankings).
abilities_atp <- atp_ranking_data$Reversed_Index
names(abilities_atp) <- normalise(atp_ranking_data$Player)

matches_2026 <- df_2026[c("winner_name", "loser_name")]

#normalising string player names
normalise <- function(x) {
  x |>
    tolower() |>                          # no capitals
    gsub("[[:space:]]+", "", x = _) |>    # remove all whitespace
    gsub("-", "", x = _)                  # remove hyphens
}

new_matches <- data.frame(
  p1_2026 = normalise(matches_2026$winner_name),
  p2_2026 = normalise(matches_2026$loser_name)
)

#initialise match results with player ranking scores
correct_pred_count <- integer(length(new_matches$p1_2026))
matches_rankings_atp <- cbind(abilities_atp[new_matches$p1_2026],
                              abilities_atp[new_matches$p2_2026])

#set NA scores to 0, players who didnt appear in top 600
matches_rankings_atp[,1][is.na(matches_rankings_atp[,1])] <- 0
matches_rankings_atp[,2][is.na(matches_rankings_atp[,2])] <- 0

#drop any results where an unranked player played another unranked player.
#this just means rows where we have 0 and 0 for player ranking scores
matches_rankings_atp  <- matches_rankings_atp[rowSums(matches_rankings_atp != 0) > 0, , drop = FALSE]

#vector of corrected predict results, +1 for correct, +0 for incorrect
res_vec_atp <- ifelse(matches_rankings_atp[,1] > matches_rankings_atp[,2],1,0)

percentage_correct_atp <- 100*sum(res_vec_atp)/length(res_vec_atp)
cat("Percentage correct for ATP:", percentage_correct_atp)



#player abilities according to Bradley-Terry model
BT_abilities <- read.csv("player_abilities3.csv", stringsAsFactors = FALSE)

#Essentially the same code as above from here just with Bradley-Terry scores
#instead of ATP rankings scores.

ab_idx_p1 <- match(new_matches$p1_2026,BT_abilities$name)
ab_idx_p2 <- match(new_matches$p2_2026,BT_abilities$name)

BT_matches_rankings <- cbind(BT_abilities$value[ab_idx_p1],
                             BT_abilities$value[ab_idx_p2])

BT_matches_rankings[,1][is.na(BT_matches_rankings[,1])] <- 0
BT_matches_rankings[,2][is.na(BT_matches_rankings[,2])] <- 0

BT_matches_rankings   <- BT_matches_rankings[rowSums(BT_matches_rankings != 0) > 0, , drop = FALSE]


BT_res_vec <- ifelse(BT_matches_rankings[,1] > BT_matches_rankings[,2],1,0)

BT_percentage_correct <- 100*sum(BT_res_vec)/length(BT_res_vec)
cat("Percentage correct for Bradley-Terry Model:",BT_percentage_correct)


#the code here was simply for checking the cases where abilities were equal

BT_equal_rows <- BT_matches_rankings[, 1] == BT_matches_rankings[, 2] 
BT_equals_idx <- which(BT_equal_rows)

atp_equal_rows <- matches_rankings_atp[, 1] == matches_rankings_atp[, 2] 
atp_equals_idx <- which(atp_equal_rows)



#BT_player_idx <- match(normalise(BT_abilities$name),normalise(atp_ranking_data$Player))