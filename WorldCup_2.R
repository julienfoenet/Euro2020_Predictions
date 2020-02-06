

#==============================================================================#
#--------------------------------  FUNCTIONS  ---------------------------------#
#==============================================================================#


#---------------------  USEFUL TOOLS  -----------------------#

##### Function to extract rating #####

extract_rating = function(team){
  return (ratings[ratings$Team == team,]$Rating)
}

##### Function to get best third of groups #####

# Function used for Euro only: get the four best firds of each groups which will access the eight finals
letter_in = function(sample_3rd, scheduled_3rd){
  if ((sample_3rd[1] %in% strsplit(scheduled_3rd[1],'')[[1]]) && (sample_3rd[2] %in% strsplit(scheduled_3rd[2],'')[[1]])
      && (sample_3rd[3] %in% strsplit(scheduled_3rd[3],'')[[1]]) && (sample_3rd[4] %in% strsplit(scheduled_3rd[4],'')[[1]])){
    return (TRUE)
  }
  else{
    return (FALSE)
  }
}


#---------------  PREDICTIVE DISTRIBUTION  -----------------#


########## i) Predictive before real data (used for group stage) ##########

# Negative binamiale distribution of part i) (before real data - group stage)
predictive_i = function(team_A, team_B, delta0, beta0, n){
  S = 3
  m = 3
  # sum des y_ij
  d = historical_data[historical_data$TeamA == team_A,]$GoalsA
  y = sum(d[!(is.na(d))])
  # sum des RA / ROAi
  RO = 0
  for (i in 1:((S+1)*m)){
    if ((i-1) %% (S+1) != 0){
      RO = RO + historical_data[historical_data$TeamA == team_A,][i,]$RatingA / 
        historical_data[historical_data$TeamA == team_A,][i,]$RatingB
    } 
  }
  # rating of A and B
  RA = extract_rating(team_A)
  RB = extract_rating(team_B)
  
  # Negative Binomial
  X_AB = rnbinom(n, size=y+delta0, prob=(RO+beta0)/(RO+beta0+RA/RB))
  if (n > 1){
    return(mean(X_AB))
  }
  else return (X_AB)
}

# Negative binomiale distribution of part ii) (after real data - elimination stage)
########## ii) Predictive after real data (used for elimination stage) ##########

predictive_ii = function(team_A, team_B, delta0, beta0, n, stage, extra_time=FALSE, simulation=NA, tournament){
  if (stage == 'eight final'){
    if (tournament == "WorldCup"){
      idx_data = 1:48
    } else if (tournament == "Euro"){
      idx_data = 1:36
    }
  } else if (stage == 'quarter final'){
    if (tournament == "WorldCup"){
      idx_data = 1:56
    } else if (tournament == "Euro"){
      idx_data = 1:44
    }
  } else if (stage == 'semi final'){
    if (tournament == "WorldCup"){
      idx_data = 1:60
    } else if (tournament == "Euro"){
      idx_data = 1:48
    }
  } else if (stage == 'final'){
    if (tournament == "WorldCup"){
      idx_data = 1:62
    } else if (tournament == "Euro"){
      idx_data = 1:50
    }
  }
  
  S = 3
  m = 3
  # sum des y_ij
  d = historical_data[historical_data$TeamA == team_A,]$GoalsA
  y = sum(d[!(is.na(d))])
  
  # sum des x_l
  if (typeof(simulation) == "logical"){
    results_groups = results[idx_data,]
    x = sum(c(results_groups[results_groups$TeamA == team_A,]$GoalsA,
              results_groups[results_groups$TeamB == team_A,]$GoalsB))
  } else if (typeof(simulation) == "list"){
      x = sum(c(simulation[simulation$teamA == team_A,]$goalA,
                simulation[simulation$teamB == team_A,]$goalB))
  }
    
  # sum des RA / ROAi => opponents in historical data
  RO = 0
  for (i in 1:((S+1)*m)){
    if ((i-1) %% (S+1) != 0){
      RO = RO + historical_data[historical_data$TeamA == team_A,][i,]$RatingA / 
        historical_data[historical_data$TeamA == team_A,][i,]$RatingB
    } 
  }
  
  # sum des RA / RCl => opponents in tournament (real data)
  if (typeof(simulation) == "logical"){
    r = c()
    for (i in idx_data){
      if (results_groups$TeamA[i] == team_A) {
        r = c(r, extract_rating(as.character(results_groups$TeamB[i])))
      }
      else if (results_groups$TeamB[i] == team_A) {
        r = c(r, extract_rating(as.character(results_groups$TeamA[i])))
      }
    }
    RA = ratings[ratings$Team == team_A,]$Rating
    RC = sum(RA / r) 
  } else if (typeof(simulation) == "list"){
    r = c()
    if (tournament == "WorldCup"){
      idx_r = 63
    } else if (tournament == "Euro"){
      idx_r = 51
    }
    for (i in 1:idx_r){
      if (simulation$teamA[i] == team_A) {
        r = c(r, extract_rating(as.character(simulation$teamB[i])))
      }
      else if (simulation$teamB[i] == team_A) {
        r = c(r, extract_rating(as.character(simulation$teamA[i])))
      }
    }
    RA = ratings[ratings$Team == team_A,]$Rating
    RC = sum(RA / r)     
  }
  
  # rating of B
  RB = ratings[ratings$Team == team_B,]$Rating
  
  # Negative Binomial
  z=1
  if (extra_time == TRUE) z=0.3
  X_AB = rnbinom(n, size=(y+x+delta0)*z, prob=(RO+RC+beta0)/(RO+RC+beta0+RA/RB))
  if (n > 1){
    return (mean(X_AB))
  }
  else return (X_AB)  
}


#-----------------  PROBA OF WIN/DRAW/LOSS  -----------------#


# Proba win/draw/loss for group stage (used for DeFinetti distance and good prediction rate)
proba_win_i = function(team_A, team_B, delta0, beta0){
  S = 3
  m = 3
  a0=1
  # sum des y_ij pour A
  d_A = historical_data[historical_data$TeamA == team_A,]$GoalsA
  y_A = sum(d_A[!(is.na(d_A))])
  # sum des RA / ROAi
  RO_A = 0
  for (i in 1:((S+1)*m)){
    if ((i-1) %% (S+1) != 0){
      RO_A = RO_A + historical_data[historical_data$TeamA == team_A,][i,]$RatingA / 
        historical_data[historical_data$TeamA == team_A,][i,]$RatingB
    } 
  }
  # sum des y_ij pour B
  d_B = historical_data[historical_data$TeamA == team_B,]$GoalsA
  y_B = sum(d_B[!(is.na(d_B))])
  # sum des RB / ROBi
  RO_B = 0
  for (i in 1:((S+1)*m)){
    if ((i-1) %% (S+1) != 0){
      RO_B = RO_B + historical_data[historical_data$TeamA == team_B,][i,]$RatingA / 
        historical_data[historical_data$TeamA == team_B,][i,]$RatingB
    } 
  }
  # rating of A and B
  RA = extract_rating(team_A)
  RB = extract_rating(team_B)
  
  # Negative Binomial
  #dnbinom(0:20, size=y_A+delta0, prob=(RO_A+beta0)/(RO_A+beta0+RA/RB)) # P(X_AB = i)
  #dnbinom(0:20, size=y_B+delta0, prob=(RO_B+beta0)/(RO_B+beta0+RB/RA)) # P(X_BA = i)
  p_win = 0
  for (i in 1:20){
    for (j in 0:(i-1)){
      p_win = p_win + dnbinom(i, size=a0*y_A+delta0, prob=(a0*RO_A+beta0)/(a0*RO_A+beta0+RA/RB)) *
        dnbinom(j, size=a0*y_B+delta0, prob=(a0*RO_B+beta0)/(a0*RO_B+beta0+RB/RA))
    }
  }
  p_draw = 0
  for (i in 0:20){
    p_draw = p_draw + dnbinom(i, size=a0*y_A+delta0, prob=(a0*RO_A+beta0)/(a0*RO_A+beta0+RA/RB)) *
      dnbinom(i, size=a0*y_B+delta0, prob=(a0*RO_B+beta0)/(a0*RO_B+beta0+RB/RA))
  }
  p_loss = 0
  for (j in 1:20){
    for (i in 0:(j-1)){
      p_loss = p_loss + dnbinom(i, size=a0*y_A+delta0, prob=(a0*RO_A+beta0)/(a0*RO_A+beta0+RA/RB)) *
        dnbinom(j, size=a0*y_B+delta0, prob=(a0*RO_B+beta0)/(a0*RO_B+beta0+RB/RA))
    }
  }
  return (c(p_win, p_draw, p_loss))
}

# Proba win/draw/loss for elimination stage (used for DeFinetti distance and good prediction rate)
proba_win_ii = function(team_A, team_B, delta0, beta0, stage, tournament){
  if (stage == 'eight final'){
    if (tournament == "Euro"){
      idx_data = 1:36
    } else if (tournament == "WorldCup"){
      idx_data = 1:48
    }
  }
  else if (stage == 'quarter final'){
    if (tournament == "Euro"){
      idx_data = 1:44
    } else if (tournament == "WorldCup"){
      idx_data = 1:56
    }
  }
  else if (stage == 'semi final'){
    if (tournament == "Euro"){
      idx_data = 1:48
    } else if (tournament == "WorldCup"){
      idx_data = 1:60
    }
  }
  else if (stage == 'final'){
    idx_data = 1:50
  } else if (tournament == "WorldCup"){
    idx_data = 1:60
  }
  
  S = 3
  m = 3
  a0=1
  # sum des y_ij pour A
  d_A = historical_data[historical_data$TeamA == team_A,]$GoalsA
  y_A = sum(d_A[!(is.na(d_A))])
  
  # sum des y_ij pour B
  d_B = historical_data[historical_data$TeamA == team_B,]$GoalsA
  y_B = sum(d_B[!(is.na(d_B))])
  
  # sum des x_l pour A
  results_groups = results[idx_data,]
  x_A = sum(c(results_groups[results_groups$TeamA == team_A,]$GoalsA, 
              results_groups[results_groups$TeamB == team_A,]$GoalsB))
  
  # sum des x_l pour B
  results_groups = results[idx_data,]
  x_B = sum(c(results_groups[results_groups$TeamA == team_B,]$GoalsA, 
              results_groups[results_groups$TeamB == team_B,]$GoalsB))
  
  # sum des RA / ROAi => opponents in historical data
  RO_A = 0
  for (i in 1:((S+1)*m)){
    if ((i-1) %% (S+1) != 0){
      RO_A = RO_A + historical_data[historical_data$TeamA == team_A,][i,]$RatingA / 
        historical_data[historical_data$TeamA == team_A,][i,]$RatingB
    } 
  }
  
  # sum des RB / ROBi => opponents in historical data
  RO_B = 0
  for (i in 1:((S+1)*m)){
    if ((i-1) %% (S+1) != 0){
      RO_B = RO_B + historical_data[historical_data$TeamA == team_B,][i,]$RatingA / 
        historical_data[historical_data$TeamA == team_B,][i,]$RatingB
    } 
  }
  
  # sum des RA / RCl => opponents in tournament (real data)
  r_A = c()
  for (i in idx_data){
    if (results_groups$TeamA[i] == team_A) {
      r_A = c(r_A, extract_rating(as.character(results_groups$TeamB[i])))
    }
    else if (results_groups$TeamB[i] == team_A) {
      r_A = c(r_A, extract_rating(as.character(results_groups$TeamA[i])))
    }
  }
  RA = ratings[ratings$Team == team_A,]$Rating
  RC_A = sum(RA / r_A)
  
  # sum des RB / RCl => opponents in tournament (real data)
  r_B = c()
  for (i in idx_data){
    if (results_groups$TeamA[i] == team_B) {
      r_B = c(r_B, extract_rating(as.character(results_groups$TeamB[i])))
    }
    else if (results_groups$TeamB[i] == team_B) {
      r_B = c(r_B, extract_rating(as.character(results_groups$TeamA[i])))
    }
  }
  RB = ratings[ratings$Team == team_B,]$Rating
  RC_B = sum(RB / r_B)
  
  # Negative Binomial
  p_win = 0
  for (i in 1:20){
    for (j in 0:(i-1)){
      p_win = p_win + dnbinom(i, size=a0*y_A+x_A+delta0, prob=(a0*RO_A+RC_A+beta0)/(a0*RO_A+RC_A+beta0+RA/RB)) *
        dnbinom(j, size=a0*y_B+x_B+delta0, prob=(a0*RO_B+RC_B+beta0)/(a0*RO_B+RC_B+beta0+RB/RA))
    }
  }
  p_draw = 0
  for (i in 0:20){
    p_draw = p_draw + dnbinom(i, size=a0*y_A+x_A+delta0, prob=(a0*RO_A+RC_A+beta0)/(a0*RO_A+RC_A+beta0+RA/RB)) *
      dnbinom(i, size=a0*y_B+x_B+delta0, prob=(a0*RO_B+RC_B+beta0)/(a0*RO_B+RC_B+beta0+RB/RA))
  }
  p_loss = 0
  for (j in 1:20){
    for (i in 0:(j-1)){
      p_loss = p_loss + dnbinom(i, size=a0*y_A+x_B+delta0, prob=(a0*RO_A+RC_A+beta0)/(a0*RO_A+RC_A+beta0+RA/RB)) *
        dnbinom(j, size=a0*y_B+x_B+delta0, prob=(a0*RO_B+RC_B+beta0)/(a0*RO_B+RC_B+beta0+RB/RA))
    }
  }
  return (c(p_win, p_draw, p_loss))
}

############ Predictions group stage & final rounds ############ 

# Table of predictions (proba win/draw/loss, DeFinetti distance, good prediction rate)
prediction_table = function(delta, beta, tournament){
  
  if (tournament == "WorldCup"){
    n_total = 63
    n_groups = 48
  } else if (tournament == "Euro"){
    n_total = 51
    n_groups = 36
  }
  
  matches_df = data.frame("teamA"=rep(0, n_total), "score"=rep(0,n_total), 
                          "teamB"=rep(0, n_total), "pred1"=rep(0, n_total), "deFi1"=rep(0, n_total), "good_pred1"=rep(0, n_total),
                          "pred2"=rep(0, n_total), "deFi2"=rep(0, n_total), "good_pred2"=rep(0, n_total))
  
  # group stage
  for (i in 1:n_groups){
    teamA = as.character(results$TeamA[i])
    matches_df$teamA[i] = teamA
    teamB = as.character(results$TeamB[i])
    matches_df$teamB[i] = teamB
    matches_df$score[i] = paste(results$GoalsA[i], results$GoalsB[i], sep="-")
    if (results$GoalsA[i] > results$GoalsB[i]){
      res = c(1,0,0)
    } else if (results$GoalsA[i] < results$GoalsB[i]){
      res = c(0,0,1)
    } else res = c(0,1,0)
    pred1 = proba_win_i(teamA, teamB, delta, beta)
    matches_df$pred1[i] = paste0('(', round(pred1[1], 2), ',', round(pred1[2], 2), ',', round(pred1[3], 2), ')')
    matches_df$deFi1[i] = round(sum((res-pred1)^2), 3)
    if ((which.max(pred1) == which.max(res))){
      matches_df$good_pred1[i] = 1
    }
    pred2 = c(as.numeric(extract_rating(teamA) > extract_rating(teamB)), 0, as.numeric(extract_rating(teamA) < extract_rating(teamB)))
    matches_df$pred2[i] = paste0('(', pred2[1], ',', 0, ',', pred2[3], ')')
    matches_df$deFi2[i] = sum((res-pred2)^2)
    if ((which.max(pred2) == which.max(res))){
      matches_df$good_pred2[i] = 1
    }
  }
  
  # final rounds
  idx_start = n_groups+1
  k_x = 0
  stage_vec = c("eight final", "quarter final", "semi final", "final")
  for (k in c(8, 4, 2, 1)){
    idx_end = idx_start+k-1
    idx_data = idx_start:(idx_end)
    k_x = k_x + 1
    stage = stage_vec[k_x]
    
    for (i in idx_data){
      teamA = as.character(results$TeamA[i])
      matches_df$teamA[i] = teamA
      teamB = as.character(results$TeamB[i])
      matches_df$teamB[i] = teamB
      matches_df$score[i] = paste(results$GoalsA[i], results$GoalsB[i], sep="-")
      if (results$GoalsA[i] > results$GoalsB[i]){
        res = c(1,0,0)
      } else if (results$GoalsA[i] < results$GoalsB[i]){
        res = c(0,0,1)
      } else res = c(0,1,0)
      pred1 = proba_win_ii(teamA, teamB, delta, beta, stage, tournament)
      matches_df$pred1[i] = paste0('(', round(pred1[1], 2), ',', round(pred1[2], 2), ',', round(pred1[3], 2), ')')
      matches_df$deFi1[i] = round(sum((res-pred1)^2), 3)
      if ((which.max(pred1) == which.max(res))){
        matches_df$good_pred1[i] = 1
      }
      pred2 = c(as.numeric(extract_rating(teamA) > extract_rating(teamB)), 0, as.numeric(extract_rating(teamA) < extract_rating(teamB)))
      matches_df$pred2[i] = paste0('(', pred2[1], ',', 0, ',', pred2[3], ')')
      matches_df$deFi2[i] = sum((res-pred2)^2)
      if ((which.max(pred2) == which.max(res))){
        matches_df$good_pred2[i] = 1
      }
    }
    idx_start = idx_end + 1
  }
  return (matches_df)
}


#-----------------  TOURNAMENT SIMULATION  ----------------#


# WorldCup (2014 & 2018)
tournament_simulation_WorldCup = function(delta, beta, tournament="WorldCup"){
  # group stage
  list_team = union(results$TeamA, results$TeamB)
  teamA_vec = c()
  teamB_vec = c()
  score_df = data.frame("team"=list_team, "points"=rep(0, 32), "diff"=rep(0, 32), "nbr_goals"=rep(0, 32))
  matches_df = data.frame("teamA"=rep(0, 63), "goalA"=rep(0,63), "teamB"=rep(0, 63), "goalB"=rep(0,63))
  for (i in 1:48){
    teamA = as.character(results$TeamA[i])
    matches_df$teamA[i] = teamA
    teamB = as.character(results$TeamB[i])
    matches_df$teamB[i] = teamB
    teamA_vec = c(teamA_vec, teamA)
    teamB_vec = c(teamB_vec, teamB)
    goalsA = predictive_i(teamA, teamB, delta, beta, 1)
    matches_df$goalA[i] = goalsA
    goalsB = predictive_i(teamB, teamA, delta, beta, 1)
    matches_df$goalB[i] = goalsB
    score_df$diff[score_df$team == teamA] = score_df$diff[score_df$team == teamA] + (goalsA-goalsB)
    score_df$diff[score_df$team == teamB] = score_df$diff[score_df$team == teamB] + (goalsB-goalsA)
    score_df$nbr_goals[score_df$team == teamA] = score_df$nbr_goals[score_df$team == teamA] + goalsA
    score_df$nbr_goals[score_df$team == teamB] = score_df$nbr_goals[score_df$team == teamB] + goalsB
    if (goalsA > goalsB){
      score_df$points[score_df$team == teamA] = score_df$points[score_df$team == teamA] + 3
    }
    else if (goalsA == goalsB){
      score_df$points[score_df$team == teamA] = score_df$points[score_df$team == teamA] + 1
      score_df$points[score_df$team == teamB] = score_df$points[score_df$team == teamB] + 1
    }
    else{
      score_df$points[score_df$team == teamB] = score_df$points[score_df$team == teamB] + 3
    }
  }
  first_qualified = c()
  second_qualified = c()
  for (i in 1:8){
    group_df = score_df[(i*4-3):(i*4),]
    group_df = group_df[order(group_df$points, group_df$diff, group_df$nbr_goals, decreasing=TRUE),]
    first_qualified = c(first_qualified, as.character(group_df[1,]$team))
    second_qualified = c(second_qualified, as.character(group_df[2,]$team))
    #print(group_df)
  }
  team_qualified_group = c(first_qualified, second_qualified)
  #print("---------------------------------------------------------------------------")
  #print("    ---------------------   EIGHT FINAL  ----------------------------")
  #print("---------------------------------------------------------------------------")
  # eight final
  team_A_ef = rep(NA, 8)
  team_B_ef = rep(NA, 8)
  for (i in 1:4){
    team_A_ef[i] = first_qualified[i*2-1]
    team_B_ef[i] = second_qualified[i*2]
    team_A_ef[i+4] = first_qualified[i*2]
    team_B_ef[i+4] = second_qualified[i*2-1]
  }
  goalsA_ef_vec = c()
  goalsB_ef_vec = c()
  score_string = c()
  team_qualified_ef = rep(NA, 8)
  for (i in 1:8){
    teamA = team_A_ef[i]
    matches_df$teamA[i+48] = teamA
    teamB = team_B_ef[i]
    matches_df$teamB[i+48] = teamB
    goalsA = predictive_ii(teamA, teamB, delta, beta, 1, stage="eight final", simulation=matches_df, tournament=tournament)
    matches_df$goalA[i+48] = goalsA
    goalsB = predictive_ii(teamB, teamA, delta, beta, 1, stage="eight final", simulation=matches_df, tournament=tournament)
    matches_df$goalB[i+48] = goalsB
    goalsA_ef_vec = c(goalsA_ef_vec, goalsA)
    goalsB_ef_vec = c(goalsB_ef_vec, goalsB)
    if (goalsA > goalsB){
      team_qualified_ef[i] = teamA
    }
    else if (goalsA < goalsB){
      team_qualified_ef[i] = teamB
    }
    else if (goalsA == goalsB){
      goalsA = goalsA + predictive_ii(teamA, teamB, delta, beta, 1, stage="eight final", extra_time=TRUE, simulation=matches_df, tournament=tournament)
      matches_df$goalA[i+48] = goalsA
      goalsB = goalsB + predictive_ii(teamB, teamA, delta, beta, 1, stage="eight final", extra_time=TRUE, simulation=matches_df, tournament=tournament)
      matches_df$goalB[i+48] = goalsB
      if (goalsA > goalsB){
        team_qualified_ef[i] = teamA
      }
      else if (goalsA < goalsB){
        team_qualified_ef[i] = teamB
      }
      else if (goalsA == goalsB){
        team_qualified_ef[i] = sample(c(teamA, teamB), 1)
      }
    }
    score_string = c(score_string, paste(goalsA, goalsB, sep="-"))
  }
  score_ef_df = data.frame("teamA"=team_A_ef, "score"=score_string, "teamB"=team_B_ef, "goalsA"=goalsA_ef_vec, "goalsB"=goalsB_ef_vec)
  #print(score_ef_df[,1:3])
  #print("---------------------------------------------------------------------------")
  #print("    ---------------------   QUARTER FINAL  ----------------------------")
  #print("---------------------------------------------------------------------------")
  # quarter final
  team_A_qf = rep(NA, 4)
  team_B_qf = rep(NA, 4)
  for (i in 1:4){
    team_A_qf[i] = team_qualified_ef[i*2-1]
    team_B_qf[i] = team_qualified_ef[2*i]
  }
  goalsA_qf_vec = c()
  goalsB_qf_vec = c()
  score_string = c()
  team_qualified_qf = rep(NA, 4)
  for (i in 1:4){
    teamA = team_A_qf[i]
    matches_df$teamA[i+56] = teamA
    teamB = team_B_qf[i]
    matches_df$teamB[i+56] = teamB
    goalsA = predictive_ii(teamA, teamB, delta, beta, 1, stage="quarter final", simulation=matches_df, tournament=tournament)
    matches_df$goalA[i+56] = goalsA
    goalsB = predictive_ii(teamB, teamA, delta, beta, 1, stage="quarter final", simulation=matches_df, tournament=tournament)
    matches_df$goalB[i+56] = goalsB
    goalsA_qf_vec = c(goalsA_qf_vec, goalsA)
    goalsB_qf_vec = c(goalsB_qf_vec, goalsB)
    if (goalsA > goalsB){
      team_qualified_qf[i] = teamA
    }
    else if (goalsA < goalsB){
      team_qualified_qf[i] = teamB
    }
    else if (goalsA == goalsB){
      goalsA = goalsA + predictive_ii(teamA, teamB, delta, beta, 1, stage="quarter final", extra_time=TRUE, simulation=matches_df, tournament=tournament)
      matches_df$goalA[i+56] = goalsA
      goalsB = goalsB + predictive_ii(teamB, teamA, delta, beta, 1, stage="quarter final", extra_time=TRUE, simulation=matches_df, tournament=tournament)
      matches_df$goalB[i+56] = goalsB
      if (goalsA > goalsB){
        team_qualified_qf[i] = teamA
      }
      else if (goalsA < goalsB){
        team_qualified_qf[i] = teamB
      }
      else if (goalsA == goalsB){
        team_qualified_qf[i] = sample(c(teamA, teamB), 1)
      }
    }
    score_string = c(score_string, paste(goalsA, goalsB, sep="-"))
  }
  score_qf_df = data.frame("teamA"=team_A_qf, "score"=score_string, "teamB"=team_B_qf, "goalsA"=goalsA_qf_vec, "goalsB"=goalsB_qf_vec)
  #print(score_qf_df[,1:3])
  
  #print("---------------------------------------------------------------------------")
  #print("    ---------------------   SEMI FINAL  ----------------------------")
  #print("---------------------------------------------------------------------------")
  # semi final
  team_A_sf = rep(NA, 2)
  team_B_sf = rep(NA, 2)
  for (i in 1:2){
    team_A_sf[i] = team_qualified_qf[i*2-1]
    team_B_sf[i] = team_qualified_qf[2*i]
  }
  goalsA_sf_vec = c()
  goalsB_sf_vec = c()
  score_string = c()
  team_qualified_sf = rep(NA, 2)
  for (i in 1:2){
    teamA = team_A_sf[i]
    matches_df$teamA[i+60] = teamA
    teamB = team_B_sf[i]
    matches_df$teamB[i+60] = teamB
    goalsA = predictive_ii(teamA, teamB, delta, beta, 1, stage="semi final", simulation=matches_df, tournament=tournament)
    matches_df$goalA[i+60] = goalsA
    goalsB = predictive_ii(teamB, teamA, delta, beta, 1, stage="semi final", simulation=matches_df, tournament=tournament)
    matches_df$goalB[i+60] = goalsB
    goalsA_sf_vec = c(goalsA_sf_vec, goalsA)
    goalsB_sf_vec = c(goalsB_sf_vec, goalsB)
    if (goalsA > goalsB){
      team_qualified_sf[i] = teamA
    }
    else if (goalsA < goalsB){
      team_qualified_sf[i] = teamB
    }
    else if (goalsA == goalsB){
      goalsA = goalsA + predictive_ii(teamA, teamB, delta, beta, 1, stage="semi final", extra_time=TRUE, simulation=matches_df, tournament=tournament)
      matches_df$goalA[i+60] = goalsA
      goalsB = goalsB + predictive_ii(teamB, teamA, delta, beta, 1, stage="semi final", extra_time=TRUE, simulation=matches_df, tournament=tournament)
      matches_df$goalB[i+60] = goalsB
      if (goalsA > goalsB){
        team_qualified_sf[i] = teamA
      }
      else if (goalsA < goalsB){
        team_qualified_sf[i] = teamB
      }
      else if (goalsA == goalsB){
        team_qualified_sf[i] = sample(c(teamA, teamB), 1)
      }
    }
    score_string = c(score_string, paste(goalsA, goalsB, sep="-"))
  }
  score_sf_df = data.frame("teamA"=team_A_sf, "score"=score_string, "teamB"=team_B_sf, "goalsA"=goalsA_sf_vec, "goalsB"=goalsB_sf_vec)
  #print(score_sf_df[,1:3])
  
  #print("---------------------------------------------------------------------------")
  #print("    ------------------------  FINAL  -------------------------------")
  #print("---------------------------------------------------------------------------")
  # final
  team_A_f = team_qualified_sf[1]
  matches_df$teamA[63] = team_A_f
  team_B_f = team_qualified_sf[2]
  matches_df$teamB[63] = team_B_f
  goalsA_f = predictive_ii(team_A_f, team_B_f, delta, beta, 1, stage="final", simulation=matches_df, tournament=tournament)
  matches_df$goalA[63] = goalsA_f
  goalsB_f = predictive_ii(team_B_f, team_A_f, delta, beta, 1, stage="final", simulation=matches_df, tournament=tournament)
  matches_df$goalB[63] = goalsB_f
  if (goalsA_f > goalsB_f){
    team_winner = team_A_f
  } else if (goalsA_f < goalsB_f){
    team_winner = team_B_f
  } else if (goalsA_f == goalsB_f){
    goalsA_f = goalsA_f + predictive_ii(team_A_f, team_B_f, delta, beta, 1, stage="final", extra_time=TRUE, simulation=matches_df, tournament=tournament)
    matches_df$goalA[63] = goalsA_f
    goalsB_f = goalsB_f + predictive_ii(team_B_f, team_A_f, delta, beta, 1, stage="final", extra_time=TRUE, simulation=matches_df, tournament=tournament)
    matches_df$goalB[63] = goalsB_f
    if (goalsA_f > goalsB_f){
      team_winner = team_A_f
    }
    else if (goalsA_f < goalsB_f){
      team_winner = team_B_f
    }
    else if (goalsA_f == goalsB_f){
      team_winner = sample(c(team_A_f, team_B_f), 1)
    }
  }
  score_string = paste(goalsA_f, goalsB_f, sep="-")
  score_sf_df = data.frame("teamA"=team_A_f, "score"=score_string, "teamB"=team_B_f, "goalsA"=goalsA_f, "goalsB"=goalsB_f)
  #print(score_sf_df[,1:3])
  #print(paste("Winner: ", team_winner))
  return(list(team_winner, team_qualified_sf, team_qualified_qf, team_qualified_ef, team_qualified_group))
}
result_tournament_WorldCup = function(delta, beta, n){
  winner = c()
  finalists = c()
  semi_finalists = c()
  quarter_finalists = c()
  eight_finalists = c()
  for (i in 1:n){
    print(i)
    if (i%%10 == 0){
      #print(i)
    }
    result_simu = tournament_simulation_WorldCup(delta, beta)
    winner = c(winner, result_simu[[1]])
    for (i in 1:2){
      finalists = c(finalists, result_simu[[2]][i])
    }
    for (i in 1:4){
      semi_finalists = c(semi_finalists, result_simu[[3]][i])
    }
    for (i in 1:8){
      quarter_finalists = c(quarter_finalists, result_simu[[4]][i])
    }
    for (i in 1:16){
      eight_finalists = c(eight_finalists, result_simu[[5]][i])
    }
  }
  list_team = union(results$TeamA, results$TeamB)
  df_outcome = data.frame(list_team, matrix(NA, length(list_team), 4))
  colnames(df_outcome) = c("Team", "final", "semi final", "quarter final", "eight final")
  
  df_outcome = data.frame("Team"=list_team)
  df1 = as.data.frame(table(winner, dnn=c("Team"))/n, responseName="winner")
  df2 = as.data.frame(table(finalists, dnn=c("Team"))/n, responseName="final")
  df3 = as.data.frame(table(semi_finalists, dnn=c("Team"))/n, responseName="semi final")
  df4 = as.data.frame(table(quarter_finalists, dnn=c("Team"))/n, responseName="quarter final")
  df5 = as.data.frame(table(eight_finalists, dnn=c("Team"))/n, responseName="eigth final")
  df_outcome = merge(x=df_outcome, y=df1, by='Team', all.x=TRUE)
  df_outcome = merge(x=df_outcome, y=df2, by='Team', all.x=TRUE)
  df_outcome = merge(x=df_outcome, y=df3, by='Team', all.x=TRUE)
  df_outcome = merge(x=df_outcome, y=df4, by='Team', all.x=TRUE)
  df_outcome = merge(x=df_outcome, y=df5, by='Team', all.x=TRUE)
  df_outcome[is.na(df_outcome)] = 0
  df_outcome = df_outcome[order(df_outcome$winner, decreasing=TRUE),]
  rownames(df_outcome) <- NULL
  print(df_outcome)
}

# Euro 2016
tournament_simulation_Euro_2016 = function(delta, beta){
  # group stage
  list_team = union(results$TeamA, results$TeamB)
  teamA_vec = c()
  teamB_vec = c()
  score_df = data.frame("team"=list_team, "points"=rep(0, 24), "diff"=rep(0, 24), "nbr_goals"=rep(0, 24))
  matches_df = data.frame("teamA"=rep(0, 51), "goalA"=rep(0,51), "teamB"=rep(0, 51), "goalB"=rep(0,51))
  
  for (i in 1:36){
    teamA = as.character(results$TeamA[i])
    matches_df$teamA[i] = teamA
    teamB = as.character(results$TeamB[i])
    matches_df$teamB[i] = teamB
    teamA_vec = c(teamA_vec, teamA)
    teamB_vec = c(teamB_vec, teamB)
    goalsA = predictive_i(teamA, teamB, delta, beta, 1)
    matches_df$goalA[i] = goalsA
    goalsB = predictive_i(teamB, teamA, delta, beta, 1)
    matches_df$goalB[i] = goalsB
    score_df$diff[score_df$team == teamA] = score_df$diff[score_df$team == teamA] + (goalsA-goalsB)
    score_df$diff[score_df$team == teamB] = score_df$diff[score_df$team == teamB] + (goalsB-goalsA)
    score_df$nbr_goals[score_df$team == teamA] = score_df$nbr_goals[score_df$team == teamA] + goalsA
    score_df$nbr_goals[score_df$team == teamB] = score_df$nbr_goals[score_df$team == teamB] + goalsB
    if (goalsA > goalsB){
      score_df$points[score_df$team == teamA] = score_df$points[score_df$team == teamA] + 3
    }
    else if (goalsA == goalsB){
      score_df$points[score_df$team == teamA] = score_df$points[score_df$team == teamA] + 1
      score_df$points[score_df$team == teamB] = score_df$points[score_df$team == teamB] + 1
    }
    else{
      score_df$points[score_df$team == teamB] = score_df$points[score_df$team == teamB] + 3
    }
  }
  first_qualified = c()
  second_qualified = c()
  third_teams = c()
  for (i in 1:6){
    group_df = score_df[(i*4-3):(i*4),]
    group_df = group_df[order(group_df$points, group_df$diff, group_df$nbr_goals, decreasing=TRUE),]
    first_qualified = c(first_qualified, as.character(group_df[1,]$team))
    second_qualified = c(second_qualified, as.character(group_df[2,]$team))
    third_teams = c(third_teams, as.character(group_df[3,]$team))
  }
  # Meilleurs 3iemes de groupes
  third_teams_df = score_df[score_df$team %in% third_teams,]
  third_teams_df["group"] = c("A", "B", "C", "D", "E", "F")
  third_teams_df_ordered = third_teams_df[order(third_teams_df$points, third_teams_df$diff, third_teams_df$nbr_goals, decreasing=TRUE),]
  third_qualified = as.character(third_teams_df_ordered$team)[1:4]
  
  scheduled_3rd = c("BEF", "ACD", "ABF", "CDE") # specific to Euro 2016
  best_third = third_teams_df_ordered[1:4,]
  
  match = FALSE
  while (!match){
    sample_3rd = sample(best_third$group)
    match = letter_in(sample_3rd, scheduled_3rd)
  }
  
  team_qualified_group = c(first_qualified, second_qualified, third_qualified)
  #print("---------------------------------------------------------------------------")
  #print("    ---------------------   EIGHT FINAL  ----------------------------")
  #print("---------------------------------------------------------------------------")
  # eight final
  team_A_ef = rep(NA, 8)
  team_B_ef = rep(NA, 8)
  team_A_ef[1] = team_qualified_group[7] # 2A
  team_B_ef[1] = team_qualified_group[9] # 2C
  team_A_ef[2] = team_qualified_group[4] # 1D
  team_B_ef[2] = as.character(best_third[best_third$group == sample_3rd[1],]$team) # 3BEF
  team_A_ef[3] = team_qualified_group[2] # 1B
  team_B_ef[3] = as.character(best_third[best_third$group == sample_3rd[2],]$team) # 3ACD
  team_A_ef[4] = team_qualified_group[6] # 1F
  team_B_ef[4] = team_qualified_group[11] # 2E
  team_A_ef[5] = team_qualified_group[3] # 1C
  team_B_ef[5] = as.character(best_third[best_third$group == sample_3rd[3],]$team) # 3ABF
  team_A_ef[6] = team_qualified_group[5] # 1E
  team_B_ef[6] = team_qualified_group[10] # 2D
  team_A_ef[7] = team_qualified_group[1] # 1A
  team_B_ef[7] = as.character(best_third[best_third$group == sample_3rd[4],]$team) # 3CDE
  team_A_ef[8] = team_qualified_group[8] # 2B
  team_B_ef[8] = team_qualified_group[12] # 2F
  
  goalsA_ef_vec = c()
  goalsB_ef_vec = c()
  score_string = c()
  team_qualified_ef = rep(NA, 8)
  for (i in 1:8){
    teamA = team_A_ef[i]
    matches_df$teamA[i+36] = teamA
    teamB = team_B_ef[i]
    matches_df$teamB[i+36] = teamB
    goalsA = predictive_ii(teamA, teamB, delta, beta, 1, stage="eight final", simulation=matches_df, tournament="Euro")
    matches_df$goalA[i+36] = goalsA
    goalsB = predictive_ii(teamB, teamA, delta, beta, 1, stage="eight final", simulation=matches_df, tournament="Euro")
    matches_df$goalB[i+36] = goalsB
    goalsA_ef_vec = c(goalsA_ef_vec, goalsA)
    goalsB_ef_vec = c(goalsB_ef_vec, goalsB)
    if (goalsA > goalsB){
      team_qualified_ef[i] = teamA
    }
    else if (goalsA < goalsB){
      team_qualified_ef[i] = teamB
    }
    else if (goalsA == goalsB){
      goalsA = goalsA + predictive_ii(teamA, teamB, delta, beta, 1, stage="eight final", extra_time=TRUE, simulation=matches_df, tournament="Euro")
      matches_df$goalA[i+36] = goalsA
      goalsB = goalsB + predictive_ii(teamB, teamA, delta, beta, 1, stage="eight final", extra_time=TRUE, simulation=matches_df, tournament="Euro")
      matches_df$goalB[i+36] = goalsB
      if (goalsA > goalsB){
        team_qualified_ef[i] = teamA
      }
      else if (goalsA < goalsB){
        team_qualified_ef[i] = teamB
      }
      else if (goalsA == goalsB){
        team_qualified_ef[i] = sample(c(teamA, teamB), 1)
      }
    }
    score_string = c(score_string, paste(goalsA, goalsB, sep="-"))
  }
  score_ef_df = data.frame("teamA"=team_A_ef, "score"=score_string, "teamB"=team_B_ef, "goalsA"=goalsA_ef_vec, "goalsB"=goalsB_ef_vec)
  #print(score_ef_df[,1:3])
  #print("---------------------------------------------------------------------------")
  #print("    ---------------------   QUARTER FINAL  ----------------------------")
  #print("---------------------------------------------------------------------------")
  # quarter final
  team_A_qf = rep(NA, 4)
  team_B_qf = rep(NA, 4)
  for (i in 1:4){
    team_A_qf[i] = team_qualified_ef[i*2-1]
    team_B_qf[i] = team_qualified_ef[2*i]
  }
  goalsA_qf_vec = c()
  goalsB_qf_vec = c()
  score_string = c()
  team_qualified_qf = rep(NA, 4)
  for (i in 1:4){
    teamA = team_A_qf[i]
    matches_df$teamA[i+44] = teamA
    teamB = team_B_qf[i]
    matches_df$teamB[i+44] = teamB
    goalsA = predictive_ii(teamA, teamB, delta, beta, 1, stage="quarter final", simulation=matches_df, tournament="Euro")
    matches_df$goalA[i+44] = goalsA
    goalsB = predictive_ii(teamB, teamA, delta, beta, 1, stage="quarter final", simulation=matches_df, tournament="Euro")
    matches_df$goalB[i+44] = goalsB
    goalsA_qf_vec = c(goalsA_qf_vec, goalsA)
    goalsB_qf_vec = c(goalsB_qf_vec, goalsB)
    if (goalsA > goalsB){
      team_qualified_qf[i] = teamA
    }
    else if (goalsA < goalsB){
      team_qualified_qf[i] = teamB
    }
    else if (goalsA == goalsB){
      goalsA = goalsA + predictive_ii(teamA, teamB, delta, beta, 1, stage="quarter final", extra_time=TRUE, simulation=matches_df, tournament="Euro")
      matches_df$goalA[i+44] = goalsA
      goalsB = goalsB + predictive_ii(teamB, teamA, delta, beta, 1, stage="quarter final", extra_time=TRUE, simulation=matches_df, tournament="Euro")
      matches_df$goalB[i+44] = goalsB
      if (goalsA > goalsB){
        team_qualified_qf[i] = teamA
      }
      else if (goalsA < goalsB){
        team_qualified_qf[i] = teamB
      }
      else if (goalsA == goalsB){
        team_qualified_qf[i] = sample(c(teamA, teamB), 1)
      }
    }
    score_string = c(score_string, paste(goalsA, goalsB, sep="-"))
  }
  score_qf_df = data.frame("teamA"=team_A_qf, "score"=score_string, "teamB"=team_B_qf, "goalsA"=goalsA_qf_vec, "goalsB"=goalsB_qf_vec)
  #print(score_qf_df[,1:3])
  
  #print("---------------------------------------------------------------------------")
  #print("    ---------------------   SEMI FINAL  ----------------------------")
  #print("---------------------------------------------------------------------------")
  # semi final
  team_A_sf = rep(NA, 2)
  team_B_sf = rep(NA, 2)
  for (i in 1:2){
    team_A_sf[i] = team_qualified_qf[i*2-1]
    team_B_sf[i] = team_qualified_qf[2*i]
  }
  goalsA_sf_vec = c()
  goalsB_sf_vec = c()
  score_string = c()
  team_qualified_sf = rep(NA, 2)
  for (i in 1:2){
    teamA = team_A_sf[i]
    matches_df$teamA[i+48] = teamA
    teamB = team_B_sf[i]
    matches_df$teamB[i+48] = teamB
    goalsA = predictive_ii(teamA, teamB, delta, beta, 1, stage="semi final", simulation=matches_df, tournament="Euro")
    matches_df$goalA[i+48] = goalsA
    goalsB = predictive_ii(teamB, teamA, delta, beta, 1, stage="semi final", simulation=matches_df, tournament="Euro")
    matches_df$goalB[i+48] = goalsB
    goalsA_sf_vec = c(goalsA_sf_vec, goalsA)
    goalsB_sf_vec = c(goalsB_sf_vec, goalsB)
    if (goalsA > goalsB){
      team_qualified_sf[i] = teamA
    }
    else if (goalsA < goalsB){
      team_qualified_sf[i] = teamB
    }
    else if (goalsA == goalsB){
      goalsA = goalsA + predictive_ii(teamA, teamB, delta, beta, 1, stage="semi final", extra_time=TRUE, simulation=matches_df, tournament="Euro")
      matches_df$goalA[i+48] = goalsA
      goalsB = goalsB + predictive_ii(teamB, teamA, delta, beta, 1, stage="semi final", extra_time=TRUE, simulation=matches_df, tournament="Euro")
      matches_df$goalB[i+48] = goalsB
      if (goalsA > goalsB){
        team_qualified_sf[i] = teamA
      }
      else if (goalsA < goalsB){
        team_qualified_sf[i] = teamB
      }
      else if (goalsA == goalsB){
        team_qualified_sf[i] = sample(c(teamA, teamB), 1)
      }
    }
    score_string = c(score_string, paste(goalsA, goalsB, sep="-"))
  }
  score_sf_df = data.frame("teamA"=team_A_sf, "score"=score_string, "teamB"=team_B_sf, "goalsA"=goalsA_sf_vec, "goalsB"=goalsB_sf_vec)
  #print(score_sf_df[,1:3])
  
  #print("---------------------------------------------------------------------------")
  #print("    ------------------------  FINAL  -------------------------------")
  #print("---------------------------------------------------------------------------")
  # final
  team_A_f = team_qualified_sf[1]
  matches_df$teamA[51] = team_A_f
  team_B_f = team_qualified_sf[2]
  matches_df$teamB[51] = team_B_f
  goalsA_f = predictive_ii(team_A_f, team_B_f, delta, beta, 1, stage="final", simulation=matches_df, tournament="Euro")
  matches_df$goalA[51] = goalsA_f
  goalsB_f = predictive_ii(team_B_f, team_A_f, delta, beta, 1, stage="final", simulation=matches_df, tournament="Euro")
  matches_df$goalB[51] = goalsB_f
  if (goalsA_f > goalsB_f){
    team_winner = team_A_f
  } else if (goalsA_f < goalsB_f){
    team_winner = team_B_f
  } else if (goalsA_f == goalsB_f){
    goalsA_f = goalsA_f + predictive_ii(team_A_f, team_B_f, delta, beta, 1, stage="final", extra_time=TRUE, simulation=matches_df, tournament="Euro")
    matches_df$goalA[51] = goalsA_f
    goalsB_f = goalsB_f + predictive_ii(team_B_f, team_A_f, delta, beta, 1, stage="final", extra_time=TRUE, simulation=matches_df, tournament="Euro")
    matches_df$goalB[51] = goalsB_f
    if (goalsA_f > goalsB_f){
      team_winner = team_A_f
    }
    else if (goalsA_f < goalsB_f){
      team_winner = team_B_f
    }
    else if (goalsA_f == goalsB_f){
      team_winner = sample(c(team_A_f, team_B_f), 1)
    }
  }
  score_string = paste(goalsA_f, goalsB_f, sep="-")
  score_sf_df = data.frame("teamA"=team_A_f, "score"=score_string, "teamB"=team_B_f, "goalsA"=goalsA_f, "goalsB"=goalsB_f)
  #print(score_sf_df[,1:3])
  #print(paste("Winner: ", team_winner))
  return(list(team_winner, team_qualified_sf, team_qualified_qf, team_qualified_ef, team_qualified_group))
}
result_tournament_Euro_2016 = function(delta, beta, n){
  winner = c()
  finalists = c()
  semi_finalists = c()
  quarter_finalists = c()
  eight_finalists = c()
  for (i in 1:n){
    if (i%%10 == 0){
      #print(i)
    }
    result_simu = tournament_simulation_Euro_2016(delta, beta)
    winner = c(winner, result_simu[[1]])
    for (i in 1:2){
      finalists = c(finalists, result_simu[[2]][i])
    }
    for (i in 1:4){
      semi_finalists = c(semi_finalists, result_simu[[3]][i])
    }
    for (i in 1:8){
      quarter_finalists = c(quarter_finalists, result_simu[[4]][i])
    }
    for (i in 1:16){
      eight_finalists = c(eight_finalists, result_simu[[5]][i])
    }
  }
  
  list_team = union(results$TeamA, results$TeamB)
  df_outcome = data.frame(list_team, matrix(NA, length(list_team), 4))
  colnames(df_outcome) = c("Team", "final", "semi final", "quarter final", "eight final")
  
  df_outcome = data.frame("Team"=list_team)
  df1 = as.data.frame(table(winner, dnn=c("Team"))/n, responseName="winner")
  df2 = as.data.frame(table(finalists, dnn=c("Team"))/n, responseName="final")
  df3 = as.data.frame(table(semi_finalists, dnn=c("Team"))/n, responseName="semi final")
  df4 = as.data.frame(table(quarter_finalists, dnn=c("Team"))/n, responseName="quarter final")
  df5 = as.data.frame(table(eight_finalists, dnn=c("Team"))/n, responseName="eigth final")
  df_outcome = merge(x=df_outcome, y=df1, by='Team', all.x=TRUE)
  df_outcome = merge(x=df_outcome, y=df2, by='Team', all.x=TRUE)
  df_outcome = merge(x=df_outcome, y=df3, by='Team', all.x=TRUE)
  df_outcome = merge(x=df_outcome, y=df4, by='Team', all.x=TRUE)
  df_outcome = merge(x=df_outcome, y=df5, by='Team', all.x=TRUE)
  df_outcome[is.na(df_outcome)] = 0
  df_outcome = df_outcome[order(df_outcome$winner, decreasing=TRUE),]
  rownames(df_outcome) <- NULL
  print(df_outcome)
}

# Euro 2020
tournament_simulation_Euro_2020 = function(delta, beta){
  # group stage
  list_team = union(results$TeamA, results$TeamB)
  teamA_vec = c()
  teamB_vec = c()
  score_df = data.frame("team"=list_team, "points"=rep(0, 24), "diff"=rep(0, 24), "nbr_goals"=rep(0, 24))
  matches_df = data.frame("teamA"=rep(0, 51), "goalA"=rep(0,51), "teamB"=rep(0, 51), "goalB"=rep(0,51))
  
  for (i in 1:36){
    teamA = as.character(results$TeamA[i])
    matches_df$teamA[i] = teamA
    teamB = as.character(results$TeamB[i])
    matches_df$teamB[i] = teamB
    teamA_vec = c(teamA_vec, teamA)
    teamB_vec = c(teamB_vec, teamB)
    goalsA = predictive_i(teamA, teamB, delta, beta, 1)
    matches_df$goalA[i] = goalsA
    goalsB = predictive_i(teamB, teamA, delta, beta, 1)
    matches_df$goalB[i] = goalsB
    score_df$diff[score_df$team == teamA] = score_df$diff[score_df$team == teamA] + (goalsA-goalsB)
    score_df$diff[score_df$team == teamB] = score_df$diff[score_df$team == teamB] + (goalsB-goalsA)
    score_df$nbr_goals[score_df$team == teamA] = score_df$nbr_goals[score_df$team == teamA] + goalsA
    score_df$nbr_goals[score_df$team == teamB] = score_df$nbr_goals[score_df$team == teamB] + goalsB
    if (goalsA > goalsB){
      score_df$points[score_df$team == teamA] = score_df$points[score_df$team == teamA] + 3
    }
    else if (goalsA == goalsB){
      score_df$points[score_df$team == teamA] = score_df$points[score_df$team == teamA] + 1
      score_df$points[score_df$team == teamB] = score_df$points[score_df$team == teamB] + 1
    }
    else{
      score_df$points[score_df$team == teamB] = score_df$points[score_df$team == teamB] + 3
    }
  }
  first_qualified = c()
  second_qualified = c()
  third_teams = c()
  for (i in 1:6){
    group_df = score_df[(i*4-3):(i*4),]
    group_df = group_df[order(group_df$points, group_df$diff, group_df$nbr_goals, decreasing=TRUE),]
    first_qualified = c(first_qualified, as.character(group_df[1,]$team))
    second_qualified = c(second_qualified, as.character(group_df[2,]$team))
    third_teams = c(third_teams, as.character(group_df[3,]$team))
  }
  # Meilleurs 3iemes de groupes
  third_teams_df = score_df[score_df$team %in% third_teams,]
  third_teams_df["group"] = c("A", "B", "C", "D", "E", "F")
  third_teams_df_ordered = third_teams_df[order(third_teams_df$points, third_teams_df$diff, third_teams_df$nbr_goals, decreasing=TRUE),]
  third_qualified = as.character(third_teams_df_ordered$team)[1:4]
  
  scheduled_3rd = c("ABC", "ADEF", "DEF", "ABCD") # specific to Euro 2020
  best_third = third_teams_df_ordered[1:4,]
  
  match = FALSE
  while (!match){
    sample_3rd = sample(best_third$group)
    match = letter_in(sample_3rd, scheduled_3rd)
  }
  
  team_qualified_group = c(first_qualified, second_qualified, third_qualified)
  #print("---------------------------------------------------------------------------")
  #print("    ---------------------   EIGHT FINAL  ----------------------------")
  #print("---------------------------------------------------------------------------")
  # eight final
  team_A_ef = rep(NA, 8)
  team_B_ef = rep(NA, 8)
  team_A_ef[1] = team_qualified_group[6] # 1F
  team_B_ef[1] = as.character(best_third[best_third$group == sample_3rd[1],]$team) # 3ABC
  team_A_ef[2] = team_qualified_group[10] # 2D
  team_B_ef[2] = team_qualified_group[11] # 2E
  team_A_ef[3] = team_qualified_group[2] # 1B
  team_B_ef[3] = as.character(best_third[best_third$group == sample_3rd[2],]$team) # 3ADEF
  team_A_ef[4] = team_qualified_group[1] # 1A
  team_B_ef[4] = team_qualified_group[9] # 2C
  team_A_ef[5] = team_qualified_group[3] # 1C
  team_B_ef[5] = as.character(best_third[best_third$group == sample_3rd[3],]$team) # 3DEF
  team_A_ef[6] = team_qualified_group[7] # 2A
  team_B_ef[6] = team_qualified_group[8] # 2B
  team_A_ef[7] = team_qualified_group[5] # 1E
  team_B_ef[7] = as.character(best_third[best_third$group == sample_3rd[4],]$team) # 3ABCD
  team_A_ef[8] = team_qualified_group[4] # 1D
  team_B_ef[8] = team_qualified_group[12] # 2F
  
  goalsA_ef_vec = c()
  goalsB_ef_vec = c()
  score_string = c()
  team_qualified_ef = rep(NA, 8)
  for (i in 1:8){
    teamA = team_A_ef[i]
    matches_df$teamA[i+36] = teamA
    teamB = team_B_ef[i]
    matches_df$teamB[i+36] = teamB
    goalsA = predictive_ii(teamA, teamB, delta, beta, 1, stage="eight final", simulation=matches_df, tournament="Euro")
    matches_df$goalA[i+36] = goalsA
    goalsB = predictive_ii(teamB, teamA, delta, beta, 1, stage="eight final", simulation=matches_df, tournament="Euro")
    matches_df$goalB[i+36] = goalsB
    goalsA_ef_vec = c(goalsA_ef_vec, goalsA)
    goalsB_ef_vec = c(goalsB_ef_vec, goalsB)
    if (goalsA > goalsB){
      team_qualified_ef[i] = teamA
    }
    else if (goalsA < goalsB){
      team_qualified_ef[i] = teamB
    }
    else if (goalsA == goalsB){
      goalsA = goalsA + predictive_ii(teamA, teamB, delta, beta, 1, stage="eight final", extra_time=TRUE, simulation=matches_df, tournament="Euro")
      matches_df$goalA[i+36] = goalsA
      goalsB = goalsB + predictive_ii(teamB, teamA, delta, beta, 1, stage="eight final", extra_time=TRUE, simulation=matches_df, tournament="Euro")
      matches_df$goalB[i+36] = goalsB
      if (goalsA > goalsB){
        team_qualified_ef[i] = teamA
      }
      else if (goalsA < goalsB){
        team_qualified_ef[i] = teamB
      }
      else if (goalsA == goalsB){
        team_qualified_ef[i] = sample(c(teamA, teamB), 1)
      }
    }
    score_string = c(score_string, paste(goalsA, goalsB, sep="-"))
  }
  score_ef_df = data.frame("teamA"=team_A_ef, "score"=score_string, "teamB"=team_B_ef, "goalsA"=goalsA_ef_vec, "goalsB"=goalsB_ef_vec)
  #print(score_ef_df[,1:3])
  #print("---------------------------------------------------------------------------")
  #print("    ---------------------   QUARTER FINAL  ----------------------------")
  #print("---------------------------------------------------------------------------")
  # quarter final
  team_A_qf = rep(NA, 4)
  team_B_qf = rep(NA, 4)
  for (i in 1:4){
    team_A_qf[i] = team_qualified_ef[i*2-1]
    team_B_qf[i] = team_qualified_ef[2*i]
  }
  goalsA_qf_vec = c()
  goalsB_qf_vec = c()
  score_string = c()
  team_qualified_qf = rep(NA, 4)
  for (i in 1:4){
    teamA = team_A_qf[i]
    matches_df$teamA[i+44] = teamA
    teamB = team_B_qf[i]
    matches_df$teamB[i+44] = teamB
    goalsA = predictive_ii(teamA, teamB, delta, beta, 1, stage="quarter final", simulation=matches_df, tournament="Euro")
    matches_df$goalA[i+44] = goalsA
    goalsB = predictive_ii(teamB, teamA, delta, beta, 1, stage="quarter final", simulation=matches_df, tournament="Euro")
    matches_df$goalB[i+44] = goalsB
    goalsA_qf_vec = c(goalsA_qf_vec, goalsA)
    goalsB_qf_vec = c(goalsB_qf_vec, goalsB)
    if (goalsA > goalsB){
      team_qualified_qf[i] = teamA
    }
    else if (goalsA < goalsB){
      team_qualified_qf[i] = teamB
    }
    else if (goalsA == goalsB){
      goalsA = goalsA + predictive_ii(teamA, teamB, delta, beta, 1, stage="quarter final", extra_time=TRUE, simulation=matches_df, tournament="Euro")
      matches_df$goalA[i+44] = goalsA
      goalsB = goalsB + predictive_ii(teamB, teamA, delta, beta, 1, stage="quarter final", extra_time=TRUE, simulation=matches_df, tournament="Euro")
      matches_df$goalB[i+44] = goalsB
      if (goalsA > goalsB){
        team_qualified_qf[i] = teamA
      }
      else if (goalsA < goalsB){
        team_qualified_qf[i] = teamB
      }
      else if (goalsA == goalsB){
        team_qualified_qf[i] = sample(c(teamA, teamB), 1)
      }
    }
    score_string = c(score_string, paste(goalsA, goalsB, sep="-"))
  }
  score_qf_df = data.frame("teamA"=team_A_qf, "score"=score_string, "teamB"=team_B_qf, "goalsA"=goalsA_qf_vec, "goalsB"=goalsB_qf_vec)
  #print(score_qf_df[,1:3])
  
  #print("---------------------------------------------------------------------------")
  #print("    ---------------------   SEMI FINAL  ----------------------------")
  #print("---------------------------------------------------------------------------")
  # semi final
  team_A_sf = rep(NA, 2)
  team_B_sf = rep(NA, 2)
  for (i in 1:2){
    team_A_sf[i] = team_qualified_qf[i*2-1]
    team_B_sf[i] = team_qualified_qf[2*i]
  }
  goalsA_sf_vec = c()
  goalsB_sf_vec = c()
  score_string = c()
  team_qualified_sf = rep(NA, 2)
  for (i in 1:2){
    teamA = team_A_sf[i]
    matches_df$teamA[i+48] = teamA
    teamB = team_B_sf[i]
    matches_df$teamB[i+48] = teamB
    goalsA = predictive_ii(teamA, teamB, delta, beta, 1, stage="semi final", simulation=matches_df, tournament="Euro")
    matches_df$goalA[i+48] = goalsA
    goalsB = predictive_ii(teamB, teamA, delta, beta, 1, stage="semi final", simulation=matches_df, tournament="Euro")
    matches_df$goalB[i+48] = goalsB
    goalsA_sf_vec = c(goalsA_sf_vec, goalsA)
    goalsB_sf_vec = c(goalsB_sf_vec, goalsB)
    if (goalsA > goalsB){
      team_qualified_sf[i] = teamA
    }
    else if (goalsA < goalsB){
      team_qualified_sf[i] = teamB
    }
    else if (goalsA == goalsB){
      goalsA = goalsA + predictive_ii(teamA, teamB, delta, beta, 1, stage="semi final", extra_time=TRUE, simulation=matches_df, tournament="Euro")
      matches_df$goalA[i+48] = goalsA
      goalsB = goalsB + predictive_ii(teamB, teamA, delta, beta, 1, stage="semi final", extra_time=TRUE, simulation=matches_df, tournament="Euro")
      matches_df$goalB[i+48] = goalsB
      if (goalsA > goalsB){
        team_qualified_sf[i] = teamA
      }
      else if (goalsA < goalsB){
        team_qualified_sf[i] = teamB
      }
      else if (goalsA == goalsB){
        team_qualified_sf[i] = sample(c(teamA, teamB), 1)
      }
    }
    score_string = c(score_string, paste(goalsA, goalsB, sep="-"))
  }
  score_sf_df = data.frame("teamA"=team_A_sf, "score"=score_string, "teamB"=team_B_sf, "goalsA"=goalsA_sf_vec, "goalsB"=goalsB_sf_vec)
  #print(score_sf_df[,1:3])
  
  #print("---------------------------------------------------------------------------")
  #print("    ------------------------  FINAL  -------------------------------")
  #print("---------------------------------------------------------------------------")
  # final
  team_A_f = team_qualified_sf[1]
  matches_df$teamA[51] = team_A_f
  team_B_f = team_qualified_sf[2]
  matches_df$teamB[51] = team_B_f
  goalsA_f = predictive_ii(team_A_f, team_B_f, delta, beta, 1, stage="final", simulation=matches_df, tournament="Euro")
  matches_df$goalA[51] = goalsA_f
  goalsB_f = predictive_ii(team_B_f, team_A_f, delta, beta, 1, stage="final", simulation=matches_df, tournament="Euro")
  matches_df$goalB[51] = goalsB_f
  if (goalsA_f > goalsB_f){
    team_winner = team_A_f
  } else if (goalsA_f < goalsB_f){
    team_winner = team_B_f
  } else if (goalsA_f == goalsB_f){
    goalsA_f = goalsA_f + predictive_ii(team_A_f, team_B_f, delta, beta, 1, stage="final", extra_time=TRUE, simulation=matches_df, tournament="Euro")
    matches_df$goalA[51] = goalsA_f
    goalsB_f = goalsB_f + predictive_ii(team_B_f, team_A_f, delta, beta, 1, stage="final", extra_time=TRUE, simulation=matches_df, tournament="Euro")
    matches_df$goalB[51] = goalsB_f
    if (goalsA_f > goalsB_f){
      team_winner = team_A_f
    }
    else if (goalsA_f < goalsB_f){
      team_winner = team_B_f
    }
    else if (goalsA_f == goalsB_f){
      team_winner = sample(c(team_A_f, team_B_f), 1)
    }
  }
  score_string = paste(goalsA_f, goalsB_f, sep="-")
  score_sf_df = data.frame("teamA"=team_A_f, "score"=score_string, "teamB"=team_B_f, "goalsA"=goalsA_f, "goalsB"=goalsB_f)
  #print(score_sf_df[,1:3])
  #print(paste("Winner: ", team_winner))
  return(list(team_winner, team_qualified_sf, team_qualified_qf, team_qualified_ef, team_qualified_group))
}
result_tournament_Euro_2020 = function(delta, beta, n){
  winner = c()
  finalists = c()
  semi_finalists = c()
  quarter_finalists = c()
  eight_finalists = c()
  for (i in 1:n){
    if (i%%10 == 0){
      #print(i)
    }
    result_simu = tournament_simulation_Euro_2020(delta, beta)
    winner = c(winner, result_simu[[1]])
    for (i in 1:2){
      finalists = c(finalists, result_simu[[2]][i])
    }
    for (i in 1:4){
      semi_finalists = c(semi_finalists, result_simu[[3]][i])
    }
    for (i in 1:8){
      quarter_finalists = c(quarter_finalists, result_simu[[4]][i])
    }
    for (i in 1:16){
      eight_finalists = c(eight_finalists, result_simu[[5]][i])
    }
  }
  
  list_team = union(results$TeamA, results$TeamB)
  df_outcome = data.frame(list_team, matrix(NA, length(list_team), 4))
  colnames(df_outcome) = c("Team", "final", "semi final", "quarter final", "eight final")
  
  df_outcome = data.frame("Team"=list_team)
  df1 = as.data.frame(table(winner, dnn=c("Team"))/n, responseName="winner")
  df2 = as.data.frame(table(finalists, dnn=c("Team"))/n, responseName="final")
  df3 = as.data.frame(table(semi_finalists, dnn=c("Team"))/n, responseName="semi final")
  df4 = as.data.frame(table(quarter_finalists, dnn=c("Team"))/n, responseName="quarter final")
  df5 = as.data.frame(table(eight_finalists, dnn=c("Team"))/n, responseName="eigth final")
  df_outcome = merge(x=df_outcome, y=df1, by='Team', all.x=TRUE)
  df_outcome = merge(x=df_outcome, y=df2, by='Team', all.x=TRUE)
  df_outcome = merge(x=df_outcome, y=df3, by='Team', all.x=TRUE)
  df_outcome = merge(x=df_outcome, y=df4, by='Team', all.x=TRUE)
  df_outcome = merge(x=df_outcome, y=df5, by='Team', all.x=TRUE)
  df_outcome[is.na(df_outcome)] = 0
  df_outcome = df_outcome[order(df_outcome$winner, decreasing=TRUE),]
  rownames(df_outcome) <- NULL
  print(df_outcome)
}


#==============================================================================#
#---------------------------------  RESULTS  ----------------------------------#
#==============================================================================#


#   -----------------------  WORLD CUP 2014  -------------------------#


# World Cup 2014 - Importation
ratings = read.csv("/Users/julienfoenet/Desktop/ENSAE/Statistiques_Bayesian/Projet/WorldCup/data/WorldCup_2014/RatingFIFA_2014.csv")
results = read.csv("/Users/julienfoenet/Desktop/ENSAE/Statistiques_Bayesian/Projet/WorldCup/data/WorldCup_2014/WorldCup2014_results.csv")
historical_data = read.csv("/Users/julienfoenet/Desktop/ENSAE/Statistiques_Bayesian/Projet/WorldCup/data/WorldCup_2014/Historical_Data_2014.csv", sep='\t')
list_team = union(results$TeamA, results$TeamB)

# World Cup 2014 - Prediction table
matches_df = prediction_table(0.01, 0.5, tournament="WorldCup")
print(c(mean(matches_df$deFi1), mean(matches_df$deFi2)))
print(c(mean(matches_df$good_pred1), mean(matches_df$good_pred2)))

# World Cup 2014 - Tournament simulation
result_tournament_WorldCup(0.01, 0.5, 5000)


#   -----------------------  WORLD CUP 2018  -------------------------#


# World Cup 2018 - Importation
ratings = read.csv("/Users/julienfoenet/Desktop/ENSAE/Statistiques_Bayesian/Projet/WorldCup/data/WorldCup_2018/RatingFIFA_2018.csv")
results = read.csv("/Users/julienfoenet/Desktop/ENSAE/Statistiques_Bayesian/Projet/WorldCup/data/WorldCup_2018/WorldCup2018_results.csv")
historical_data = read.csv("/Users/julienfoenet/Desktop/ENSAE/Statistiques_Bayesian/Projet/WorldCup/data/WorldCup_2018/Historical_Data_2018.csv", sep='\t')
list_team = union(results$TeamA, results$TeamB)

# World Cup 2018 - Prediction table
matches_df = prediction_table(0.01, 0.5, tournament="WorldCup")
print(c(mean(matches_df$deFi1), mean(matches_df$deFi2)))
print(c(mean(matches_df$good_pred1), mean(matches_df$good_pred2)))


# World Cup 2018 - Tournament simulation
result_tournament_WorldCup(0.01, 0.5, 5000)


#   -----------------------  EURO 2016  -------------------------#


# Euro 2016 - Importation
ratings = read.csv("/Users/julienfoenet/Desktop/ENSAE/Statistiques_Bayesian/Projet/WorldCup/data/Euro_2016/RatingFIFA_2016.csv")
results = read.csv("/Users/julienfoenet/Desktop/ENSAE/Statistiques_Bayesian/Projet/WorldCup/data/Euro_2016/Euro2016_results.csv")
historical_data = read.csv("/Users/julienfoenet/Desktop/ENSAE/Statistiques_Bayesian/Projet/WorldCup/data/Euro_2016/Historical_Data_2016.csv")
list_team = union(results$TeamA, results$TeamB)

# Euro 2016 - Prediction table
matches_df = prediction_table(0.01, 0.5, tournament="Euro")
print(c(mean(matches_df$deFi1), mean(matches_df$deFi2)))
print(c(mean(matches_df$good_pred1), mean(matches_df$good_pred2)))

# Euro 2016 - Tournament simulation
result_tournament_Euro_2016(0.01, 0.5, 5000)


#   -----------------------  EURO 2020  -------------------------#


# Euro 2020 - Importation
ratings = read.csv("/Users/julienfoenet/Desktop/ENSAE/Statistiques_Bayesian/Projet/WorldCup/data/Euro_2020/RatingFIFA_2020.csv")
results = read.csv("/Users/julienfoenet/Desktop/ENSAE/Statistiques_Bayesian/Projet/WorldCup/data/Euro_2020/Euro2020_results.csv")
historical_data = read.csv("/Users/julienfoenet/Desktop/ENSAE/Statistiques_Bayesian/Projet/WorldCup/data/Euro_2020/Historical_Data_2020.csv")
list_team = union(results$TeamA, results$TeamB)

# Euro 2020 - Tournament simulation
result_tournament_Euro_2020(0.01, 0.5, 5000)


