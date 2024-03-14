## This script creates a mixed effects model for each specific apparatus
## competition event for men and women

set.seed(425)

d <- later_scores

#====================================================#
#=== Mixed Effects Models by gender and apparatus ===#
#====================================================#

#=======================#
#=== Men's Apparatus ===#
#=======================#

#=== High Bar ===#
# filter the data for men's High Bar apparatus
m_HB <- filter(d, gender == 'm' & apparatus == 'HB')

# define the number of folds for cross-validation
k = 5
# create a vector 'folds' that assigns each observation to a fold
folds = rep(1:k, length.out = nrow(m_HB))
# randomly assign each observation to a fold
m_HB$fold = sample(folds, nrow(m_HB), replace = FALSE)

# initialize a column to store the predicted High Bar scores
m_HB$lmer1 <- NA
m_HB$raw_means <- NA

# perform k-fold cross-validation
for(j in 1:k){
  train.rows <- m_HB$fold != j
  test.rows <- m_HB$fold == j
  
  # fit a linear mixed-effects model (lmer) to the training data
  lmer1 <- lmer(score ~ (1|fullname) + round + location, data = m_HB[train.rows,])
  
  # predict the scores on the test data using the trained model
  m_HB[test.rows,]$lmer1 <- predict(lmer1, newdata = m_HB[test.rows,], 
                                    type = 'response', allow.new.levels = TRUE)
  
  # calculate the mean scores for the training data grouped by gymnast
  raw_means <- m_HB[train.rows,] %>% group_by(fullname) %>% summarise(mean_score = mean(score))
  
  # fill in the mean scores for the test data, either from the calculated means or the overall mean if not available
  m_HB[test.rows,]$raw_means <- ifelse(m_HB[test.rows,]$fullname %in% raw_means$fullname, 
                                       raw_means$mean_score[match(m_HB[test.rows,]$fullname, raw_means$fullname)], 
                                       mean(raw_means$mean_score))
}

# calculate the root mean squared error (RMSE) for the mixed-effects predictions
sqrt(mean((m_HB$lmer1 - m_HB$score)^2))

# calculate the root mean squared error (RMSE) for the mean-based predictions
sqrt(mean((m_HB$raw_means - m_HB$score)^2))

# compute the mean score for each male gymnast that participated in High Bar
m_HB_mean <- m_HB %>% group_by(fullname) %>% summarise(mean_lmer1 = mean(lmer1))


#=== Pommel Horse ===#
# filter the data for men's Pommel Horse apparatus
m_PH <- filter(d, gender == 'm' & apparatus == 'PH')

# define the number of folds for cross-validation
k = 5
# create a vector 'folds' that assigns each observation to a fold
folds = rep(1:k, length.out = nrow(m_PH))
# randomly assign each observation to a fold
m_PH$fold = sample(folds, nrow(m_PH), replace = FALSE)

# initialize a column to store the predicted Pommel Horse scores
m_PH$lmer1 <- NA
m_PH$raw_means <- NA

# perform k-fold cross-validation
for(j in 1:k){
  train.rows <- m_PH$fold != j
  test.rows <- m_PH$fold == j
  
  # fit a linear mixed-effects model (lmer) to the training data
  lmer1 <- lmer(score ~ (1|fullname) + round + location, data = m_PH[train.rows,])
  
  # predict the scores on the test data using the trained model
  m_PH[test.rows,]$lmer1 <- predict(lmer1, newdata = m_PH[test.rows,],
                                    type = 'response', allow.new.levels = TRUE)
  
  # calculate the mean scores for the training data grouped by gymnast
  raw_means <- m_PH[train.rows,] %>% group_by(fullname) %>% summarise(mean_score = mean(score))
  
  # fill in the mean scores for the test data, either from the calculated means or the overall mean if not available
  m_PH[test.rows,]$raw_means <- ifelse(m_PH[test.rows,]$fullname %in% raw_means$fullname, 
                                       raw_means$mean_score[match(m_PH[test.rows,]$fullname, raw_means$fullname)], 
                                       mean(raw_means$mean_score))
}

# calculate the root mean squared error (RMSE) for the mixed-effects predictions
sqrt(mean((m_PH$lmer1 - m_PH$score)^2))

# calculate the root mean squared error (RMSE) for the mean-based predictions
sqrt(mean((m_PH$raw_means - m_PH$score)^2))

# compute the mean score for each male gymnast that participated in Pommel Horse
m_PH_mean <- m_PH %>% group_by(fullname) %>% summarise(mean_lmer1 = mean(lmer1))


#=== Floor Exercise ===#
# filter the data for men's Floor Exercise apparatus
m_FX <- filter(d, gender == 'm' & apparatus == 'FX')

# define the number of folds for cross-validation
k = 5
# create a vector 'folds' that assigns each observation to a fold
folds = rep(1:k, length.out = nrow(m_FX))
# randomly assign each observation to a fold
m_FX$fold = sample(folds, nrow(m_FX), replace = FALSE)

# initialize a column to store the predicted Floor Exercise scores
m_FX$lmer1 <- NA
m_FX$raw_means <- NA

# perform k-fold cross-validation
for(j in 1:k){
  train.rows <- m_FX$fold != j
  test.rows <- m_FX$fold == j
  
  # fit a linear mixed-effects model (lmer) to the training data
  lmer1 <- lmer(score ~ (1|fullname) + round + location, data = m_FX[train.rows,])
  
  # predict the scores on the test data using the trained model
  m_FX[test.rows,]$lmer1 <- predict(lmer1, newdata = m_FX[test.rows,],
                                    type = 'response', allow.new.levels = TRUE)
  
  # calculate the mean scores for the training data grouped by gymnast
  raw_means <- m_FX[train.rows,] %>% group_by(fullname) %>% summarise(mean_score = mean(score))
  
  # fill in the mean scores for the test data, either from the calculated means or the overall mean if not available
  m_FX[test.rows,]$raw_means <- ifelse(m_FX[test.rows,]$fullname %in% raw_means$fullname, 
                                       raw_means$mean_score[match(m_FX[test.rows,]$fullname, raw_means$fullname)], 
                                       mean(raw_means$mean_score))
}

# calculate the root mean squared error (RMSE) for the mixed-effects predictions
sqrt(mean((m_FX$lmer1 - m_FX$score)^2))

# calculate the root mean squared error (RMSE) for the mean-based predictions
sqrt(mean((m_FX$raw_means - m_FX$score)^2))

# compute the mean score for each male gymnast that participated in Floor Exercise
m_FX_mean <- m_FX %>% group_by(fullname) %>% summarise(mean_lmer1 = mean(lmer1))


#=== Parallel Bars ===#
# filter the data for men's Parallel Bars apparatus
m_PB <- filter(d, gender == 'm' & apparatus == 'PB')

# define the number of folds for cross-validation
k = 5
# create a vector 'folds' that assigns each observation to a fold
folds = rep(1:k, length.out = nrow(m_PB))
# randomly assign each observation to a fold
m_PB$fold = sample(folds, nrow(m_PB), replace = FALSE)

# initialize a column to store the predicted Parallel Bars scores
m_PB$lmer1 <- NA
m_PB$raw_means <- NA

# perform k-fold cross-validation
for(j in 1:k){
  train.rows <- m_PB$fold != j
  test.rows <- m_PB$fold == j
  
  # fit a linear mixed-effects model (lmer) to the training data
  lmer1 <- lmer(score ~ (1|fullname) + round + location, data = m_PB[train.rows,])
  
  # predict the scores on the test data using the trained model
  m_PB[test.rows,]$lmer1 <- predict(lmer1, newdata = m_PB[test.rows,],
                                    type = 'response', allow.new.levels = TRUE)
  
  # calculate the mean scores for the training data grouped by gymnast
  raw_means <- m_PB[train.rows,] %>% group_by(fullname) %>% summarise(mean_score = mean(score))
  
  # fill in the mean scores for the test data, either from the calculated means or the overall mean if not available
  m_PB[test.rows,]$raw_means <- ifelse(m_PB[test.rows,]$fullname %in% raw_means$fullname, 
                                       raw_means$mean_score[match(m_PB[test.rows,]$fullname, raw_means$fullname)], 
                                       mean(raw_means$mean_score))
}

# calculate the root mean squared error (RMSE) for the mixed-effects predictions
sqrt(mean((m_PB$lmer1 - m_PB$score)^2))

# calculate the root mean squared error (RMSE) for the mean-based predictions
sqrt(mean((m_PB$raw_means - m_PB$score)^2))

# compute the mean score for each male gymnast that participated in Parallel Bars
m_PB_mean <- m_PB %>% group_by(fullname) %>% summarise(mean_lmer1 = mean(lmer1))


#=== Still Rings ===#
# filter the data for men's Still Rings apparatus
m_SR <- filter(d, gender == 'm' & apparatus == 'SR')

# define the number of folds for cross-validation
k = 5
# create a vector 'folds' that assigns each observation to a fold
folds = rep(1:k, length.out = nrow(m_SR))
# randomly assign each observation to a fold
m_SR$fold = sample(folds, nrow(m_SR), replace = FALSE)

# initialize a column to store the predicted Still Rings scores
m_SR$lmer1 <- NA
m_SR$raw_means <- NA

# perform k-fold cross-validation
for(j in 1:k){
  train.rows <- m_SR$fold != j
  test.rows <- m_SR$fold == j
  
  # fit a linear mixed-effects model (lmer) to the training data
  lmer1 <- lmer(score ~ (1|fullname) + round + location, data = m_SR[train.rows,])
  
  # predict the scores on the test data using the trained model
  m_SR[test.rows,]$lmer1 <- predict(lmer1, newdata = m_SR[test.rows,],
                                    type = 'response', allow.new.levels = TRUE)
  
  # calculate the mean scores for the training data grouped by gymnast
  raw_means <- m_SR[train.rows,] %>% group_by(fullname) %>% summarise(mean_score = mean(score))
  
  # fill in the mean scores for the test data, either from the calculated means or the overall mean if not available
  m_SR[test.rows,]$raw_means <- ifelse(m_SR[test.rows,]$fullname %in% raw_means$fullname, 
                                       raw_means$mean_score[match(m_SR[test.rows,]$fullname, raw_means$fullname)], 
                                       mean(raw_means$mean_score))
}

# calculate the root mean squared error (RMSE) for the mixed-effects predictions
sqrt(mean((m_SR$lmer1 - m_SR$score)^2))

# calculate the root mean squared error (RMSE) for the mean-based predictions
sqrt(mean((m_SR$raw_means - m_SR$score)^2))

# compute the mean score for each male gymnast that participated in Still Rings
m_SR_mean <- m_SR %>% group_by(fullname) %>% summarise(mean_lmer1 = mean(lmer1))


#=== Vault ===#
# filter the data for men's Vault apparatus
m_VT <- filter(d, gender == 'm' & apparatus == 'VT')

# define the number of folds for cross-validation
k = 5
# create a vector 'folds' that assigns each observation to a fold
folds = rep(1:k, length.out = nrow(m_VT))
# randomly assign each observation to a fold
m_VT$fold = sample(folds, nrow(m_VT), replace = FALSE)

# initialize a column to store the predicted Vault scores
m_VT$lmer1 <- NA
m_VT$raw_means <- NA

# perform k-fold cross-validation
for(j in 1:k){
  train.rows <- m_VT$fold != j
  test.rows <- m_VT$fold == j
  
  # fit a linear mixed-effects model (lmer) to the training data
  lmer1 <- lmer(score ~ (1|fullname) + round + location, data = m_VT[train.rows,])
  
  # predict the scores on the test data using the trained model
  m_VT[test.rows,]$lmer1 <- predict(lmer1, newdata = m_VT[test.rows,],
                                    type = 'response', allow.new.levels = TRUE)
  
  # calculate the mean scores for the training data grouped by gymnast
  raw_means <- m_VT[train.rows,] %>% group_by(fullname) %>% summarise(mean_score = mean(score))
  
  # fill in the mean scores for the test data, either from the calculated means or the overall mean if not available
  m_VT[test.rows,]$raw_means <- ifelse(m_VT[test.rows,]$fullname %in% raw_means$fullname, 
                                       raw_means$mean_score[match(m_VT[test.rows,]$fullname, raw_means$fullname)], 
                                       mean(raw_means$mean_score))
}

# calculate the root mean squared error (RMSE) for the mixed-effects predictions
sqrt(mean((m_VT$lmer1 - m_VT$score)^2))

# calculate the root mean squared error (RMSE) for the mean-based predictions
sqrt(mean((m_VT$raw_means - m_VT$score)^2))

# compute the mean score for each male gymnast that participated in Vault
m_VT_mean <- m_VT %>% group_by(fullname) %>% summarise(mean_lmer1 = mean(lmer1))



#=========================#
#=== Women's Apparatus ===#
#=========================#

#=== Balance Beam ===#
# filter the data for women's Balance Beam apparatus
w_BB <- filter(d, gender == 'w' & apparatus == 'BB')

# define the number of folds for cross-validation
k = 5
# create a vector 'folds' that assigns each observation to a fold
folds = rep(1:k, length.out = nrow(w_BB))
# randomly assign each observation to a fold
w_BB$fold = sample(folds, nrow(w_BB), replace = FALSE)

# initialize a column to store the predicted Balance Beam scores
w_BB$lmer1 <- NA
w_BB$raw_means <- NA

# perform k-fold cross-validation
for(j in 1:k){
  train.rows <- w_BB$fold != j
  test.rows <- w_BB$fold == j
  
  # fit a linear mixed-effects model (lmer) to the training data
  lmer1 <- lmer(score ~ (1|fullname) + round + location, data = w_BB[train.rows,])
  
  # predict the scores on the test data using the trained model
  w_BB[test.rows,]$lmer1 <- predict(lmer1, newdata = w_BB[test.rows,],
                                    type = 'response', allow.new.levels = TRUE)
  
  # calculate the mean scores for the training data grouped by gymnast
  raw_means <- w_BB[train.rows,] %>% group_by(fullname) %>% summarise(mean_score = mean(score))
  
  # fill in the mean scores for the test data, either from the calculated means or the overall mean if not available
  w_BB[test.rows,]$raw_means <- ifelse(w_BB[test.rows,]$fullname %in% raw_means$fullname, 
                                       raw_means$mean_score[match(w_BB[test.rows,]$fullname, raw_means$fullname)], 
                                       mean(raw_means$mean_score))
}

# calculate the root mean squared error (RMSE) for the mixed-effects predictions
sqrt(mean((w_BB$lmer1 - w_BB$score)^2))

# calculate the root mean squared error (RMSE) for the mean-based predictions
sqrt(mean((w_BB$raw_means - w_BB$score)^2))

# compute the mean score for each male gymnast that participated in High Bar
w_BB_mean <- w_BB %>% group_by(fullname) %>% summarise(mean_lmer1 = mean(lmer1))


#=== Floor Exercise ===#
# filter the data for women's Floor Exercise apparatus
w_FX <- filter(d, gender == 'w' & apparatus == 'FX')

# define the number of folds for cross-validation
k = 5
# create a vector 'folds' that assigns each observation to a fold
folds = rep(1:k, length.out = nrow(w_FX))
# randomly assign each observation to a fold
w_FX$fold = sample(folds, nrow(w_FX), replace = FALSE)

# initialize a column to store the predicted Floor Exercise scores
w_FX$lmer1 <- NA
w_FX$raw_means <- NA

# perform k-fold cross-validation
for(j in 1:k){
  train.rows <- w_FX$fold != j
  test.rows <- w_FX$fold == j
  
  # fit a linear mixed-effects model (lmer) to the training data
  lmer1 <- lmer(score ~ (1|fullname) + round + location, data = w_FX[train.rows,])
  
  # predict the scores on the test data using the trained model
  w_FX[test.rows,]$lmer1 <- predict(lmer1, newdata = w_FX[test.rows,],
                                    type = 'response', allow.new.levels = TRUE)
  
  # calculate the mean scores for the training data grouped by gymnast
  raw_means <- w_FX[train.rows,] %>% group_by(fullname) %>% summarise(mean_score = mean(score))
  
  # fill in the mean scores for the test data, either from the calculated means or the overall mean if not available
  w_FX[test.rows,]$raw_means <- ifelse(w_FX[test.rows,]$fullname %in% raw_means$fullname, 
                                       raw_means$mean_score[match(w_FX[test.rows,]$fullname, raw_means$fullname)], 
                                       mean(raw_means$mean_score))
}

# calculate the root mean squared error (RMSE) for the mixed-effects predictions
sqrt(mean((w_FX$lmer1 - w_FX$score)^2))

# calculate the root mean squared error (RMSE) for the mean-based predictions
sqrt(mean((w_FX$raw_means - w_FX$score)^2))

# compute the mean score for each male gymnast that participated in Floor Exercise
w_FX_mean <- w_FX %>% group_by(fullname) %>% summarise(mean_lmer1 = mean(lmer1))


#=== Uneven Bars ===#
# filter the data for women's Uneven Bars apparatus
w_UB <- filter(d, gender == 'w' & apparatus == 'UB')

# define the number of folds for cross-validation
k = 5
# create a vector 'folds' that assigns each observation to a fold
folds = rep(1:k, length.out = nrow(w_UB))
# randomly assign each observation to a fold
w_UB$fold = sample(folds, nrow(w_UB), replace = FALSE)

# initialize a column to store the predicted Uneven Bars scores
w_UB$lmer1 <- NA
w_UB$raw_means <- NA

# perform k-fold cross-validation
for(j in 1:k){
  train.rows <- w_UB$fold != j
  test.rows <- w_UB$fold == j
  
  # fit a linear mixed-effects model (lmer) to the training data
  lmer1 <- lmer(score ~ (1|fullname) + round + location, data = w_UB[train.rows,])
  
  # predict the scores on the test data using the trained model
  w_UB[test.rows,]$lmer1 <- predict(lmer1, newdata = w_UB[test.rows,],
                                    type = 'response', allow.new.levels = TRUE)
  
  # calculate the mean scores for the training data grouped by gymnast
  raw_means <- w_UB[train.rows,] %>% group_by(fullname) %>% summarise(mean_score = mean(score))
  
  # fill in the mean scores for the test data, either from the calculated means or the overall mean if not available
  w_UB[test.rows,]$raw_means <- ifelse(w_UB[test.rows,]$fullname %in% raw_means$fullname, 
                                       raw_means$mean_score[match(w_UB[test.rows,]$fullname, raw_means$fullname)], 
                                       mean(raw_means$mean_score))
}

# calculate the root mean squared error (RMSE) for the mixed-effects predictions
sqrt(mean((w_UB$lmer1 - w_UB$score)^2))

# calculate the root mean squared error (RMSE) for the mean-based predictions
sqrt(mean((w_UB$raw_means - w_UB$score)^2))

# compute the mean score for each male gymnast that participated in High Bar
w_UB_mean <- w_UB %>% group_by(fullname) %>% summarise(mean_lmer1 = mean(lmer1))


#=== Vault ===#
# filter the data for women's Vault apparatus
w_VT <- filter(d, gender == 'w' & apparatus == 'VT')

# update the 'round' column to avoid non-conformable arguments error
w_VT <- w_VT %>% mutate(round = if_else(round == "AAqual", "qual", round))

# define the number of folds for cross-validation
k = 5
# create a vector 'folds' that assigns each observation to a fold
folds = rep(1:k, length.out = nrow(w_VT))
# randomly assign each observation to a fold
w_VT$fold = sample(folds, nrow(w_VT), replace = FALSE)

# initialize a column to store the predicted Vault scores
w_VT$lmer1 <- NA
w_VT$raw_means <- NA

# perform k-fold cross-validation
for(j in 1:k){
  train.rows <- w_VT$fold != j
  test.rows <- w_VT$fold == j
  
  # fit a linear mixed-effects model (lmer) to the training data
  lmer1 <- lmer(score ~ (1|fullname) + round + location, data = w_VT[train.rows,])
  
  # predict the scores on the test data using the trained model
  w_VT[test.rows,]$lmer1 <- predict(lmer1, newdata = w_VT[test.rows,],
                                    type = 'response', allow.new.levels = TRUE)
  
  # calculate the mean scores for the training data grouped by gymnast
  raw_means <- w_VT[train.rows,] %>% group_by(fullname) %>% summarise(mean_score = mean(score))
  
  # fill in the mean scores for the test data, either from the calculated means or the overall mean if not available
  w_VT[test.rows,]$raw_means <- ifelse(w_VT[test.rows,]$fullname %in% raw_means$fullname, 
                                       raw_means$mean_score[match(w_VT[test.rows,]$fullname, raw_means$fullname)], 
                                       mean(raw_means$mean_score))
}

# calculate the root mean squared error (RMSE) for the mixed-effects predictions
sqrt(mean((w_VT$lmer1 - w_VT$score)^2))

# calculate the root mean squared error (RMSE) for the mean-based predictions
sqrt(mean((w_VT$raw_means - w_VT$score)^2))

# compute the mean score for each male gymnast that participated in Vault
w_VT_mean <- w_VT %>% group_by(fullname) %>% summarise(mean_lmer1 = mean(lmer1))



#=====================================#
#=== Mixed Effects Model by gender ===#
#=====================================#

# # separate scores by gender and remove direct predictors of score or irrelevant columns
# # removing lastname, firstname, gender, e_score, d_score, penalty
# men_later_scores <- later_scores[later_scores$gender == "m", c(-1, -2, -3, -11, -12, -13)]
# # removing lastname, firstname, gender, e_score, d_score, penalty
# women_later_scores <- later_scores[later_scores$gender == "w", c(-1, -2, -3, -11, -12, -13)]
# 
# #=== Men's Score LME Model ===#
# # define the number of folds for cross-validation
# k = 5
# # create a vector 'folds' that assigns each observation to a fold
# folds = rep(1:k, length.out = nrow(men_later_scores))
# # randomly assign each observation to a fold
# men_later_scores$fold = sample(folds, nrow(men_later_scores), replace = FALSE)
# 
# # initialize a column to store the predicted men's scores
# men_later_scores$lm1 <- NA
# 
# # perform k-fold cross-validation
# for(j in 1:k){
#   train.rows <- men_later_scores$fold != j
#   test.rows <- men_later_scores$fold == j
# 
#   # fit a linear mixed-effects model (lmer) to the training data
#   lm1 <- lm(score ~ fullname + rank + competition + apparatus + round,
#             data = men_later_scores)
# 
#   # predict the scores on the test data using the trained model
#   men_later_scores[test.rows,]$lm1 <- predict(lm1, newdata = men_later_scores[test.rows,],
#                                               type = 'response', allow.new.levels = TRUE)
# }
# 
# men_later_scores <- men_later_scores %>%
#                     mutate(lm1 = if_else(is.na(lm1), 0, lm1)) %>%
#                     mutate(score = if_else(is.na(score), 0, score))
# 
# # calculate the root mean squared error (RMSE) for the predictions
# sqrt(mean((men_later_scores$lm1 - men_later_scores$score)^2))
# 
# 
# #=== Women's Score LME Model ===#
# # define the number of folds for cross-validation
# k = 5
# # create a vector 'folds' that assigns each observation to a fold
# folds = rep(1:k, length.out = nrow(women_later_scores))
# # randomly assign each observation to a fold
# women_later_scores$fold = sample(folds, nrow(women_later_scores), replace = FALSE)
# 
# # initialize a column to store the predicted women's scores
# women_later_scores$lm1 <- NA
# 
# # perform k-fold cross-validation
# for(j in 1:k){
#   train.rows <- women_later_scores$fold != j
#   test.rows <- women_later_scores$fold == j
# 
#   # fit a linear mixed-effects model (lmer) to the training data
#   lm1 <- lm(score ~ fullname + rank + competition + apparatus + round + date,
#             data = women_later_scores)
# 
#   # predict the scores on the test data using the trained model
#   women_later_scores[test.rows,]$lm1 <- predict(lm1, newdata = women_later_scores[test.rows,],
#                                               type = 'response', allow.new.levels = TRUE)
# }
# 
# women_later_scores <- women_later_scores %>%
#                       mutate(lm1 = if_else(is.na(lm1), 0, lm1)) %>%
#                       mutate(score = if_else(is.na(score), 0, score))
# 
# # calculate the root mean squared error (RMSE) for the predictions
# sqrt(mean((women_later_scores$lm1 - women_later_scores$score)^2))