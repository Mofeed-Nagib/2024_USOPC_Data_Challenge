## This script creates a mixed effects model for each specific apparatus
## competition event for men and women

set.seed(425)

d <- later_scores

#=======================#
#=== Men's Apparatus ===#
#=======================#

#=== High Bar ===#
# Filter the data for men's High Bar apparatus
m_HB <- filter(d, gender == 'm' & apparatus == 'HB')

# Define the number of folds for cross-validation
k = 5
# Create a vector 'folds' that assigns each observation to a fold
folds = rep(1:k, length.out = nrow(m_HB))
# Randomly assign each observation to a fold
m_HB$fold = sample(folds, nrow(m_HB), replace = FALSE)

# Initialize a column to store the predicted High Bar scores
m_HB$lmer1 <- NA

# Perform k-fold cross-validation
for(j in 1:k){
  train.rows <- m_HB$fold != j
  test.rows <- m_HB$fold == j
  
  # Fit a linear mixed-effects model (lmer) to the training data
  lmer1 <- lmer(score ~ (1|fullname) + round, data = m_HB[train.rows,])
  
  # Predict the scores on the test data using the trained model
  m_HB[test.rows,]$lmer1 <- predict(lmer1, newdata = m_HB[test.rows,], 
                                    type = 'response', allow.new.levels = TRUE)
}

# Calculate the root mean squared error (RMSE) for the predictions
sqrt(mean((m_HB$lmer1 - m_HB$score)^2))

# Compute the mean score for each male gymnast that participated in High Bar
m_HB_mean <- m_HB %>% group_by(fullname) %>% summarise(mean_lmer1 = mean(lmer1))


#=== Pommel Horse ===#
# Filter the data for men's Pommel Horse apparatus
m_PH <- filter(d, gender == 'm' & apparatus == 'PH')

# Define the number of folds for cross-validation
k = 5
# Create a vector 'folds' that assigns each observation to a fold
folds = rep(1:k, length.out = nrow(m_PH))
# Randomly assign each observation to a fold
m_PH$fold = sample(folds, nrow(m_PH), replace = FALSE)

# Initialize a column to store the predicted Pommel Horse scores
m_PH$lmer1 <- NA

# Perform k-fold cross-validation
for(j in 1:k){
  train.rows <- m_PH$fold != j
  test.rows <- m_PH$fold == j
  
  # Fit a linear mixed-effects model (lmer) to the training data
  lmer1 <- lmer(score ~ (1|fullname) + round + location, data = m_PH[train.rows,])
  
  # Predict the scores on the test data using the trained model
  m_PH[test.rows,]$lmer1 <- predict(lmer1, newdata = m_PH[test.rows,],
                                    type = 'response', allow.new.levels = TRUE)
}

# Calculate the root mean squared error (RMSE) for the predictions
sqrt(mean((m_PH$lmer1 - m_PH$score)^2))

# Compute the mean score for each male gymnast that participated in Pommel Horse
m_PH_mean <- m_PH %>% group_by(fullname) %>% summarise(mean_lmer1 = mean(lmer1))


#=== Floor Exercise ===#
# Filter the data for men's Floor Exercise apparatus
m_FX <- filter(d, gender == 'm' & apparatus == 'FX')

# Define the number of folds for cross-validation
k = 5
# Create a vector 'folds' that assigns each observation to a fold
folds = rep(1:k, length.out = nrow(m_FX))
# Randomly assign each observation to a fold
m_FX$fold = sample(folds, nrow(m_FX), replace = FALSE)

# Initialize a column to store the predicted Floor Exercise scores
m_FX$lmer1 <- NA

# Perform k-fold cross-validation
for(j in 1:k){
  train.rows <- m_FX$fold != j
  test.rows <- m_FX$fold == j
  
  # Fit a linear mixed-effects model (lmer) to the training data
  lmer1 <- lmer(score ~ (1|fullname) + round + location, data = m_FX[train.rows,])
  
  # Predict the scores on the test data using the trained model
  m_FX[test.rows,]$lmer1 <- predict(lmer1, newdata = m_FX[test.rows,],
                                    type = 'response', allow.new.levels = TRUE)
}

# Calculate the root mean squared error (RMSE) for the predictions
sqrt(mean((m_FX$lmer1 - m_FX$score)^2))

# Compute the mean score for each male gymnast that participated in Floor Exercise
m_FX_mean <- m_FX %>% group_by(fullname) %>% summarise(mean_lmer1 = mean(lmer1))


#=== Parallel Bars ===#
# Filter the data for men's Parallel Bars apparatus
m_PB <- filter(d, gender == 'm' & apparatus == 'PB')

# Define the number of folds for cross-validation
k = 5
# Create a vector 'folds' that assigns each observation to a fold
folds = rep(1:k, length.out = nrow(m_PB))
# Randomly assign each observation to a fold
m_PB$fold = sample(folds, nrow(m_PB), replace = FALSE)

# Initialize a column to store the predicted Parallel Bars scores
m_PB$lmer1 <- NA

# Perform k-fold cross-validation
for(j in 1:k){
  train.rows <- m_PB$fold != j
  test.rows <- m_PB$fold == j
  
  # Fit a linear mixed-effects model (lmer) to the training data
  lmer1 <- lmer(score ~ (1|fullname) + round + location, data = m_PB[train.rows,])

  # Predict the scores on the test data using the trained model
  m_PB[test.rows,]$lmer1 <- predict(lmer1, newdata = m_PB[test.rows,],
                                    type = 'response', allow.new.levels = TRUE)
}

# Calculate the root mean squared error (RMSE) for the predictions
sqrt(mean((m_PB$lmer1 - m_PB$score)^2))

# Compute the mean score for each male gymnast that participated in Parallel Bars
m_PB_mean <- m_PB %>% group_by(fullname) %>% summarise(mean_lmer1 = mean(lmer1))


#=== Still Rings ===#
# Filter the data for men's Still Rings apparatus
m_SR <- filter(d, gender == 'm' & apparatus == 'SR')

# Define the number of folds for cross-validation
k = 5
# Create a vector 'folds' that assigns each observation to a fold
folds = rep(1:k, length.out = nrow(m_SR))
# Randomly assign each observation to a fold
m_SR$fold = sample(folds, nrow(m_SR), replace = FALSE)

# Initialize a column to store the predicted Still Rings scores
m_SR$lmer1 <- NA

# Perform k-fold cross-validation
for(j in 1:k){
  train.rows <- m_SR$fold != j
  test.rows <- m_SR$fold == j

  # Fit a linear mixed-effects model (lmer) to the training data
  lmer1 <- lmer(score ~ (1|fullname) + round + location, data = m_SR[train.rows,])

  # Predict the scores on the test data using the trained model
  m_SR[test.rows,]$lmer1 <- predict(lmer1, newdata = m_SR[test.rows,],
                                    type = 'response', allow.new.levels = TRUE)
}

# Calculate the root mean squared error (RMSE) for the predictions
sqrt(mean((m_SR$lmer1 - m_SR$score)^2))

# Compute the mean score for each male gymnast that participated in Still Rings
m_SR_mean <- m_SR %>% group_by(fullname) %>% summarise(mean_lmer1 = mean(lmer1))


#=== Vault ===#
# Filter the data for men's Vault apparatus
m_VT <- filter(d, gender == 'm' & apparatus == 'VT')

# Define the number of folds for cross-validation
k = 5
# Create a vector 'folds' that assigns each observation to a fold
folds = rep(1:k, length.out = nrow(m_VT))
# Randomly assign each observation to a fold
m_VT$fold = sample(folds, nrow(m_VT), replace = FALSE)

# Initialize a column to store the predicted Vault scores
m_VT$lmer1 <- NA

# Perform k-fold cross-validation
for(j in 1:k){
  train.rows <- m_VT$fold != j
  test.rows <- m_VT$fold == j

  # Fit a linear mixed-effects model (lmer) to the training data
  lmer1 <- lmer(score ~ (1|fullname) + round + location, data = m_VT[train.rows,])
  
  # Predict the scores on the test data using the trained model
  m_VT[test.rows,]$lmer1 <- predict(lmer1, newdata = m_VT[test.rows,],
                                    type = 'response', allow.new.levels = TRUE)
}

# Calculate the root mean squared error (RMSE) for the predictions
sqrt(mean((m_VT$lmer1 - m_VT$score)^2))

# Compute the mean score for each male gymnast that participated in Vault
m_VT_mean <- m_VT %>% group_by(fullname) %>% summarise(mean_lmer1 = mean(lmer1))



#=========================#
#=== Women's Apparatus ===#
#=========================#

#=== Balance Beam ===#
# Filter the data for women's Balance Beam apparatus
w_BB <- filter(d, gender == 'w' & apparatus == 'BB')

# Define the number of folds for cross-validation
k = 5
# Create a vector 'folds' that assigns each observation to a fold
folds = rep(1:k, length.out = nrow(w_BB))
# Randomly assign each observation to a fold
w_BB$fold = sample(folds, nrow(w_BB), replace = FALSE)

# Initialize a column to store the predicted Balance Beam scores
w_BB$lmer1 <- NA

# Perform k-fold cross-validation
for(j in 1:k){
  train.rows <- w_BB$fold != j
  test.rows <- w_BB$fold == j

  # Fit a linear mixed-effects model (lmer) to the training data
  lmer1 <- lmer(score ~ (1|fullname) + round + location, data = w_BB[train.rows,])
  
  # Predict the scores on the test data using the trained model
  w_BB[test.rows,]$lmer1 <- predict(lmer1, newdata = w_BB[test.rows,],
                                    type = 'response', allow.new.levels = TRUE)
}

# Calculate the root mean squared error (RMSE) for the predictions
sqrt(mean((w_BB$lmer1 - w_BB$score)^2))

# Compute the mean score for each male gymnast that participated in High Bar
w_BB_mean <- w_BB %>% group_by(fullname) %>% summarise(mean_lmer1 = mean(lmer1))


#=== Floor Exercise ===#
# Filter the data for women's Floor Exercise apparatus
w_FX <- filter(d, gender == 'w' & apparatus == 'FX')

# Define the number of folds for cross-validation
k = 5
# Create a vector 'folds' that assigns each observation to a fold
folds = rep(1:k, length.out = nrow(w_FX))
# Randomly assign each observation to a fold
w_FX$fold = sample(folds, nrow(w_FX), replace = FALSE)

# Initialize a column to store the predicted Floor Exercise scores
w_FX$lmer1 <- NA

# Perform k-fold cross-validation
for(j in 1:k){
  train.rows <- w_FX$fold != j
  test.rows <- w_FX$fold == j
  
  # Fit a linear mixed-effects model (lmer) to the training data
  lmer1 <- lmer(score ~ (1|fullname) + round + location, data = w_FX[train.rows,])

  # Predict the scores on the test data using the trained model
  w_FX[test.rows,]$lmer1 <- predict(lmer1, newdata = w_FX[test.rows,],
                                    type = 'response', allow.new.levels = TRUE)
}

# Calculate the root mean squared error (RMSE) for the predictions
sqrt(mean((w_FX$lmer1 - w_FX$score)^2))

# Compute the mean score for each male gymnast that participated in Floor Exercise
w_FX_mean <- w_FX %>% group_by(fullname) %>% summarise(mean_lmer1 = mean(lmer1))


#=== Uneven Bars ===#
# Filter the data for women's Uneven Bars apparatus
w_UB <- filter(d, gender == 'w' & apparatus == 'UB')

# Define the number of folds for cross-validation
k = 5
# Create a vector 'folds' that assigns each observation to a fold
folds = rep(1:k, length.out = nrow(w_UB))
# Randomly assign each observation to a fold
w_UB$fold = sample(folds, nrow(w_UB), replace = FALSE)

# Initialize a column to store the predicted Uneven Bars scores
w_UB$lmer1 <- NA

# Perform k-fold cross-validation
for(j in 1:k){
  train.rows <- w_UB$fold != j
  test.rows <- w_UB$fold == j
  
  # Fit a linear mixed-effects model (lmer) to the training data
  lmer1 <- lmer(score ~ (1|fullname) + round + location, data = w_UB[train.rows,])
  
  # Predict the scores on the test data using the trained model
  w_UB[test.rows,]$lmer1 <- predict(lmer1, newdata = w_UB[test.rows,],
                                    type = 'response', allow.new.levels = TRUE)
}

# Calculate the root mean squared error (RMSE) for the predictions
sqrt(mean((w_UB$lmer1 - w_UB$score)^2))

# Compute the mean score for each male gymnast that participated in High Bar
w_UB_mean <- w_UB %>% group_by(fullname) %>% summarise(mean_lmer1 = mean(lmer1))


#=== Vault ===#
# Filter the data for women's Vault apparatus
w_VT <- filter(d, gender == 'w' & apparatus == 'VT')

w_VT <- w_VT %>% mutate(round = if_else(round == "AAqual", "qual", round))

# Define the number of folds for cross-validation
k = 5
# Create a vector 'folds' that assigns each observation to a fold
folds = rep(1:k, length.out = nrow(w_VT))
# Randomly assign each observation to a fold
w_VT$fold = sample(folds, nrow(w_VT), replace = FALSE)

# Initialize a column to store the predicted Vault scores
w_VT$lmer1 <- NA

# Perform k-fold cross-validation
for(j in 1:k){
  train.rows <- w_VT$fold != j
  test.rows <- w_VT$fold == j
  
  # Fit a linear mixed-effects model (lmer) to the training data
  lmer1 <- lmer(score ~ (1|fullname) + round + location, data = w_VT[train.rows,])
  
  # Predict the scores on the test data using the trained model
  w_VT[test.rows,]$lmer1 <- predict(lmer1, newdata = w_VT[test.rows,],
                                    type = 'response', allow.new.levels = TRUE)
}

# Calculate the root mean squared error (RMSE) for the predictions
sqrt(mean((w_VT$lmer1 - w_VT$score)^2))

# Compute the mean score for each male gymnast that participated in Vault
w_VT_mean <- w_VT %>% group_by(fullname) %>% summarise(mean_lmer1 = mean(lmer1))
