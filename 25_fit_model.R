## This script predicts scores for each gymnast and apparatus.

#===============================================#
#=== player score distributions by apparatus ===#
#===============================================#

# create data frame to hold gymnast names, country, apparatus, score means and sds,
# and sample size
gymnast_dist <- data.frame()

# for each gymnast (unique name)
for (x in unique(later_scores$fullname)) {
  
  # grab that gymnast's apparatus events and gender
  apparatus = unique(later_scores[later_scores$fullname == x, ]$apparatus)
  gender = unique(later_scores[later_scores$fullname == x, ]$gender)
  
  # for each apparatus
  for (y in apparatus) {
    # for each gender
    for (z in gender)
      # get scores for current gymnast and apparatus
      gymnast_apparatus_scores <- later_scores %>% 
                                  filter(fullname == x & apparatus == y & gender == z)
      
      # get sample size of each gymnast + apparatus combo
      sample_size <- as.numeric(length(gymnast_apparatus_scores$score))
      
      if (sample_size > 1 & sd(gymnast_apparatus_scores$score, na.rm = TRUE) != 0) {
        # take mean of scores
        dist_mean <- mean(gymnast_apparatus_scores$score)
      
        # calculate sd under CLT
        dist_sd <- sd(gymnast_apparatus_scores$score) / sqrt(sample_size)
        
        # create row of dataframe
        current_row <- data.frame("fullname" = x, "apparatus" = y, "gender" = z,
                                  "mean" = dist_mean, "sd" = dist_sd,
                                  "sample_size" = sample_size)
        
        # stack onto df
        gymnast_dist <- rbind(gymnast_dist, current_row)
        
    }
  }
}

# initialize empty column in gymnast distributions dataframe to store the calculated 
# mixed-effects model mean score for each gender and apparatus combination
gymnast_dist$lmer_mean <- NA

# === Men's High Bar ===#
# for each gymnast with a distribution
for(i in 1:nrow(gymnast_dist)) {
  # check if the gymnast is male and participated in High Bar apparatus
  if (gymnast_dist[i,]$apparatus == "HB" & gymnast_dist[i,]$gender == "m") {
    # filter the men's High Bar mean data for the current gymnast
    m_HB_mean_row <- m_HB_mean %>% filter(fullname == gymnast_dist[i,]$fullname)
    # store the calculated mixed-effects model mean score in High Bar for this gymnast
    gymnast_dist[i,]$lmer_mean <- m_HB_mean_row$mean_lmer1
  }
}

# === Men's Pommel Horse ===#
# for each gymnast with a distribution
for(i in 1:nrow(gymnast_dist)) {
  # check if the gymnast is male and participated in Pommel Horse apparatus
  if (gymnast_dist[i,]$apparatus == "PH" & gymnast_dist[i,]$gender == "m") {
    # filter the men's Pommel Horse mean data for the current gymnast
    m_PH_mean_row <- m_PH_mean %>% filter(fullname == gymnast_dist[i,]$fullname)
    # store the calculated mixed-effects model mean score in Pommel Horse for this gymnast
    gymnast_dist[i,]$lmer_mean <- m_PH_mean_row$mean_lmer1
  }
}

# === Men's Floor Exercise ===#
# for each gymnast with a distribution
for(i in 1:nrow(gymnast_dist)) {
  # check if the gymnast is male and participated in Floor Exercise apparatus
  if (gymnast_dist[i,]$apparatus == "FX" & gymnast_dist[i,]$gender == "m") {
    # filter the men's Floor Exercise mean data for the current gymnast
    m_FX_mean_row <- m_FX_mean %>% filter(fullname == gymnast_dist[i,]$fullname)
    # store the calculated mixed-effects model mean score in Floor Exercise for this gymnast
    gymnast_dist[i,]$lmer_mean <- m_FX_mean_row$mean_lmer1
  }
}

# === Men's Parallel Bars ===#
# for each gymnast with a distribution
for(i in 1:nrow(gymnast_dist)) {
  # check if the gymnast is male and participated in Parallel Bars apparatus
  if (gymnast_dist[i,]$apparatus == "PB" & gymnast_dist[i,]$gender == "m") {
    # filter the men's Parallel Bars mean data for the current gymnast
    m_PB_mean_row <- m_PB_mean %>% filter(fullname == gymnast_dist[i,]$fullname)
    # store the calculated mixed-effects model mean score in Parallel Bars for this gymnast
    gymnast_dist[i,]$lmer_mean <- m_PB_mean_row$mean_lmer1
  }
}

# === Men's Still Rings ===#
# for each gymnast with a distribution
for(i in 1:nrow(gymnast_dist)) {
  # check if the gymnast is male and participated in Still Rings apparatus
  if (gymnast_dist[i,]$apparatus == "SR" & gymnast_dist[i,]$gender == "m") {
    # filter the men's Still Rings mean data for the current gymnast
    m_SR_mean_row <- m_SR_mean %>% filter(fullname == gymnast_dist[i,]$fullname)
    # store the calculated mixed-effects model mean score in Still Rings for this gymnast
    gymnast_dist[i,]$lmer_mean <- m_SR_mean_row$mean_lmer1
  }
}

# === Men's Vault ===#
# for each gymnast with a distribution
for(i in 1:nrow(gymnast_dist)) {
  # check if the gymnast is male and participated in Vault apparatus
  if (gymnast_dist[i,]$apparatus == "VT" & gymnast_dist[i,]$gender == "m") {
    # filter the men's Vault mean data for the current gymnast
    m_VT_mean_row <- m_VT_mean %>% filter(fullname == gymnast_dist[i,]$fullname)
    # store the calculated mixed-effects model mean score in Vault for this gymnast
    gymnast_dist[i,]$lmer_mean <- m_VT_mean_row$mean_lmer1
  }
}

# === Women's Balance Beam ===#
# for each gymnast with a distribution
for(i in 1:nrow(gymnast_dist)) {
  # check if the gymnast is female and participated in Balance Beam apparatus
  if (gymnast_dist[i,]$apparatus == "BB" & gymnast_dist[i,]$gender == "w") {
    # filter the women's Balance Beam mean data for the current gymnast
    w_BB_mean_row <- w_BB_mean %>% filter(fullname == gymnast_dist[i,]$fullname)
    # store the calculated mixed-effects model mean score in Balance Beam for this gymnast
    gymnast_dist[i,]$lmer_mean <- w_BB_mean_row$mean_lmer1
  }
}

# === Women's Floor Exercise ===#
# for each gymnast with a distribution
for(i in 1:nrow(gymnast_dist)) {
  # check if the gymnast is female and participated in Floor Exercise apparatus
  if (gymnast_dist[i,]$apparatus == "FX" & gymnast_dist[i,]$gender == "w") {
    # filter the women's Floor Exercise mean data for the current gymnast
    w_FX_mean_row <- w_FX_mean %>% filter(fullname == gymnast_dist[i,]$fullname)
    # store the calculated mixed-effects model mean score in Floor Exercise for this gymnast
    gymnast_dist[i,]$lmer_mean <- w_FX_mean_row$mean_lmer1
  }
}

# === Women's Uneven Bars ===#
# for each gymnast with a distribution
for(i in 1:nrow(gymnast_dist)) {
  # check if the gymnast is female and participated in Uneven Bars apparatus
  if (gymnast_dist[i,]$apparatus == "UB" & gymnast_dist[i,]$gender == "w") {
    # filter the women's Uneven Bars mean data for the current gymnast
    w_UB_mean_row <- w_UB_mean %>% filter(fullname == gymnast_dist[i,]$fullname)
    # store the calculated mixed-effects model mean score in Uneven Bars for this gymnast
    gymnast_dist[i,]$lmer_mean <- w_UB_mean_row$mean_lmer1
  }
}

# === Women's Vault ===#
# for each gymnast with a distribution
for(i in 1:nrow(gymnast_dist)) {
  # check if the gymnast is female and participated in Vault apparatus
  if (gymnast_dist[i,]$apparatus == "VT" & gymnast_dist[i,]$gender == "w") {
    # filter the women's Vault mean data for the current gymnast
    w_VT_mean_row <- w_VT_mean %>% filter(fullname == gymnast_dist[i,]$fullname)
    # store the calculated mixed-effects model mean score in Vault for this gymnast
    gymnast_dist[i,]$lmer_mean <- w_VT_mean_row$mean_lmer1
  }
}


# #=== Check mean squared error ===#
# later_scores$distscore <- NA
# for (i in 1:nrow(later_scores)) {
#   for (j in 1:nrow(gymnast_dist))
#     if (later_scores$fullname[i] == gymnast_dist$fullname[j] &
#         later_scores$apparatus[i] == gymnast_dist$apparatus[j]) {
#       later_scores$distscore[i] <- gymnast_dist$mean[j]
#     }
# }
# 
# later_scores <- later_scores %>%
#                 mutate(distscore = ifelse(is.na(distscore), 0, distscore))
# 
# sqrt(mean((later_scores$distscore - later_scores$score)^2))


#====================================================#
#=== plot player score distributions by apparatus ===#
#====================================================#

# # using mean, make histograms of gymnast distribution by apparatus
# for (i in seq(1, nrow(gymnast_dist))) {
# 
#   # generate 10000 Gaussian deviates from mean and standard deviation
#   data = rnorm(10000, mean = as.numeric(gymnast_dist[i, 'mean']),
#                sd = as.numeric(gymnast_dist[i, 'sd']))
# 
#   # plot histogram with 20 bins
#   hist(data, main = paste("Histogram of", paste0(gymnast_dist[i, 'fullname'], "'s"),
#                           gymnast_dist[i, 'apparatus'], "Score Distribution"),
#        xlab = "Scores", col = "blue", breaks = 20)
# }
# 
# # using lmer_mean, make histograms of gymnast distribution by apparatus
# for (i in seq(1, nrow(gymnast_dist))) {
# 
#   # generate 10000 Gaussian deviates from lmer_mean and standard deviation
#   data = rnorm(10000, mean = as.numeric(gymnast_dist[i, 'lmer_mean']),
#                sd = as.numeric(gymnast_dist[i, 'sd']))
# 
#   # plot histogram with 20 bins
#   hist(data, main = paste("Histogram of", paste0(gymnast_dist[i, 'fullname'], "'s"),
#                           gymnast_dist[i, 'apparatus'], "Score Distribution"),
#        xlab = "Scores", col = "blue", breaks = 20)
# }

#============================#
#=== score prediction lms ===#
#============================#

# # separate scores by gender and remove direct predictors of score or irrelevant columns
# # removing lastname, firstname, gender, e_score, d_score, penalty
# men_later_scores <- later_scores[later_scores$gender == "m", c(-1, -2, -3, -11, -12, -13)]
# # removing lastname, firstname, gender, e_score, d_score, penalty
# women_later_scores <- later_scores[later_scores$gender == "w", c(-1, -2, -3, -11, -12, -13)]
# 
# # Forward AIC model for men's scores
# men_lm_full <- lm(score ~ ., data = na.omit(men_later_scores))
# men_lm0 <- lm(score ~ 1, data = na.omit(men_later_scores))
# men_forward_AIC <- stepAIC(men_lm0, scope = formula(men_lm_full),
#                            direction = "forward", data = men_later_scores)
# summary(men_forward_AIC)$adj.r.squared
# 
# # Forward AIC model for women's scores
# women_lm_full <- lm(score ~ ., data = na.omit(women_later_scores))
# women_lm0 <- lm(score ~ 1, data = na.omit(women_later_scores))
# women_forward_AIC <- stepAIC(women_lm0, scope = formula(women_lm_full),
#                              direction = "forward", data = women_later_scores)
# summary(women_forward_AIC)$adj.r.squared
