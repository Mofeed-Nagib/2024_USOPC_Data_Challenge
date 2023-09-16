## This script predicts scores for each gymnast and apparatus.

#===============================================#
#=== player score distributions by apparatus ===#
#===============================================#

# create data frame to hold gymnast names, country, apparatus, score means and sds, and sample size
gymnast_dist <- data.frame()

# for each gymnast (unique name)
for (x in unique(later_scores$fullname)) {
  
  # grab that gymnast's events
  apparatus = unique(later_scores[later_scores$fullname == x, ]$apparatus)
  
  # for each apparatus
  for (y in apparatus) {
    
    # get scores for current gymnast and apparatus
    gymnast_apparatus_scores <- later_scores %>% filter(fullname == x & apparatus == y)
    
    # get sample size of each gymnast + apparatus combo
    sample_size <- as.numeric(length(gymnast_apparatus_scores$score))
    
    if (sample_size > 1 & sd(gymnast_apparatus_scores$score, na.rm=TRUE) != 0) {
      # take mean of scores
      dist_mean <- mean(gymnast_apparatus_scores$score)
    
      # calculate sd under CLT
      dist_sd <- sd(gymnast_apparatus_scores$score) / sqrt(sample_size)
      
      # create row of dataframe
      current_row <- data.frame("fullname" = x, "apparatus" = y, "mean" = dist_mean, "sd" = dist_sd, "sample_size" = sample_size)
      
      # stack onto df
      gymnast_dist <- rbind(gymnast_dist, current_row)
    }
  }
}

# make histograms of gymnast distribution by apparatus
for (i in seq(1, nrow(gymnast_dist))) {
  
  # generate 10000 Gaussian deviates from mean and standard deviation
  data = rnorm(10000, mean = as.numeric(gymnast_dist[i, 'mean']), sd = as.numeric(gymnast_dist[i, 'sd']))

  # plot histogram with 20 bins
  hist(data, main = paste("Histogram of", paste0(gymnast_dist[i, 'fullname'], "'s"), gymnast_dist[i, 'apparatus'],
                          "Score Distribution"), xlab = "Scores", col = "blue", breaks = 20)
}

#============================#
#=== score prediction lms ===#
#============================#

# # linear model with all predictors
# lm_full <- lm(score ~ ., data = later_scores)
#   # evaluation with r-squared
#   summary(lm_full)


# stepAIC(lm_full, scope = formula(lm0), direction = "backward", data = data_sample)
