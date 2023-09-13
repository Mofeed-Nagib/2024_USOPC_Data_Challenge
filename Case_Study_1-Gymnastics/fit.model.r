## This script predicts scores for each gymnast and apparatus.

# create list to hold gymnast means and sds
ls_gymnast_dist <- list()

# for each gymnast (unique name)
for (x in later_scores$fullname) {
  
  # grab that gymnast's events
  apparatus = unique(later_scores[later_scores$fullname == x, ]$apparatus)
  
  # for each apparatus
  for (y in apparatus) {
    
    # get scores for current gymnast and apparatus
    gymnast_apparatus_scores <- later_scores %>% filter(fullname == x & apparatus == y)
    
    # get sample size of each gymnast + apparatus combo
    sample_size <- as.numeric(length(gymnast_apparatus_scores$score))
    
    if (sample_size > 1) {
      # take mean of scores
      dist_mean <- mean(gymnast_apparatus_scores$score)
    
      # calculate sd under CLT (?? validity of this -- consult with Brian)
      dist_sd <- sd(gymnast_apparatus_scores$score) / sqrt(sample_size)
      
      # save mean and sd into list 
      ls_gymnast_dist[[x]][[y]] <- c("fullname" = x, "apparatus" = y, "mean" = dist_mean, "sd" = dist_sd, "sample_size" = sample_size)
    }
  }
}

# make histograms of gymnast distribution by apparatus
for (i in seq(1, length(ls_gymnast_dist))) {
  for (j in seq(1, length(ls_gymnast_dist[[i]]))) {
    # generate 10000 Gaussian deviates from mean and standard deviation
    data = rnorm(10000, mean = as.numeric(ls_gymnast_dist[[i]][[j]]['mean']), sd = as.numeric(ls_gymnast_dist[[i]][[j]]['sd']))

    # plot histogram with 20 bins
    hist(data, breaks=20, col="red", main=paste("Histogram of", paste0(ls_gymnast_dist[[i]][[j]]['fullname'], "'s"), ls_gymnast_dist[[i]][[j]]['apparatus'], "Score Distribution"), xlab = "Scores")
  }
}
