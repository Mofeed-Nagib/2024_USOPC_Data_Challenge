## This script predicts scores for each gymnast and apparatus.

# create list to hold gymnast means and sds
ls_gymnast_dist <- list()

# For each gymnast (unique name)
for (x in later_scores$fullname) {
  
  # grab that gymnast's events
  apparatus = unique(later_scores[later_scores$fullname == x, ]$apparatus)
  
  # for each apparatus
  for (y in apparatus) {
    
    # get scores for current gymnast and apparatus
    gymnast_apparatus_scores <- later_scores %>% filter(fullname == x & apparatus == y)
    
    # get sample size of each gymnast + apparatus combo
    sample_size <- as.numeric(length(gymnast_apparatus_scores$score))
    
    # take mean of scores
    dist_mean <- mean(gymnast_apparatus_scores$score)
    
    # calculate sd under CLT (?? validity of this -- consult with Brian)
    dist_sd <- sd(gymnast_apparatus_scores$score) / sqrt(sample_size)
    
    # save mean and sd into list 
    ls_gymnast_dist[[x]][[y]] <- c("fullname" = x, "apparatus" = y, "mean" = dist_mean, "sd" = dist_sd, "sample_size" = sample_size)
  }
}

# Make histograms of gymnast distribution by apparatus
for (i in ls_gymnast_dist) {
  # 
  # # Generate 10000 Gaussian deviates from mean and standard deviation
  # data = rnorm(10000, mean = i["mean"], sd = i["sd"])
  # 
  # # Plot histogram with 40 bins
  # hist(data, breaks=40, col="red", main=paste("Histogram of ", i['fullname'], "'s ", i['apparatus'], " Score Distribution"), col.main="blue")
}


# Grab that gymnast's earlier scores for that apparatus

# Grab that gymnast's later scores for that apparatus
