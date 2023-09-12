## This script predicts scores for each gymnast and apparatus.

# For each gymnast (unique name)
for (x in later_scores$fullname) {
  apparatus = unique(later_scores[later_scores$fullname == x, ]$apparatus)
  print(apparatus)
  for (y in apparatus) {
    gymnast_apparatus_scores <- later_scores %>% filter(fullname == x & apparatus == y)
    
  }
}

# For each apparatus

# Grab that gymnast's earlier scores for that apparatus

# Grab that gymnast's later scores for that apparatus
