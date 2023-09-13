set.seed(1)

d <- later_scores

k=5
folds=rep(1:k, length.out=nrow(d))
d$fold = sample(folds, nrow(d), replace=F)

d$lm1 <- NA
d$lmer1 <- NA
for(j in 1:k){
  cat(j, "")
  train.rows <- d$fold!=j
  test.rows <- d$fold==j
  
  # lm1 <- lm(score ~ apparatus + fullname + round, data = d[train.rows,])
  lmer1<- lmer(score ~ (1 + apparatus|fullname) + round, data = d[train.rows,])
  
  # d[test.rows,]$lm1 <- predict(lm1, newdata = d[test.rows,], type = 'response')
  d[test.rows,]$lmer1 <- predict(lmer1, newdata = d[test.rows,], type = 'response', allow.new.levels = T)
}

# sqrt(mean(d$lm1 - d$score)^2)
sqrt(mean(d$lmer1 - d$score)^2)
