

d <- later_scores

summary(model)

set.seed(361)
lm2 <- lm(score ~ apparatus + lastname + round, data = d)
lmer2 <- lmer(score ~ (1 + apparatus|lastname) + round, data = d)

k=5
folds=rep(1:k, length.out=nrow(d))
d$fold = sample(folds, nrow(d), replace=F)

for(j in 1:k){
  train.rows <- d$fold!=j
  test.rows <- d$fold==j
  
  lm1 <- lm(score ~ apparatus + lastname + round, data =d[train.rows,])
  
  lmer1<- lmer(score ~ (1 + apparatus|lastname) + round, data=d[train.rows,])
  

  
  d$lm1[test.rows] <- predict(lm1, newdata=d[test.rows,])
  d$lmer1[test.rows] <- predict(lmer1, newdata=d[test.rows,])
}

d %>% dplyr::select(score, lastname, lm1, lmer1)

sqrt(mean(d$lm1 - d$score)^2)

sqrt(mean(d$lmer1 - d$score)^2)

