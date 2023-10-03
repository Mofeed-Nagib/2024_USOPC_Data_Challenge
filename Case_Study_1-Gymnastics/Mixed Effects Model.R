set.seed(12)

d <- later_scores

# Men High Bar
m_HB <- filter(d, gender=='m' & apparatus=='HB')
m_HB$country <- factor(m_HB$country)  

k=5
folds=rep(1:k, length.out=nrow(m_HB))
m_HB$fold = sample(folds, nrow(m_HB), replace=F)


m_HB$lmer1 <- NA
lmer1<- lmer(score ~ (1|fullname) + round, data = m_HB[train.rows,])
for(j in 1:k){
  cat(j, "")
  train.rows <- m_HB$fold!=j
  test.rows <- m_HB$fold==j
  
  lmer1<- lmer(score ~ (1|fullname) + round, data = m_HB[train.rows,])
  
  m_HB[test.rows,]$lmer1 <- predict(lmer1, newdata = m_HB[test.rows,], type = 'response', allow.new.levels = T)


}
# error = 1.39
sqrt(mean((m_HB$lmer1 - m_HB$score)^2))
AIC_1 <- AIC(lmer1)
print(AIC_1)
# Men Pommel Horse

m_PH <- filter(d, gender=='m' & apparatus=='PH')


k=5
folds=rep(1:k, length.out=nrow(m_PH))
m_PH$fold = sample(folds, nrow(m_PH), replace=F)

m_PH$lmer1 <- NA
lmer2<- lmer(score ~ (1|fullname) + round + location, data = m_PH[train.rows,])
for(j in 1:k){
  cat(j, "")
  train.rows <- m_PH$fold!=j
  test.rows <- m_PH$fold==j
  
  
  lmer1<- lmer(score ~ (1|fullname) + round + location, data = m_PH[train.rows,])
  
  m_PH[test.rows,]$lmer1 <- predict(lmer1, newdata = m_PH[test.rows,], type = 'response', allow.new.levels = T)

}

# error = 1.53
sqrt(mean((m_PH$lmer1 - m_PH$score)^2))


AIC_2 <- AIC(lmer2)
print(AIC_2)
# Men Floor

m_FX <- filter(d, gender=='m' & apparatus=='FX')


k=5
folds=rep(1:k, length.out=nrow(m_FX))
m_FX$fold = sample(folds, nrow(m_FX), replace=F)

m_FX$lm1 <- NA
m_FX$lmer1 <- NA
for(j in 1:k){
  cat(j, "")
  train.rows <- m_FX$fold!=j
  test.rows <- m_FX$fold==j
  
  # lm1 <- lm(score ~ apparatus + fullname + round, data = d[train.rows,])
  lmer1<- lmer(score ~ (1|fullname) + round + location, data = m_FX[train.rows,])
  
  # d[test.rows,]$lm1 <- predict(lm1, newdata = d[test.rows,], type = 'response')
  m_FX[test.rows,]$lmer1 <- predict(lmer1, newdata = m_FX[test.rows,], type = 'response', allow.new.levels = T)
}
# sqrt(mean(d$lm1 - d$score)^2)
# error = 1.43
sqrt(mean((m_FX$lmer1 - m_FX$score)^2))


# Men Parallel Bars

m_PB <- filter(d, gender=='m' & apparatus=='PB')


k=5
folds=rep(1:k, length.out=nrow(m_PB))
m_PB$fold = sample(folds, nrow(m_PB), replace=F)

m_PB$lm1 <- NA
m_PB$lmer1 <- NA
for(j in 1:k){
  cat(j, "")
  train.rows <- m_PB$fold!=j
  test.rows <- m_PB$fold==j
  
  # lm1 <- lm(score ~ apparatus + fullname + round, data = d[train.rows,])
  lmer1<- lmer(score ~ (1|fullname) + round + location, data = m_PB[train.rows,])
  
  # d[test.rows,]$lm1 <- predict(lm1, newdata = d[test.rows,], type = 'response')
  m_PB[test.rows,]$lmer1 <- predict(lmer1, newdata = m_PB[test.rows,], type = 'response', allow.new.levels = T)
}
# sqrt(mean(d$lm1 - d$score)^2)
# error = 1.27
sqrt(mean((m_PB$lmer1 - m_PB$score)^2))

# Men Rings

m_SR <- filter(d, gender=='m' & apparatus=='SR')


k=5
folds=rep(1:k, length.out=nrow(m_SR))
m_SR$fold = sample(folds, nrow(m_SR), replace=F)

m_SR$lm1 <- NA
m_SR$lmer1 <- NA
for(j in 1:k){
  cat(j, "")
  train.rows <- m_SR$fold!=j
  test.rows <- m_SR$fold==j
  
  # lm1 <- lm(score ~ apparatus + fullname + round, data = d[train.rows,])
  lmer1<- lmer(score ~ (1|fullname) + round + location, data = m_SR[train.rows,])
  
  # d[test.rows,]$lm1 <- predict(lm1, newdata = d[test.rows,], type = 'response')
  m_SR[test.rows,]$lmer1 <- predict(lmer1, newdata = m_SR[test.rows,], type = 'response', allow.new.levels = T)
}
# sqrt(mean(d$lm1 - d$score)^2)
# error = 1.28
sqrt(mean((m_SR$lmer1 - m_SR$score)^2))

# Men Vault

m_VT <- filter(d, gender=='m' & apparatus=='VT')


k=5
folds=rep(1:k, length.out=nrow(m_VT))
m_VT$fold = sample(folds, nrow(m_VT), replace=F)

m_VT$lm1 <- NA
m_VT$lmer1 <- NA
for(j in 1:k){
  cat(j, "")
  train.rows <- m_VT$fold!=j
  test.rows <- m_VT$fold==j
  
  # lm1 <- lm(score ~ apparatus + fullname + round, data = d[train.rows,])
  lmer1<- lmer(score ~ (1|fullname) + round + location, data = m_VT[train.rows,])
  
  # d[test.rows,]$lm1 <- predict(lm1, newdata = d[test.rows,], type = 'response')
  m_VT[test.rows,]$lmer1 <- predict(lmer1, newdata = m_VT[test.rows,], type = 'response', allow.new.levels = T)
}
# sqrt(mean(d$lm1 - d$score)^2)
# error = 2.26
sqrt(mean((m_VT$lmer1 - m_VT$score)^2))


# Women Balance Beam

w_BB <- filter(d, gender=='w' & apparatus=='BB')


k=5
folds=rep(1:k, length.out=nrow(w_BB))
w_BB$fold = sample(folds, nrow(w_BB), replace=F)

w_BB$lm1 <- NA
w_BB$lmer1 <- NA
for(j in 1:k){
  cat(j, "")
  train.rows <- w_BB$fold!=j
  test.rows <- w_BB$fold==j
  
  # lm1 <- lm(score ~ apparatus + fullname + round, data = d[train.rows,])
  lmer1<- lmer(score ~ (1|fullname) + round + location, data = w_BB[train.rows,])
  
  # d[test.rows,]$lm1 <- predict(lm1, newdata = d[test.rows,], type = 'response')
  w_BB[test.rows,]$lmer1 <- predict(lmer1, newdata = w_BB[test.rows,], type = 'response', allow.new.levels = T)
}
# sqrt(mean(d$lm1 - d$score)^2)
# error = .99
sqrt(mean((w_BB$lmer1 - w_BB$score)^2))


# Women Floor

w_FX <- filter(d, gender=='w' & apparatus=='FX')


k=5
folds=rep(1:k, length.out=nrow(w_FX))
w_FX$fold = sample(folds, nrow(w_FX), replace=F)

w_FX$lm1 <- NA
w_FX$lmer1 <- NA
for(j in 1:k){
  cat(j, "")
  train.rows <- w_FX$fold!=j
  test.rows <- w_FX$fold==j
  
  # lm1 <- lm(score ~ apparatus + fullname + round, data = d[train.rows,])
  lmer1<- lmer(score ~ (1|fullname) + round + location, data = w_FX[train.rows,])
  
  # d[test.rows,]$lm1 <- predict(lm1, newdata = d[test.rows,], type = 'response')
  w_FX[test.rows,]$lmer1 <- predict(lmer1, newdata = w_FX[test.rows,], type = 'response', allow.new.levels = T)
}
# sqrt(mean(d$lm1 - d$score)^2)
# error = .94
sqrt(mean((w_FX$lmer1 - w_FX$score)^2))

# Women Unbalanced Beam

w_UB <- filter(d, gender=='w' & apparatus=='UB')


k=5
folds=rep(1:k, length.out=nrow(w_UB))
w_UB$fold = sample(folds, nrow(w_UB), replace=F)

w_UB$lm1 <- NA
w_UB$lmer1 <- NA
for(j in 1:k){
  cat(j, "")
  train.rows <- w_UB$fold!=j
  test.rows <- w_UB$fold==j
  
  # lm1 <- lm(score ~ apparatus + fullname + round, data = d[train.rows,])
  lmer1<- lmer(score ~ (1|fullname) + round + location, data = w_UB[train.rows,])
  
  # d[test.rows,]$lm1 <- predict(lm1, newdata = d[test.rows,], type = 'response')
  w_UB[test.rows,]$lmer1 <- predict(lmer1, newdata = w_UB[test.rows,], type = 'response', allow.new.levels = T)
}
# sqrt(mean(d$lm1 - d$score)^2)
# error = 1.17
sqrt(mean((w_UB$lmer1 - w_UB$score)^2))

# Women Vault

w_VT <- filter(d, gender=='w' & apparatus=='VT')


k=5
folds=rep(1:k, length.out=nrow(w_VT))
w_VT$fold = sample(folds, nrow(w_VT), replace=F)

w_VT$lm1 <- NA
w_VT$lmer1 <- NA
for(j in 1:k){
  cat(j, "")
  train.rows <- w_VT$fold!=j
  test.rows <- w_VT$fold==j
  
  # lm1 <- lm(score ~ apparatus + fullname + round, data = d[train.rows,])
  lmer1<- lmer(score ~ (1|fullname) + round + location, data = w_VT[train.rows,])
  
  # d[test.rows,]$lm1 <- predict(lm1, newdata = d[test.rows,], type = 'response')
  w_VT[test.rows,]$lmer1 <- predict(lmer1, newdata = w_VT[test.rows,], type = 'response', allow.new.levels = T)
}
# sqrt(mean(d$lm1 - d$score)^2)
# error = 1.88
sqrt(mean((w_VT$lmer1 - w_VT$score)^2))
