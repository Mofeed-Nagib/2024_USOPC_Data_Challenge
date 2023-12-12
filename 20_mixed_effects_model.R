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

m_HB$lmer1 <- NA
m_HB$lmer2 <- NA
m_HB$lmer3 <- NA
m_HB$lmer4 <- NA
m_HB$pred <- NA

#Chose model as it had best AIC and SE amongst lmer models. Also, we chose lmer as we found that it had by far the lowest standard error compared to lm and using the mean data for each player
lmer1<- lmer(score ~ (1|fullname) + round, data = m_HB)
lmer2<- lmer(score ~ (1|fullname) + location, data = m_HB[train.rows,])
lmer3<- lmer(score ~ (1|fullname) + competition, data = m_HB[train.rows,])
lmer4<- lmer(score ~ (1|fullname), data = m_HB[train.rows,])

for(j in 1:k){
  cat(j, "")
  train.rows <- m_HB$fold!=j
  test.rows <- m_HB$fold==j
  
  lmer1<- lmer(score ~ (1|fullname) + round, data = m_HB[train.rows,])
  m_HB[test.rows,]$lmer1 <- predict(lmer1, newdata = m_HB[test.rows,], type = 'response', allow.new.levels = T)
  lmer2<- lmer(score ~ (1|fullname) + location, data = m_HB[train.rows,])
  m_HB[test.rows,]$lmer2 <- predict(lmer2, newdata = m_HB[test.rows,], type = 'response', allow.new.levels = T)
  lmer3<- lmer(score ~ (1|fullname) + competition, data = m_HB[train.rows,])
  m_HB[test.rows,]$lmer3 <- predict(lmer3, newdata = m_HB[test.rows,], type = 'response', allow.new.levels = T)
  lmer4<- lmer(score ~ (1|fullname), data = m_HB[train.rows,])
  m_HB[test.rows,]$lmer4 <- predict(lmer4, newdata = m_HB[test.rows,], type = 'response', allow.new.levels = T)
  
}


m_HB_mean <- m_HB %>% group_by(fullname) %>% summarise(mean_lmer1 = mean(lmer1))

# error = 1.375
# This is MSE
sqrt(mean((m_HB$lmer1 - m_HB$score)^2))
#1.362
sqrt(mean((m_HB$lmer2 - m_HB$score)^2))
#1.357
sqrt(mean((m_HB$lmer3 - m_HB$score)^2))
#1.375
sqrt(mean((m_HB$lmer4 - m_HB$score)^2))


# the code below is the same but for different apparatuses and gender

# Men Pommel Horse

m_PH <- filter(d, gender=='m' & apparatus=='PH')


k=5
folds=rep(1:k, length.out=nrow(m_PH))
m_PH$fold = sample(folds, nrow(m_PH), replace=F)

m_PH$lmer1 <- NA
m_PH$lmer2 <- NA
m_PH$lmer3 <- NA
m_PH$lmer4 <- NA

for(j in 1:k){
  cat(j, "")
  train.rows <- m_PH$fold!=j
  test.rows <- m_PH$fold==j
  
  
  lmer1<- lmer(score ~ (1|fullname) + round + location, data = m_PH[train.rows,])
  m_PH[test.rows,]$lmer1 <- predict(lmer1, newdata = m_PH[test.rows,], type = 'response', allow.new.levels = T)
  lmer2<- lmer(score ~ (1|fullname) + location, data = m_PH[train.rows,])
  m_PH[test.rows,]$lmer2 <- predict(lmer2, newdata = m_PH[test.rows,], type = 'response', allow.new.levels = T)
  lmer3<- lmer(score ~ (1|fullname) + competition, data = m_PH[train.rows,])
  m_PH[test.rows,]$lmer3 <- predict(lmer3, newdata = m_PH[test.rows,], type = 'response', allow.new.levels = T)
  lmer4<- lmer(score ~ (1|fullname), data = m_PH[train.rows,])
  m_PH[test.rows,]$lmer4 <- predict(lmer4, newdata = m_PH[test.rows,], type = 'response', allow.new.levels = T)
  
}

m_PH_mean <- m_PH %>% group_by(fullname) %>% summarise(mean_lmer1 = mean(lmer1))
# error = 1.511
sqrt(mean((m_PH$lmer1 - m_PH$score)^2))
# error = 1.511
sqrt(mean((m_PH$lmer2 - m_PH$score)^2))
# error = 1.501
sqrt(mean((m_PH$lmer3 - m_PH$score)^2))
# error = 1.528
sqrt(mean((m_PH$lmer4 - m_PH$score)^2))


# Men Floor

m_FX <- filter(d, gender=='m' & apparatus=='FX')


k=5
folds=rep(1:k, length.out=nrow(m_FX))
m_FX$fold = sample(folds, nrow(m_FX), replace=F)

m_FX$lm1 <- NA
m_FX$lmer1 <- NA
m_FX$lmer2 <- NA
m_FX$lmer3 <- NA
m_FX$lmer4 <- NA
for(j in 1:k){
  cat(j, "")
  train.rows <- m_FX$fold!=j
  test.rows <- m_FX$fold==j
  
  lmer1<- lmer(score ~ (1|fullname) + round + location, data = m_FX[train.rows,])
  m_FX[test.rows,]$lmer1 <- predict(lmer1, newdata = m_FX[test.rows,], type = 'response', allow.new.levels = T)
  lmer2<- lmer(score ~ (1|fullname) + location, data = m_FX[train.rows,])
  m_FX[test.rows,]$lmer2 <- predict(lmer2, newdata = m_FX[test.rows,], type = 'response', allow.new.levels = T)
  lmer3<- lmer(score ~ (1|fullname) + competition, data = m_FX[train.rows,])
  m_FX[test.rows,]$lmer3 <- predict(lmer3, newdata = m_FX[test.rows,], type = 'response', allow.new.levels = T)
  lmer4<- lmer(score ~ (1|fullname), data = m_FX[train.rows,])
  m_FX[test.rows,]$lmer4 <- predict(lmer4, newdata = m_FX[test.rows,], type = 'response', allow.new.levels = T)
}

# error = 1.430
sqrt(mean((m_FX$lmer1 - m_FX$score)^2))
# error = 1.428
sqrt(mean((m_FX$lmer2 - m_FX$score)^2))
# error = 1.431
sqrt(mean((m_FX$lmer3 - m_FX$score)^2))
# error = 1.442
sqrt(mean((m_FX$lmer4 - m_FX$score)^2))

m_FX_mean <- m_FX %>% group_by(fullname) %>% summarise(mean_lmer1 = mean(lmer1))
# Men Parallel Bars

m_PB <- filter(d, gender=='m' & apparatus=='PB')


k=5
folds=rep(1:k, length.out=nrow(m_PB))
m_PB$fold = sample(folds, nrow(m_PB), replace=F)

m_PB$lm1 <- NA
m_PB$lmer1 <- NA
m_PB$lmer2 <- NA
m_PB$lmer3 <- NA
m_PB$lmer4 <- NA
for(j in 1:k){
  cat(j, "")
  train.rows <- m_PB$fold!=j
  test.rows <- m_PB$fold==j
  
  lmer1<- lmer(score ~ (1|fullname) + round + location, data = m_PB[train.rows,])
  m_PB[test.rows,]$lmer1 <- predict(lmer1, newdata = m_PB[test.rows,], type = 'response', allow.new.levels = T)
  lmer2<- lmer(score ~ (1|fullname) + location, data = m_PB[train.rows,])
  m_PB[test.rows,]$lmer2 <- predict(lmer2, newdata = m_PB[test.rows,], type = 'response', allow.new.levels = T)
  lmer3<- lmer(score ~ (1|fullname) + competition, data = m_PB[train.rows,])
  m_PB[test.rows,]$lmer3 <- predict(lmer3, newdata = m_PB[test.rows,], type = 'response', allow.new.levels = T)
  lmer4<- lmer(score ~ (1|fullname), data = m_PB[train.rows,])
  m_PB[test.rows,]$lmer4 <- predict(lmer4, newdata = m_PB[test.rows,], type = 'response', allow.new.levels = T)
}

# error = 1.286
sqrt(mean((m_PB$lmer1 - m_PB$score)^2))
# error = 1.288
sqrt(mean((m_PB$lmer2 - m_PB$score)^2))
# error = 1.280
sqrt(mean((m_PB$lmer3 - m_PB$score)^2))
# error = 1.289
sqrt(mean((m_PB$lmer4 - m_PB$score)^2))
m_PB_mean <- m_PB %>% group_by(fullname) %>% summarise(mean_lmer1 = mean(lmer1))
# Men Rings

m_SR <- filter(d, gender=='m' & apparatus=='SR')


k=5
folds=rep(1:k, length.out=nrow(m_SR))
m_SR$fold = sample(folds, nrow(m_SR), replace=F)

m_SR$lm1 <- NA
m_SR$lmer1 <- NA
m_SR$lmer2 <- NA
m_SR$lmer3 <- NA
m_SR$lmer4 <- NA
for(j in 1:k){
  cat(j, "")
  train.rows <- m_SR$fold!=j
  test.rows <- m_SR$fold==j
  
  lmer1<- lmer(score ~ (1|fullname) + round + location, data = m_SR[train.rows,])
  m_SR[test.rows,]$lmer1 <- predict(lmer1, newdata = m_SR[test.rows,], type = 'response', allow.new.levels = T)
  lmer2<- lmer(score ~ (1|fullname) + location, data = m_SR[train.rows,])
  m_SR[test.rows,]$lmer2 <- predict(lmer2, newdata = m_SR[test.rows,], type = 'response', allow.new.levels = T)
  lmer3<- lmer(score ~ (1|fullname) + competition, data = m_SR[train.rows,])
  m_SR[test.rows,]$lmer3 <- predict(lmer3, newdata = m_SR[test.rows,], type = 'response', allow.new.levels = T)
  lmer4<- lmer(score ~ (1|fullname), data = m_SR[train.rows,])
  m_SR[test.rows,]$lmer4 <- predict(lmer4, newdata = m_SR[test.rows,], type = 'response', allow.new.levels = T)
}

# error = 1.278
sqrt(mean((m_SR$lmer1 - m_SR$score)^2))
# error = 1.273
sqrt(mean((m_SR$lmer2 - m_SR$score)^2))
# error = 1.274
sqrt(mean((m_SR$lmer3 - m_SR$score)^2))
# error = 1.284
sqrt(mean((m_SR$lmer4 - m_SR$score)^2))
m_SR_mean <- m_SR %>% group_by(fullname) %>% summarise(mean_lmer1 = mean(lmer1))
# Men Vault

m_VT <- filter(d, gender=='m' & apparatus=='VT')


k=5
folds=rep(1:k, length.out=nrow(m_VT))
m_VT$fold = sample(folds, nrow(m_VT), replace=F)

m_VT$lm1 <- NA
m_VT$lmer1 <- NA
m_VT$lmer2 <- NA
m_VT$lmer3 <- NA
m_VT$lmer4 <- NA
for(j in 1:k){
  cat(j, "")
  train.rows <- m_VT$fold!=j
  test.rows <- m_VT$fold==j
  
  lmer1<- lmer(score ~ (1|fullname) + round + location, data = m_VT[train.rows,])
  m_VT[test.rows,]$lmer1 <- predict(lmer1, newdata = m_VT[test.rows,], type = 'response', allow.new.levels = T)
  lmer2<- lmer(score ~ (1|fullname) + location, data = m_VT[train.rows,])
  m_VT[test.rows,]$lmer2 <- predict(lmer2, newdata = m_VT[test.rows,], type = 'response', allow.new.levels = T)
  lmer3<- lmer(score ~ (1|fullname) + competition, data = m_VT[train.rows,])
  m_VT[test.rows,]$lmer3 <- predict(lmer3, newdata = m_VT[test.rows,], type = 'response', allow.new.levels = T)
  lmer4<- lmer(score ~ (1|fullname), data = m_VT[train.rows,])
  m_VT[test.rows,]$lmer4 <- predict(lmer4, newdata = m_VT[test.rows,], type = 'response', allow.new.levels = T)
}
# error = 2.255
sqrt(mean((m_VT$lmer1 - m_VT$score)^2))
# error = 2.257
sqrt(mean((m_VT$lmer2 - m_VT$score)^2))
# error = 2.257
sqrt(mean((m_VT$lmer3 - m_VT$score)^2))
# error = 2.544
sqrt(mean((m_VT$lmer4 - m_VT$score)^2))
m_VT_mean <- m_VT %>% group_by(fullname) %>% summarise(mean_lmer1 = mean(lmer1))

# Women Balance Beam

w_BB <- filter(d, gender=='w' & apparatus=='BB')


k=5
folds=rep(1:k, length.out=nrow(w_BB))
w_BB$fold = sample(folds, nrow(w_BB), replace=F)

w_BB$lm1 <- NA
w_BB$lmer1 <- NA
w_BB$lmer2 <- NA
w_BB$lmer3 <- NA
w_BB$lmer4 <- NA
for(j in 1:k){
  cat(j, "")
  train.rows <- w_BB$fold!=j
  test.rows <- w_BB$fold==j
  
  lmer1<- lmer(score ~ (1|fullname) + round + location, data = w_BB[train.rows,])
  w_BB[test.rows,]$lmer1 <- predict(lmer1, newdata = w_BB[test.rows,], type = 'response', allow.new.levels = T)
  lmer2<- lmer(score ~ (1|fullname) + location, data = w_BB[train.rows,])
  w_BB[test.rows,]$lmer2 <- predict(lmer2, newdata = w_BB[test.rows,], type = 'response', allow.new.levels = T)
  lmer3<- lmer(score ~ (1|fullname) + competition, data = w_BB[train.rows,])
  w_BB[test.rows,]$lmer3 <- predict(lmer3, newdata = w_BB[test.rows,], type = 'response', allow.new.levels = T)
  lmer4<- lmer(score ~ (1|fullname), data = w_BB[train.rows,])
  w_BB[test.rows,]$lmer4 <- predict(lmer4, newdata = w_BB[test.rows,], type = 'response', allow.new.levels = T)
}

# error = .997
sqrt(mean((w_BB$lmer1 - w_BB$score)^2))
# error = .999
sqrt(mean((w_BB$lmer2 - w_BB$score)^2))
# error = 1.002
sqrt(mean((w_BB$lmer3 - w_BB$score)^2))
# error = 1.061
sqrt(mean((w_BB$lmer4 - w_BB$score)^2))
w_BB_mean <- w_BB %>% group_by(fullname) %>% summarise(mean_lmer1 = mean(lmer1))

# Women Floor

w_FX <- filter(d, gender=='w' & apparatus=='FX')


k=5
folds=rep(1:k, length.out=nrow(w_FX))
w_FX$fold = sample(folds, nrow(w_FX), replace=F)

w_FX$lm1 <- NA
w_FX$lmer1 <- NA
w_FX$lmer2 <- NA
w_FX$lmer3 <- NA
w_FX$lmer4 <- NA
for(j in 1:k){
  cat(j, "")
  train.rows <- w_FX$fold!=j
  test.rows <- w_FX$fold==j
  
  lmer1<- lmer(score ~ (1|fullname) + round + location, data = w_FX[train.rows,])
  w_FX[test.rows,]$lmer1 <- predict(lmer1, newdata = w_FX[test.rows,], type = 'response', allow.new.levels = T)
  lmer2<- lmer(score ~ (1|fullname) + location, data = w_FX[train.rows,])
  w_FX[test.rows,]$lmer2 <- predict(lmer2, newdata = w_FX[test.rows,], type = 'response', allow.new.levels = T)
  lmer3<- lmer(score ~ (1|fullname) + competition, data = w_FX[train.rows,])
  w_FX[test.rows,]$lmer3 <- predict(lmer3, newdata = w_FX[test.rows,], type = 'response', allow.new.levels = T)
  lmer4<- lmer(score ~ (1|fullname), data = w_FX[train.rows,])
  w_FX[test.rows,]$lmer4 <- predict(lmer4, newdata = w_FX[test.rows,], type = 'response', allow.new.levels = T)
}

#error = .925
sqrt(mean((w_FX$lmer1 - w_FX$score)^2))
#error = .929
sqrt(mean((w_FX$lmer2 - w_FX$score)^2))
#error = .920
sqrt(mean((w_FX$lmer3 - w_FX$score)^2))
#error = .930
sqrt(mean((w_FX$lmer4 - w_FX$score)^2))
w_FX_mean <- w_FX %>% group_by(fullname) %>% summarise(mean_lmer1 = mean(lmer1))

# Women Unbalanced Beam

w_UB <- filter(d, gender=='w' & apparatus=='UB')


k=5
folds=rep(1:k, length.out=nrow(w_UB))
w_UB$fold = sample(folds, nrow(w_UB), replace=F)

w_UB$lm1 <- NA
w_UB$lmer1 <- NA
w_UB$lmer2 <- NA
w_UB$lmer3 <- NA
w_UB$lmer4 <- NA
for(j in 1:k){
  cat(j, "")
  train.rows <- w_UB$fold!=j
  test.rows <- w_UB$fold==j
  
  lmer1<- lmer(score ~ (1|fullname) + round + location, data = w_UB[train.rows,])
  w_UB[test.rows,]$lmer1 <- predict(lmer1, newdata = w_UB[test.rows,], type = 'response', allow.new.levels = T)
  lmer2<- lmer(score ~ (1|fullname) + location, data = w_UB[train.rows,])
  w_UB[test.rows,]$lmer2 <- predict(lmer2, newdata = w_UB[test.rows,], type = 'response', allow.new.levels = T)
  lmer3<- lmer(score ~ (1|fullname) + competition, data = w_UB[train.rows,])
  w_UB[test.rows,]$lmer3 <- predict(lmer3, newdata = w_UB[test.rows,], type = 'response', allow.new.levels = T)
  lmer4<- lmer(score ~ (1|fullname), data = w_UB[train.rows,])
  w_UB[test.rows,]$lmer4 <- predict(lmer4, newdata = w_UB[test.rows,], type = 'response', allow.new.levels = T)
}

# error = 1.208
sqrt(mean((w_UB$lmer1 - w_UB$score)^2))
# error = 1.205
sqrt(mean((w_UB$lmer2 - w_UB$score)^2))
# error = 1.196
sqrt(mean((w_UB$lmer3 - w_UB$score)^2))
# error = 1.218
sqrt(mean((w_UB$lmer4 - w_UB$score)^2))
w_UB_mean <- w_UB %>% group_by(fullname) %>% summarise(mean_lmer1 = mean(lmer1))
# Women Vault

w_VT <- filter(d, gender=='w' & apparatus=='VT')

w_VT <- w_VT %>% mutate(round = if_else(round == "AAqual", "qual", round))

k=5
folds=rep(1:k, length.out=nrow(w_VT))
w_VT$fold = sample(folds, nrow(w_VT), replace=F)

w_VT$lm1 <- NA
w_VT$lmer1 <- NA
w_VT$lmer2 <- NA
w_VT$lmer3 <- NA
w_VT$lmer4 <- NA
for(j in 1:k){
  cat(j, "")
  train.rows <- w_VT$fold!=j
  test.rows <- w_VT$fold==j
  
  lmer1<- lmer(score ~ (1|fullname) + round + location, data = w_VT[train.rows,])
  w_VT[test.rows,]$lmer1 <- predict(lmer1, newdata = w_VT[test.rows,], type = 'response', allow.new.levels = T)
  lmer2<- lmer(score ~ (1|fullname) + location, data = w_VT[train.rows,])
  w_VT[test.rows,]$lmer2 <- predict(lmer2, newdata = w_VT[test.rows,], type = 'response', allow.new.levels = T)
  lmer3<- lmer(score ~ (1|fullname) + competition, data = w_VT[train.rows,])
  w_VT[test.rows,]$lmer3 <- predict(lmer3, newdata = w_VT[test.rows,], type = 'response', allow.new.levels = T)
  lmer4<- lmer(score ~ (1|fullname), data = w_VT[train.rows,])
  w_VT[test.rows,]$lmer4 <- predict(lmer4, newdata = w_VT[test.rows,], type = 'response', allow.new.levels = T)
}

# error = 1.887
sqrt(mean((w_VT$lmer1 - w_VT$score)^2))
# error = 1.909
sqrt(mean((w_VT$lmer2 - w_VT$score)^2))
# error = 1.902
sqrt(mean((w_VT$lmer3 - w_VT$score)^2))
# error = 2.277
sqrt(mean((w_VT$lmer4 - w_VT$score)^2))
w_VT_mean <- w_VT %>% group_by(fullname) %>% summarise(mean_lmer1 = mean(lmer1))

# Getting average error of the different lmer models

lmer1_err <- c(1.375, 1.511, 1.430, 1.286, 1.278, 2.255, 0.997, 0.925, 1.208, 1.887)
lmer2_err <- c(1.362, 1.511, 1.428, 1.288, 1.273, 2.257, 0.999, 0.929, 1.205, 1.909)
lmer3_err <- c(1.357, 1.501, 1.431, 1.280, 1.274, 2.257, 1.002, 0.920, 1.196, 1.903)
lmer4_err <- c(1.375, 1.528, 1.442, 1.289, 1.284, 2.544, 1.061, 0.930, 1.218, 2.277)


library(ggplot2)

# bar chart for average error.
means <- c(mean(lmer1_err), mean(lmer2_err), mean(lmer3_err), mean(lmer4_err))

# Specify colors for each bar
bar_colors <- c("red", "green", "blue", "orange")

# Set a limit for the y-axis
y_limit <- c(0, 2)  # Adjust the limits as needed

# Create a data frame
data <- data.frame(Model = c("lmer1", "lmer2", "lmer3", "lmer4"), Mean = means)

# Create a bar chart with varying colors, limited y-axis, and labels using ggplot2
ggplot(data, aes(x = Model, y = Mean, fill = Model)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(values = bar_colors) +
  labs(title = "Mean Values for Different Models", x = "Model", y = "Mean") +
  theme_minimal() +
  coord_cartesian(ylim = y_limit) +
  geom_text(aes(label = round(Mean, 2)), vjust = -0.5, color = "black")




# graph of error by
categories <- c("m_HB", "m_PH", "m_FX", "m_PB", "m_SR", "m_VT", "w_BB", "w_FX", "w_UB", "w_VT")


# Combine the values into a data frame
values <- c(lmer1_err, lmer2_err, lmer3_err, lmer4_err)
groups <- rep(c("lmer1", "lmer2", "lmer3", "lmer4"), each = 10)

# Combine data into a data frame
data <- data.frame(Category = categories, Value = values, Group = groups)

ggplot(data, aes(x = Category, y = Value, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Bar Plot with Multiple Bars per Category", x = "Categories", y = "Values") +
  theme_minimal()

# AIC chart for models
aic_values <- c(AIC(lmer1), AIC(lmer2), AIC(lmer3), AIC(lmer4))

data <- data.frame(Model = c("Model 1", "Model 2", "Model 3", "Model 4"), AIC = aic_values)
y_limit <- c(min(aic_values) - 200, max(aic_values) + 200)
ggplot(data, aes(x = Model, y = AIC, fill = Model)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(aic_values,1), vjust = -0.5, size = 3)) +
  labs(title = "Average AIC Values for Different Models", x = "Model", y = "Average AIC") +
  theme_minimal() +
  coord_cartesian(ylim = y_limit)

means <- c(mean(lmer3_err), mean(lmer2_err), mean(lmer1_err), mean(lmer4_err))

data2 <- data.frame(Model = c("Model 1", "Model 2", "Model 3", "Model 4"), Means = means)
y_limit <- c(min(means) - .2, max(means) + .1)
ggplot(data2, aes(x = Model, y = means, fill = Model)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(means, 2)), vjust = -0.5, size = 3) +
  labs(title = "Avg Error for Different Models", x = "Model", y = "Avg MSE") +
  theme_minimal() +
  coord_cartesian(ylim = y_limit)

