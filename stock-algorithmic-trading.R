
#### Required Libraries
```{r message=F}
library(fmlr)
library(lubridate)
library(quantmod)
library(TTR) # for various indicators
library(randomForest)
library(ROCR)
library(caret)
library(MLmetrics) # for logloss

```


### Loading datasets
```{r}
mydir = c("201804", '201805','201806')
#################### load data
myfiles <-  list.files(path=mydir, pattern=".zip", full.names=TRUE)
# myfiles
```


### Data Preprocessing
```{r eval=FALSE, include=TRUE}
l_d3 <- list()
for (i in myfiles) {l_d3[i] <- read_algoseek_equity_taq(i, whichData = 'NVDA.csv')}

# function to transfer loaded data into the fmlr-friendly format
redef <- function(dat){
  dat <- subset(dat, EventType %in% c("TRADE", "TRADE NB"))
  dat <- subset(dat, lubridate::hour(dat$Timestamp)*60+lubridate::minute(dat$Timestamp) >= 9*60+30)
  dat <- subset(dat, lubridate::hour(dat$Timestamp)*60+lubridate::minute(dat$Timestamp) <= 16*60)
  name <- names(dat)
  name[name=="Timestamp"] <- "tStamp"
  name[name=="Quantity"] <- "Size"
  names(dat) <- name
  dat$tStamp <- as.POSIXct( paste(dat$Date, dat$tStamp), format="%Y-%m-%d %H:%M:%OS", tz="EST")
  return(dat)
}

# transfer the data
lr_d3 <- list()
for (i in 1:length(l_d3)) {lr_d3[[i]] <- redef(l_d3[[i]])}

```

#### Setting Bars
```{r eval=FALSE, include=TRUE}
tick_bar <- list()
for (i in 1:length(lr_d3)) {
  tick_bar[[i]] <- bar_tick(lr_d3[[i]], nTic=1000)
}

tick_df <- data.frame()
for (i in 1:length(tick_bar)) {
  tick_bar[[i]] <- as.data.frame(tick_bar[[i]])
  tick_df <- rbind(tick_df, tick_bar[[i]])
}
# write tick data in file
write.csv(tick_df, file = 'tick_df.csv')


unit_bar <- list()
for (i in 1:length(lr_d3)) {
  unit_bar[[i]] <- bar_unit(lr_d3[[i]], unit = 1000000)
}

unit_df <- data.frame()
for (i in 1:length(unit_bar)) {
  unit_bar[[i]] <- as.data.frame(unit_bar[[i]])
  unit_df <- rbind(unit_df, unit_bar[[i]])
}
write.csv(unit_df, file = 'unit_df.csv')
```

```{r}
tick_df <- read.csv('tick_df.csv')
plot(1, type = 'n', 
     xlim=c(0, length(tick_df$H)),
     ylim = c(min(tick_df$L), max(tick_df$H)),
     xlab = 'The ith Bar', ylab = 'Price', main = 'NVDA')
lines(tick_df$O, col = 'orange', lty=2, lwd=2)
lines(tick_df$H, col='blue',  lty=3, lwd=2)
lines(tick_df$L, col='red', lty=4, lwd=2)
lines(tick_df$C, col='brown', lty=5, lwd=2)
legend('bottomright', legend = c('O', 'H', 'L', 'C'), 
       col= c('orange', 'blue', 'red', 'brown'), 
       lty=c(2, 3, 4, 5), lwd=c(2, 2, 2, 2), merge = T)

```

#### Adding indicators
```{r}
############################ By Tick Bars
########### Features Setting
DAT_T <- read.csv('tick_df.csv')
dat <- DAT_T[,c('H', 'L', 'O', 'C', 'V')]
dat$V <- as.numeric(dat$V/1e6)
dat$C <- as.numeric(dat$C)
dat$H <- as.numeric(dat$H)
dat$L <- as.numeric(dat$L)
dat$O <- as.numeric(dat$O)
names(dat) <- c("High", "Low", "Open", "Close", "Volume")

# functions used to prepare the following indicators from TTR
HL <- function(dat){cbind(dat$High, dat$Low)}
HLC <- function(dat){cbind(dat$High, dat$Low, dat$Close)}

# add various indicators
dat_used <- cbind(dat, 
                  ADX=ADX(dat)[,4],
                  aroon=aroon(HL(dat))[,3],
                  ATR=ATR(dat)[,2],
                  BBands(HLC(dat)),
                  CCI=CCI(HLC(dat)),
                  chaikinAD=chaikinAD(HLC(dat), dat$Volume),
                  chaikinVolatility=chaikinVolatility(dat),
                  CLV=CLV(dat),
                  CMF=CMF(HLC(dat), dat$Volume),
                  CMOClose=CMO(dat$Close),
                  CMOVol=CMO(dat$Volume),
                  DonchianChannel(HL(dat)),
                  DPOClose=DPO(dat$Close),
                  DPOVol=DPO(dat$Volume),
                  DVI(dat$Close),
                  EMV=EMV(HLC(dat), dat$Volume)[,1],
                  GMMA(dat$Close),
                  GMMA(dat$Volume),
                  KST=KST(dat$Close)[,1],
                  MACDClose=MACD(dat$Close)[,1],
                  MACDVol=MACD(dat$Volume)[,1],
                  MFI=MFI(HLC(dat), dat$Volume),
                  OBV=OBV(dat$Close, dat$Volume),
                  PBands(dat$Close),
                  ROCClose=ROC(dat$Close),
                  ROCVol=ROC(dat$Volume),
                  momentum=momentum(dat$Close),
                  RSI=RSI(dat$Close),
                  runPerRankClose=runPercentRank(dat$Close),
                  runPerRankVolume=runPercentRank(dat$Volume),
                  SAR=SAR(HL(dat)),
                  VWAP=VWAP(dat$Close, volume=dat$Volume),
                  SNR=SNR(HLC(dat), n=30),
                  stoch(HLC(dat)),
                  SMI=SMI(HLC(dat))[,1],
                  TDI=TDI(dat$Close)[,1],
                  TRIX=TRIX(dat$Close)[,1],
                  ultimateOsc=ultimateOscillator(HLC(dat)),
                  VHF=VHF(dat$Close),
                  vola=volatility(dat),
                  williamsAD=williamsAD(HLC(dat)),
                  WPR=WPR(HLC(dat))
)
dim(dat_used)
```

#### CUSUM to Access Features and Labels

```{r}
## plot for visualization, just part of the data included
hvec <- na.locf(c(NA,0.5*runSD(tick_df[1:100,'C'])), fromLast = T)
i_CUSUM <- fmlr::istar_CUSUM(tick_df[1:100,'C'], h=hvec)
n_Event <- length(i_CUSUM) 

plot(tick_df[1:100,'C'], main="Sample features by the CUSUM filter")
abline(v=i_CUSUM+1, lty = 2)
```

```{r}
################ CUSUMs, prepare features and labels
hvec <- na.locf(c(NA,0.5*runSD(dat_used$Close)), fromLast = T)
i_CUSUM <- fmlr::istar_CUSUM(dat_used$Close, h=hvec)
n_Event <- length(i_CUSUM)

events <- data.frame(t0=i_CUSUM+1, 
                     t1 = i_CUSUM+200, 
                     trgt = rep(0.001, n_Event),
                     side=rep(1,n_Event))
ptSl <- c(1,1)

out0 <- fmlr::label_meta(dat_used$Close, events, ptSl)
table(out0$label) # imbalanced data, need smote
```


#### Combine Labels, Features and Indicators
```{r}
########## Combine labels, features and indicators
fMat0 <- dat_used[out0$t1Fea,]
allSet <- data.frame(Y=as.factor(out0$label),fMat0, t1Fea=out0$t1Fea, tLabel=out0$tLabel)

# exclude NA at the begining of the indicators
idx_NA <- apply(allSet,1,function(x){sum(is.na(x))>0})
# train-test-split
allSet <- subset(allSet, !idx_NA)
nx <- nrow(allSet)
trainSet <- allSet[1:floor(nx*2/3),]
testSet <- allSet[(floor(nx*2/3)+1):nx,]
dim(allSet)
dim(trainSet) 
dim(testSet)
```

#### SMOTE
```{r}
################## SMOTE
tb <- table(trainSet$Y)
ratio <- tb[names(tb)=='1']/tb[names(tb)=='0']
ratio
if(ratio > 1) perc <- list("0"=ratio, "1"=1) else perc <- list("0"=1, "1"= (1/ratio))

trainSet_balanced <- UBL::SmoteClassif(Y ~ . - Close - t1Fea - tLabel, dat = trainSet, C.perc = perc)
table(trainSet_balanced$Y)

```

### Model Fitting and Feature Importance Analysis
#### Feature Importance

```{r}
logistic <- glm(Y~., family = binomial(link='logit'), data=trainSet)
prob_test <- predict(logistic, newdata = testSet, type='response')
test.res <- ifelse(prob_test>=0.5, 1, 0)
table(testSet$Y, test.res)
pred <- prediction(prob_test, testSet$Y)
tb_test <- table(testSet$Y)
acc_perf <- performance(pred, measure = "acc")
acc_vec <- acc_perf@y.values[[1]]
acc <- acc_vec[max(which(acc_perf@x.values[[1]] >= 0.5))]
acc
lucky_score <- fmlr::acc_lucky(train_class = table(trainSet$Y),
                               test_class = tb_test,
                               my_acc = acc)
lucky_score

summary(logistic)
varImp(logistic)

```

```{r}
# try random forest 
# feature importance
mtry <- tuneRF(trainSet_balanced[,-1], trainSet_balanced$Y, plot=F)
mtry <- mtry[which.min(mtry[,2]),1] 
mtry
bag <- randomForest(Y ~ . - Close - t1Fea - tLabel, data = trainSet_balanced, mtry = mtry, importance = TRUE, ntree = 400, SB=0)
plot(bag)
legend("topright", colnames(bag$err.rate),col=1:3,cex=0.8,lty=1:3)
varImpPlot(bag)
# evaluating auc based on the test set
prob_test <- predict(bag, newdata=testSet, type="prob")
pred <- prediction(prob_test[,2], testSet$Y) # the 2nd column is where the label "1" is
acc_perf <- performance(pred, measure = "acc")
acc_vec <- acc_perf@y.values[[1]]
acc <- acc_vec[max(which(acc_perf@x.values[[1]] >= 0.5))]
acc
lucky_score <- fmlr::acc_lucky(train_class = table(trainSet$Y),
                               test_class = table(testSet$Y),
                               my_acc = acc)
lucky_score
```

#### PCA importance
```{r}
# PCA importance
table(testSet$Y, prob_test[,2] >= 0.5)
trainFea <- trainSet_balanced[, !(names(trainSet)%in%c('Y', 'Close', 't1Fea', 'tLabel'))]
pca <- prcomp(trainFea, center = TRUE, scale. = TRUE)
summary(pca)
plot(pca)
trainPCA <- data.frame(Y=trainSet_balanced$Y, pca$x)
mtry_p <- tuneRF(trainPCA[,-1], trainPCA$Y, plot = F)
mtry_p <- mtry_p[which.min(mtry_p[,2]),1] #mtry=18
mtry_p
bag_pca <- randomForest(Y ~ ., data = trainPCA, mtry = mtry_p, importance = TRUE, ntree = 400, SB=0)
plot(bag_pca)
legend("topright", colnames(bag$err.rate),col=1:3,cex=0.8,lty=1:3)
varImpPlot(bag_pca)
testFea <- testSet[, !(names(testSet)%in%c('Y', 'Close', 't1Fea', 'tLabel'))]
testPCA <- data.frame(Y=testSet$Y, (scale(testFea, center= pca$center, scale = pca$scale) %*% pca$rotation))

prob_test <- predict(bag_pca, newdata=testPCA, type="prob")
table(testPCA$Y, prob_test[,2] >= 0.5)
pred <- prediction(prob_test[,2], testPCA$Y)
tb_test <- table(testPCA$Y)
acc_perf <- performance(pred, measure = "acc")
acc_vec <- acc_perf@y.values[[1]]
acc <- acc_vec[max(which(acc_perf@x.values[[1]] >= 0.5))]
lucky_score <- fmlr::acc_lucky(train_class = table(trainPCA$Y),
                               test_class = tb_test,
                               my_acc = acc)
lucky_score
```
```{r =F}
logistic <- glm(Y~., family = binomial(link='logit'), data=trainPCA)
prob_test <- predict(logistic, newdata = testPCA, type='response')
test.res <- ifelse(prob_test>=0.5, 1, 0)
table(testPCA$Y, test.res)
pred <- prediction(prob_test, testPCA$Y)
tb_test <- table(testSet$Y)
acc_perf <- performance(pred, measure = "acc")
acc_vec <- acc_perf@y.values[[1]]
acc <- acc_vec[max(which(acc_perf@x.values[[1]] >= 0.5))]
acc
lucky_score <- fmlr::acc_lucky(train_class = table(trainSet$Y),
                               test_class = tb_test,
                               my_acc = acc)
lucky_score
summary(logistic)
varImp(logistic)

```

### Parameter Tuning
```{r paged.print=TRUE}
################################################### Tuning
# vectors for two parameters to be tuned
hvec <- seq(0.5, 2,length=5)
trgtvec <- seq(0.001, 0.01, length=4)
k <- 5 # k-fold CV
gam <- 0.01 # embargo parameter
run <- FALSE # whether run the grid search?
run <- TRUE
###################################################
if(run==TRUE)
{
  rst <- NULL
  for(ih in 1:length(hvec))
  {
    for(jtrgt in 1:length(trgtvec))
    {
      ###################################################
      i_CUSUM <- fmlr::istar_CUSUM(dat_used$Close, h=hvec[ih])  # <---------------- tuning parameter 1
      n_Event <- length(i_CUSUM)
      
      events <- data.frame(t0=i_CUSUM+1, 
                           t1 = i_CUSUM+200, 
                           trgt = rep(trgtvec[jtrgt], n_Event), # <---------------- tuning parameter 2
                           side=rep(1,n_Event))
      ptSl <- c(1,1)
      
      out0 <- fmlr::label_meta(dat_used$Close, events, ptSl)
      table(out0$label)
      
      # feature matrix
      fMat0 <- dat_used[out0$t1Fea,] 
      
      # t1Fea and tLabel have to be included in order to use purged k-CV
      allSet <- data.frame(Y=as.factor(out0$label),fMat0, t1Fea=out0$t1Fea, tLabel=out0$tLabel)
      
      # exclude NA at the begining of the indicators
      idx_NA <- apply(allSet,1,function(x){sum(is.na(x))>0})
      allSet <- subset(allSet, !idx_NA)
      nx <- nrow(allSet)
      
      #####################################
      # prepare data for purged k-fold CV #
      #####################################
      CVobj <- fmlr::purged_k_CV(allSet, k=k, gam=gam)
      
      ##################
      ## randomforest ##
      ##################
      set.seed(1)
      for(i in 1:k)
      {
        trainSet <- CVobj[[i]]$trainSet
        trainSet <- trainSet[,!names(trainSet)%in%c("Close", "t1Fea", "tLabel")]
        
        testSet <- CVobj[[i]]$testSet
        testSet <- testSet[,!names(testSet)%in%c("Close", "t1Fea", "tLabel")]
        
        # smote
        (tb <- table(trainSet$Y))
        (ratio <- tb[names(tb)=="1"] / tb[names(tb)=="0"])
        if(ratio > 1) perc <- list("0"=ratio, "1"=1) else perc <- list("0"=1, "1"=(1/ratio) )
        
        trainSet_balanced <- UBL::SmoteClassif(Y ~ ., dat = trainSet, C.perc = perc)
        table(trainSet_balanced$Y)
        
        # automatically choose mtry
        # mtry <- tuneRF(trainSet_balanced[,-1], trainSet_balanced$Y, plot = F)
        # mtry <- mtry[which.min(mtry[,2]),1]
        
        fit <- randomForest(Y ~ ., data = trainSet_balanced, importance = FALSE, ntrees = 500) # use default mtry
        
        pre <- predict(fit, newdata = testSet) #predicted labels
        acc <- mean(testSet$Y==pre)
        
        # can also use R caret package to calculate F1 score
        # predictions <- predict(fit, newdata=testSet)
        precision <- posPredValue(pre, testSet$Y, positive="1")
        recall <- sensitivity(pre, testSet$Y, positive="1")
        F1 <- (2 * precision * recall) / (precision + recall)
        
        roc_prob <- predict(fit, newdata=testSet, type="prob")
        pred <- prediction(roc_prob[,2], testSet$Y) 
        # the 2nd column is where the label "1" is
        # the default order of factors 0 and 1 is 0 < 1
        # so "1" is treated as positive, and a ligher prob.
        # means being closer to "1"
        
        auc <- tryCatch(performance(pred, measure = "auc")@y.values[[1]], 
                        error=function(e) NA, warning=function(w) NA)
        
        # logloss / cross entropy loss
        logloss <- MLmetrics::LogLoss(roc_prob[,2], as.numeric(testSet$Y==1))
        
        rst <- rbind(rst, c(ih, jtrgt, i, hvec[ih], trgtvec[jtrgt], 
                            acc, auc, F1, logloss, table(trainSet$Y), table(testSet$Y), table(trainSet_balanced$Y)))
        cat(ih, jtrgt, i, hvec[ih], trgtvec[jtrgt], acc, auc, F1, logloss,
            table(trainSet$Y), table(testSet$Y), table(trainSet_balanced$Y), "\n")
      }
    } # end of jtrgt loop
  } # end of ih loop
  
  rst <- data.frame(rst)
  names(rst) <- c("ih", "jtrgt", "iCV", "hCUSUM", "trgt", "acc", "auc", "F1", "logloss",
                  "train0", "train1", "test0", "test1", "train_bal0", "train_bal1")
  write.csv(rst, "tuning_purgedCV_logloss-2021.csv", row.names = F)
  
}
```

### Summarizing Performance From Tuning
```{r}
perfCV <- read.csv("tuning_purgedCV_logloss-2021.csv", header = T)
perfCV

perfCV <- subset(perfCV, (!is.na(acc))&(!is.na(auc))&(!is.na(F1))&(!is.na(logloss)))
dim(perfCV)

cnt <- aggregate(perfCV$acc, by=list(perfCV$hCUSUM, perfCV$trgt), FUN=length)
acc <- aggregate(perfCV$acc, by=list(perfCV$hCUSUM, perfCV$trgt), FUN=mean)
auc <- aggregate(perfCV$auc, by=list(perfCV$hCUSUM, perfCV$trgt), FUN=mean)
f1 <- aggregate(perfCV$F1, by=list(perfCV$hCUSUM, perfCV$trgt), FUN=mean)
logloss <- aggregate(perfCV$logloss, by=list(perfCV$hCUSUM, perfCV$trgt), FUN=mean)
train1 <- aggregate(perfCV$train1, by=list(perfCV$hCUSUM, perfCV$trgt), FUN=mean)
train0 <- aggregate(perfCV$train0, by=list(perfCV$hCUSUM, perfCV$trgt), FUN=mean)
test1 <- aggregate(perfCV$test1, by=list(perfCV$hCUSUM, perfCV$trgt), FUN=mean)
test0 <- aggregate(perfCV$test0, by=list(perfCV$hCUSUM, perfCV$trgt), FUN=mean)

# disable warning
options(warn=-1)

# combine results by merging multiple data.frame together
mer <- Reduce(function(...) merge(..., by=c("Group.1","Group.2")), 
              list(cnt, acc, auc, f1,logloss, train1,train0,test1,test0))
names(mer) <- c("hCUSUM", "trgt", "kCV", "acc", "auc", "f1", "logloss",
                "train1","train0","test1", "test0")

tail(mer)
dim(mer)

# rstF1 <- mer[order(mer$f1, decreasing=T),]
# rstF1
rstlogloss <- mer[order(mer$logloss, decreasing=F),]
head(rstlogloss)

```






