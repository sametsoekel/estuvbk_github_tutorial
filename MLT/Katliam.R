tahminler <- data.frame(dummy=rep('a',40000))
k=0

for(i in 2:50){
  set.seed(1234+k)
  
  
  
  
  birler <- df[df$target==1,]
  sifirlar <- df[df$target==0,]
  sifirindex <- sample(1:nrow(sifirlar),2440)
  sifirlar <- sifirlar[sifirindex,]
  
  
  xgtrain <- rbind(birler,sifirlar)
  
  set.seed(1234+k)
  k=k+1
  
  indeks <- sample(1:nrow(xgtrain),0.8*nrow(xgtrain))
  training <- xgtrain[indeks,]
  testing <- xgtrain[-indeks,]
  
  train_x <- training %>% dplyr::select(-target)
  train_y <- training$target
  
  test_x <- testing %>% dplyr::select(-target)
  test_y <- testing$target
  
  dtrain <- lgb.Dataset(data = as.matrix(train_x), label = train_y)
  dtest <- lgb.Dataset(data = as.matrix(test_x), label = test_y)
  watchlist <- list(train=dtrain, test=dtest)
  
  
  
  lgb_skor <-0
  deneme1 <- 0
  
  xgb_skor <-0
  deneme2 <- 0
  
  #subsample
  while(lgb_skor<0.79 | deneme1 <150){
    print(paste('SUAN ',i,'. ITERASYONDAYIM...'))
    params <- list(objective='binary',metric='auc',max.depth=sample(seq(4,15,1),1),
                   eta=sample(seq(0.001,0.2,0.001),1),
                   min_child_weight =sample(seq(35,50,1),1),boosting='gbdt')
    valids <- list(test=dtest)
    
    bst <- lgb.train(params=params,data=dtrain,valids=valids, nrounds=9500, 
                     early_stopping_rounds =200)
    deneme1 <- deneme1 + 1
    lgb_skor<-bst$best_score
    
  }
  
  
  pred <- predict(bst,as.matrix(hedef))
  tahminler[,i]<-pred
  names(tahminler)[i]<-as.character(bst$best_score)
  
  dtrain <- xgb.DMatrix(data = as.matrix(train_x), label = train_y)
  dtest <- xgb.DMatrix(data = as.matrix(test_x), label = test_y)
  
  
  while(xgb_skor<0.79 | deneme2 <150){
    print(paste('SUAN ',i,'. ITERASYONDAYIM...'))
    params <- list(objective='binary',metric='auc',max.depth=sample(seq(4,15,1),1),
                   eta=sample(seq(0.001,0.2,0.001),1),
                   min_child_weight =sample(seq(35,50,1),1),boosting='gbdt')
    watchlist <- list(train=dtrain, test=dtest)
    
    xgbst <- xgb.train(data=dtrain, max.depth=sample(seq(4,15,1),1), eta=sample(seq(0.001,0.2,0.001),1),
                      nrounds=9500, 
                     early_stopping_rounds =200,min_child_weight =sample(seq(35,50,1),1),
                     watchlist=watchlist, eval_metric = "auc",objective = "binary:logistic",
                     booster = "gbtree")
    deneme2 <- deneme2 + 1
    xgb_skor<-xgbst$best_score
    
  }
  
  
  pred <- predict(xgbst,as.matrix(hedef))
  tahminler[,i+1]<-pred
  names(tahminler)[i+1]<-as.character(xgbst$best_score)
  
  print(paste('SUAN ',i,'. ITERASYONDAYIM...'))
}

