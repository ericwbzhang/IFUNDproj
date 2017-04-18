### This script screens the data set and gives an overview of what the data looks like. 


#
rm(list=ls())
require(xts)
require(TTR)
require(roll)
# set wd
homedir<- "/Users/Eric/Documents/IFUND_comp"
datadir<- "/Users/Eric/Documents/IFUND_comp/data"
setwd(homedir)

load("U_train.RData")
load("U_test.RData")


##
betaExposure<- function(portf, UniverseBeta){ # portf: list; UniverseBeta: df
  tmp<- as.data.frame(unlist(portf, use.names = T))
  colnames(tmp)<- 'weight'
  tmp<- merge( tmp, UniverseBeta, by= 0, all.x=T)
  return(sum(tmp$weight* tmp$Beta, na.rm=T))
  
  
}


portfConst<- function( UniverseNames, weights){
  res<- list()
  for ( c in UniverseNames){
    if( c %in% names(weights)) {
      res[[c]]<- as.numeric(weights[c])
    }else  res[[c]]<-0
  }
  
  return(res)
  
}

portfRet<- function( Universe, portf, betahedge= F, INDX, UniverseBeta){ # Universe: list; portf: list
  tmp<- list()
  for ( x in names(Universe)){
    tmp[[x]]<- Universe[[x]]$RET.CC.1 * portf[[x]]
  }
  tmp<- as.data.frame(tmp)
  tmp<- apply(tmp, 1, sum)
  tmp<- xts(tmp, order.by = as.Date(index(Universe[[1]])))
  if( betahedge){
    beta<- betaExposure(portf, UniverseBeta)
    tmp<- tmp- beta* INDX$RET.CC.1
    
  }
  
  return(tmp)
  
}

ret2value<- function(ret){ # ret: xts
  value<- rep(0, length(ret))
  tmp<-1
  for (i in 1: length(value)){
    if(! is.na(ret[i])){
      value[i]<- tmp* (1+ ret[i])
    }else{
      value[i]<- tmp
    }
    tmp<- value[i]
    
  }
  return(xts(value, order.by = as.Date(index(ret))))
}


## work over liquid Universal (in sample daily AMOUT median > 5e5)

tmp<- sapply(U_train, function(x) median(x$AMOUNT))> 5e5
U_train_liq<- U_train[tmp]
liq_name<- names(U_train_liq)
U_test_liq<- U_test[liq_name]



U_train_liq.Vol.1<- data.frame(unlist(lapply(U_train_liq, function(x) sd(x$RET.CC.1, na.rm = T)), use.names = T))
colnames(U_train_liq.Vol.1)<- "Vol"
U_train_liq.iVol.1<- as.data.frame(unlist(lapply(U_train_liq, function(x) sd(x$RET.CO.1, na.rm = T)), use.names = T))
colnames(U_train_liq.iVol.1)<- "iVol"
U_train_liq.oVol.1<- as.data.frame(unlist(lapply(U_train_liq, function(x) sd(x$RET.OC.1, na.rm = T)), use.names = T))
colnames(U_train_liq.oVol.1)<- "oVol"
U_train_liq.ioCor.1<-as.data.frame(unlist( lapply(U_train_liq, function(x) as.numeric(cor(x$RET.CO.1, x$RET.OC.1, method = "spearman", use= "complete.obs"))), use.names = T))
colnames(U_train_liq.ioCor.1)<- "ioCor"

UTrainLiqVol<- cbind(U_train_liq.iVol.1,
                     U_train_liq.oVol.1,
                     U_train_liq.Vol.1,
                     U_train_liq.ioCor.1)
UTrainLiqVol$Names<- as.character(rownames(UTrainLiqVol))
UTrainLiqVolPtle<-as.data.frame( apply( UTrainLiqVol, 2, FUN = function(x) rank(as.numeric(x))/ (dim(UTrainLiqVol)[1])))
UTrainLiqVolPtle$Names<- UTrainLiqVol$Names
rownames(UTrainLiqVolPtle)<- rownames(UTrainLiqVol)

# overnight and intrday vol contribution




flag1<- UTrainLiqVolPtle$Vol< 0.25
flag2<- UTrainLiqVolPtle$Vol> 0.75 #& UTrainLiqVolPtle$Vol< 0.8


flag1Names<- as.character(UTrainLiqVolPtle[flag1, ]$Names)
flag2Names<- as.character(UTrainLiqVolPtle[flag2, ]$Names)
a<- rep(1/ length(flag1Names), length(flag1Names))
names(a)<- flag1Names
b<- rep(1/ length(flag2Names), length(flag2Names))
names(b)<- flag2Names
w<- a



# # low vol and high vol
# U_train_liq_IO$lowVolFlag<- U_train_liq_IO$U_train_liq.Vol.1.orderPct< 0.25
# U_train_liq_IO$highVolFlag<- U_train_liq_IO$U_train_liq.Vol.1.orderPct>0.75
# 
# lvNames<- U_train_liq_IO[U_train_liq_IO$lowVolFlag, ]$Names
# hvNames<- U_train_liq_IO[U_train_liq_IO$highVolFlag,]$Names
# lvWeights<- rep(1/ length(lvNames), length(lvNames))
# names(lvWeights)<- as.character(lvNames)
# hvWeights<- -rep(1/ length(hvNames), length(hvNames))
# names(hvWeights)<- as.character(hvNames)
# 
# 
# 
# a<- U_train_liq_IO$U_train_liq.oVol.1.orderPct< 0.25 & U_train_liq_IO$U_train_liq.Vol.1.orderPct<0.5
# b<- U_train_liq_IO$U_train_liq.oVol.1.orderPct>0.75 &  U_train_liq_IO$U_train_liq.Vol.1.orderPct>0.5
# 
# longNames<- as.character(U_train_liq_IO[a, ]$Names)
# shortNames<- as.character(U_train_liq_IO[b,]$Names)
# longs<- rep(1/ length(longNames), length(longNames))
# shorts<- rep(-1/length(shortNames), length(shortNames))
# names(longs)<- longNames
# names(shorts)<- shortNames
# 
# 

## test 
## 
load( "INDX_EQW.RData")
INDX_EQW$AMOUNT_ema15<- EMA(INDX_EQW$AMOUNT, n = 15)
INDX_EQW$AMOUNT_ema60<- EMA(INDX_EQW$AMOUNT, n= 60)
INDX_EQW$AMOUNT_ema15g60<- ifelse( INDX_EQW$AMOUNT_ema15> INDX_EQW$AMOUNT_ema60, 1, -1)



INDX_EQW$VALUE<- ret2value(INDX_EQW$RET.CC.1)
INDX_EQW$VOLUME<- INDX_EQW$AMOUNT/ INDX_EQW$VALUE
INDX_EQW$sd50<- runSD(x = INDX_EQW$RET.CC.1, n = 50)
INDX_EQW$signedlogVolume<- log(INDX_EQW$VOLUME)* sign(INDX_EQW$RET.CC.1)
INDX_EQW$signedsd50<- INDX_EQW$sd50* sign(INDX_EQW$RET.CC.1)
INDX_EQW$signedsd50logVolume<- INDX_EQW$sd50* INDX_EQW$signedlogVolume
INDX_EQW$lag5signedlogVolume<- lag(INDX_EQW$signedlogVolume, k = 5)
INDX_EQW$lag10signedlogVolume<- lag(INDX_EQW$signedlogVolume, k=10)
INDX_EQW$ma30signedlogVolume<- runMean(INDX_EQW$signedlogVolume, n = 30)
INDX_EQW$lag5ma30signedlogVolume<- lag(INDX_EQW$ma30signedlogVolume, k = 5)
INDX_EQW$lag5signedsd50<- lag(INDX_EQW$signedsd50, k = 5)
split_flag= as.Date('2016-01-01')
INDX_EQW_train<- INDX_EQW[ index(INDX_EQW)<  split_flag, ]
INDX_EQW_test<- INDX_EQW[index(INDX_EQW)>  split_flag, ]

UTrainLiqBeta<- as.data.frame(unlist( lapply(U_train_liq, FUN= function(x) as.numeric(lm(x$RET.CC.1~ INDX_EQW_train$RET.CC.1)$coef[2] )), use.names = T))
colnames(UTrainLiqBeta)<- "Beta"

portf_a<- portfConst( UniverseNames = names(U_train_liq), a)
portfBeta_a<- betaExposure(portf_a, UTrainLiqBeta)

portfret_a<- portfRet(Universe = U_test_liq, portf = portf_a, betahedge = T, INDX = INDX_EQW_test, UniverseBeta = UTrainLiqBeta )
portfValue_a<- ret2value(portfret_a)
summary(portfret_a)
plot(portfValue_a)


portf_b<- portfConst( UniverseNames = names(U_train_liq), b)
portfBeta_b<- betaExposure(portf_b, UTrainLiqBeta)

portfret_b<- portfRet(Universe = U_test_liq, portf = portf_b, betahedge = T, INDX = INDX_EQW_test, UniverseBeta = UTrainLiqBeta )
portfValue_b<- ret2value(portfret_b)
summary(portfret_b)
plot(portfValue_b)



tmp.1<- cbind( INDX_EQW_test$AMOUNT_ema15g60, portfret_a, portfret_b)
colnames(tmp.1)<- c("flag", "portfret_a", 'portfret_b')
tmp.1$portfret_c<- as.numeric(apply( tmp.1, 1, function(x) ifelse(is.na(x[1]), NA, ifelse(x[1]>0 , x[3], x[2] ))))

portfret_c<- tmp.1$portfret_c
summary(tmp.1)

plot(ret2value(portfret_c))
plot(ret2value(INDX_EQW_test$RET.CC.1))

portf_d<- portfConst(UniverseNames = names(U_train_liq), c(a, -b))
portfret_d<- portfRet(Universe =  U_train_liq, portf = portf_d, betahedge = T, INDX = INDX_EQW_train, UniverseBeta = UTrainLiqBeta)







# plot(x = as.zoo( xts(ret2value(INDX_EQW_train$RET.CC.1), portfValue, order.by = index(portfValue))))
# portfret_adj<- portfret* INDX_EQW_test$AMOUNT_ema15g60
# summary(portfret_adj)
# plot(ret2value(portfret_adj))

# 
# save(U_train_liq, file = 'U_train_liq.RData')
# save(U_test_liq, file = 'U_test_liq.RData')

