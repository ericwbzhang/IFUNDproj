datadir<- "/Users/Eric/Desktop/IFUND_comp/data"
setwd(homedir)
load("U_train.RData")
load("U_test.RData")
## work over liquid Universal (in sample daily AMOUT median > 5e5)
liq_threshold<- 5e5
tmp<- sapply(U_train, function(x) median(x$AMOUNT)> liq_threshold)
U_train_liq<- U_train[tmp]
UniverseLiqNames<- names(U_train_liq)
tmp<- sapply(U_train, function(x) median(x$AMOUNT))> 5e5
U_train_liq<- U_train[tmp]
liq_name<- names(U_train_liq)
U_test_liq<- U_test[liq_name]
U_train_liq.Vol.1<- lapply(U_train_liq, function(x) sd(x$RET.CC.1, na.rm = T))
U_train_liq.iVol.1<- lapply(U_train_liq, function(x) sd(x$RET.CO.1, na.rm = T))
U_train_liq.oVol.1<- lapply(U_train_liq, function(x) sd(x$RET.OC.1, na.rm = T))
U_train_liq.ioCor.1<- lapply(U_train_liq, function(x) as.numeric(cor(x$RET.CO.1, x$RET.OC.1, method = "spearman", use= "complete.obs")))
U_train_liq.Vol.1.orderPct<- order(unlist(U_train_liq.Vol.1, use.names = T), decreasing = F)/ length(U_train_liq.Vol.1)
U_train_liq.iVol.1.orderPct<- order(unlist(U_train_liq.iVol.1, use.names = T), decreasing = F)/ length(U_train_liq.iVol.1)
U_train_liq.oVol.1.orderPct<- order(unlist(U_train_liq.iVol.1, use.names = T), decreasing = F)/ length(U_train_liq.oVol.1)
U_train_liq_IO<- data.frame(
Names= names(U_train_liq),
U_train_liq.ioCor.1= unlist(U_train_liq.ioCor.1),
U_train_liq.iVol.1.orderPct= U_train_liq.iVol.1.orderPct,
U_train_liq.oVol.1.orderPct= U_train_liq.oVol.1.orderPct,
U_train_liq.Vol.1.orderPct=  U_train_liq.Vol.1.orderPct
)
View(U_train_liq_IO)
### This script screens the data set and gives an overview of what the data looks like.
#
rm(list=ls())
require(xts)
# set wd
homedir<- "/Users/Eric/Desktop/IFUND_comp"
datadir<- "/Users/Eric/Desktop/IFUND_comp/data"
setwd(homedir)
load("U_train.RData")
load("U_test.RData")
## work over liquid Universal (in sample daily AMOUT median > 5e5)
liq_threshold<- 5e5
tmp<- sapply(U_train, function(x) median(x$AMOUNT)> liq_threshold)
U_train_liq<- U_train[tmp]
UniverseLiqNames<- names(U_train_liq)
tmp<- sapply(U_train, function(x) median(x$AMOUNT))> 5e5
U_train_liq<- U_train[tmp]
liq_name<- names(U_train_liq)
U_test_liq<- U_test[liq_name]
U_train_liq.Vol.1<- lapply(U_train_liq, function(x) sd(x$RET.CC.1, na.rm = T))
U_train_liq.iVol.1<- lapply(U_train_liq, function(x) sd(x$RET.CO.1, na.rm = T))
U_train_liq.oVol.1<- lapply(U_train_liq, function(x) sd(x$RET.OC.1, na.rm = T))
U_train_liq.ioCor.1<- lapply(U_train_liq, function(x) as.numeric(cor(x$RET.CO.1, x$RET.OC.1, method = "spearman", use= "complete.obs")))
U_train_liq.ioCor.1.orderPct<- order(unlist(U_train_liq.ioCor.1, use.names = T), decreasing = F)/ length(U_train_liq.ioCor.1)
U_train_liq.Vol.1.orderPct<- order(unlist(U_train_liq.Vol.1, use.names = T), decreasing = F)/ length(U_train_liq.Vol.1)
U_train_liq.iVol.1.orderPct<- order(unlist(U_train_liq.iVol.1, use.names = T), decreasing = F)/ length(U_train_liq.iVol.1)
U_train_liq.oVol.1.orderPct<- order(unlist(U_train_liq.iVol.1, use.names = T), decreasing = F)/ length(U_train_liq.oVol.1)
U_train_liq_IO<- data.frame(
Names= names(U_train_liq),
U_train_liq.ioCor.1.orderPct= U_train_liq.ioCor.1.orderPct,
U_train_liq.iVol.1.orderPct= U_train_liq.iVol.1.orderPct,
U_train_liq.oVol.1.orderPct= U_train_liq.oVol.1.orderPct,
U_train_liq.Vol.1.orderPct=  U_train_liq.Vol.1.orderPct
)
View(U_train_liq_IO)
### This script screens the data set and gives an overview of what the data looks like.
#
rm(list=ls())
require(xts)
# set wd
homedir<- "/Users/Eric/Desktop/IFUND_comp"
datadir<- "/Users/Eric/Desktop/IFUND_comp/data"
setwd(homedir)
load("U_train.RData")
load("U_test.RData")
## work over liquid Universal (in sample daily AMOUT median > 5e5)
liq_threshold<- 5e5
tmp<- sapply(U_train, function(x) median(x$AMOUNT)> liq_threshold)
U_train_liq<- U_train[tmp]
UniverseLiqNames<- names(U_train_liq)
tmp<- sapply(U_train, function(x) median(x$AMOUNT))> 5e5
U_train_liq<- U_train[tmp]
liq_name<- names(U_train_liq)
U_test_liq<- U_test[liq_name]
U_train_liq.Vol.1<- lapply(U_train_liq, function(x) sd(x$RET.CC.1, na.rm = T))
U_train_liq.iVol.1<- lapply(U_train_liq, function(x) sd(x$RET.CO.1, na.rm = T))
U_train_liq.oVol.1<- lapply(U_train_liq, function(x) sd(x$RET.OC.1, na.rm = T))
U_train_liq.ioCor.1<- lapply(U_train_liq, function(x) as.numeric(cor(x$RET.CO.1, x$RET.OC.1, method = "spearman", use= "complete.obs")))
U_train_liq.ioCor.1.orderPct<- order(unlist(U_train_liq.ioCor.1, use.names = T), decreasing = F)/ length(U_train_liq.ioCor.1)
U_train_liq.Vol.1.orderPct<- order(unlist(U_train_liq.Vol.1, use.names = T), decreasing = F)/ length(U_train_liq.Vol.1)
U_train_liq.iVol.1.orderPct<- order(unlist(U_train_liq.iVol.1, use.names = T), decreasing = F)/ length(U_train_liq.iVol.1)
U_train_liq.oVol.1.orderPct<- order(unlist(U_train_liq.iVol.1, use.names = T), decreasing = F)/ length(U_train_liq.oVol.1)
U_train_liq_IO<- data.frame(
Names= names(U_train_liq),
U_train_liq.ioCor.1.orderPct= U_train_liq.ioCor.1.orderPct,
U_train_liq.iVol.1.orderPct= U_train_liq.iVol.1.orderPct,
U_train_liq.oVol.1.orderPct= U_train_liq.oVol.1.orderPct,
U_train_liq.Vol.1.orderPct=  U_train_liq.Vol.1.orderPct
)
U_train_liq_IO$iwFlag<- (U_train_liq_IO$U_train_liq.iVol.1.orderPct-
U_train_liq_IO$U_train_liq.oVol.1.orderPct) >0.25 & U_train_liq_IO$U_train_liq.Vol.1.orderPct< .95
U_train_liq_IO$owFlag<- (U_train_liq_IO$U_train_liq.oVol.1.orderPct-
U_train_liq_IO$U_train_liq.iVol.1.orderPct) > 0.25 & U_train_liq_IO$U_train_liq.Vol.1.orderPct< 0.95
#
# save(U_train_liq, file = 'U_train_liq.RData')
# save(U_test_liq, file = 'U_test_liq.RData')
View(U_train_liq_IO)
### This script screens the data set and gives an overview of what the data looks like.
#
rm(list=ls())
require(xts)
# set wd
homedir<- "/Users/Eric/Desktop/IFUND_comp"
datadir<- "/Users/Eric/Desktop/IFUND_comp/data"
setwd(homedir)
load("U_train.RData")
load("U_test.RData")
## work over liquid Universal (in sample daily AMOUT median > 5e5)
liq_threshold<- 5e5
tmp<- sapply(U_train, function(x) median(x$AMOUNT)> liq_threshold)
U_train_liq<- U_train[tmp]
UniverseLiqNames<- names(U_train_liq)
tmp<- sapply(U_train, function(x) median(x$AMOUNT))> 5e5
U_train_liq<- U_train[tmp]
liq_name<- names(U_train_liq)
U_test_liq<- U_test[liq_name]
U_train_liq.Vol.1<- lapply(U_train_liq, function(x) sd(x$RET.CC.1, na.rm = T))
U_train_liq.iVol.1<- lapply(U_train_liq, function(x) sd(x$RET.CO.1, na.rm = T))
U_train_liq.oVol.1<- lapply(U_train_liq, function(x) sd(x$RET.OC.1, na.rm = T))
U_train_liq.ioCor.1<- lapply(U_train_liq, function(x) as.numeric(cor(x$RET.CO.1, x$RET.OC.1, method = "spearman", use= "complete.obs")))
U_train_liq.ioCor.1.orderPct<- order(unlist(U_train_liq.ioCor.1, use.names = T), decreasing = F)/ length(U_train_liq.ioCor.1)
U_train_liq.Vol.1.orderPct<- order(unlist(U_train_liq.Vol.1, use.names = T), decreasing = F)/ length(U_train_liq.Vol.1)
U_train_liq.iVol.1.orderPct<- order(unlist(U_train_liq.iVol.1, use.names = T), decreasing = F)/ length(U_train_liq.iVol.1)
U_train_liq.oVol.1.orderPct<- order(unlist(U_train_liq.oVol.1, use.names = T), decreasing = F)/ length(U_train_liq.oVol.1)
U_train_liq_IO<- data.frame(
Names= names(U_train_liq),
U_train_liq.ioCor.1.orderPct= U_train_liq.ioCor.1.orderPct,
U_train_liq.iVol.1.orderPct= U_train_liq.iVol.1.orderPct,
U_train_liq.oVol.1.orderPct= U_train_liq.oVol.1.orderPct,
U_train_liq.Vol.1.orderPct=  U_train_liq.Vol.1.orderPct
)
U_train_liq_IO$iwFlag<- (U_train_liq_IO$U_train_liq.iVol.1.orderPct-
U_train_liq_IO$U_train_liq.oVol.1.orderPct) >0.25 & U_train_liq_IO$U_train_liq.Vol.1.orderPct< .95
U_train_liq_IO$owFlag<- (U_train_liq_IO$U_train_liq.oVol.1.orderPct-
U_train_liq_IO$U_train_liq.iVol.1.orderPct) > 0.25 & U_train_liq_IO$U_train_liq.Vol.1.orderPct< 0.95
#
# save(U_train_liq, file = 'U_train_liq.RData')
# save(U_test_liq, file = 'U_test_liq.RData')
View(U_train_liq_IO)
iwNames<- as.character(U_train_liq_IO[U_train_liq_IO$iwFlag, ]$Names)
owNames<- as.character(U_train_liq_IO[U_train_liq_IO$owFlag, ]$Names)
iwNames<- as.character(U_train_liq_IO[U_train_liq_IO$iwFlag, ]$Names)
owNames<- as.character(U_train_liq_IO[U_train_liq_IO$owFlag, ]$Names)
iwWeights<- rep(1/ length(iwNames), length(iwNames))
names(iwWeights)<- iwNames
owWeights<- rep(1/ length(owNames), length(owNames))
names(owWeights)<- owNames
### This script screens the data set and gives an overview of what the data looks like.
#
rm(list=ls())
require(xts)
# set wd
homedir<- "/Users/Eric/Desktop/IFUND_comp"
datadir<- "/Users/Eric/Desktop/IFUND_comp/data"
setwd(homedir)
load("U_train.RData")
load("U_test.RData")
## work over liquid Universal (in sample daily AMOUT median > 5e5)
liq_threshold<- 5e5
tmp<- sapply(U_train, function(x) median(x$AMOUNT)> liq_threshold)
U_train_liq<- U_train[tmp]
UniverseLiqNames<- names(U_train_liq)
tmp<- sapply(U_train, function(x) median(x$AMOUNT))> 5e5
U_train_liq<- U_train[tmp]
liq_name<- names(U_train_liq)
U_test_liq<- U_test[liq_name]
U_train_liq.Vol.1<- lapply(U_train_liq, function(x) sd(x$RET.CC.1, na.rm = T))
U_train_liq.iVol.1<- lapply(U_train_liq, function(x) sd(x$RET.CO.1, na.rm = T))
U_train_liq.oVol.1<- lapply(U_train_liq, function(x) sd(x$RET.OC.1, na.rm = T))
U_train_liq.ioCor.1<- lapply(U_train_liq, function(x) as.numeric(cor(x$RET.CO.1, x$RET.OC.1, method = "spearman", use= "complete.obs")))
U_train_liq.ioCor.1.orderPct<- order(unlist(U_train_liq.ioCor.1, use.names = T), decreasing = F)/ length(U_train_liq.ioCor.1)
U_train_liq.Vol.1.orderPct<- order(unlist(U_train_liq.Vol.1, use.names = T), decreasing = F)/ length(U_train_liq.Vol.1)
U_train_liq.iVol.1.orderPct<- order(unlist(U_train_liq.iVol.1, use.names = T), decreasing = F)/ length(U_train_liq.iVol.1)
U_train_liq.oVol.1.orderPct<- order(unlist(U_train_liq.oVol.1, use.names = T), decreasing = F)/ length(U_train_liq.oVol.1)
U_train_liq_IO<- data.frame(
Names= names(U_train_liq),
U_train_liq.ioCor.1.orderPct= U_train_liq.ioCor.1.orderPct,
U_train_liq.iVol.1.orderPct= U_train_liq.iVol.1.orderPct,
U_train_liq.oVol.1.orderPct= U_train_liq.oVol.1.orderPct,
U_train_liq.Vol.1.orderPct=  U_train_liq.Vol.1.orderPct
)
U_train_liq_IO$iwFlag<- (U_train_liq_IO$U_train_liq.iVol.1.orderPct-
U_train_liq_IO$U_train_liq.oVol.1.orderPct) >0.25 & U_train_liq_IO$U_train_liq.Vol.1.orderPct< .95
U_train_liq_IO$owFlag<- (U_train_liq_IO$U_train_liq.oVol.1.orderPct-
U_train_liq_IO$U_train_liq.iVol.1.orderPct) > 0.25 & U_train_liq_IO$U_train_liq.Vol.1.orderPct< 0.95
portfConst<- function( UniverseNames, longs, shorts){
res<- list()
for ( c in UniverseNames){
if( c %in% names(longs)) {
res[[c]]<- longs[c]
}else if (c %in% names(shorts)){
res[[c]]<- shorts[c]
}else
res[[c]]<-0
}
return(unlist(res, use.names = T))
}
iwNames<- as.character(U_train_liq_IO[U_train_liq_IO$iwFlag, ]$Names)
owNames<- as.character(U_train_liq_IO[U_train_liq_IO$owFlag, ]$Names)
iwWeights<- rep(1/ length(iwNames), length(iwNames))
names(iwWeights)<- iwNames
owWeights<- rep(1/ length(owNames), length(owNames))
names(owWeights)<- owNames
portf<- portfConst( UniverseNames = names(U_train_liq), longs = iwWeights,shorts = owWeights)
#
# save(U_train_liq, file = 'U_train_liq.RData')
# save(U_test_liq, file = 'U_test_liq.RData')
### This script screens the data set and gives an overview of what the data looks like.
#
rm(list=ls())
require(xts)
# set wd
homedir<- "/Users/Eric/Desktop/IFUND_comp"
datadir<- "/Users/Eric/Desktop/IFUND_comp/data"
setwd(homedir)
load("U_train.RData")
load("U_test.RData")
## work over liquid Universal (in sample daily AMOUT median > 5e5)
liq_threshold<- 5e5
tmp<- sapply(U_train, function(x) median(x$AMOUNT)> liq_threshold)
U_train_liq<- U_train[tmp]
UniverseLiqNames<- names(U_train_liq)
tmp<- sapply(U_train, function(x) median(x$AMOUNT))> 5e5
U_train_liq<- U_train[tmp]
liq_name<- names(U_train_liq)
U_test_liq<- U_test[liq_name]
U_train_liq.Vol.1<- lapply(U_train_liq, function(x) sd(x$RET.CC.1, na.rm = T))
U_train_liq.iVol.1<- lapply(U_train_liq, function(x) sd(x$RET.CO.1, na.rm = T))
U_train_liq.oVol.1<- lapply(U_train_liq, function(x) sd(x$RET.OC.1, na.rm = T))
U_train_liq.ioCor.1<- lapply(U_train_liq, function(x) as.numeric(cor(x$RET.CO.1, x$RET.OC.1, method = "spearman", use= "complete.obs")))
U_train_liq.ioCor.1.orderPct<- order(unlist(U_train_liq.ioCor.1, use.names = T), decreasing = F)/ length(U_train_liq.ioCor.1)
U_train_liq.Vol.1.orderPct<- order(unlist(U_train_liq.Vol.1, use.names = T), decreasing = F)/ length(U_train_liq.Vol.1)
U_train_liq.iVol.1.orderPct<- order(unlist(U_train_liq.iVol.1, use.names = T), decreasing = F)/ length(U_train_liq.iVol.1)
U_train_liq.oVol.1.orderPct<- order(unlist(U_train_liq.oVol.1, use.names = T), decreasing = F)/ length(U_train_liq.oVol.1)
U_train_liq_IO<- data.frame(
Names= names(U_train_liq),
U_train_liq.ioCor.1.orderPct= U_train_liq.ioCor.1.orderPct,
U_train_liq.iVol.1.orderPct= U_train_liq.iVol.1.orderPct,
U_train_liq.oVol.1.orderPct= U_train_liq.oVol.1.orderPct,
U_train_liq.Vol.1.orderPct=  U_train_liq.Vol.1.orderPct
)
U_train_liq_IO$iwFlag<- (U_train_liq_IO$U_train_liq.iVol.1.orderPct-
U_train_liq_IO$U_train_liq.oVol.1.orderPct) >0.25 & U_train_liq_IO$U_train_liq.Vol.1.orderPct< .95
U_train_liq_IO$owFlag<- (U_train_liq_IO$U_train_liq.oVol.1.orderPct-
U_train_liq_IO$U_train_liq.iVol.1.orderPct) > 0.25 & U_train_liq_IO$U_train_liq.Vol.1.orderPct< 0.95
portfConst<- function( UniverseNames, longs, shorts){
res<- list()
for ( c in UniverseNames){
if( c %in% names(longs)) {
res[[c]]<- longs[c]
}else if (c %in% names(shorts)){
res[[c]]<- shorts[c]
}else
res[[c]]<-0
}
return(unlist(res, use.names = T))
}
iwNames<- as.character(U_train_liq_IO[U_train_liq_IO$iwFlag, ]$Names)
owNames<- as.character(U_train_liq_IO[U_train_liq_IO$owFlag, ]$Names)
iwWeights<- rep(1/ length(iwNames), length(iwNames))
names(iwWeights)<- iwNames
owWeights<- -rep(1/ length(owNames), length(owNames))
names(owWeights)<- owNames
portf<- portfConst( UniverseNames = names(U_train_liq), longs = iwWeights,shorts = owWeights)
#
# save(U_train_liq, file = 'U_train_liq.RData')
# save(U_test_liq, file = 'U_test_liq.RData')
### This script screens the data set and gives an overview of what the data looks like.
#
rm(list=ls())
require(xts)
# set wd
homedir<- "/Users/Eric/Desktop/IFUND_comp"
datadir<- "/Users/Eric/Desktop/IFUND_comp/data"
setwd(homedir)
load("U_train.RData")
load("U_test.RData")
## work over liquid Universal (in sample daily AMOUT median > 5e5)
liq_threshold<- 5e5
tmp<- sapply(U_train, function(x) median(x$AMOUNT)> liq_threshold)
U_train_liq<- U_train[tmp]
UniverseLiqNames<- names(U_train_liq)
tmp<- sapply(U_train, function(x) median(x$AMOUNT))> 5e5
U_train_liq<- U_train[tmp]
liq_name<- names(U_train_liq)
U_test_liq<- U_test[liq_name]
U_train_liq.Vol.1<- lapply(U_train_liq, function(x) sd(x$RET.CC.1, na.rm = T))
U_train_liq.iVol.1<- lapply(U_train_liq, function(x) sd(x$RET.CO.1, na.rm = T))
U_train_liq.oVol.1<- lapply(U_train_liq, function(x) sd(x$RET.OC.1, na.rm = T))
U_train_liq.ioCor.1<- lapply(U_train_liq, function(x) as.numeric(cor(x$RET.CO.1, x$RET.OC.1, method = "spearman", use= "complete.obs")))
U_train_liq.ioCor.1.orderPct<- order(unlist(U_train_liq.ioCor.1, use.names = T), decreasing = F)/ length(U_train_liq.ioCor.1)
U_train_liq.Vol.1.orderPct<- order(unlist(U_train_liq.Vol.1, use.names = T), decreasing = F)/ length(U_train_liq.Vol.1)
U_train_liq.iVol.1.orderPct<- order(unlist(U_train_liq.iVol.1, use.names = T), decreasing = F)/ length(U_train_liq.iVol.1)
U_train_liq.oVol.1.orderPct<- order(unlist(U_train_liq.oVol.1, use.names = T), decreasing = F)/ length(U_train_liq.oVol.1)
U_train_liq_IO<- data.frame(
Names= names(U_train_liq),
U_train_liq.ioCor.1.orderPct= U_train_liq.ioCor.1.orderPct,
U_train_liq.iVol.1.orderPct= U_train_liq.iVol.1.orderPct,
U_train_liq.oVol.1.orderPct= U_train_liq.oVol.1.orderPct,
U_train_liq.Vol.1.orderPct=  U_train_liq.Vol.1.orderPct
)
U_train_liq_IO$iwFlag<- (U_train_liq_IO$U_train_liq.iVol.1.orderPct-
U_train_liq_IO$U_train_liq.oVol.1.orderPct) >0.25 & U_train_liq_IO$U_train_liq.Vol.1.orderPct< .95
U_train_liq_IO$owFlag<- (U_train_liq_IO$U_train_liq.oVol.1.orderPct-
U_train_liq_IO$U_train_liq.iVol.1.orderPct) > 0.25 & U_train_liq_IO$U_train_liq.Vol.1.orderPct< 0.95
portfConst<- function( UniverseNames, longs, shorts){
res<- list()
for ( c in UniverseNames){
if( c %in% names(longs)) {
res[[c]]<- longs[c]
}else if (c %in% names(shorts)){
res[[c]]<- shorts[c]
}else
res[[c]]<-0
}
return(res)
}
iwNames<- as.character(U_train_liq_IO[U_train_liq_IO$iwFlag, ]$Names)
owNames<- as.character(U_train_liq_IO[U_train_liq_IO$owFlag, ]$Names)
iwWeights<- rep(1/ length(iwNames), length(iwNames))
names(iwWeights)<- iwNames
owWeights<- -rep(1/ length(owNames), length(owNames))
names(owWeights)<- owNames
portf<- portfConst( UniverseNames = names(U_train_liq), longs = iwWeights,shorts = owWeights)
## test
#
# save(U_train_liq, file = 'U_train_liq.RData')
# save(U_test_liq, file = 'U_test_liq.RData')
### This script screens the data set and gives an overview of what the data looks like.
#
rm(list=ls())
require(xts)
# set wd
homedir<- "/Users/Eric/Desktop/IFUND_comp"
datadir<- "/Users/Eric/Desktop/IFUND_comp/data"
setwd(homedir)
load("U_train.RData")
load("U_test.RData")
## work over liquid Universal (in sample daily AMOUT median > 5e5)
liq_threshold<- 5e5
tmp<- sapply(U_train, function(x) median(x$AMOUNT)> liq_threshold)
U_train_liq<- U_train[tmp]
UniverseLiqNames<- names(U_train_liq)
tmp<- sapply(U_train, function(x) median(x$AMOUNT))> 5e5
U_train_liq<- U_train[tmp]
liq_name<- names(U_train_liq)
U_test_liq<- U_test[liq_name]
U_train_liq.Vol.1<- lapply(U_train_liq, function(x) sd(x$RET.CC.1, na.rm = T))
U_train_liq.iVol.1<- lapply(U_train_liq, function(x) sd(x$RET.CO.1, na.rm = T))
U_train_liq.oVol.1<- lapply(U_train_liq, function(x) sd(x$RET.OC.1, na.rm = T))
U_train_liq.ioCor.1<- lapply(U_train_liq, function(x) as.numeric(cor(x$RET.CO.1, x$RET.OC.1, method = "spearman", use= "complete.obs")))
U_train_liq.ioCor.1.orderPct<- order(unlist(U_train_liq.ioCor.1, use.names = T), decreasing = F)/ length(U_train_liq.ioCor.1)
U_train_liq.Vol.1.orderPct<- order(unlist(U_train_liq.Vol.1, use.names = T), decreasing = F)/ length(U_train_liq.Vol.1)
U_train_liq.iVol.1.orderPct<- order(unlist(U_train_liq.iVol.1, use.names = T), decreasing = F)/ length(U_train_liq.iVol.1)
U_train_liq.oVol.1.orderPct<- order(unlist(U_train_liq.oVol.1, use.names = T), decreasing = F)/ length(U_train_liq.oVol.1)
U_train_liq_IO<- data.frame(
Names= names(U_train_liq),
U_train_liq.ioCor.1.orderPct= U_train_liq.ioCor.1.orderPct,
U_train_liq.iVol.1.orderPct= U_train_liq.iVol.1.orderPct,
U_train_liq.oVol.1.orderPct= U_train_liq.oVol.1.orderPct,
U_train_liq.Vol.1.orderPct=  U_train_liq.Vol.1.orderPct
)
U_train_liq_IO$iwFlag<- (U_train_liq_IO$U_train_liq.iVol.1.orderPct-
U_train_liq_IO$U_train_liq.oVol.1.orderPct) >0.25 & U_train_liq_IO$U_train_liq.Vol.1.orderPct< .95
U_train_liq_IO$owFlag<- (U_train_liq_IO$U_train_liq.oVol.1.orderPct-
U_train_liq_IO$U_train_liq.iVol.1.orderPct) > 0.25 & U_train_liq_IO$U_train_liq.Vol.1.orderPct< 0.95
portfConst<- function( UniverseNames, longs, shorts){
res<- list()
for ( c in UniverseNames){
if( c %in% names(longs)) {
res[[c]]<- longs[c]
}else if (c %in% names(shorts)){
res[[c]]<- shorts[c]
}else
res[[c]]<-0
}
return(res)
}
iwNames<- as.character(U_train_liq_IO[U_train_liq_IO$iwFlag, ]$Names)
owNames<- as.character(U_train_liq_IO[U_train_liq_IO$owFlag, ]$Names)
iwWeights<- rep(1/ length(iwNames), length(iwNames))
names(iwWeights)<- iwNames
owWeights<- -rep(1/ length(owNames), length(owNames))
names(owWeights)<- owNames
portf<- portfConst( UniverseNames = names(U_train_liq), longs = iwWeights,shorts = owWeights)
## test
portfRet<- function( Universe, portf){ # Universe: list; portf: list
tmp<- list()
for ( x in names(Universe)){
tmp[[x]]<- Universe[[c]]$RET.CC.1*portf[[c]]
}
tmp<- as.data.frame(tmp)
tmp<- apply(tmp, 1, mean)
tmp<- xts(tmp, order.by = as.Date(index(Universe[[1]])))
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
portfret<- portfRet(Universe = U_test_liq, portf = portf )
portfValue<- ret2value(portfret)
#
# save(U_train_liq, file = 'U_train_liq.RData')
# save(U_test_liq, file = 'U_test_liq.RData')
### This script screens the data set and gives an overview of what the data looks like.
#
rm(list=ls())
require(xts)
# set wd
homedir<- "/Users/Eric/Desktop/IFUND_comp"
datadir<- "/Users/Eric/Desktop/IFUND_comp/data"
setwd(homedir)
load("U_train.RData")
load("U_test.RData")
## work over liquid Universal (in sample daily AMOUT median > 5e5)
liq_threshold<- 5e5
tmp<- sapply(U_train, function(x) median(x$AMOUNT)> liq_threshold)
U_train_liq<- U_train[tmp]
UniverseLiqNames<- names(U_train_liq)
tmp<- sapply(U_train, function(x) median(x$AMOUNT))> 5e5
U_train_liq<- U_train[tmp]
liq_name<- names(U_train_liq)
U_test_liq<- U_test[liq_name]
U_train_liq.Vol.1<- lapply(U_train_liq, function(x) sd(x$RET.CC.1, na.rm = T))
U_train_liq.iVol.1<- lapply(U_train_liq, function(x) sd(x$RET.CO.1, na.rm = T))
U_train_liq.oVol.1<- lapply(U_train_liq, function(x) sd(x$RET.OC.1, na.rm = T))
U_train_liq.ioCor.1<- lapply(U_train_liq, function(x) as.numeric(cor(x$RET.CO.1, x$RET.OC.1, method = "spearman", use= "complete.obs")))
U_train_liq.ioCor.1.orderPct<- order(unlist(U_train_liq.ioCor.1, use.names = T), decreasing = F)/ length(U_train_liq.ioCor.1)
U_train_liq.Vol.1.orderPct<- order(unlist(U_train_liq.Vol.1, use.names = T), decreasing = F)/ length(U_train_liq.Vol.1)
U_train_liq.iVol.1.orderPct<- order(unlist(U_train_liq.iVol.1, use.names = T), decreasing = F)/ length(U_train_liq.iVol.1)
U_train_liq.oVol.1.orderPct<- order(unlist(U_train_liq.oVol.1, use.names = T), decreasing = F)/ length(U_train_liq.oVol.1)
U_train_liq_IO<- data.frame(
Names= names(U_train_liq),
U_train_liq.ioCor.1.orderPct= U_train_liq.ioCor.1.orderPct,
U_train_liq.iVol.1.orderPct= U_train_liq.iVol.1.orderPct,
U_train_liq.oVol.1.orderPct= U_train_liq.oVol.1.orderPct,
U_train_liq.Vol.1.orderPct=  U_train_liq.Vol.1.orderPct
)
U_train_liq_IO$iwFlag<- (U_train_liq_IO$U_train_liq.iVol.1.orderPct-
U_train_liq_IO$U_train_liq.oVol.1.orderPct) >0.25 & U_train_liq_IO$U_train_liq.Vol.1.orderPct< .95
U_train_liq_IO$owFlag<- (U_train_liq_IO$U_train_liq.oVol.1.orderPct-
U_train_liq_IO$U_train_liq.iVol.1.orderPct) > 0.25 & U_train_liq_IO$U_train_liq.Vol.1.orderPct< 0.95
portfConst<- function( UniverseNames, longs, shorts){
res<- list()
for ( c in UniverseNames){
if( c %in% names(longs)) {
res[[c]]<- longs[c]
}else if (c %in% names(shorts)){
res[[c]]<- shorts[c]
}else
res[[c]]<-0
}
return(res)
}
iwNames<- as.character(U_train_liq_IO[U_train_liq_IO$iwFlag, ]$Names)
owNames<- as.character(U_train_liq_IO[U_train_liq_IO$owFlag, ]$Names)
iwWeights<- rep(1/ length(iwNames), length(iwNames))
names(iwWeights)<- iwNames
owWeights<- -rep(1/ length(owNames), length(owNames))
names(owWeights)<- owNames
portf<- portfConst( UniverseNames = names(U_train_liq), longs = iwWeights,shorts = owWeights)
## test
portfRet<- function( Universe, portf){ # Universe: list; portf: list
tmp<- list()
for ( x in names(Universe)){
tmp[[x]]<- Universe[[x]]$RET.CC.1*portf[[x]]
}
tmp<- as.data.frame(tmp)
tmp<- apply(tmp, 1, mean)
tmp<- xts(tmp, order.by = as.Date(index(Universe[[1]])))
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
portfret<- portfRet(Universe = U_test_liq, portf = portf )
portfValue<- ret2value(portfret)
#
# save(U_train_liq, file = 'U_train_liq.RData')
# save(U_test_liq, file = 'U_test_liq.RData')
View(portfValue)
View(portfret)
summary(portfret)
savehistory("~/Desktop/IFUND_comp/untitled.r")
