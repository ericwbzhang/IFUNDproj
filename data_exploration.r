### This script screens the data set and gives an overview of what the data looks like. 


#
rm(list=ls())
require(xts)
# set wd
homedir<- "/Users/Eric/Desktop/IFUND_comp"
datadir<- "/Users/Eric/Desktop/IFUND_comp/data"
setwd(homedir)

load('U_train.RData')



hist(sapply(U_train, function(x) median(x$AMOUNT)))
liq_order<- order(sapply(U_train, function(x) median(x$AMOUNT)), decreasing = T)
U_train<- U_train[liq_order]
liq_threshold<- 5e5
tmp<- sapply(U_train, function(x) median(x$AMOUNT)> liq_threshold)
U_train_liq<- U_train[tmp]
UniverseLiqNames<- names(U_train_liq)

load('U_test.RData')
U_test_liq<- U_test[UniverseLiqNames]
# U_train_liq<- lapply(U_train_liq, function(x) xts(x = x[, colnames(x)!= "DATE"], order.by = as.Date(as.character(x$DATE))))
# U_test_liq<-  lapply(U_test_liq,  function(x) xts(x=  x[, colnames(x)!= "DATE"], order.by = as.Date(as.character(x$DATE))) )
# save(U_train_liq, file = 'U_train_liq.RData')
# save(U_test_liq, file = 'U_test_liq.RData')
## work over liquid Universal (in sample daily AMOUT median > 5e5)
# 
# ret_nperiod<- function(df, type='cc', N=1){ #df must be xts obj
#   tmp<-df
#   for (ty in type){
#     for( n in N){
#       if(ty=='co'){ # close to open, intraday ret
#         colName<- paste('RET.CO.', n, sep="")
#         a<-(tmp$CLOSE- tmp$OPEN)/ tmp$OPEN
#         eval(parse(text= paste("tmp$",colName, "<- a", sep="")))
#     
#       }else if (ty=='oc'){ # open to close overnigth ret
#         colName<- paste("RET.OC.",n, sep="")
#         a<- (tmp$OPEN- lag(tmp$CLOSE, k=n))/ lag(tmp$CLOSE, k=n)
#         eval(parse(text= paste("tmp$",colName, "<- a", sep="")))
#       }else if (ty=='ho'){ # high to open intraday 
#         colName<- paste("RET.HO.",n, sep="")
#         a<- (tmp$HIGH- tmp$OPEN)/ tmp$OPEN
#         eval(parse(text= paste("tmp$",colName, "<- a", sep="")))
#       }else if(ty=='lo'){ # low to open intra day
#         colName<- paste("RET.LO.", n, sep="")
#         a<- (tmp$LOW- tmp$OPEN)/ tmp$OPEN
#         eval(parse(text= paste("tmp$",colName, "<- a", sep="")))
#       }else if(ty== 'hl'){ # high to low intra day
#         colName<- paste("RET.HL.", n, sep="")
#         a<- (tmp$HIGH- tmp$LOW)/ tmp$OPEN
#         eval(parse(text= paste("tmp$",colName, "<- a", sep="")))
#         
#       }
#       else { ## close to close, daily ret
#         colName<- paste("RET.CC.",n, sep="")
#         a<- (tmp$CLOSE- lag(tmp$CLOSE, k=n))/ lag(tmp$CLOSE, k=n)
#         eval(parse(text= paste("tmp$",colName, "<- a", sep="")))
#       }
#     }
#   }
# 
#   return(tmp)
# }
# 
# 
# 
# U_train_liq<- lapply(U_train_liq, function(x) ret_nperiod(x, type = "cc", N = c(1,3,7,20)))
# U_train_liq<- lapply(U_train_liq, function(x) ret_nperiod(x, type =c( "co", "oc",'ho','lo','hl')))
# U_test_liq<- lapply(U_test_liq, function(x) ret_nperiod(x, type = "cc", N=c(1,3,7,20)))
# U_test_liq<- lapply(U_test_liq, function(x) ret_nperiod(x, type =c( "co", "oc",'ho','lo','hl')))

U_train_vol.1<- lapply(U_train_liq, function(x) sd(x$RET.CC.1, na.rm = T))
U_train_iVol.1<- lapply(U_train_liq, function(x) sd(x$RET.CO.1, na.rm = T))
U_train_oVol.1<- lapply(U_train_liq, function(x) sd(x$RET.OC.1, na.rm = T))

hist(unlist(U_train_oVol.1))


# overNight<- order(unlist(U_train_oVol.1), decreasing = F)
# overNight<- names(U_train_oVol.1)[overNight]
# ONLVNames<- overNight[1: int(length(overNight)*0.25)]
# ONLVNames<- overNight[1: round(length(overNight)*0.25)]
# ONHVNames<- overNight[round(length(overNight)* 0.5): round(length(overNight)* 0.75)]
# a<- U_train_oVol.1[ONLVNames]
# b<- U_train_oVol.1[ONHVNames]
# ONHVNames<- overNight[round(length(overNight)* 0.65): round(length(overNight)* 0.9)]
# b<- U_train_oVol.1[ONHVNames]
# mean(unlist(U_train_iVol.1[ONLVNames]))
# mean(unlist(U_train_iVol.1[ONHVNames]))
# volOrder<- data.frame(Names= names(U_train_liq), OVorder= order(U_train_oVol.1, decreasing = F)/length(names(U_train_oVol.1)),IVorder= order(U_train_iVol.1, decreasing = F)/length(names(U_train_iVol.1)), Vorder= order(U_train_vol.1, decreasing = F)/ length(names(U_train_vol.1)) )
volOrder<- data.frame(Names= names(U_train_liq), OVorder= order(unlist(U_train_oVol.1), decreasing = F)/length(names(U_train_oVol.1)),IVorder= order(unlist(U_train_iVol.1), decreasing = F)/length(names(U_train_iVol.1)), Vorder= order(unlist(U_train_vol.1), decreasing = F)/ length(names(U_train_vol.1)) )

# voldf<- data.frame(Names= names(U_train_liq), OVorder= unlist(U_train_oVol.1),IVorder= unlist(U_train_iVol.1), Vorder= unlist(U_train_vol.1))
# View(volOrder)
# volOrder$flag<- abs(volOrder$OVorder- volOrder$IVorder)> .25
# volOrder$ONweighted<- volOrder$OVorder- volOrder$IVorder> .25
# volOrder$Iweighted<-  volOrder$IVorder- volOrder$OVorder> .25
# u<- U_train_liq$GT3J87VA7
# 

# View(u)
volOrder$ONweighted<- (volOrder$OVorder- volOrder$IVorder> .25) & volOrder$Vorder< .9
volOrder$Iweighted<-  (volOrder$IVorder- volOrder$OVorder> .25) & volOrder$Vorder< .9
# View(volOrder)
IweightedNames<- volOrder$Names[volOrder$Iweighted]
ONweightedNames<-volOrder$Names[volOrder$ONweighted]
IweightedNames<- as.character( volOrder$Names[volOrder$Iweighted])
ONweightedNames<-as.character(volOrder$Names[volOrder$ONweighted])

# U_test_liq<- lapply(U_test_liq, function(x) ret_nperiod(x, type = "cc", N=c(1,3,7,20)))
# U_test_liq<- lapply(U_test_liq, function(x) ret_nperiod(x, type =c( "co", "oc",'ho','lo','hl')))
IWPortf<- U_test_liq[IweightedNames]
OWPortf<- U_test_liq[ONweightedNames]
IWPortfRet<- data.frame(lapply(IWPortf, function(x) x$RET.CC.1))
OWPortfRet<- data.frame(lapply(OWPortf, function(x) x$RET.CC.1))
# View(IWPortfRet)
IWPortfRet<- xts(IWPortfRet, order.by = as.Date(index(IWPortfRet)))
OWPortfRet<- xts(OWPortfRet, order.by = as.Date(index(OWPortfRet)))
# View(IWPortfRet)
# View(OWPortfRet)
# View(IWPortfRet)
OWret<- apply(OWPortfRet, 1, mean)
IWret<- apply(IWPortfRet, 1, mean)
plot(OWret)
summary(OWret)
summary(IWret)
#####################################################
portfConst<- function( UniverseNames, weights){
  res<- list()
  for ( c in UniverseNames){
    if( c %in% names(weights)) {
      res[[c]]<- as.numeric(weights[c])
    }else  res[[c]]<-0
  }
  
  return(res)
  
}

portfRet<- function( Universe, portf){ # Universe: list; portf: list
  tmp<- list()
  for ( x in names(Universe)){
    tmp[[x]]<- Universe[[x]]$RET.CC.1 * portf[[x]]
  }
  tmp<- as.data.frame(tmp)
  tmp<- apply(tmp, 1, sum)
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

IwWeights<- rep(1/length(IweightedNames), length(IweightedNames))
ONweights<- rep(-1/length(ONweightedNames), length(ONweightedNames))
names(IwWeights)<- IweightedNames
names(ONweights)<- ONweightedNames
w<- c(IwWeights, ONweights)
portf<- portfConst(UniverseNames = names(U_train_liq), -w)
portfret<- portfRet(Universe = U_train_liq, portf = portf)
portfvalue<- ret2value(portfret)
plot(portfvalue)

#############################################
# Ret<- data.frame(OWret= OWret, IWret= IWret, index= index(OWret))
# Ret$Ret<- Ret$IWret- Ret$OWret
# # View(portfRet)
# summary(Ret$Ret)
# Ret$Value<-100
# Ret$lagValue<-100
# Ret$Value<-Ret$lagValue* (1+ Ret$Ret)
# ret2value.1<- function(ret){
#   value<- rep(0, length(ret))
#   tmp<-1
#   for (i in 1: length(value)){
#     value[i]<- tmp* (1+ ret[i])
#     tmp<- value[i]
#   }
#   return(value)
# }
# Ret$Value<- ret2value.1(Ret$Ret)
# # View(portfRet)
# 
# 
# # View(portfRet)
# # View(portfRet)
# mean(Ret$Ret)/sd(Ret$Ret)
# mean(Ret$Ret, na.rm = T)/sd(Ret$Ret, na.rm = T)
# mean(Ret$Ret, na.rm = T)/sd(Ret$Ret, na.rm = T)* sqrt(252)
# summary(Ret$Value)
# Ret<- xts(Ret, order.by = as.Date(index(U_test_liq)))
# Ret<- xts(Ret, order.by = as.Date(index(U_test_liq$`8NLW4QBX4`)))
# # View(portfRet)
# plot(Ret$Value, 'l')
# plot(Ret$Value)
# # index(portfRet)
# # as.Date('2016-03-28')> as.Date('2016-09-01')
# # as.Date('2016-03-28')< as.Date('2016-09-01')
# # portfRet$dateMarch<- sapply(as.character(index(portfRet)), function(x) ifelse( x> as.Date("2016-03-01"), 1, -1))
# # View(portfRet)
# # View(portfRet)
# # sapply(as.character(index(portfRet)), function(x) ifelse( x> as.Date("2016-03-01"), 1, -1))
# # z<- apply(portfRet, 1, function(x) all(is.na(x)))
# # portfRet<- portfRet[!z, ]
# # View(portfRet)
# # z
# # portfRet<- portfRet[is.na(portfRet$dateMarch), ]
# # View(portfRet)
# 
