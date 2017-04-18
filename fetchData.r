### This script screens the data set and gives an overview of what the data looks like. Construct database 
# DFList. A list of df from each csv file
# Universe. The stock universe where each has same length
# U_train. A list of elements from universe, starting from 20140102 to 20151231
# U_test, A list of elements from universe, starting from 20160104 to 20161231


#
require(xts)
rm(list=ls())
# set wd
homedir<- "/Users/Eric/Documents/IFUND_comp"
datadir<- "/Users/Eric/Documents/IFUND_comp/data"
setwd(datadir)
# # read csv
fileNames<- list.files(pattern = "*.csv")
DFList<- list()

for (x in fileNames) {
  eval(parse(text= paste( "DFList$'", strsplit(x, split='_')[[1]][1], "'<- read.csv(x, header=T, col.names= c('DATE', 'OPEN', 'HIGH', 'LOW', 'CLOSE', 'VOLUME', 'AMOUNT') )", sep="")))
}

setwd(homedir)
save(DFList,file = 'DFList.RData')

# load DFList
setwd(homedir)
load('DFList.RData')

DFList<- lapply(DFList,  function(x) xts(x[,c("OPEN", "HIGH", "LOW", "CLOSE", "VOLUME", "AMOUNT")], order.by = as.Date(as.character(x$DATE))))

tmp<- lapply(DFList, function(x) dim(x)[1])
tmp<- unlist(tmp)> 50
DFList<- DFList[tmp]

ret_nperiod<- function(df, type='cc', N=1){ #df must be xts obj
  tmp<-df
  for (ty in type){
    for( n in N){
      if(ty=='co'){ # close to open, intraday ret
        colName<- paste('RET.CO.', n, sep="")
        a<-(tmp$CLOSE- tmp$OPEN)/ tmp$OPEN
        eval(parse(text= paste("tmp$",colName, "<- a", sep="")))
        
      }else if (ty=='oc'){ # open to close overnigth ret
        colName<- paste("RET.OC.",n, sep="")
        a<- (tmp$OPEN- lag(tmp$CLOSE, k=n))/ lag(tmp$CLOSE, k=n)
        eval(parse(text= paste("tmp$",colName, "<- a", sep="")))
      }else if (ty=='ho'){ # high to open intraday 
        colName<- paste("RET.HO.",n, sep="")
        a<- (tmp$HIGH- tmp$OPEN)/ tmp$OPEN
        eval(parse(text= paste("tmp$",colName, "<- a", sep="")))
      }else if(ty=='lo'){ # low to open intra day
        colName<- paste("RET.LO.", n, sep="")
        a<- (tmp$LOW- tmp$OPEN)/ tmp$OPEN
        eval(parse(text= paste("tmp$",colName, "<- a", sep="")))
      }else if(ty== 'hl'){ # high to low intra day
        colName<- paste("RET.HL.", n, sep="")
        a<- (tmp$HIGH- tmp$LOW)/ tmp$OPEN
        eval(parse(text= paste("tmp$",colName, "<- a", sep="")))
        
      }
      else { ## close to close, daily ret
        colName<- paste("RET.CC.",n, sep="")
        a<- (tmp$CLOSE- lag(tmp$CLOSE, k=n))/ lag(tmp$CLOSE, k=n)
        eval(parse(text= paste("tmp$",colName, "<- a", sep="")))
      }
    }
  }
  
  return(tmp)
}


DFList<- lapply(DFList, function(x) ret_nperiod(df = x, type = "cc", N = c(1,3,7,20)))
DFList<- lapply(DFList, function(x) ret_nperiod(df=x, type = c("co", "oc","hl", "ho","lo")))

WHOLE<- DFList
save(WHOLE, file = "WHOLE.RData")

# DFList contains entites with shorter history (new stocks). Remove them to uniform the length. Save the Universe

stdLen<- 733
tmp<- sapply(names(DFList), function (x)  eval(parse(text= paste("dim(DFList$'", x, "')[1]== stdLen ", sep=""))))
Universe<- DFList[tmp]

d<- names(DFList[[1]])
e<- grepl("RET.CC.*", d)
f<- grepl("RET.CO.*", d)
g<- grepl("RET.OC.*", d)
h<- data.frame( e=e, f=f, g=g)
h<- apply(h, 1, any )
indx_col<- d[h]

INDX_EQW<- list()
for ( c in indx_col){
  tmp<- lapply(Universe, function(x) x[, c])
  tmp<- as.data.frame(tmp)
  tmp<- apply(tmp, 1, mean)
  INDX_EQW[[c]]<- tmp
}
INDX_EQW$AMOUNT<- apply( as.data.frame( lapply(Universe, function(x) x$AMOUNT)), 1, function(x)  sum(x, na.rm = T))

INDX_EQW<- xts(as.data.frame(INDX_EQW) , order.by = as.Date(as.character(index(Universe[[1]]))))
save(Universe, file= 'Universe.RData')
save(INDX_EQW, file= 'INDX_EQW.RData')

# Save U_train and U_test. train tasks 20140102- 20151231. test takes the else
split_flag= 489
U_train<- Universe
U_train<- lapply(U_train, function(x) x[1:split_flag,])
U_test<-  Universe
U_test<-  lapply(U_test,  function(x) x[(split_flag+1): dim(x)[1], ])
save(U_train, file = 'U_train.RData')
save(U_test,  file = 'U_test.RData')
INDX_EQW_train<- INDX_EQW[1:split_flag]
INDX_EQW_test<- INDX_EQW[(split_flag+1) : dim(INDX_EQW)[1]]




