### Details -----------------------------------------------------------------
##Restrykcje:
##R2adjusted > od założonego poziomu (default = 0.5)
##Wszystkie zmienne istotne na zadanym poziomie "alfa" (default = 0.1)
##Mozliwosc nakladania restrykcji na znaki
##Mozliwosc wyboru ilosci najlepszych modeli
##Możliwość odrzucenia modeli, których reszty nie są normalne (default = 0.05)
##Możliwość odrzucenia modeli, których reszty są heteroskedastyczne (default = 0.05)
##Mozliwosc okreslenia zmiennych "must.have" - conajmniej jedna z tego wektora musi byc w regresji

### Pakiety -----------------------------------------------------------------

 
#ladujemy pakiety (trzeba je zainstalowac)
#install.packages(lmtest)
#install.packages(tseries)
library(lmtest)
library(tseries)
library(mFilter)

### HP filter------------------------------------------------------------------------
##hp filter
## funkcja filer, robi gap z skladnika cylkicznego zmiennej zlogarytmowanej oraz trend z odlogarytmowanego skladnika cyklicznego zmiennej zlogarytmowanej

filter.gap <- function(df, column, lambda = 6.25) {
   data <- df   
   for( i in 1:length(column) ) {
      temp <- vector()
      if(any(is.na(data[,column[i]])) == TRUE) {
      temp[as.vector(attributes(na.omit(data[,column[i]]))$na.action)] <- NA
      temp[-as.vector(attributes(na.omit(data[,column[i]]))$na.action)] <-hpfilter(log(na.omit(data[,column[i]])), freq = lambda, type = "lambda")$cycle
      }
      temp <- hpfilter(log(na.omit(data[,column[i]])), freq = lambda, type = "lambda")$cycle
      
      data <- cbind(data, temp)
   }
   
   if (is.numeric(column) == TRUE) 
   {
      colnames(data) <- c(colnames(df), paste("gap", colnames(df)[column], sep = "_")) 
   } else {
      colnames(data) <- c(colnames(df), paste("gap", column, sep = "_")) 
   }
   
   data
}

filter.trend <- function(df, column, lambda = 6.25) {
   data <- df   
   for( i in 1:length(column) ) {
      temp <- vector()
      if(any(is.na(data[,column[i]])) == TRUE) {
      temp[as.vector(attributes(na.omit(data[,column[i]]))$na.action)] <- NA
      temp[-as.vector(attributes(na.omit(data[,column[i]]))$na.action)] <- exp(hpfilter(log(na.omit(data[,column[i]])), freq = lambda, type = "lambda")$trend)
      }
      else temp <- exp(hpfilter(log(na.omit(data[,column[i]])), freq = lambda, type = "lambda")$trend)
      
      data <- cbind(data, temp)
   }
   
   if (is.numeric(column) == TRUE) 
   {
      colnames(data) <- c(colnames(df), paste("trend", colnames(df)[column], sep = "_")) 
   } else {
      colnames(data) <- c(colnames(df), paste("trend", column, sep = "_")) 
   }
   
   data
}

### Dummy variable example --------------------------------------------------
Bratskoye$dummy <- ifelse(Bratskoye$X == 2009, 1, 0)
dim(BMA_DG_price)
BMA_DG_price$dummy <- c(rep(0,14), rep(1,2), rep(0,5))

### Dodawanie logarytm�w ----------------------------------------------------

logs <- function(df, column) {
   data <- cbind(df, log(df[,column]))
   if (is.numeric(column) == TRUE) 
      {
      colnames(data) <- c(colnames(df), paste("ln", colnames(df)[column], sep = "_")) 
   } else {
      colnames(data) <- c(colnames(df), paste("ln", column, sep = "_")) 
   }
   data
}

### Przyrostowanie ----------------------------------------------------------


differ <- function(df, column) {
data <- df   
for( i in 1:length(column) ) {
data <- cbind(data, c(NA, diff(data[,column[i]])))
}

if (is.numeric(column) == TRUE) 
{
   colnames(data) <- c(colnames(df), paste("diff", colnames(df)[column], sep = "_")) 
} else {
   colnames(data) <- c(colnames(df), paste("diff", column, sep = "_")) 
}

data
}


colnames(Serbia)

Serbia <- differ(Serbia, c(1:9, 12:30))

### Zapisywanie danych przyklad ---------------------------------------------

write.csv(Serbia, file = "Serbia_2.csv")

### Wczytywanie danych ------------------------------------------------------

#asd<- function(x, y = length(x)){
#   print(y)
#}

#tutaj dorobic osobno dla formuly osobno dla vector + matrix
# setData <- function(x, y, positive = rep(NA, dim(x)[2]), negative = rep(NA, dim(x)[2]), must.have = rep(NA, dim(x)[2]), at.most = rep(NA, dim(x)[2])){
#    
#    positive <- positive==1
#    negative <- negative==1
#    must.have <- must.have == 1
#    at.most <- at.most == 1
#    names(positive) <- colnames(x)
#    names(negative) <- colnames(x)
#    names(must.have) <- colnames(x)
#    names(at.most) <- colnames(x)   
#    data.frame(positive = positive, negative = negative, must.have = must.have, at.most = at.most)
# }



#wczytanie danych surowych
colnames(...)
dane <- cbind(BMA_DG_volume[,3, drop = FALSE], BMA_DG_volume[,-c(1:4)])
dane <- iris[,1:4]

#wybieranie znaku zmiennych

colnames(dane)[-1] #do podgladniecia kolejnosci zmiennych
length(colnames(dane)[-1])

positive <- rep(NA, length(colnames(dane)[-1]))
positive[c(3,5,6,8,10,13,14,15,16,17)] <-1

negative <- rep(NA, length(colnames(dane)[-1]))
negative[c(9,11)] <- 1

must.have<- rep(NA, length(colnames(dane)[-1]))

at.most  <- rep(NA, length(colnames(dane)[-1]))
at.most  <- c(NA,c(1,1), rep(NA, length(colnames(dane)[-1])-3))

### Przeksztalcenia na danych -----------------------------------------------

positive <- positive==1
negative <- negative==1
must.have <- must.have == 1
at.most <- at.most == 1
names(positive) <- colnames(dane)[-1]
names(negative) <- colnames(dane)[-1]
names(must.have) <- colnames(dane)[-1]
names(at.most) <- colnames(dane)[-1]
#ponizsze do sprawdzenia poprawnosci wprowadzonych restrykcji
positive
negative
must.have
at.most
# pytanie na kurs R o setData
# function(x, y, conditions = setData(x,y) , m = 1, nv.max = min(T-2, length(colnames(x)),3), alfa = 0.1, j.b = 0.05, bp = 0.05){
#    
# }

### Specyfikacja modelu -----------------------------------------------------
              #~! Do modyfikacji przez użytkownika !~#
T <- dim(na.omit(dane))[1] # rzeczywiscie dostepna liczba obserwacja (po wyruzceniu z rows NAow)
m <- 1 #maksymalna liczba zmiennych danego typu
nv.max <- min(T-2, length(colnames(dane)[-1]),3)  #jezeli nie okreslono inaczej jest to: liczba zm egzogenicznych albo T-2 (min) (T-1 z wyrazem wolnym)
alfa <- 0.10       #poziom istotnosci do odrzucania modeli
j.b <- 0.05       #poziom istotnosci do odrzucania modeli ze wzgledu na nienormalnosc reszt(0 dla niesprawdzania kryterium)
b.p <- 0.05        #poziom istotnosci do odrzucenia modeli ze względu na heteroskedastycznosc reszt (0 dla niesprawdzenia kryterium)
R.s <- 0    #R2 adjusted ponizej ktorego modele mają być odrzucane

### Kod wlasciwy ------------------------------------------------------------

#lista <- list()
#for (k in 1:20){ #dla petli po danych
#dane <- cbind(klasyfikacja[,0+k],klasyfikacja[,21:32])

lst<-list()
R2 <- vector()
R2.DW <- vector()
#wlasciwy loop dla wszystkich kombinacji
#licznik dla progress baru
n.max <- 0
progress.bar <-0
for (i in 1:nv.max) {
  for (s in 1:dim(combn(length(colnames(dane)[-1]),i))[2]){
    n.max <- n.max + 1
  }}


pb <- txtProgressBar(min = 0, max = n.max , style = 3)
for (i in 1:nv.max) {
  for (s in 1:dim(combn(length(colnames(dane)[-1]),i))[2]){
    #print(combn(length(colnames(dane)[-1]),i)[,s])
    
    progress.bar <- progress.bar + 1
    Sys.sleep(0.1)
    # update progress bar
    setTxtProgressBar(pb, progress.bar)  
    
    
    kolumny<-combn(length(colnames(dane)[-1]),i)[,s]
    #if (dim(dane[,c(1,kolumny+1)])[1] < 4) next
    
    b <-lm(dane[,c(1,kolumny+1)])
    #fit b <- glm(dane[,c(1,kolumny+1)],family=gaussian())
    
#    if(any(c(  !negative[c(names(which(b$coefficients[-1]>=0)))] ,  !positive[c(names(which(b$coefficients[-1]<=0)))],          #summary(b)[[4]][,4][-1]<alfa)==FALSE,na.rm=TRUE)) next # warunki na znaki oraz istotnos
#    #if(summary(b)$r.squared<R.s) next # r kwadrat adjusted 
#    if(jarque.bera.test(residuals(b))[3]$p.value<j.b) next #normalnosc 
#    if(bptest(b)[4]$p.value<b.p) next # heteroskedastycznosc
#    if(any(must.have, na.rm=TRUE)==TRUE){
#    if(!any(must.have[c(names(b$coefficients[-1]))],na.rm=TRUE)==TRUE) next #must have
#    }
#    if(any(at.most, na.rm=TRUE)==TRUE){
#      if(sum(at.most[c(names(b$coefficients[-1]))],na.rm=TRUE) > m) next #at.most
#    }
    lst <- c(lst,list(b))
    R2 <- c(R2,summary(b)$adj.r.squared)
    R2.DW <- c(R2.DW, (1-(exp(abs(2-dwtest(b)[1]$statistic))-1)/(exp(abs(2))-1)) * summary(b)$adj.r.squared)
    
   
  }
}
close(pb)
if(length(R2)>0) {
names(R2)<- 1:length(R2)
names(R2.DW)<- 1:length(R2.DW)
}

### Nadawianie restrykcji ---------------------------------------------------


#w ponizszym nie ma conditions$positive zamiast positive
# function(conditions = setData(x,y) , m = 1, nv.max = min(T-2, length(colnames(x)),3), alfa = 0.1, j.b = 0.05, bp = 0.05){
# }
# ?list
restrykcjeModele <- function() {
temp <- numeric()
for (i in 1:length(lst)) {
   if(any(c(  !negative[c(names(which(lst[[i]]$coefficients[-1]>=0)))] ,  !positive[c(names(which(lst[[i]]$coefficients[-1]<=0)))],          summary(lst[[i]])[[4]][,4][-1]<alfa)==FALSE,na.rm=TRUE)) next # warunki na znaki oraz istotnos
   if(summary(lst[[i]])$r.squared<R.s) next # r kwadrat adjusted 
   if(jarque.bera.test(residuals(lst[[i]]))[3]$p.value<j.b) next #normalnosc 
   if(bptest(lst[[i]])[4]$p.value<b.p) next # heteroskedastycznosc
   if(any(must.have, na.rm=TRUE)==TRUE){
      if(!any(must.have[c(names(lst[[i]]$coefficients[-1]))],na.rm=TRUE)==TRUE) next #must have
   }
   if(any(at.most, na.rm=TRUE)==TRUE){
      if(sum(at.most[c(names(lst[[i]]$coefficients[-1]))],na.rm=TRUE) > m) next #at.most
   }
   temp <- c(temp, i)
   #print(temp)
}
return(lst[temp])
}

# restrykcjeKtore <- function() {
#    temp <- numeric()
#    for (i in 1:length(lst)) {
#       if(any(c(  !negative[c(names(which(lst[[i]]$coefficients[-1]>=0)))] ,  !positive[c(names(which(lst[[i]]$coefficients[-1]<=0)))],          summary(lst[[i]])[[4]][,4][-1]<alfa)==FALSE,na.rm=TRUE)) next # warunki na znaki oraz istotnos
#       if(summary(lst[[i]])$r.squared<R.s) next # r kwadrat adjusted 
#       if(jarque.bera.test(residuals(lst[[i]]))[3]$p.value<j.b) next #normalnosc 
#       if(bptest(lst[[i]])[4]$p.value<b.p) next # heteroskedastycznosc
#       if(any(must.have, na.rm=TRUE)==TRUE){
#          if(!any(must.have[c(names(lst[[i]]$coefficients[-1]))],na.rm=TRUE)==TRUE) next #must have
#       }
#       if(any(at.most, na.rm=TRUE)==TRUE){
#          if(sum(at.most[c(names(lst[[i]]$coefficients[-1]))],na.rm=TRUE) > m) next #at.most
#       }
#       temp <- c(temp, i)
#       #print(temp)
#    }
#    return(temp)
# }

restrykcjeKtore <- function(lista) {
   temp <- numeric()
   for (i in 1:length(lista)) {
      if(any(c(  !negative[c(names(which(lista[[i]]$coefficients[-1]>=0)))] ,  !positive[c(names(which(lista[[i]]$coefficients[-1]<=0)))],          summary(lista[[i]])[[4]][,4][-1]<alfa)==FALSE,na.rm=TRUE)) next # warunki na znaki oraz istotnos
      if(summary(lista[[i]])$r.squared<R.s) next # r kwadrat adjusted 
      if(jarque.bera.test(residuals(lista[[i]]))[3]$p.value<j.b) next #normalnosc 
      if(bptest(lista[[i]])[4]$p.value<b.p) next # heteroskedastycznosc
      if(any(must.have, na.rm=TRUE)==TRUE){
         if(!any(must.have[c(names(lista[[i]]$coefficients[-1]))],na.rm=TRUE)==TRUE) next #must have
      }
      if(any(at.most, na.rm=TRUE)==TRUE){
         if(sum(at.most[c(names(lista[[i]]$coefficients[-1]))],na.rm=TRUE) > m) next #at.most
      }
      temp <- c(temp, i)
      #print(temp)
   }
   return(temp)
}


my.list     <- lst
my.list.rst <- restrykcjeModele()

# power_big2 <-lst
# power_big2.rst <- restrykcjeModele()
# topModels(16, power_big2.rst)
# dane
# 
# 
# telecom.models <- lst
# telecom.rest <- restrykcjeModele()
# topModels(16, telecom.rest)
# head(telecom_old_consensus)

### Zapisywanie modeli ------------------------------------------------------


###########Zapisywanie list modeli
load(modele.telecom.rest, file = "//STORAGE/Data_Analysis/DATA CENTRE - WORKING AREA/!EKONOMETRYCY/!Projects/Cummins/Scripts/modele.txt")
a <- readRDS(file = "//STORAGE/Data_Analysis/DATA CENTRE - WORKING AREA/!EKONOMETRYCY/!Projects/Cummins/Scripts/modele.txt")
saveRDS(modele.telecom.rest, file = "//STORAGE/Data_Analysis/DATA CENTRE - WORKING AREA/!EKONOMETRYCY/!Projects/Cummins/Scripts/modele.txt")


topModels.nr(30, volume.c)

##########


getModelDesc = function(fit)
{
   # w colnames(fit$model) znajduja sie nazwy zmiennych
   # uzytych do budowy modelu
   # na pierwszej pozycji jest zmienna objasniana
   cname = colnames(fit$model) 
   
   # nazwy zmiennych objasaniacych sa laczone w jednen napisa
   # w miejsce laczenia wstawiany jest " + "
   tmp = paste(cname[-1], collapse = " + ")
   # doklejenie zmiennej objasnianej
   paste(cname[1], " = ", tmp)
}

getModelDesc(volume.c[[1]])
summary(volume.c[[1]])$coefficients
paste0("R.squared: ", round(summary(volume.c[[1]])$adj.r.squared,2),", R.squared with DW:")

attributes(summary(volume.c[[1]]))

############






warnings()
restrykcjeKtore(volume.d)
volume.d[[1]]$model
summary(volume.d[[27]])
[1]   1   2   3   4   5   6   7   8   9  13  15  16  17  27  40  42  53  56  65  69  81 100 102 111 209 231 235 339 340 483 507 509
[33] 575 675

R2 <- c(R2,summary(b)$adj.r.squared)
R2.DW <- c(R2.DW, (1-(exp(abs(2-dwtest(b)[1]$statistic))-1)/(exp(abs(2))-1)) * summary(b)$adj.r.squared)
topModels.nr(9, modele.total.value.rest)


modele.total.value.rest[[1]]
#lista <- c(lista,colnames(klasyfikacja)[k],lst[as.numeric(names(which(sort(R2, decreasing=TRUE)>0.6)))])
#}

### Output ------------------------------------------------------------------
#x najlepszych modeli!
# names(R2.DW) <- 1:length(modele.total.value.rest)
# class(R2.DW)
# 
# 
# topModels <- function(a, lista){
#    par(mfrow=c(ceiling(sqrt(a)),ceiling(sqrt(a))))
#    R2temp <-R2.DW[restrykcjeKtore()]
#    names(R2temp) <- 1:length(lista)
#    for(i in 1:a){
#       print(summary(lista[[as.numeric(names(sort(R2temp, decreasing=TRUE)))[i]]]))
#       plot(predict(lista[[as.numeric(names(sort(R2temp, decreasing=TRUE)))[i]]], dane), col = "red", type = "l", xlab = "", ylab = "")
#       lines(dane[,1], type = "b")
#       
#    }
#    par(mfrow=c(1,1))
# }

########### POTREBNY JEST WARUNEK NA TO CZY SA DOSTACZONE DANE
topModels.nr <- function(a, lista, data = NULL, restrykcje = TRUE){
   par(mfrow=c(ceiling(sqrt(a)),ceiling(sqrt(a))))
   
   R2.DW <- vector()
   for(i in 1: length(lista)){
   R2.DW <- c(R2.DW, (1-(exp(abs(2-dwtest(lista[[i]])[1]$statistic))-1)/(exp(abs(2))-1)) * summary(lista[[i]])$adj.r.squared)
   }
   
   if (restrykcje == TRUE) {
      R2temp <-R2.DW[restrykcjeKtore(lista)]
      names(R2temp) <- restrykcjeKtore(lista)
   }else{
      R2temp <- R2.DW
      names(R2temp) <- 1:length(R2temp)
   }
   
   #names(R2temp) <- 1:length(lista)
   for(i in 1:a){
      print(summary(lista[[as.numeric(names(sort(R2temp, decreasing=TRUE)))[i]]]))
      
   if(is.null(data) == FALSE) {
      plot(predict(lista[[as.numeric(names(sort(R2temp, decreasing=TRUE)))[i]]], dane), col = "red", type = "l", xlab = "", ylab = "")
      lines(dane[,1], type = "b")
   }
   
   }
   par(mfrow=c(1,1))
}

topModels.nr(9, lista = my.list.rst, data = iris[,1:4])

# ####
# topModels.nr(9, price.c, data = NULL)
# R2.DW <- vector()
# for(i in 1: length(price.c)){
#    R2.DW <- c(R2.DW, (1-(exp(abs(2-dwtest(price.c[[i]])[1]$statistic))-1)/(exp(abs(2))-1)) * summary(price.c[[i]])$adj.r.squared)
# }
# R2temp <- R2.DW
# names(R2temp) <- 1:length(price.c)
# for(i in 1:25){
#    print(summary(price.c[[as.numeric(names(sort(R2temp, decreasing=TRUE)))[i]]]))
#   # plot(predict(price.c[[as.numeric(names(sort(R2temp, decreasing=TRUE)))[i]]], dane), col = "red", type = "l", xlab = "", ylab = "")
#   # lines(dane[,1], type = "b")
#    
# }
# par(mfrow=c(1,1))
# }
# 
# temp <- NULL
# 
# is.null(temp)
# 
# 
# 
# topModels <- function(a){
#    par(mfrow=c(ceiling(sqrt(a)),ceiling(sqrt(a))))
#    for(i in 1:a){
#       print(summary(lst[[as.numeric(names(sort(R2.DW, decreasing=TRUE)))[i]]]))
#       plot(predict(lst[[as.numeric(names(sort(R2.DW, decreasing=TRUE)))[i]]], dane), col = "red", type = "l", xlab = "", ylab = "")
#       lines(dane[,1], type = "b")
#       
#    }
#    par(mfrow=c(1,1))
# }
# as.logical("yes")

#example
topModels(9)

#liczba wybranych modeli
length(lst)

### Inny output -------------------------------------------------------------

#lista z wybranymi modelami 
lst[as.numeric(names(which(sort(R2.DW, decreasing=TRUE)>0.55)))]

# r2 adj dla poszczegolnych modeli (nagl?wki oznaczaja numer modelu)
sort(R2, decreasing=TRUE)

#wskaznik syntetyczny Radj z kara za DW
sort(R2.DW, decreasing=TRUE)
as.numeric(names(sort(R2.DW, decreasing=TRUE)))




#Wyprintuj zgadzajace sie modele
for(i in 1:length(lst)){
  print(summary(lst[[i]]))
}

### Metoda holta recznie ------------------------------------------------------------

library(forecast)
holt <- matrix(nrow=7, ncol=21)
holt.normal <- matrix(nrow=7, ncol=21)
holt.damped <- matrix(nrow=7, ncol=21)
?holt
wybrane.kolumny <- c("Leki.Sulfonamidy", "Leki.witaminy", "Kosmetyki.do.wlosow", "Substancje.zapachowe", "Srodki.przeciwstukowe", "Zapobiegajace.zamarzaniu", "Wegiel.aktywny", "Tworzywa.sztuczne")
#which(colnames(klasyfikacja) = wybrane.kolumny)
wybrane.kolumny <- colnames(klasyfikacja)[1:21]

colnames(holt) <- wybrane.kolumny
colnames(holt.normal) <- wybrane.kolumny
colnames(holt.damped) <- wybrane.kolumny
for( i in 1: length(wybrane.kolumny) ) {

fit.hw.normal <- holt(ts(klasyfikacja[1:8,wybrane.kolumny[i]], start = 2005), damped=FALSE, initial="simple", h=7) 
fit.hw.damped <- holt(ts(klasyfikacja[1:8,wybrane.kolumny[i]], start = 2005), damped=TRUE, initial="simple", h=7) 
(as.vector(fit.hw.damped$mean) + as.vector(fit.hw.normal$mean))/2

holt[,wybrane.kolumny[i]] <- (as.vector(fit.hw.damped$mean) + as.vector(fit.hw.normal$mean))/2
holt.normal[,wybrane.kolumny[i]] <- as.vector(fit.hw.normal$mean) 
holt.damped[,wybrane.kolumny[i]] <- as.vector(fit.hw.damped$mean)
}
library(forecast)
holt
holt.normal
holt.damped

ts(c(57604.9027530443, 82834.7948605683, 106002.789798681, 135025.405172045, 95309.3446348063, 114448.245084554, 128391.136191158, 105909.657404092))
fit.hw.normal <- holt(ts(c(57604.9027530443, 82834.7948605683, 106002.789798681, 135025.405172045, 95309.3446348063, 114448.245084554, 128391.136191158, 105909.657404092)), damped=FALSE, initial="simple", h=7) 
fit.hw.damped <- holt(ts(c(57604.9027530443, 82834.7948605683, 106002.789798681, 135025.405172045, 95309.3446348063, 114448.245084554, 128391.136191158, 105909.657404092)), damped=TRUE, initial="simple", h=7) 
(as.vector(fit.hw.damped$mean) + as.vector(fit.hw.normal$mean))/2

"Pasze.dla.zwierzat.hodowlanych"
holt(ts(klasyfikacja[1:8,"Materialy.ciekle_polysk"], start = 2005), damped=FALSE, initial="simple", h=7)$mean

### Soruces -----------------------------------------------------------------


#adjusted r squared
summary(b)$adj.r.squared

#test DW
library(lmtest)
dwtest(b)[1]$statistic

#miara sztuczna R2 adjs + kara za test DW
(1-(exp(abs(2-dwtest(b)[1]$statistic))-1)/(exp(abs(2))-1)) * summary(b)$adj.r.squared

plot(a,scale="adjr2") # ten wykres jest przed wyborem zmiennych
#to do: wykres po wyborze zmiennych
