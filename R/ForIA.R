#'setData function

setData <- function(data,  positive    = rep(NA, length(colnames(dane)[-1])), 
                           negative    = rep(NA, length(colnames(dane)[-1])), 
                           must.have   = rep(NA, length(colnames(dane)[-1])), 
                           at.most     = rep(NA, length(colnames(dane)[-1])))
{
   positive    <- positive==1
   negative    <- negative==1
   must.have   <- must.have == 1
   at.most  <- at.most == 1
   
   names(positive)   <- colnames(dane)[-1]
   names(negative)   <- colnames(dane)[-1]
   names(must.have)  <- colnames(dane)[-1]
   names(at.most)    <- colnames(dane)[-1]
   
   return(list(positive    = positive,
               negative    = negative,
               must.have   = must.have,
               at.most     = at.most))
}

#'setSpec function

setSpec <- function(data,
                    T      = dim(na.omit(data))[1],
                    m      = 1,
                    nv.max = min(T-2, length(colnames(data)[-1]),3),
                    alfa   = 0.1,
                    j.b    = 0.05,
                    b.p    = 0.05,
                    R.s    = 0)
{
   return(list(T        = T,
               m        = m,
               nv.max   = nv.max,
               alfa     = alfa,
               j.b      = j.b,
               b.p      = b.p,
               R.s      = R.s))          
}

#'getModels function
#'Moar

getModels <- function(data, specs = setSpec(data))   
{

lst   <-list()
R2    <- vector()
R2.DW <- vector()

#wlasciwy loop dla wszystkich kombinacji
#licznik dla progress baru
n.max          <- 0
progress.bar   <-0
for   (i in 1:specs$nv.max) {
      for   (s in 1:dim(combn(length(colnames(dane)[-1]),i))[2]){
            n.max <- n.max + 1
   }}


pb <- txtProgressBar(min = 0, max = n.max , style = 3)
for   (i in 1:specs$nv.max) {
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
       
      lst   <- c(lst,list(b))
      R2    <- c(R2,summary(b)$adj.r.squared)
      R2.DW <- c(R2.DW, (1-(exp(abs(2-dwtest(b)[1]$statistic))-1)/(exp(abs(2))-1)) * summary(b)$adj.r.squared)
      
      
   }
}
close(pb)
if(length(R2)>0) {
   names(R2)   <- 1:length(R2)
   names(R2.DW)<- 1:length(R2.DW)
}

return(list(models   = lst,
            R2       = R2,
            R2.DW    = R2.DW,
            data     = data,
            specs    = specs))
}

#'restrykcjeModele function

restrykcjeModele <- function(object, specs = object$specs) {
   temp <- numeric()
   
   for (i in 1:length(object$models)) {
      if(any(c(  !negative[c(names(which(object$models[[i]]$coefficients[-1]>=0)))] ,  !positive[c(names(which(object$models[[i]]$coefficients[-1]<=0)))],          summary(object$models[[i]])[[4]][,4][-1]<alfa)==FALSE,na.rm=TRUE)) next # warunki na znaki oraz istotnos

      if(summary(object$models[[i]])$r.squared<R.s) next # r kwadrat adjusted 
      
      if(jarque.bera.test(residuals(object$models[[i]]))[3]$p.value<j.b) next #normalnosc 
      
      if(bptest(object$models[[i]])[4]$p.value<b.p) next # heteroskedastycznosc
      
      if(any(must.have, na.rm=TRUE)==TRUE){
         if(!any(must.have[c(names(object$models[[i]]$coefficients[-1]))],na.rm=TRUE)==TRUE) next #must have
      }
      
      if(any(at.most, na.rm=TRUE)==TRUE){
         if(sum(at.most[c(names(object$models[[i]]$coefficients[-1]))],na.rm=TRUE) > m) next #at.most
      }
      
      temp <- c(temp, i)
      #print(temp)
   }
   #return(object$models[temp])
   return(list(models   = object$models[temp],
                R2      = object$R2[temp],
                R2.DW   = object$R2.DW[temp],
                data    = object$data,
                specs   = object$specs))
                
}

#'topModels function

topModels <- function(object, a = 9, restrictions = TRUE, ...)
{
   if (restrictions == TRUE) object = restrykcjeModele(object, ...)
   
   par(mfrow=c(ceiling(sqrt(a)),ceiling(sqrt(a))))
   
   names(object$R2.DW) <- 1:length(object$R2.DW)
   
   #names(R2temp) <- 1:length(lista)
   for(i in 1:a){
      print(summary(object$models[[as.numeric(names(sort(object$R2.DW, decreasing = TRUE)))[i]]]))
      
      if(is.null(object$data) == FALSE) {
         plot(predict(object$models[[as.numeric(names(sort(object$R2.DW, decreasing = TRUE)))[i]]], object$data), col = "red", type = "l", xlab = "", ylab = "")
         lines(object$data[,1], type = "b")
      }
      
   }
   par(mfrow=c(1,1))
}

#'saveModels function

saveModels <- function(object, ...) saveRDS(object, ...)

#'loadModels function

loadModels <- function(...) load(...)


