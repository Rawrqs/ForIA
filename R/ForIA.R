#'setData function
#'
#'This function allows to set restrinctions on signs and number of variables included in the space of restricted models
#'This function is used by the getModels function and then transfered to some other funcions such as topModels
#'@param data the data, used for colnames and calculating number of variables
#'@param positive represents vector of variables with positive signs
#'@param negative represents vector of variables with negative signs
#'@param must.have represents vector of variables that must be included at least one
#'@param at.most limits the amount of variables to m (option in setSpec function) in a model
#'@export


setData <- function(data,  positive    = rep(NA, length(colnames(data)[-1])), 
                           negative    = rep(NA, length(colnames(data)[-1])), 
                           must.have   = rep(NA, length(colnames(data)[-1])), 
                           at.most     = rep(NA, length(colnames(data)[-1])))
{
   positive    <- positive==1
   negative    <- negative==1
   must.have   <- must.have == 1
   at.most     <- at.most == 1
   
   names(positive)   <- colnames(data)[-1]
   names(negative)   <- colnames(data)[-1]
   names(must.have)  <- colnames(data)[-1]
   names(at.most)    <- colnames(data)[-1]
   
   return(list(positive    = positive,
               negative    = negative,
               must.have   = must.have,
               at.most     = at.most))
}

#'setSpec function
#'This function allows to set restrictions on some models characteristics when calling the restrictModels function
#'@param data the data, used to set limit on the maximum amount of variables
#'@param T the length of the data, should not be changed by user if not necessary
#'@param m refers to the at.most parameter from setData function
#'@param nv.max the maximum number of variables in a model, (default = min(T-2, length(colnames(data)[-1]),3)) due to computation time
#'@param alfa significance level of the t-test 
#'@param j.b p value for jb statistics
#'@param b.p p value for b.p statistics
#'@param R.s minimum accepted r squared adjusted
#'@export

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

#' getModels function
#'
#' This is the main function to use in the ForIA package
#' The input is a data.frame, where the first column is the dependent variable. This function uses 2 optional functions as arguments
#' specs - which specifies all restrictions on the model selection
#' signs - which restricts the model space by restrictions on parameters
#' @export
#' @param data test
#' @param specs test
#' @param signs test
#' @examples
#' getModels(cars)

getModels <- function(data, specs = setSpec(data), signs = setData(data))   
{

lst   <-list()
R2    <- vector()
R2.DW <- vector()

#wlasciwy loop dla wszystkich kombinacji
#licznik dla progress baru
n.max          <- 0
progress.bar   <-0
for   (i in 1:specs$nv.max) {
      for   (s in 1:dim(combn(length(colnames(data)[-1]),i))[2]){
            n.max <- n.max + 1
   }}


pb <- txtProgressBar(min = 0, max = n.max , style = 3)
for   (i in 1:specs$nv.max) {
      for (s in 1:dim(combn(length(colnames(data)[-1]),i))[2]){
      #print(combn(length(colnames(data)[-1]),i)[,s])
      
      progress.bar <- progress.bar + 1
      Sys.sleep(0.1)
      # update progress bar
      setTxtProgressBar(pb, progress.bar)  
      
      kolumny<-combn(length(colnames(data)[-1]),i)[,s]
      #if (dim(data[,c(1,kolumny+1)])[1] < 4) next
      
      b <-lm(data[,c(1,kolumny+1)])
      #fit b <- glm(data[,c(1,kolumny+1)],family=gaussian())
       
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
            specs    = specs,
            signs    = signs))
}

#'restrictModels function
#'
#'This function is used by the topModels functions when restrictions = TRUE. It sets the restrictions which vere specified in setData and setSpec functions
#'@param object the object from getModels function
#'@param specs specs from the object, may be replaced by setSpecs output
#'@param signs signs from the object, may be replaced by setSigns output
#' @import lmtest
#' @import tseries
#' @export

restrictModels <- function(object, specs = object$specs, signs = object$signs) {
   temp <- numeric()
   
   for (i in 1:length(object$models)) {
      if(any(c(  !signs$negative[c(names(which(object$models[[i]]$coefficients[-1]>=0)))] ,  !signs$positive[c(names(which(object$models[[i]]$coefficients[-1]<=0)))],          summary(object$models[[i]])[[4]][,4][-1]<specs$alfa)==FALSE,na.rm=TRUE)) next # warunki na znaki oraz istotnos

      if(summary(object$models[[i]])$r.squared<specs$R.s) next # r kwadrat adjusted 
      
      if(jarque.bera.test(residuals(object$models[[i]]))[3]$p.value<specs$j.b) next #normalnosc 
      
      if(bptest(object$models[[i]])[4]$p.value<specs$b.p) next # heteroskedastycznosc
      
      if(any(signs$must.have, na.rm=TRUE)==TRUE){
         if(!any(signs$must.have[c(names(object$models[[i]]$coefficients[-1]))],na.rm=TRUE)==TRUE) next #must have
      }
      
      if(any(signs$at.most, na.rm=TRUE)==TRUE){
         if(sum(signs$at.most[c(names(object$models[[i]]$coefficients[-1]))],na.rm=TRUE) > specs$m) next #at.most
      }
      
      temp <- c(temp, i)
      #print(temp)
   }
   #return(object$models[temp])
   return(list(models   = object$models[temp],
                R2      = object$R2[temp],
                R2.DW   = object$R2.DW[temp],
                data    = object$data,
                specs   = object$specs,
                signs   = object$signs))
                
}

#'topModels function
#'
#'Returns "a" best models. When restrictions is set TRUE, a restrictModels function is used to set restrictions on the model importance.
#'@param object an object from the getModels function
#'@param a the number of best models to show based on R2.DW statistic
#'@param restrictions boolean specifying if restrictions from restrictModels should be applied
#'@export

topModels <- function(object, a = 9, restrictions = TRUE, ...)
{
   if (restrictions == TRUE) object = restrictModels(object, ...)
   a <- min(a, length(object$models))
   
   par(mfrow=c(ceiling(sqrt(a)),ceiling(sqrt(a))))
   
   names(object$R2.DW) <- 1:length(object$R2.DW)
   
   #names(R2temp) <- 1:length(lista)
   for(i in 1:a){
      #print(summary(object$models[[as.numeric(names(sort(object$R2.DW, decreasing = TRUE)))[i]]]))
      
      cat(cat(colnames(object$data)[as.numeric(names(sort(object$R2.DW, decreasing = TRUE)))[i]], " = "), cat(names(object$models[[as.numeric(names(sort(object$R2.DW, decreasing = TRUE)))[i]]]$coefficients), sep = " + "))
      cat("\n")
      print(summary(object$models[[as.numeric(names(sort(object$R2.DW, decreasing = TRUE)))[i]]])[[4]])
      cat("\n")
      cat(paste0("R2: ", round(object$R2[[as.numeric(names(sort(object$R2.DW, decreasing = TRUE)))[i]]],2), ", R2 DW: ", round(object$R2.DW[[as.numeric(names(sort(object$R2.DW, decreasing = TRUE)))[i]]],2)))
      cat("\n")
      cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
      cat("\n")
      
      if(is.null(object$data) == FALSE) {
         plot(predict(object$models[[as.numeric(names(sort(object$R2.DW, decreasing = TRUE)))[i]]], object$data), col = "red", type = "l", xlab = "", ylab = "")
         lines(object$data[,1], type = "b")
      }
      
   }
   par(mfrow=c(1,1))
}

#'loadModels function
#'loading list of models models from a text file
#'@export

loadModels <- function(...) load(...)

#'saveModels function
#'saving list of models to a text file
#'@param object object from getModels function
#'@export

saveModels <- function(object, ...) save(object, ...)

#'filterGap function
#'adds hp filter gap calculated on logs
#'@import mFilter
#'@export

filterGap  <- function(df, column, lambda = 6.25) {
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
   
   return(data)
}

#'filterTrend function
#'adds trend line from hp filter
#'@import mFilter
#'@export

filterTrend <- function(df, column, lambda = 6.25) {
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

#'addLogs function
#'adds Logs to the data.frame
#'@export

addLogs <- function(df, column) {
   data <- cbind(df, log(df[,column]))
   if (is.numeric(column) == TRUE) 
   {
      colnames(data) <- c(colnames(df), paste("ln", colnames(df)[column], sep = "_")) 
   } else {
      colnames(data) <- c(colnames(df), paste("ln", column, sep = "_")) 
   }
   data
}

#'addDiffs function
#'adds diffs to the data.frame
#'@export

addDiffs <- function(df, column) {
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

