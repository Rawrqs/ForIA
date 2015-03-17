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
setData <- function(data,  cutCpls = 1,
                           positive    = rep(NA, length(colnames(data)[-cutCpls])), 
                           negative    = rep(NA, length(colnames(data)[-cutCpls])), 
                           must.have   = rep(NA, length(colnames(data)[-cutCpls])), 
                           at.most     = rep(NA, length(colnames(data)[-cutCpls])))
{
   positive    <- positive==1
   negative    <- negative==1
   must.have   <- must.have == 1
   at.most     <- at.most == 1
   
   names(positive)   <- colnames(data)[-cutCpls]
   names(negative)   <- colnames(data)[-cutCpls]
   names(must.have)  <- colnames(data)[-cutCpls]
   names(at.most)    <- colnames(data)[-cutCpls]
   
   return(list(positive    = positive,
               negative    = negative,
               must.have   = must.have,
               at.most     = at.most))
}

#'setData3 function
#'
#'This function allows to set restrinctions on signs and number of variables included in the space of restricted models
#'This function is used by the getModels function and then transfered to some other funcions such as topModels
#'@param data the data, used for colnames and calculating number of variables
#'@param positive represents vector of variables with positive signs
#'@param negative represents vector of variables with negative signs
#'@param must.have represents vector of variables that must be included at least one
#'@param at.most limits the amount of variables to m (option in setSpec function) in a model
#'@export
setData2 <- function(data,  cutCols = 1,
                    positive    = NA, 
                    negative    = NA, 
                    must.have   = NA, 
                    at.most     = NA)
   {
   class(NA) == "logical"
   class(1)
   class("asd")
 
   #if(class(positive) == "logical") 
   positive.t <- negative.t <- must.have.t <- at.most.t <- rep(NA, dim(data)[2] - cutCols)
   
   if(class(positive) == "numeric"){
      
   positive.t[positive] = TRUE   
   negative.t[negative] = TRUE 
   must.have.t[must.have] = TRUE 
   at.most.t[at.most] = TRUE  
   }
   
   if(class(positive) == "character") {
   positive.t[which(colnames(data)[-c(1:cutCols)] == positive)] = TRUE
   negative.t[which(colnames(data)[-c(1:cutCols)] == negative)] = TRUE
   must.have.t[which(colnames(data)[-c(1:cutCols)] == must.have)] = TRUE
   at.most.t[which(colnames(data)[-c(1:cutCols)] == at.most)] = TRUE
      
   }
   
   temp <- c("a","a","b")
   which(temp) == "a"
   
   positive    <- positive.t
   negative    <- negative.t
   must.have   <- must.have.t
   at.most     <- at.most.t
   
   names(positive)   <- colnames(data)[-1:cutCpls]
   names(negative)   <- colnames(data)[-1:cutCpls]
   names(must.have)  <- colnames(data)[-1:cutCpls]
   names(at.most)    <- colnames(data)[-1:cutCpls]
   
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
#' @import plm
#' @param data test
#' @param specs test
#' @param signs test
#' @examples
#' getModels(cars)
getModels <- function(data, type = "lm", specs = setSpec(data),
                      signs = if(type == "plm") setData(data[,-c(1,2)]) else setData(data),
#                      signs = setData(data),
                      ...)   
{

lst   <-list()
R2    <- vector()
R2.DW <- vector()

#wlasciwy loop dla wszystkich kombinacji
#licznik dla progress baru
if (type == "lm") {
   
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
   
      
      kolumny <- combn(length(colnames(data)[-1]),i)[,s]
      b <-lm(data[,c(1,kolumny+1)], ...)
         
      lst   <- c(lst,list(b))
      R2    <- c(R2,summary(b)$adj.r.squared)
      R2.DW <- c(R2.DW, (1-(exp(abs(2-dwtest(b)[1]$statistic))-1)/(exp(abs(2))-1)) * summary(b)$adj.r.squared)        
      }

}}
      
if (type == "plm") {
   n.max          <- 0
   progress.bar   <-0
   for   (i in 1:specs$nv.max) {
      for   (s in 1:dim(combn(length(colnames(data)[-c(1:3)]),i))[2]){
         n.max <- n.max + 1
      }}
   
   
pb <- txtProgressBar(min = 0, max = n.max , style = 3)
for   (i in 1:specs$nv.max) {
         for (s in 1:dim(combn(length(colnames(data)[-c(1:3)]),i))[2]){
         #print(combn(length(colnames(data)[-1]),i)[,s])
         
         progress.bar <- progress.bar + 1
         Sys.sleep(0.1)
         # update progress bar
         setTxtProgressBar(pb, progress.bar) 
         
         kolumny <- combn(length(colnames(data)[-c(1:3)]),i)[,s]
         model = paste0(colnames(data)[3],"~",paste(colnames(data[,-c(1:3)][,kolumny, drop = FALSE]),collapse="+"))
         b     = plm(formula(model),data = data, ...)  
         
         lst   <- c(lst,list(b))
         R2    <- c(R2,summary(b)$r.squared[2])
         R2.DW <- c(R2.DW, (1-(exp(abs(2-pdwtest(b)[[1]]))-1)/(exp(abs(2))-1)) * summary(b)$r.squared[2])
         }
   
}}

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
            signs    = signs,
            type     = type,
            modelID  = 1:length(R2)))
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
restrictModels <- function(object, specs = object$specs, signs = object$signs, type = object$type) {
   temp <- numeric()
   

if (type == "lm") {
   for (i in 1:length(object$models)) {
      if(any(c(  !signs$negative[c(names(which(object$models[[i]]$coefficients[-1]>=0)))] ,  !signs$positive[c(names(which(object$models[[i]]$coefficients[-1]<=0)))],          summary(object$models[[i]])[[4]][,4][-1]<specs$alfa)==FALSE,na.rm=TRUE)) next # warunki na znaki oraz istotnos

      if(summary(object$models[[i]])$adj.r.squared<specs$R.s) next # r kwadrat adjusted 
      
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
}
if (type == "plm"){
   for (i in 1:length(object$models)) {   
      if(any(c(  !signs$negative[c(names(which(object$models[[i]]$coefficients>=0)))] ,  !signs$positive[c(names(which(object$models[[i]]$coefficients<=0)))],          summary(object$models[[i]])[[1]][,4]<specs$alfa)==FALSE,na.rm=TRUE)) next # warunki na znaki oraz istotnos   
      
      if(summary(object$models[[i]])$r.squared[2]<specs$R.s) next # r kwadrat adjusted 
      
      
      if(any(signs$must.have, na.rm=TRUE)==TRUE){
         if(!any(signs$must.have[c(names(object$models[[i]]$coefficients))],na.rm=TRUE)==TRUE) next #must have
      }
      
      if(any(signs$at.most, na.rm=TRUE)==TRUE){
         if(sum(signs$at.most[c(names(object$models[[i]]$coefficients))],na.rm=TRUE) > specs$m) next #at.most
      }
      #UWAGA NIE MA ZAIMPLEMENTOWANEGO TESTU NA NORMALNOSC ORAZ HETEROSKEDASTYCZNOSC
      temp <- c(temp, i)
      #print(temp)
   }
}
   #return(object$models[temp])
   return(list(models   = object$models[temp],
                R2      = object$R2[temp],
                R2.DW   = object$R2.DW[temp],
                data    = object$data,
                specs   = object$specs, # jak nizej
                signs   = object$signs, #mo??e powinny byc brane od usera
                type    = object$type,
                modelID = object$modelID))
                
}

#'restrictModels2 function
#'
#'This function is used by the topModels functions when restrictions = TRUE. It sets the restrictions which vere specified in setData and setSpec functions
#'@param object the object from getModels function
#'@param specs specs from the object, may be replaced by setSpecs output
#'@param signs signs from the object, may be replaced by setSigns output
#' @import lmtest
#' @import tseries
#' @export
restrictModels2 <- function(object, specs = object$specs, signs = object$signs, type = object$type) {
   temp <- list()

   if (type == "lm") {
      for (i in 1:length(object$models)) {
         if(any(c(  !signs$negative[c(names(which(object$models[[i]]$coefficients[-1]>=0)))] ,  !signs$positive[c(names(which(object$models[[i]]$coefficients[-1]<=0)))],          summary(object$models[[i]])[[4]][,4][-1]<specs$alfa)==FALSE,na.rm=TRUE)) temp$parameters <- append(temp$parameters, i) # warunki na znaki oraz istotnos
         
         if(summary(object$models[[i]])$adj.r.squared<specs$R.s) temp$R2 <- append(temp$R2, i) # r kwadrat adjusted 
         
         if(jarque.bera.test(residuals(object$models[[i]]))[3]$p.value<specs$j.b) temp$j.b <- append(temp$j.b, i) #normalnosc 
         
         if(bptest(object$models[[i]])[4]$p.value<specs$b.p) temp$b.p <- append(temp$b.p, i) # heteroskedastycznosc
         
         if(any(signs$must.have, na.rm=TRUE)==TRUE){
            if(!any(signs$must.have[c(names(object$models[[i]]$coefficients[-1]))],na.rm=TRUE)==TRUE) temp$must.have <- append(temp$must.have, i) #must have
         }
         
         if(any(signs$at.most, na.rm=TRUE)==TRUE){
            if(sum(signs$at.most[c(names(object$models[[i]]$coefficients[-1]))],na.rm=TRUE) > specs$m) temp$at.most <- append(temp$at.most, i) #at.most
         }
         
         #temp <- c(temp, i)
         #print(temp)
      }
   }
#    if (type == "plm"){
#       for (i in 1:length(object$models)) {   
#          if(any(c(  !signs$negative[c(names(which(object$models[[i]]$coefficients>=0)))] ,  !signs$positive[c(names(which(object$models[[i]]$coefficients<=0)))],          summary(object$models[[i]])[[1]][,4]<specs$alfa)==FALSE,na.rm=TRUE)) next # warunki na znaki oraz istotnos   
#          
#          if(summary(object$models[[i]])$r.squared[2]<specs$R.s) next # r kwadrat adjusted 
#          
#          
#          if(any(signs$must.have, na.rm=TRUE)==TRUE){
#             if(!any(signs$must.have[c(names(object$models[[i]]$coefficients))],na.rm=TRUE)==TRUE) next #must have
#          }
#          
#          if(any(signs$at.most, na.rm=TRUE)==TRUE){
#             if(sum(signs$at.most[c(names(object$models[[i]]$coefficients))],na.rm=TRUE) > specs$m) next #at.most
#          }
#          #UWAGA NIE MA ZAIMPLEMENTOWANEGO TESTU NA NORMALNOSC ORAZ HETEROSKEDASTYCZNOSC
#          temp <- c(temp, i)
#          #print(temp)
#       }
#    }
   #return(object$models[temp])
   return(list(models      = temp$parameters,
               R2          = temp$R2,
               j.b         = temp$j.b,
               b.p         = temp$b.p,
               must.have   = temp$must.have, # jak nizej
               at.most     = temp$at.most))
   #mozna zrobic jakies summary na podstawie tego
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
   
   if (object$type == "plm") {
      for(i in 1:a){
      cat(cat(colnames(object$data)[1], " = "), cat(names(object$models[[as.numeric(names(sort(object$R2.DW, decreasing = TRUE)))[i]]]$coefficients), sep = " + "))      
      cat("\n")      
      print(summary(object$models[[as.numeric(names(sort(object$R2.DW, decreasing = TRUE)))[i]]])[[1]])
      cat("\n")
      cat(fixef(object$models[[as.numeric(names(sort(object$R2.DW, decreasing = TRUE)))[i]]]))
      cat("\n")
      cat(paste0("R2 adjusted: ", round(object$R2[[as.numeric(names(sort(object$R2.DW, decreasing = TRUE)))[i]]],2), ", R2 with DW correction: ", round(object$R2.DW[[as.numeric(names(sort(object$R2.DW, decreasing = TRUE)))[i]]],2)))
      cat("\n")
      cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
      cat("\n")
      }
   }
   ######################### dodac do pritnowania modelID ##################
   if (object$type == "lm") { 
   for(i in 1:a){
      #print(summary(object$models[[as.numeric(names(sort(object$R2.DW, decreasing = TRUE)))[i]]]))
      
      cat(cat(colnames(object$data)[1], " = "), cat(names(object$models[[as.numeric(names(sort(object$R2.DW, decreasing = TRUE)))[i]]]$coefficients), sep = " + "))
      cat("\n")
      print(summary(object$models[[as.numeric(names(sort(object$R2.DW, decreasing = TRUE)))[i]]])[[4]])
      cat("\n")
      cat(paste0("R2 adjusted: ", round(object$R2[[as.numeric(names(sort(object$R2.DW, decreasing = TRUE)))[i]]],2), ", R2 with DW correction: ", round(object$R2.DW[[as.numeric(names(sort(object$R2.DW, decreasing = TRUE)))[i]]],2)))
      cat("\n")
      cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
      cat("\n")
      
      par(mar=c(1,1,1,1))
          
      if (is.null(object$data) == FALSE) {
             plot(predict(object$models[[as.numeric(names(sort(object$R2.DW, decreasing = TRUE)))[i]]], object$data), col = "red", type = "l", xlab = "", ylab = "")
             lines(object$data[,1], type = "b")
          }
      par(mar = c(5.1, 4.1, 4.1, 2.1))
      
   }
   par(mfrow=c(1,1))
   
   }
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
   if(class(df)[1] == "mts") temp.start = start(df)[1]
   data <- as.data.frame(df)   
   for( i in 1:length(column) ) {
      temp <- vector()
      if(any(is.na(data[,column[i]])) == TRUE) {
         temp[as.vector(attributes(na.omit(data[,column[i]]))$na.action)] <- NA
         temp[-as.vector(attributes(na.omit(data[,column[i]]))$na.action)] <-hpfilter(log(na.omit(data[,column[i]])), freq = lambda, type = "lambda")$cycle
      }else {
      temp <- hpfilter(log(na.omit(data[,column[i]])), freq = lambda, type = "lambda")$cycle
      }
      data <- cbind(data, temp)
   }
   
   if (is.numeric(column) == TRUE) 
   {
      colnames(data) <- c(colnames(df), paste("hp", colnames(df)[column], sep = "_")) 
   } else {
      colnames(data) <- c(colnames(df), paste("hp", column, sep = "_")) 
   }
   if(class(df)[1] == "mts") data = ts(data, start = temp.start)
   return(data)
}

#'filterTrend function
#'adds trend line from hp filter
#'@import mFilter
#'@export
filterTrend <- function(df, column, lambda = 6.25) {
   if(class(df)[1] == "mts") temp.start = start(df)[1]
   data <- as.data.frame(df)   
   for( i in 1:length(column) ) {
      temp <- vector()
      if(any(is.na(data[,column[i]])) == TRUE) {
         temp[as.vector(attributes(na.omit(data[,column[i]]))$na.action)] <- NA
         temp[-as.vector(attributes(na.omit(data[,column[i]]))$na.action)] <-exp(hpfilter(log(na.omit(data[,column[i]])), freq = lambda, type = "lambda")$trend)
      }else {
         temp <- exp(hpfilter(log(na.omit(data[,column[i]])), freq = lambda, type = "lambda")$trend)
      }
      data <- cbind(data, temp)
   }
   
   if (is.numeric(column) == TRUE) 
   {
      colnames(data) <- c(colnames(df), paste("hpt", colnames(df)[column], sep = "_")) 
   } else {
      colnames(data) <- c(colnames(df), paste("hpt", column, sep = "_")) 
   }
   if(class(df)[1] == "mts") data = ts(data, start = temp.start)
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
   if(class(df)[1] == "mts") temp.start = start(df)[1]
   data <- as.data.frame(df)  
   for( i in 1:length(column) ) {
      data <- cbind(data, c(NA, diff(data[,column[i]])))
   }
   
   if (is.numeric(column) == TRUE) 
   {
      colnames(data) <- c(colnames(df), paste("d", colnames(df)[column], sep = "_")) 
   } else {
      colnames(data) <- c(colnames(df), paste("d", column, sep = "_")) 
   }
   if(class(df)[1] == "mts") data = ts(data, start = temp.start)
   data
}

#'saveExcel function
#'save excel accodring to Anrzej's format
#'@export
#'@import xlsx
saveExcel <- function(object, elements, ...)   write.xlsx( t(sapply(my.model$models[elements], function(x) predict(x, my.model$data))), ... )




#'best_models function
#'
#'Returns index of best models. 
#'@param object an object from the getModels function
#'@param a the number of best models to show based on R2.DW statistic
#'@param method a criteria used to determine the best model
#'@export
best_models <- function(object, specs = object$specs, signs = object$signs, type = object$type, method = "R2.DW") {
      temp <- numeric()
      
      
      if (type == "lm") {
         for (i in 1:length(object$models)) {
            if(any(c(  !signs$negative[c(names(which(object$models[[i]]$coefficients[-1]>=0)))] ,  !signs$positive[c(names(which(object$models[[i]]$coefficients[-1]<=0)))],          summary(object$models[[i]])[[4]][,4][-1]<specs$alfa)==FALSE,na.rm=TRUE)) next # warunki na znaki oraz istotnos
            
            if(summary(object$models[[i]])$adj.r.squared<specs$R.s) next # r kwadrat adjusted 
            
            if(jarque.bera.test(residuals(object$models[[i]]))[3]$p.value<specs$j.b) next #normalnosc 
            
            if(bptest(object$models[[i]])[4]$p.value<specs$b.p) next # heteroskedastycznosc
            
            if(any(signs$must.have, na.rm=TRUE)==TRUE){
               if(!any(signs$must.have[c(names(object$models[[i]]$coefficients[-1]))],na.rm=TRUE)==TRUE) next #must have
            }
            
            if(any(signs$at.most, na.rm=TRUE)==TRUE){
               if(sum(signs$at.most[c(names(object$models[[i]]$coefficients[-1]))],na.rm=TRUE) > specs$m) next #at.most
            }
            
            temp <- c(temp, i)
            
         }
         if(method == "R2.DW") return( as.numeric(names(sort(object$R2.DW[temp], decreasing = TRUE))) )
         if(method == "R2"   ) return( as.numeric(names(sort(object$R2[temp],    decreasing = TRUE))) )

      }}

#'ggplotRegression
#'
#'returns wrapped plot of y vs fitted values 
#'@param object an object from the getModels function
#'@param selection an index of the models to plot; works best with best_models function
#'@param method a criteria used to determine the best model
#'@export
ggplotRegression <- function(object, selection = 1:length(object$models)) 
{
   get_seq <- function() {
      temp2 <- vector()
      for (i in 1:length(object$models[selection])) temp2 <- append(temp2, 
                                                                    rep(selection[i], length(object$data[, 1])))
      return(temp2)
   }
   part1 = data.frame(values = rep(object$data[, 1], length(object$models[selection])), 
                      type = "y", time = 1:length(object$data[, 1]), model = get_seq())
   part2 = data.frame(values = unlist(lapply(object$models[selection], 
                                             function(x) predict(x, object$data))), type = "fit", time = 1:length(object$data[, 
                                                                                                                              1]), model = get_seq())
   df <- rbind(part1, part2)
   df$model <- ordered(df$model, levels = selection)
   ggplot(df, aes(y = values, x = time, color = type)) + geom_line() + 
      facet_wrap(~model) + 
      ggtitle(colnames(object$data)[1])
}



#'transformation function
#'
#'n- nominal values, c- changes, i- chained index, r- relative index, p- percetange pointes or relative value
#'@param x a ts object
#'@export
ntoc <- function(x) {
   temp_change <- x
   for( i in 2:length(x) ) temp_change[i] <- x[i] / x[i-1] - 1 
   temp_change[1] = NA
   return(temp_change)
}
#'transformation function
#'
#'n- nominal values, c- changes, i- chained index, r- relative index, p- percetange pointes or relative value
#'@param x a ts object
#'@export
ntor <- function(x) {
   (ntoc(x) + 1) * 100
}
#'transformation function
#'
#'n- nominal values, c- changes, i- chained index, r- relative index, p- percetange pointes or relative value
#'@param x a ts object
#'@param base
#'@export
ntoi <- function(x, base = 2010) {
   temp_constant <- x
   temp_constant[index(temp_constant) == base] <- 100
   for( i in length(x):1 ) if( i < which((index(temp_constant) == base) == TRUE)) temp_constant[i] = temp_constant[i+1] / (x[i+1] / x[i]  )
   for( i in 1:length(x) ) if( i > which((index(temp_constant) == base) == TRUE)) temp_constant[i] = temp_constant[i-1] * (x[i  ] / x[i-1])
   return(temp_constant)
}
#'transformation function
#'
#'n- nominal values, c- changes, i- chained index, r- relative index, p- percetange pointes or relative value
#'@param x a ts object
#'@param aggregate
#'@export
ntop <- function(x, aggregate = 100) {
   return(x / aggregate)  
}
#'transformation function
#'
#'n- nominal values, c- changes, i- chained index, r- relative index, p- percetange pointes or relative value
#'@param x a ts object
#'@param base
#'@export
cton <- function(x, base = 100) {
   base.index <- start(na.omit(x))[1] - 1
   if(base.index < start(x)[1]) base.index <- start(x)[1]
   
   x[index(x) == base.index[1]] <- base
   
   for( i in 1:length(x) ) if( i > which(x == base)) x[i] <- x[i-1] * (x[i] + 1)
   return(x)
}
#'transformation function
#'
#'n- nominal values, c- changes, i- chained index, r- relative index, p- percetange pointes or relative value
#'@param x a ts object
#'@export
ctor <- function(x) {
   ((x) + 1) * 100
}
#'transformation function
#'
#'n- nominal values, c- changes, i- chained index, r- relative index, p- percetange pointes or relative value
#'@param x a ts object
#'@param base
#'@export
ctoi <- function(x, base = 2010) {
   ntoi(cton(x), base = base)
}
#'transformation function
#'
#'n- nominal values, c- changes, i- chained index, r- relative index, p- percetange pointes or relative value
#'@param x a ts object
#'@param aggregate
#'@export
ctop <- function(x, aggregate = 100) {
   cton(ntop, aggregate = 100)
}
#'transformation function
#'
#'n- nominal values, c- changes, i- chained index, r- relative index, p- percetange pointes or relative value
#'@param x a ts object
#'@param base
#'@export
rton <- function(x, base = 100) {
   rtoc(x) %>% cton(base = base)  
}
#'transformation function
#'
#'n- nominal values, c- changes, i- chained index, r- relative index, p- percetange pointes or relative value
#'@param x a ts object
#'@export
rtoc <- function(x) {
   x/100 - 1
}
#'transformation function
#'
#'n- nominal values, c- changes, i- chained index, r- relative index, p- percetange pointes or relative value
#'@param x a ts object
#'@param base
#'@export
rtoi <- function(x, base = 100) {
   rtoc(x) %>% ctoi(base = base)
}
#'transformation function
#'
#'n- nominal values, c- changes, i- chained index, r- relative index, p- percetange pointes or relative value
#'@param x a ts object
#'@param aggregate
#'@export
rtop <- function(x, aggregate = 100, ...) {
   rton(x, ...) %>% ntop(aggregate = aggregate)
}
#'transformation function
#'
#'n- nominal values, c- changes, i- chained index, r- relative index, p- percetange pointes or relative value
#'@param x a ts object
#'@param base
#'@export
iton <- function(x, base = 100) {
   base.index <- start(na.omit(x))[1]
   
   x <- x * base / x[index(x) == base.index[1]]
   
   return(x)
}
#'transformation function
#'
#'n- nominal values, c- changes, i- chained index, r- relative index, p- percetange pointes or relative value
#'@param x a ts object
#'@export
itoc <- function(x) {
   ntoc(x)
}
#'transformation function
#'
#'n- nominal values, c- changes, i- chained index, r- relative index, p- percetange pointes or relative value
#'@param x a ts object
#'@export
itor <- function(x) {
   ntor(x)
}
#'transformation function
#'
#'n- nominal values, c- changes, i- chained index, r- relative index, p- percetange pointes or relative value
#'@param x a ts object
#'@param aggregate
#'@export
itop <- function(x, aggregate = 100) {
   iton(x, ...) %>% ntop(aggregate = aggregate)
}
#'transformation function
#'
#'n- nominal values, c- changes, i- chained index, r- relative index, p- percetange pointes or relative value
#'@param x a ts object
#'@param aggregate
#'@export
pton <- function(x, aggregate = 100) {
   return(x * aggregate)  
}
#'transformation function
#'
#'n- nominal values, c- changes, i- chained index, r- relative index, p- percetange pointes or relative value
#'@param x a ts object
#'@param aggregate
#'@export
ptoc <- function(x, aggregate = 100) {
   pton(x, aggregate = aggregate) %>% ntoc()  
}
#'transformation function
#'
#'n- nominal values, c- changes, i- chained index, r- relative index, p- percetange pointes or relative value
#'@param x a ts object
#'@param aggregate
#'@export
ptor <- function(x, aggregate = 100, ...) {
   pton(x, aggregate = aggregate) %>% ntoc(...) 
}
#'transformation function
#'
#'n- nominal values, c- changes, i- chained index, r- relative index, p- percetange pointes or relative value
#'@param x a ts object
#'@param aggregate
#'@export
ptoi <- function(x, aggregate = 100, ...) {
   pton(x, aggregate = aggregate) %>% ntoi(...) 
}


#'general transformation expander function
#'
#'This function allows to use transformation function on several columns of mts or data.frame object
#'@param df a data.frame object
#'@param column a character or numeric vector 
#'@param FUN a transformation function to be used
#'@export
#'@import magrittr
addT <- function (df, column, FUN, name = deparse(substitute(FUN)), ...) {
   data <- df
   

      
   for (i in column) {
      if (is.numeric(column) == TRUE)   if(length(grep(paste0("_", substr(name, 1,1)), column[i])) == 0) stop(paste0("variable is not of a type: ", substr(name, 1,1), " or has a wrong name")) 
      if (is.character(column) == TRUE) if(length(grep(paste0("_", substr(name, 1,1)), colnames(df)[i])) == 0) stop(paste0("variable is not of a type: ", substr(name, 1,1), " or has a wrong name")) 
      
      

      
      data <- merge_ts(list(data, FUN(data[, i, drop = FALSE], ...)))
   }
   if (is.numeric(column) == TRUE) {
      colnames(data) <- c(colnames(df), 
                          gsub(paste0("_", substr(name, 1,1)), paste0("_", substr(name, 4,4)), colnames(df)[column]) )
   }
   else {
      colnames(data) <- c(colnames(df), gsub(paste0("_", substr(name, 1,1)), paste0("_", substr(name, 4,4)), column))
   }
   data
}

#'opis do zmiany
#'
#'This function allows to use transformation function on several columns of mts or data.frame object
#'@param df a data.frame object
#'@param column a character or numeric vector 
#'@param FUN a transformation function to be used
#'@export
#'@import magrittr
addI <- function (df, column, FUN, name = deparse(substitute(FUN)), ...) {
   data <- df
   if(length(grep("(i|c)to(n|q|u|r|p)", name)) == 1){
      for (i in column) {
        
         data <- merge_ts(list(data, FUN(data[, i, drop = FALSE], ...)))
      }
      if (is.numeric(column) == TRUE) {
         colnames(data) <- c(colnames(df), 
                             gsub(paste0(substr(name, 1,1), "_"), "", colnames(df)[column]) )
      }
      else {
         colnames(data) <- c(colnames(df), gsub(paste0(substr(name, 1,1), "_"), "", column))
      }
   } 
   if(length(grep("(n|q|u|r|p)to(i|c)", name)) == 1){  
      for (i in column) {
         
         data <- merge_ts(list(data, FUN(data[, i, drop = FALSE], ...)))
      }
      if (is.numeric(column) == TRUE) {
         colnames(data) <- c(colnames(df), 
                             paste0(substr(name, 4,4), "_", colnames(df)[column]) )
      }
      else {
         colnames(data) <- c(colnames(df), paste0(substr(name, 4,4), "_", column))
      } 
   }
   if(length(grep("(i|c)to(i|c)", name)) == 1){
      for (i in column) {
         
         data <- merge_ts(list(data, FUN(data[, i, drop = FALSE], ...)))
      }
      if (is.numeric(column) == TRUE) {
         colnames(data) <- c(colnames(df), 
                             gsub(paste0(substr(name, 1,1), "_"), paste0(substr(name, 4,4), "_"), colnames(df)[column]) )
      }
      else {
         colnames(data) <- c(colnames(df), gsub(paste0(substr(name, 1,1), "_"), paste0(substr(name, 4,4), "_"), column))
      }
   }
   data
}
#'transformation expander for lag function
#'
#'This function allows to use transformation function on several columns of mts or data.frame object
#'@param df a data.frame object
#'@param column a character or numeric vector 
#'@param lag is the number of lags (may be negative)
#'@export
#'@import zoo
lag_pmr <- function(df, column, lag = 1, ...){
   data <- ts.union(as.ts(df), as.ts(lag(as.zoo(df[, column, drop = FALSE]), k = lag, ...)))
   ifelse(lag < 0, d <- "l", d <- "f")
   if(lag < 0) lag <- abs(lag)
   if (is.numeric(column) == TRUE) {
      colnames(data) <- c(colnames(df), paste(paste0(d, lag), colnames(df)[column], 
                                              sep = "_"))
   }
   else {
      colnames(data) <- c(colnames(df), paste(paste0(d, lag), column, 
                                              sep = "_"))
   }
   data  
   
}

#'transform.pmr function 
#'
#'This function wraps the transformation functions allowing for easy use with magrittrl package
#'@param data is an data.frame or mts object
#'@param column a character or numeric vector 
#'@param method is the transformation method used
#'@export
#'@import zoo
transform_pmr <- function(data, column, method = "ln", ...) 
{
   df <- data
   temp_attr <- attributes(data)$output_table
   if (method == "ln") 
      data <- addLogs(data, column, ...)
   if (method == "d") 
      data <- addDiffs(data, column, ...)
   if (method == "hp") 
      data <- filterGap(data, column, ...)
   if (method == "hpt") 
      data <- filterTrend(data, column, ...)
   if (method == "(l|l\\d|f\\)") 
      if (method == "l\\") lag = as.numeric(substr(method, 2, 2))
      if (method == "f\\") lag = -as.numeric(substr(method, 2, 2))
      data <- lag_pmr(data, column, ...)
   if (method == "custom") 
      data <- addCustom(data, column, FUN = log, name = deparse(substitute(FUN)), 
                        ...)
   if (length(grep("(n|q|u|r|p)to(n|q|u|r|p)", method)) == 1){
      if(length(grep("(n|q|u)to(n|q|u)", method)) == 1) stop("unable to do such transformation, please revise your input to output desired")
      data <- addT(data, column, FUN = get(method), name = method, 
                   ...)
   }
   if ((length(grep("(i|c)to(n|q|u|r|p|i|c)", method)) == 1) | (length(grep("(n|q|u|r|p|i|c)to(i|c)", method)) == 1)) {
      
      first <- substr(method, 1,1)
      last <- substr(method, 4,4)
      if(substr(method, 1,1) %in% c("q","u")) first <- "n"
      if(substr(method, 4,4) %in% c("q","u")) last <- "n"
      method <- paste0(first, "to", last)
      
      data <- addI(data, column, FUN = get(method), name = method, ...)
   }
   if (is.null(method) | (!(method %in% c("ln", "d", "hp", 
                                          "hpt", "l", "custom")) & !(length(grep("(n|q|u|r|p|c|i)to(n|q|u|r|p|c|i)", 
                                                                                 method)) == 1))) 
      stop("unsupported method")
   if (is.numeric(column) == TRUE) {
      new_output_table <- data.frame(variable = colnames(data)[column], 
                                     operation = rep(method, length(colnames(data)[column])), 
                                     output = colnames(data)[(dim(df)[2]+1):(dim(df)[2]+length(column))]
      )
   }
   else {
      new_output_table <- data.frame(input = column, operation = rep(method, 
                                                                     length(column)), output = colnames(data)[(dim(df)[2]+1):(dim(df)[2]+length(column))]
      )
   }
   attr(data, "output_table") <- rbind(temp_attr, new_output_table)
   return(data)
}


#'merge ts function
#'
#'This function merges mts objects from list of mts objects with thier column names
#'@param df_list is a list of mts objects
#'@export
merge_ts <- function(df_list){
   
   df <- do.call(ts.union, df_list)
   colnames(df) <- df_list %>% lapply(colnames) %>% unlist
   df
}


# cat(cat(colnames(models$data)[1], " = "), cat(names(models$models[best_models(models)][[1]]$coefficients), sep = " + ")) # osobno nazwa
# models$models[best_models(models)] #osobno modele
# models$R2.DW[best_models(models)] #osobno info R2DW
# models$R2[best_models(models)] #osobno info R2


#'addTrends function
#'
#'This function adds 4 types of trends to the dataframe
#'@param data is a mts object
#'@param time_start is the starting period for trends
#'@param time_names is a vector of names for created trends
#'@export
addTrends <- function(data, time_start = 2000, time_names = c("time", "ln_time", 
                                                              "hyp_time", "sqrt_time")) 
{
   assign(time_names[1], c(rep(NA, time_start - start(data)[1]), 
                           1:(end(data)[1] - time_start + 1)))
   assign(time_names[2], log(get(time_names[1])))
   assign(time_names[3], 1/(get(time_names[1])))
   assign(time_names[4], sqrt(get(time_names[1])))
   df <- cbind(data, get(time_names[1]), get(time_names[2]), 
               get(time_names[3]), get(time_names[4]))
   colnames(df) <- c(colnames(data), time_names)
   return(df)
}

#'addCustom function
#'
#'This is a function allowing to create custom transformation for selected columns in a dataframe
#'@param data is a mts object
#'@param time_start is the starting period for trends
#'@param time_names is a vector of names for created trends
#'@examples mtcars %>% addCustom(2:3, FUN = function(x) log(x), name = "lol")
#'@export
addCustom <- function(df, column, FUN = log, name = deparse(substitute(FUN)), end = TRUE){
   data <- cbind(df, FUN(df[, column, drop = FALSE]))
   if(end == FALSE) {
      if (is.numeric(column) == TRUE) {
         colnames(data) <- c(colnames(df), paste(name, colnames(df)[column], 
                                                 sep = "_"))
      }
      else {
         colnames(data) <- c(colnames(df), paste(name, column, 
                                                 sep = "_"))
      }
   }else{
      if (is.numeric(column) == TRUE) {
         colnames(data) <- c(colnames(df), paste(colnames(df)[column], name, 
                                                 sep = "_"))
      }
      else {
         colnames(data) <- c(colnames(df), paste(column, name, 
                                                 sep = "_"))
      }  
   }
   
   data
}
