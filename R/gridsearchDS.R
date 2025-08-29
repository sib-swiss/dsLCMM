gridsearchDS <- function(...){
  argList <- list(...)  
  myEnv <- parent.frame()
  argNames <- names(argList)
  if(is.null(argNames)){
    argNames <- rep('', length(argList))
  }
  
  argList <- Map(function(name, value){
    retval <- .decode.arg(value)
    
    if(name %in% c('minit')){
      retval <- .deep.extract(retval, startEnv =  myEnv)
    }

    return(retval)
  }, argNames, argList)  
  
  library(lcmm)
  val <- do.call(gridsearch, argList, envir = myEnv)
  val$call <- NULL
  val$pred$resid_m <- val$pred$resid_ss <- NA
  val$data <- NULL
  return(val)
 }
