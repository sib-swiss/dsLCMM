lcmmDS <- function(...){
  argList <- list(...)  
  myEnv <- parent.frame()
  argNames <- names(argList)
  if(is.null(argNames)){
    argNames <- rep('', length(argList))
  }
  
  argList <- Map(function(name, value){
    retval <- .decode.arg(value)
    
    if(name %in% c('data')){
      retval <- .deep.extract(retval, startEnv =  myEnv)
    }
    
    return(retval)
  }, argNames, argList)  
  
  
  val <- do.call(lcmm::lcmm, argList, envir = myEnv)
  return(val)
 }
