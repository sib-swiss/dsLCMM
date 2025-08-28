hlmeDS <- function(newObj = NULL, ...){
  
  argList <- list(...)  

  myEnv <- parent.frame()
  argNames <- names(argList)
  if(is.null(argNames)){
    argNames <- rep('', length(argList))
  }
  
  argList <- Map(function(name, value){
    retval <- .decode.arg(value)
    
    if(name %in% c('data', 'B')){
      retval <- .deep.extract(retval, startEnv =  myEnv)
    }
    
    return(retval)
  }, argNames, argList)  
 
  val <- do.call(lcmm::hlme, argList, envir = myEnv)
  if(!is.null(newObj)){
    newObj <- .decode.arg(newObj)
    assign(newObj, val, envir = myEnv)
  }
  # mask the disclosing values:
  val$call <- NULL
  val$pred$resid_m <- val$pred$resid_ss <- NA
  return(val)
 }
