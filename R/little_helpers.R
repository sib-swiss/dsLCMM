.decode.arg <- function(some.thing, simplifyMatrix = FALSE){

  if(length(some.thing) == 1 && grepl('base64', some.thing, ignore.case = TRUE)){
    some.thing <- gsub('base64', '', some.thing, ignore.case =TRUE)
    serialized <- FALSE
    if(grepl('serialized', some.thing, ignore.case = TRUE)){
      serialized <- TRUE
      some.thing <- gsub('serialized', '', some.thing, ignore.case =TRUE)
    }
    my.dictionary = c('-plus-' = '+', '-slash-' = '/', '-equals-' = '=')
    sapply(names(my.dictionary), function(x){
      some.thing <<- gsub(x, my.dictionary[x], some.thing)
    })
    #
    if(serialized){
      some.thing <- jsonlite::unserializeJSON(RCurl::base64Decode(some.thing))
    } else {
      some.thing <- jsonlite::fromJSON(RCurl::base64Decode(some.thing), simplifyMatrix = simplifyMatrix)
    }
  }
  return(some.thing)
}
.deep.extract <- function(what, startEnv = parent.frame()){
  terms <- strsplit(what, '\\$|\\[|\\]')[[1]]
  terms <- terms[terms!='']
  Reduce(function(x,y){
    get(y, envir = as.environment(x))
  }, terms, init = startEnv)
}
