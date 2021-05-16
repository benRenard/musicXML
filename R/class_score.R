#***************************************************************************----
# Constructor ----
#' Score constructor.
#'
#' Creates a new instance of a 'score' object
#'
#' @param parts List, either a list of measures for a single-part score
#' or a list of 'parts' (lists of mesures) for a multi-part score
#' @return An object of class 'score'.
#' @examples
#' m1 <- measure(number=1,notes=list(note(p=pitch('Db5'),d=duration(2)),
#'                                   note(p=pitch('B5'),d=duration(2))))
#' m2 <- measure(number=2,notes=list(note(p=pitch('A5'),d=duration(2)),
#'                                   note(p=pitch('B5'),d=duration(2))))
#' s <- score(list(m1,m2))
#' @export
score<-function(parts){
  if(class(parts[[1]])=='measure') {p <- list(parts)} else {p <- parts}
  o<-new_score(p)
  return(validate_score(o))
}

#***************************************************************************----
# toMXL function ----
#' Score to MXL (MusicXML)
#'
#' Convert an object of class 'score' into the corresponding MusicXML chunk
#'
#' @param x score to be converted.
#' @return A MusicXML string.
#' @examples
#' m1 <- measure(number=1,notes=list(note(p=pitch('Db5'),d=duration(2)),
#'                                   note(p=pitch('B5'),d=duration(2))))
#' m2 <- measure(number=2,notes=list(note(p=pitch('A5'),d=duration(2)),
#'                                   note(p=pitch('B5'),d=duration(2))))
#' s <- score(list(m1,m2))
#' toMXL(s)
#' @export
toMXL.score<-function(x){
  str <- paste0('<score-partwise version="3.1">','<part-list>')
  for (i in 1:length(x$parts)){
    str <- paste0(str,'<score-part id="P',i,'">',
                  '<part-name>P',i,'</part-name>','</score-part>')
  }
  str <- paste0(str,'</part-list>')
  for (i in 1:length(x$parts)){
    str <- paste0(str,'<part id="P',i,'">')
    for(j in 1:length(x$parts[[i]])){
      str <- paste0(str,toMXL.measure(x$parts[[i]][[j]]))
    }
    str <- paste0(str,'</part>')
  }
  str <- paste0(str,'</score-partwise>')
  return(str)
}

#***************************************************************************----
# internal constructor ----
new_score<-function(parts){
  stopifnot(is.list(parts))
  o <- list(parts=parts)
  class(o) <- 'score'
  return(o)
}

#***************************************************************************----
# validator ----
validate_score<-function(s){
  return(s)
}
