#***************************************************************************----
# Constructor ----
#' Pitch constructor.
#'
#' Creates a new instance of a 'pitch' object
#'
#' @param string character string comprising:
#' (i)   one letter in ABCDEFG (step)
#' (ii)  'b' (flat), '#' (sharp) or '' (no alteration)
#' (iii) one integer in 0:9 (octave).
#' @return An object of class 'pitch'.
#' @examples
#' p <- pitch('Db5')
#' @export
pitch<-function(string){
  o<-new_pitch(string)
  return(validate_pitch(o))
}

#***************************************************************************----
# toMXL function ----
#' Pitch to MXL (MusicXML)
#'
#' Convert an object of class 'pitch' into the corresponding MusicXML chunk
#'
#' @param x Pitch to be converted.
#' @return A MusicXML string.
#' @examples
#' toMXL.pitch(pitch('Db5'))
#' @export
toMXL.pitch<-function(x){
  str <- paste0(
    '<pitch>',
    '<step>',x$step,'</step>',
    '<alter>',x$alter,'</alter>',
    '<octave>',x$octave,'</octave>',
    '</pitch>'
  )
  return(str)
}

#***************************************************************************----
# internal constructor ----
new_pitch<-function(string){
  stopifnot(is.character(string))
  n <- nchar(string)
  stopifnot(n>1 & n<4)
  step <- toupper(substr(string,1,1))
  octave <- as.numeric(substr(string,n,n)) # last character
  if(n==2){
    alter <- 0
  } else {
    foo <- substr(string,2,2)
    alter <- ifelse(foo=='b',-1,ifelse(foo=='#',1,NA))
  }
  o <- list(step=step,alter=alter,octave=octave)
  class(o) <- 'pitch'
  return(o)
}

#***************************************************************************----
# validator ----
validate_pitch<-function(p){
  if(! p$step %in% c('A','B','C','D','E','F','G')){
    mess=paste0("Invalid step (",p$step,"). Note should start with one of ABCDEFG")
    stop(mess,call.=FALSE)
  }
  if(is.na(p$octave)){
    mess=paste0("Invalid octave. Note should end with an integer between 0 and 9.")
    stop(mess,call.=FALSE)
  }
  if(is.na(p$alter)){
    mess=paste0("Invalid alteration. Alteration can only be '', 'b' or '#'.")
    stop(mess,call.=FALSE)
  }
  return(p)
}


