#***************************************************************************----
# Constructor ----
#' Note constructor.
#'
#' Creates a new instance of a 'note' object
#'
#' @param p Pitch object (step, alter, octave).
#' @param d Duration object (type, dot, triplet).
#' @param l Numeric (>0), loudness expressed in percentage of a MIDI velocity of 90.
#' Effective range 0-141 (larger values are clipped).
#' 37: pp, 54: p, 71: mp, 89: mf, 107: f, 124: ff
#' @param tie2next Logical, is note tied with next note?.
#' @param tie2previous Logical, is note tied with previous note?.
#' @return An object of class 'note'.
#' @examples
#' n <- note(p=pitch('Db5'))
#' @export
note<-function(p,d=duration(4),l=89,tie2next=FALSE,tie2previous=FALSE){
  o<-new_note(p,d,l,tie2next,tie2previous)
  return(validate_note(o))
}

#***************************************************************************----
# toMXL function ----
#' Note to MXL (MusicXML)
#'
#' Convert an object of class 'note' into the corresponding MusicXML chunk
#'
#' @param x Note to be converted.
#' @return A MusicXML string.
#' @examples
#' toMXL(note(p=pitch('Db5')))
#' @export
toMXL.note<-function(x){
  # define nuance - dynamics relation
  nuance <- c('ppp','pp','p','mp','mf','f','ff','fff')
  dyn    <- c(18   ,37  ,54 ,71  ,89  ,107,124 ,141)
  indx <- which.min(abs(x$l-dyn))
  str <- paste0(
    '<note dynamics="',round(x$l),'">',
    toMXL.pitch(x$p),
    toMXL.duration(x$d),
    ifelse(x$tie2next,'<tie type="start"/>',''),
    ifelse(x$tie2previous,'<tie type="stop"/>',''),
    '<notations>',
    '<dynamics><',nuance[indx],'/></dynamics>',
    ifelse(x$tie2next,'<tied type="start"/>',''),
    ifelse(x$tie2previous,'<tied type="stop"/>',''),
    '</notations>',
    '</note>'
  )
  return(str)
}

#***************************************************************************----
# internal constructor ----
new_note<-function(p,d,l,tie2next,tie2previous){
  stopifnot(inherits(p,'pitch'))
  stopifnot(inherits(d,'duration'))
  stopifnot(is.numeric(l))
  stopifnot(is.logical(tie2next))
  stopifnot(is.logical(tie2previous))
  o <- list(p=p,d=d,l=l,tie2next=tie2next,tie2previous=tie2previous)
  class(o) <- 'note'
  return(o)
}

#***************************************************************************----
# validator ----
validate_note<-function(n){
  if(n$l<0){
    mess=paste0("Invalid dynamics (should be positive). ")
    stop(mess,call.=FALSE)
  }
  return(n)
}

