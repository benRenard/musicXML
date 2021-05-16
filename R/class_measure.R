#***************************************************************************----
# Constructor ----
#' Measure constructor.
#'
#' Creates a new instance of a 'measure' object
#'
#' @param number Integer (>0), measure number.
#' @param notes List of 'note' objects. Sum of notes durations should be compatible
#' with the measure time signature
#' @param beats Integer (>0), time signature is beats/beatType (default 4/4).
#' @param beatType Integer (>0), time signature is beats/beatType (default 4/4).
#' @param keySignature Integer, representing the number of flats (<0) or sharps (>0).
#' @param mode Character, mode. Can be one of  major, minor, dorian, phrygian, lydian,
#' mixolydian, aeolian, ionian, locrian, and none.
#' @return An object of class 'measure'.
#' @examples
#' notes=list(note(p=pitch('Db5'),d=duration(2)),note(p=pitch('B5'),d=duration(2)))
#' m <- measure(number=1,notes=notes)
#' @export
measure<-function(number,notes,beats=4,beatType=4,keySignature=0,mode='major'){
  o<-new_measure(number,notes,beats,beatType,keySignature,mode)
  return(validate_measure(o))
}

#***************************************************************************----
# toMXL function ----
#' Measure to MXL (MusicXML)
#'
#' Convert an object of class 'measure' into the corresponding MusicXML chunk
#'
#' @param x measure to be converted.
#' @return A MusicXML string.
#' @examples
#' notes=list(note(p=pitch('Db5'),d=duration(2)),note(p=pitch('B5'),d=duration(2)))
#' m <- measure(number=1,notes=notes)
#' toMXL.measure(m)
#' @export
toMXL.measure<-function(x){
  str <- paste0(
    '<measure number="',x$number,'">',
    '<attributes>',
      '<divisions>',getMxlDivision(x$notes),'</divisions>',
      '<key>',
        '<fifths>',x$keySignature,'</fifths>',
        '<mode>',x$mode,'</mode>',
      '</key>',
      '<time>',
        '<beats>',x$beats,'</beats>',
        '<beat-type>',x$beatType,'</beat-type>',
      '</time>',
    '</attributes>')
  for(i in 1:length(x$notes)){
    str <- paste0(str,toMXL.note(x$notes[[i]]))
  }
  str <- paste0(str,'</measure>')
  return(str)
}

#***************************************************************************----
# internal constructor ----
new_measure<-function(number,notes,beats,beatType,keySignature,mode){
  stopifnot(is.numeric(number))
  stopifnot(is.list(notes))
  stopifnot(is.numeric(beats))
  stopifnot(is.numeric(beatType))
  stopifnot(is.numeric(keySignature))
  stopifnot(is.character(mode))
  o <- list(number=number,notes=notes,beats=beats,
            beatType=beatType,keySignature=keySignature,mode=mode)
  class(o) <- 'measure'
  return(o)
}

#***************************************************************************----
# validator ----
validate_measure<-function(m){
  if(m$number%%1!=0 | m$number<=0){
    mess=paste0("Invalid measure number (should be a positive integer). ")
    stop(mess,call.=FALSE)
  }
  if(is.na(getMxlDivision(m$notes))){
    mess=paste0("All notes within measure do not share the same mxlDuration.")
    stop(mess,call.=FALSE)
  }
  if(m$beats%%1!=0 | m$beats<=0){
    mess=paste0("Invalid beats (should be a positive integer). ")
    stop(mess,call.=FALSE)
  }
  if(m$beatType%%1!=0 | m$beatType<=0){
    mess=paste0("Invalid beatType (should be a positive integer). ")
    stop(mess,call.=FALSE)
  }
  if(m$keySignature%%1!=0){
    mess=paste0("Invalid keySignature (should be integer). ")
    stop(mess,call.=FALSE)
  }
  modes <- c('major', 'minor', 'dorian', 'phrygian', 'lydian', 'mixolydian',
             'aeolian', 'ionian', 'locrian', 'none')
  if(! m$mode %in% modes){
    mess=paste0("Invalid mode (",m$mode,").")
    stop(mess,call.=FALSE)
  }
  return(m)
}

#***************************************************************************----
# get mxlDivision for 1 note ----
getMxlDivision_1 <- function(n){
  return(n$d$mxlDivision)
}

#***************************************************************************----
# get mxlDivision common to all notes, or return NA if they differ ----
getMxlDivision <- function(n){
  foo <- unlist(lapply(n,getMxlDivision_1))
  out <- ifelse(diff(range(foo))==0,foo[1],NA)
  return(out)
}
