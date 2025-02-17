#***************************************************************************----
# Mapping functions ----

#*******************************************************************************
#' Loudness Mapping.
#'
#' Map a series of values into a series of loudnesses
#'
#' @param x Vector or data frame, series to be mapped
#' @param lMin Integer, minimum loudness (default corresponds to ppp)
#' @param lMax Integer, maximum loudness (default corresponds to fff)
#' @param qtrans function, quantile transformation to be applied before mapping
#'   For instance data can be "normalized" by using qnorm.
#' @param ... further arguments to be passed to qtrans.
#' @return A vector of numerics representing loudnesses.
#' @examples
#' l <- loudnessMapping(x=rnorm(100))
#' @export
loudnessMapping <- function(x,lMin=18,lMax=141,qtrans=NULL,...){
  if(is.null(qtrans)){
    z <- x
  } else{
    prob <- (rank(x)-0.5)/length(x)
    z <- qtrans(prob,...)
  }
  if(diff(range(z))==0) {
    out <- rep(0.5*(lMin+lMin),length(x))
  } else {
    u <- (z-min(z))/(max(z)-min(z))
    out <- lMin + u*(lMax-lMin)
  }
  return(out)
}

#*******************************************************************************
#' Duration Mapping.
#'
#' Map a series of values into a series of durations
#'
#' @param x Vector or data frame, series to be mapped
#' @param expMin Integer, minimum type is 2^expMin (default: 1 <=> whole note)
#' @param expMax Integer, maximum type is 2^expMax (default: 64 <=> 16th note)
#' @param qtrans function, quantile transformation to be applied before mapping
#'   For instance data can be "normalized" by using qnorm.
#' @param ... further arguments to be passed to qtrans.
#' @return A list of duration objects.
#' @examples
#' d <- durationMapping(x=rnorm(100))
#' @export
durationMapping <- function(x,expMin=0,expMax=6,qtrans=NULL,...){
  n <- length(x)
  # use loudness mapping to create numeric values between expMin and expMax
  foo <- loudnessMapping(x,expMin,expMax,qtrans,...)
  # round them to integers
  expo <- round(foo)
  # create list of duration objects
  d <- vector(mode='list', length=n)
  for(i in 1:n){d[[i]]=duration(type=2^expo[i])}
  return(d)
}

#*******************************************************************************
#' Pitch Mapping.
#'
#' Map a series of values into a series of pitches
#'
#' @param x Vector or data frame, series to be mapped
#' @param pitches Vector of string, pitch scale (default: A minor pentatonic)
#' @param qtrans function, quantile transformation to be applied before mapping
#'   For instance data can be "normalized" by using qnorm.
#' @param ... further arguments to be passed to qtrans.
#' @return A list of pitch objects.
#' @examples
#' p <- pitchMapping(x=rnorm(100))
#' @export
pitchMapping <- function(x,pitches=c('A4','C5','D5','E5','G5','A5'),qtrans=NULL,...){
  n <- length(x)
  # use loudness mapping to create numeric values between 1 and length(pitches)
  foo <- loudnessMapping(x,1,length(pitches),qtrans,...)
  # round them to integers
  indx <- round(foo)
  # create list of pitch objects
  p <- vector(mode='list', length=n)
  for(i in 1:n){p[[i]]=pitch(pitches[indx[i]])}
  return(p)
}

#***************************************************************************----
# Functions to create note / measure lists ----

#*******************************************************************************
#' Get notes.
#'
#' Create a series of note objects from lists of pitches / durations / loudnesses
#'
#' @param pitches list of pitches (typically created by function pitchMapping)
#' @param durations list of durations (typically created by function durationMapping)
#' @param loudnesses list of loudnesses (typically created by function loudnessMapping)
#' @return A list of note objects.
#' @examples
#' n <- getNotes(pitches=pitchMapping(x=rnorm(100)))
#' @export
getNotes <- function(pitches,
                     durations=durationMapping(rep(0,length(pitches)),expMin=4,expMax=4),
                     loudnesses=loudnessMapping(rep(0,length(pitches)),lMin=89,lMax=89)){
  n <- length(pitches)
  stopifnot(length(durations)==n)
  stopifnot(length(loudnesses)==n)
  # create list of note objects
  notes <- vector(mode='list', length=n)
  for(i in 1:n){
    notes[[i]]=note(p=pitches[[i]],d=durations[[i]],l=loudnesses[[i]])
  }
  return(notes)
}

#*******************************************************************************
#' Tie notes.
#'
#' Add ties to successive notes having the same pitch.
#'
#' @param notes list of notes (typically created by function getNotes)
#' @return A list of note objects.
#' @examples
#' n <- tieNotes(getNotes(pitches=pitchMapping(x=rnorm(100))))
#' @export
tieNotes <- function(notes){
  out <- notes
  n <- length(notes)
  if(n>1) {
    for(i in 1:(n-1)){
      if(identical(notes[[i]]$p,notes[[i+1]]$p)){
        out[[i]]$tie2next=TRUE
        out[[i+1]]$tie2previous=TRUE
      }
    }
  }
  return(out)
}

#*******************************************************************************
#' Get measures
#'
#' Create a series of measure objects from a series of notes.
#'
#' @param notes list of notes (typically created by function getNotes).
#' @param beats number of beats (defaut signature is 4/4).
#' @param beatType beat type (defaut signature is 4/4).
#' @param mxlDivision Positive integer, musicXML "division" defining the
#' time resolution, i.e. the shortest possible note.
#' It is expressed as a fraction of a quarter note.
#' The value of 96 allows allows using 64th notes and their triplet/dotted versions.
#' @param ... further arguments to be passed to function measure (typically, keySignature)
#' @return A list of measure objects.
#' @examples
#' m <- getMeasures(notes=getNotes(pitches=pitchMapping(x=rnorm(100))))
#' @export
getMeasures <- function(notes,beats=4,beatType=4,mxlDivision=96,...){
  # Compute number of divisions per measure
  divPerMeasure <- mxlDivision*beats*(4/beatType)
  # initialize
  nlist <- notes # note list
  n <- length(nlist)
  durs <- vector(mode='integer',length=n)
  for(i in 1:n){durs[i] <- nlist[[i]]$d$mxlDuration}
  measures <- c() # measure list
  im <- 0 # measure counter
  # start grouping notes by measure
  while(length(durs)>0){
    # Re-initialize within-measure notes
    mnotes <- c()
    # Determine indices of notes in measure
    som <- cumsum(durs)
    indx <- which(som>=divPerMeasure)
    if(length(indx)>0){k <- indx[1]} else {k <- length(som)}
    # Collect notes in measure
    if(som[k]<=divPerMeasure){ # compatible with measure, no need to split note
      # keep notes 1:k
      for(j in 1:k){
        mnotes[[j]] <- nlist[[j]]
      }
      # remove notes from note lists
      nlist <- nlist[-(1:k)]
      durs <- durs[-(1:k)]
      } else {
      if(k>1){ # first (k-1) notes don't need no split
        for(j in 1:(k-1)){
          mnotes[[j]] <- nlist[[j]]
        }
      }
      # note k needs a split
      trim <- som[k]-divPerMeasure
      foo <- MXLDurationToType(durs[k]-trim)
      if(!is.null(foo$type)){
        for(j in 1:length(foo$type)){
          mnotes[[k-1+j]] <- note(p=nlist[[k]]$p,
                                  l=nlist[[k]]$l,
                                  d=duration(type=foo$type[j],
                                             dot=foo$dot[j],
                                             triplet=foo$triplet[j]),
                                  tie2next=TRUE)
        }
      }
      # remember note k then remove remove notes from note list
      backup <- nlist[[k]]
      nlist <- nlist[-(1:k)]
      durs <- durs[-(1:k)]
      # convert remaining duration
      foo <- MXLDurationToType(trim)
      # add corresponding notes
      if(!is.null(foo$type)){
        for(j in 1:length(foo$type)){
          nlist=c(list(note(p=backup$p,
                       d=duration(
                         type=foo$type[j],
                         dot=foo$dot[j],
                         triplet=foo$triplet[j],
                         mxlDivision=mxlDivision),
                       l=backup$l,
                       tie2next=j<length(foo$type),tie2previous=TRUE)),
                  nlist)
        }
      }
      # reinit
      n <- length(nlist)
      durs <- vector(mode='integer',length=n)
      if(n>0){
        for(i in 1:n){durs[i] <- nlist[[i]]$d$mxlDuration}
      }

      # if(k>1){
      #   # remove notes from note list
      #   nlist <- nlist[-(1:(k-1))]
      #   durs <- durs[-(1:(k-1))]
      # }
    }
    # Add notes to measure
    im <- im+1
    measures[[im]] <- measure(number=im,notes=mnotes,
                              beats=beats,beatType=beatType,...)
  }
  return(measures)
}

#***************************************************************************----
# Generics and other utilities----

#*******************************************************************************
#' Generic toMXL function
#' @param x Object (note, measure or score)
#' @return A MusicXML string.
#' @examples
#' toMXL(note(p=pitch('C5'),d=duration(1),l=107))
#' @export
toMXL<-function(x){UseMethod("toMXL")}

#*******************************************************************************
#' writeMXL function
#'
#' Write a score to a musicXML-formatted file
#'
#' @param s Score, score object to be written
#' @param file Character, destination file
#' @param ... additional arguments passed to method xml2::write_xml
#' @return No return value, called for side effects.
#' @examples
#' m <- getMeasures(notes=getNotes(pitches=pitchMapping(x=rnorm(100))))
#' s <- score(m)
#' tfile= file.path(tempdir(),'myMusicXML.xml')
#' writeMXL(s,tfile)
#' file.remove(tfile)
#' @export
writeMXL<-function(s,file,...){
  mxl=toMXL(s) # score to musicXML
  xml=xml2::read_xml(mxl) # musicXML to standard XML
  xml2::write_xml(xml,file,...) # write to file
}
