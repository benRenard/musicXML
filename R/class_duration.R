#***************************************************************************----
# Constructor ----
#' duration constructor.
#'
#' Creates a new instance of a 'duration' object
#'
#' @param type Integer in 2^(0:6), note type (longest 1=whole, shortest 64=64th).
#' 1 = whole, 2 = half, 4 = quarter, 8 = eighth, etc. down to 64 = 64th
#' @param dot Logical, is note dotted?
#' @param triplet Logical, is note triplet? (play 3 for 2)
#' @param mxlDivision Positive integer, musicXML "division" defining the
#' time resolution, i.e. the shortest possible note.
#' It is expressed as a fraction of a quarter note.
#' The value of 96 allows allows using 64th notes and their triplet/dotted versions.
#' @param mxlDuration Positive integer, music XML "duration" expressed in
#' number of mxlDivision's. In general, mxlDivision/mxlDuration should not be modified.
#' @return An object of class 'duration'.
#' @examples
#' d <- duration(8,dot=TRUE)
#' @export
duration<-function(type,dot=FALSE,triplet=FALSE,
                   mxlDivision=96,
                   mxlDuration=typeToMXLDuration(type,dot,triplet)){
  o<-new_duration(type,dot,triplet,mxlDivision,mxlDuration)
  return(validate_duration(o))
}

#***************************************************************************----
# toMXL function ----
#' Duration to MXL (MusicXML)
#'
#' Convert an object of class 'duration' into the corresponding MusicXML chunk
#'
#' @param x Duration to be converted.
#' @return A MXL string.
#' @examples
#' toMXL.duration(duration(8,dot=TRUE))
#' @export
toMXL.duration<-function(x){
  tripletStr <- paste0('<time-modification>',
                         '<actual-notes>3</actual-notes>',
                         '<normal-notes>2</normal-notes>',
                       '</time-modification>')
  str <- paste0(
    '<duration>',x$mxlDuration,'</duration>',
    '<type>',getTypeString(x$type),'</type>',
    ifelse(x$dot,'<dot/>',''),
    ifelse(x$triplet,tripletStr,'')
  )
  return(str)
}

#***************************************************************************----
# to MXL duration function ----
#' (type-dot-triplet) to MXL duration
#'
#' Convert a (type-dot-triplet) into a MusicXML duration
#'
#' @param type Integer in 2^(0:6), note type (longest 1=whole, shortest 64=64th).
#' 1 = whole, 2 = half, 4 = quarter, 8 = eighth, etc. down to 64 = 64th
#' @param dot Logical, is note dotted?
#' @param triplet Logical, is note triplet? (play 3 for 2)
#' @param mxlDivision Positive integer, musicXML "division" defining the
#' time resolution, i.e. the shortest possible note.
#' It is expressed as a fraction of a quarter note.
#' The value of 96 allows allows using 64th notes and their triplet/dotted versions.
#' @return An integer representing the mxl duration expressed in number of mxlDivision
#' @examples
#' typeToMXLDuration(type=8,dot=TRUE)
#' @export
typeToMXLDuration <- function(type,dot=FALSE,triplet=FALSE,mxlDivision=96){
  out <- mxlDivision*(4/type)*ifelse(dot,3/2,1)*ifelse(triplet,2/3,1)
  return(out)
}

#***************************************************************************----
# to type function ----
#' MXL duration to (type-dot-triplet)
#'
#' Convert a MusicXML duration into a (type-dot-triplet),
#' or a list of (type-dot-triplet) summing up to the requested duration.
#' The requested MusicXML duration may be trimmed if it cannot be expressed
#' as a multiple of the smallest available duration.
#'
#' @param mxlDuration Positive integer, music XML "duration" expressed in
#' number of mxlDivision's.
#' @param mxlDivision Positive integer, musicXML "division" defining the
#' time resolution, i.e. the shortest possible note.
#' It is expressed as a fraction of a quarter note.
#' The value of 96 allows allows using 64th notes and their triplet/dotted versions.
#' @return A list with the following fields:
#' \enumerate{
#'   \item type, numeric vector of types
#'   \item dot, logical vector of dot flags
#'   \item triplet, logical vector of triplet flags
#'   \item exact, logical, FALSE if the requested duration had to be trimmed
#' }
#' @examples
#' MXLDurationToType(972)
#' @export
MXLDurationToType <- function(mxlDuration,mxlDivision=96){
  if(mxlDuration<0){
    stop('mxlDuration should be positive',call.=FALSE)
  }
  # initialize
  type <- c();dot <- c(); triplet <- c()
  dur <- mxlDuration
  # Get all possible durations
  allDur <- cbind(typeToMXLDuration(2^(0:6),dot=FALSE,triplet=FALSE,mxlDivision=mxlDivision),
               typeToMXLDuration(2^(0:6),dot=TRUE,triplet=FALSE,mxlDivision=mxlDivision),
               typeToMXLDuration(2^(0:6),dot=FALSE,triplet=TRUE,mxlDivision=mxlDivision))

  # start dividing the requested duration
  while(dur>0){
    quot <- dur%/%allDur
    if(max(quot)==0){ # duration is smaller than smallest allDur => trim it and exit
      exact <- FALSE
      dur <- 0
    } else {
      k <- min(quot[quot>0]) # smallest non-zero quotient - there should be at least one
      remain <- dur%%allDur # remaining duration after division
      remain[quot!=k] <- Inf # "mask" remainders corresponding to non-minimal non-zero quotients
      dur <- min(remain) # smallest remainder amongst minimal non-zero quotients
      loc <- which(remain==dur,arr.ind=TRUE) # location of smallest remainder
      # add type-dot-triplet (s)
      type <- c(type,rep(2^(loc[1]-1),k))
      dot <- c(dot,rep(loc[2]==2,k))
      triplet <- c(triplet,rep(loc[2]==3,k))
      exact <- TRUE # so far ...
    }
  }
  if(is.null(type)){
    warning('mxlDuration is smaller than smallest possible duration')
  }
  return(list(type=type,dot=dot,triplet=triplet,exact=exact))
}

#***************************************************************************----
# internal constructor ----
new_duration<-function(type,dot,triplet,mxlDivision,mxlDuration){
  stopifnot(is.numeric(type))
  stopifnot(is.logical(dot))
  stopifnot(is.logical(triplet))
  stopifnot(is.numeric(mxlDivision))
  stopifnot(is.numeric(mxlDuration))
  o <- list(type=type,dot=dot,triplet=triplet,
            mxlDivision=mxlDivision,mxlDuration=mxlDuration)
  class(o) <- 'duration'
  return(o)
}

#***************************************************************************----
# validator ----
validate_duration<-function(d){
  if(! d$type %in% 2^(0:6)){
    mess=paste0("Invalid type (",d$type,"). Note type should be one of: ",
                paste0(2^(0:6),collapse=' '))
    stop(mess,call.=FALSE)
  }
  if(d$mxlDivision%%1!=0 | d$mxlDivision<=0){
    mess=paste0("Invalid mxlDivision: should be a positive integer")
    stop(mess,call.=FALSE)
  }
  if(d$mxlDuration%%1!=0 | d$mxlDuration<=0){
    mess=paste0("Invalid mxlDuration: should be a positive integer")
    stop(mess,call.=FALSE)
  }
  return(d)
}

#***************************************************************************----
# Transform Integer type into string ----
getTypeString <- function(x){
  out <- switch(as.character(x),
                '64'='64th',
                '32'='32nd',
                '16'='16th',
                '8'='eighth',
                '4'='quarter',
                '2'='half',
                '1'='whole',
                'unknown')
}
