[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.4924680.svg)](https://doi.org/10.5281/zenodo.4924680)

# Introduction

[MusicXML](https://www.musicxml.com/) is a file format for representing
musical scores ([Wikipedia](https://en.wikipedia.org/wiki/MusicXML)). It
has become the standard format for sharing music notation in a digital
form and is hence supported by [most musical notation
softwares](https://www.musicxml.com/software/).

While MusicXML is easily understood by computers, it is less friendly to
the human eye. As an illustration, consider [Wikipedia’s
example](https://en.wikipedia.org/wiki/MusicXML#Example): writing a
simple whole note in MusicXML format looks tedious, and writing a whole
score by hand would certainly take some patience.

The `musicXML` package allows using R to write files in MusicXML format.
It is built as a minimalistic interface to the MusicXML format: the
basic musical objects (notes, measures etc.) of a score can be defined
in R and the package will take care of translating them into a proper
MusicXML format. The resulting file can then be opened and further
modified using your favorite musical score software (e.g. the free
[MuseScore](https://musescore.org) or the simple online viewer/player
[SoundSlice](https://www.soundslice.com/musicxml-viewer/)). One
particular application is data sonification, as will be illustrated in
this vignette

So let’s load the package and let’s get it started!

    library(musicXML)

# Manipulating basic objects: notes, measures and scores

The `score` to be written may contain several `measures`, each of which
may contain several `notes`. These can be defined as described below.

## Notes

A `note` is defined by the following three attributes:

1.  its `pitch`, for instance ‘C5’, ‘Gb4’, ‘G#6’, etc. (using the
    [scientific pitch
    notation](https://en.wikipedia.org/wiki/Scientific_pitch_notation))

2.  its `duration`, for instance a whole note, a dotted half, a triplet
    quarter, etc.

3.  its `loudness`, controlling the volume at which it is played.

Let’s say we want to define a whole-note C5 played *forte* for instance.
We start by defining its `pitch` using a simple character string
comprising: (i) the base pitch: one letter in ABCDEFG; (ii) the
alteration: ’’ (none), ‘b’ (flat) or ‘\#’ (sharp); (iii) the octave: an
integer between 0 and 9.

    p <- pitch(string='C5') # defines a pitch object

We then define the `duration` using an integer with the following
meaning: 1 = whole, 2 = half, 4 = quarter, 8 = eighth, etc. down to 64 =
64th.

    d <- duration(type=1) # defines a duration object

We then define the `loudness`, which is expressed as a number between 0
and 141, with the following guide: 37: *pp*, 54: *p*, 71: *mp*, 89:
*mf*, 107: *f*, 124: *ff*.

    l <- 107 # defines loudness of the note

The note can finaly be defined by stitching pitch, duration and loudness
together as follows:

    n <- note(p,d,l) # defines a note object

Of course all this can be done in a single line of code:

    n <- note(p=pitch('C5'),d=duration(1),l=107)

If you want to have a look at what this note would look like in musicXML
format, you can use the function `toMXL`. Note that the resulting string
is not sufficient to define a proper musicXML file, it just corresponds
to a “note” block of the future complete file.

    toMXL(n)

    ## [1] "<note dynamics=\"107\"><pitch><step>C</step><alter>0</alter><octave>5</octave></pitch><duration>384</duration><type>whole</type><notations><dynamics><f/></dynamics></notations></note>"

Let’s define a few more notes to start a melody and to illustrate a
couple of useful options:

    # a quarter G3 played fff
    n1 <- note(p=pitch('G3'),d=duration(4),l=140)
    # a dotted 8th D4 played f. Note the use of 'dot=TRUE' in duration. There is also a 'triplet=' option.
    n2 <- note(p=pitch('D4'),d=duration(8,dot=TRUE),l=100)
    # a 16th G4 played mp
    n3 <- note(p=pitch('G4'),d=duration(16),l=80)
    # two quarter Bb4 played p and tied together (tie2next=TRUE - you could also use tie2previous=TRUE on the second one) 
    n4 <- note(p=pitch('Bb4'),d=duration(4),l=60,tie2next=TRUE)
    n5 <- note(p=pitch('Bb4'),d=duration(4),l=60)

## Measures

The five notes defined above add up to 4 full beats; they can hence be
placed into a single 4/4 `measure` as shown below:

    # defining a measure. 'number=' is the measure index in the whole score
    m <- measure(number=1,notes=list(n1,n2,n3,n4,n5))

Alternatively, you could decide to rather use two 2/4 measures:

    # note the use of 'beats=' and 'beatType=' to modify the time signature
    m1 <- measure(number=1,notes=list(n1,n2,n3),beats=2,beatType=4)
    m2 <- measure(number=2,notes=list(n4,n5),beats=2,beatType=4)

It is also possible to change the key signature of a measure. For
instance the five notes defined above would fit in a minor G scale, with
3 flats in the key signature:

    # note the use of 'keySignature=' to add flats (negative values) of sharps (positive values) to the key
    m1 <- measure(number=1,notes=list(n1,n2,n3),beats=2,beatType=4,keySignature=-3,mode='minor')
    m2 <- measure(number=2,notes=list(n4,n5),beats=2,beatType=4,keySignature=-3,mode='minor')

## Score

Finally, all measures can be grouped together to define a `score`:

    s <- score(list(m1,m2))

Note that it is possible to define a multi-part score as follows:

    part1 <- list(m1,m2)
    part2 <- list(m2,m2)
    s <- score(list(part1,part2))

The score can finally be written into a musicXML file. You can open it
with a text editor to see what it looks like, but more interestingly,
you can open it with a musical software to see, listen and modify the
resulting musical score (for a quick online look try
[SoundSlice](https://www.soundslice.com/musicxml-viewer/)).

    writeMXL(s,file='myFirstScore.xml')

# Application to data sonification

## General principles

Data [sonification](https://en.wikipedia.org/wiki/Sonification) refers
to the transformation of data into sound, using some algorithmic
process. This can be achieved in [many
ways](https://sonification.de/handbook/), but here we focus on the
approach known as [*parameter
mapping*](https://sonification.de/handbook/chapters/chapter15/): the
values taken by the data are mapped into some attributes (or parameters)
of notes, typically their pitch or loudness, or possibly their duration.
This is very much the same process as the mapping performed in data
visualization, where data values are mapped into e.g. the color, size of
type of symbols in a graph or map.

Consider, as an example, the Wagga Wagga dataset which comes with this
package. It contains the annual precipitation and temperature time
series in the city of Wagga Wagga, New South Wales, Australia, as
plotted below. A possible sonification of this dataset is to map
precipitation into pitch (so that wet years correspond to high-pitched
notes) and temperature to loudness (so that warm years correspond to
loud notes). Let’s see how this can be done.

    plot(WaggaWagga$Year,WaggaWagga$Precipitation,type='l',xlab='Year',ylab='precip. [mm]')

![](man/figures/README-unnamed-chunk-15-1.png)

    plot(WaggaWagga$Year,WaggaWagga$Temperature,type='l',xlab='Year',ylab='temp. [C]')

![](man/figures/README-unnamed-chunk-16-1.png)

## Sonification of the Wagga Wagga dataset

We start with the mapping of temperature to loudness. This is achieved
using the `loudnessMapping` function which takes a vector of data as
input and returns a vector of corresponding loudnesses using a simple
linear interpolation between a minimal and a maximal loudness. Although
not detailled here, there are options to use a nonlinear mapping, see
?loudnessMapping.

    # Compute loudnesses from temperatures
    llist <- loudnessMapping(WaggaWagga$Temperature,lMin=20,lMax=141)
    # Check the relation is just a simple linear interpolation
    plot(WaggaWagga$Temperature,llist,pch=19,col=rgb(0,0,0,0.2),xlab='Temp. [C]',ylab='loudness')

![](man/figures/README-unnamed-chunk-17-1.png)

We now perform the mapping of precipitation to pitch. This is a bit
trickier because instead of doing a continuous mapping, we want to map
values to a discrete set of notes that ‘sound good’. To achieve this, we
need to define this set of notes, typically using a musical scale (a
2-octave pentatonic A scale in the example below). Note that the output
of `pitchMapping` function is not a vector of numeric values, but rather
a list of `pitch` objects.

    scale <- c("A3", "C4", "D4", "E4", "G4", "A4", "C5", "D5", "E5", "G5")
    plist <- pitchMapping(WaggaWagga$Precipitation,pitches=scale)

We now have a list of loudnesses, a list of pitches, we can turn them
into a list of notes as shown below. Note that by default the duration
of each note will be a 16th note, but this can be changed, see
?getNotes.

    # Take a list of pitches/loudnesses and create a list of notes
    notes <- getNotes(pitches=plist,loudnesses=llist)

We can now stack these notes into measures having a given time signature
(4/4 is used below). Note that the function `getMeasures` automatically
groups notes to ensure that each measure has the correct amount of
beats.

    # Take a list of notes and stack them into a list of measures.
    m <- getMeasures(notes=notes,beats=4,beatType=4)

Finally, we can now define the `score` and write it into a musicXML
file.

    s <- score(m)
    writeMXL(s,file='WaggaWagga.xml')

That’s it! The dataset has been transformed into a musical score. The
job of the musicXML R package stops here, but of course this does not
mean that the sonification process is necessarily over: the score can be
further read, played and modified using a specialized music software.

Note that data sonification is particularly interesting when it is
combined with data animation, as shown in the example below based on the
package [gganimate](https://gganimate.com/index.html). Note that the
musicXML file has to be transformed into an audio format (such as .mp3)
to achieve this. Unfortunately this is not yet implemented in this R
package, but most specialized software can do it (I did it with
[MuseScore](https://musescore.org)). The resulting video can be seen
[here](https://vimeo.com/426641144).

    library(tidyr);library(gganimate)
    # Modify the shape of the WaggaWagga dataset to facilitate plotting
    DF <- pivot_longer(WaggaWagga,-Year) # function from tidyr
    # Plot precipitation and temperature time series using ggplot
    g <- ggplot(DF,aes(x=Year,y=value))
    g <- g + geom_line(aes(color=name),size=1)+geom_point(size=4)
    g <- g + scale_color_manual(values = c('blue','red'),guide=FALSE)
    g <- g + facet_wrap(vars(name),ncol=1,scales='free_y')
    # Make it look nicer
    g <- g+theme_bw()+theme(axis.title=element_text(size=18), 
              axis.text=element_text(size=14),
              strip.text=element_text(size=18))
    # Create an animated plot
    g <- g + transition_reveal(Year)
    # 'Render' the animated plot into a .mp4 movie
    bpm=120 # tempo (in beats per minute) used to create the audio file
    bps=(bpm/60) # convert to beats per second
    fps=bps*4 # convert to frames per second (there are 4 16th note per beat)
    animate(g,nframes=NROW(WaggaWagga),fps=fps,width=1280,height=720,
            renderer = av_renderer('WaggaWaggaMelody.mp4',audio='vignettes/WaggaWagga.mp3'))

# Conclusion

The musicXML package has been built with the idea of providing a
minimalistic R interface to the musicXML format. In particular, its
intended usage is to transform a series of data into a musical score, by
mapping data values into pitch, loudness or duration. This
transformation is the first step of a data sonification endeavor,
however it is certainly not the last. More advanced music-oriented
modifications of this score can be performed (e.g. changing instrument,
using effects, etc.), but this is deferred to specialized music
software.
