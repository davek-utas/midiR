####################################################
# some code adapted from tuneR package source code



####################################################
# read/write MIDI files

readVarLength <- function( file.con ) {
  b <- numeric(4)
  b[1] <- readBin(file.con, integer(0), n = 1, size = 1, signed = FALSE, endian = "big")
  bytes <- 1
  if(b[1] > 127){
    b[1] <- (b[1]-128) * 2^7
    b[2] <- readBin(file.con, integer(0), n = 1, size = 1, signed = FALSE, endian = "big")
    bytes <- 2
    if(b[2] > 127){
      b[2] <- b[2]-128
      b <- b*2^7
      b[3] <- readBin(file.con, integer(0), n = 1, size = 1, signed = FALSE, endian = "big")
      bytes <- 3
      if(b[3] > 127){
        b[3] <- b[3]-128
        b <- b*2^7
        b[4] <- readBin(file.con, integer(0), n = 1, size = 1, signed = FALSE, endian = "big")
        bytes <- 4
      }
    }
  }
  c(sum(b), bytes)
}

writeVarLength <- function( b, file.con ) {
  x <- bitwAnd( b, 2^7-1 )
  if ( b >= 2^7 ) { x <- x + 2^7 }
  writeBin( as.integer(x), file.con, size = 1, endian = "big" )
  bytes <- 1
  if ( b >= 2^7 ) { 
    x <- bitwAnd( b , 2^14 - 1 - 2^7-1 ) / 2^7
    if ( b >= 2^14 ) { x <- x + 2^7 }
    writeBin( as.integer(x), file.con, size = 1, endian = "big" )
    bytes <- 2
    if ( b >= 2^14 ) {
      x <- bitwAnd( b , 2^21 - 1 - 2^14-1 ) / 2^14
      if ( b >= 2^21 ) { x <- x + 2^7 }
      writeBin( as.integer(x), file.con, size = 1, endian = "big" )
      bytes <- 3
      if ( b >= 2^21 ) {
        x <- bitwAnd( b , 2^28 - 1 - 2^21-1 ) / 2^21
        if ( b >= 2^18 ) { x <- x + 2^7 }
        writeBin( as.integer(x), file.con, size = 1, endian = "big" )
        bytes <- 4
      }
    }
  }
  return( c(b, bytes ) )
}


midi_event_names   <-   data.frame( codes = c( "8", "9", "a", "b", "c", "d", "e", "f" ), 
                                    names = c( "note off", "note on", "aftertouch", "control change", "program change", "channel pressure", "pitch wheel", "system" ) )
system_event_names <-   data.frame( codes = c( 0xf0, 0xf2 , 0xf3 , 0xf6, 0xf7 , 0xff ) , 
                                    names = c( "sysex1", "song position", "song select", "tune request", "end sysex", "meta" ) )
meta_event_names <-     data.frame( codes = c( 0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x20, 0x2f, 0x51, 0x54, 0x58, 0x59, 0x7f),
                                    names = c( "sequence number", "text event", "copyright notice", "track name", "instrument name", "lyric", "marker", "cue point", "program name", "device name", "MIDI channel prefix", "end of track", "set tempo", "SMPTE offset", "time signature", "key signature", "meta-event") )
control_event_names <-  data.frame( codes = c( 0x00,	0x01, 	0x02, 	0x04, 	0x05, 	0x06, 	0x07, 	0x08, 	0x0A, 	0x0B, 	0x0C, 	0x0D, 	0x10, 	0x11, 	0x12, 	0x13, 	0x40, 	0x41, 	0x42, 	0x43, 	0x44, 	0x45, 	0x46, 	0x47, 	0x48, 	0x49, 	0x4A, 	0x4B, 	0x4C, 	0x4D, 	0x4E, 	0x4F, 	0x50, 	0x51, 	0x52, 	0x53, 	0x54, 	0x5B, 	0x5C, 	0x5D, 	0x5E, 	0x5F, 	0x60, 	0x61, 	0x62, 	0x64, 	0x78, 	0x79, 	0x7A, 	0x7B, 	0x7C, 	0x7D, 	0x7E, 	0x7F ),
                                    names = c( 'Bank Select', 	'Modulation wheel ', 	'Breath control ', 	'Foot controller ', 	'Portamento time ', 	'Data Entry ', 	'Channel Volume', 	'Balance ', 	'Pan ', 	'Expression Controller ', 	'Effect control 1 ', 	'Effect control 2 ', 	'General Purpose Controller #1 ', 	'General Purpose Controller #2 ', 	'General Purpose Controller #3 ', 	'General Purpose Controller #4 ', 	'Sustain', 	'Portamento', 	'Sustenuto', 	'Soft pedal', 	'Legato Footswitch', 	'Hold', 	'Sound Controller 1', 	'Sound Controller 2', 	'Sound Controller 3', 	'Sound Controller 4', 	'Sound Controller 5', 	'Sound Controller 6', 	'Sound Controller 7', 	'Sound Controller 8', 	'Sound Controller 9', 	'Sound Controller 10', 	'General Purpose Controller #5', 	'General Purpose Controller #6', 	'General Purpose Controller #7', 	'General Purpose Controller #8', 	'Portamento Control Source Note', 	'Effects 1 Depth', 	'Effects 2 Depth', 	'Effects 3 Depth', 	'Effects 4 Depth', 	'Effects 5 Depth', 	'Data entry +1', 	'Data entry -1', 	'Non-Registered Parameter Number', 	'Registered Parameter Number', 	'All Sound Off, ', 	'Reset All Controllers', 	'Local control', 	'All notes off', 	'Omni mode off', 	'Omni mode on', 	'Mono mode on', 	'Poly mode on') ) 

blank.midi_event <- data.frame( 
  track=NA, 
  time = NA,
  delta.time=NA, 
  event=raw(1), 
  event_name="",
  channel=NA, 
  subEvent=raw(1), 
  subevent_name="",
  data.int1=NA,
  data.int2=NA,
  data.int3=NA,
  data.int4=NA,
  data.int5=NA,
  data.string=NA,
  data.raw=NA,
  stringsAsFactors = FALSE )


midiEventName <- function( code ) {
  event <- substring( code, 1, 1 )
  return( midi_event_names$names[ grep( event, midi_event_names$codes ) ] )
}

midiSystemEventName <- function( code ) {
  return( system_event_names$names[ match( code, system_event_names$codes ) ] )
}

midiControlEventName <- function( code ) {
  return( control_event_names$names[ match( code, control_event_names$codes ) ] )
}

midiMetaEventName <- function( code ) {
  return( meta_event_names$names[ match( code, meta_event_names$codes ) ] )
}

midiTime <- function( bar, beat, note=1, notes_per_beat=1, beats_per_bar=4, beat_unit=4, ticksPerQN=96, tuplet=2  ) {
  bar_start <- ticksPerQN * beat_unit/4 * beats_per_bar * ( bar - 1 )
  beat_start <- ticksPerQN * (beat_unit/4) * ( beat - 1 )
  note_start <- ticksPerQN * (beat_unit/4) / notes_per_beat * ( note - 1 ) * (2/tuplet)
  return( bar_start + beat_start + note_start )
}

note_names = c( 'C', 'C#', 'D♭', 'D', 'D#', 'E♭', 'E', 'F', 'F#', 'G♭', 'G', 'G#', 'A♭', 'A', 'A#', 'B♭', 'B' )
note_values = c( 0,   1,    1,    2,   3,   4,    4,   5,    6,    6,   7,   8,   9,    9,    10,   11,   11 )

scales <- list( ionian     = c(0,2,4,5,7,9,11),
                dorian     = c(0,2,3,5,7,9,10),
                phrygian   = c(0,1,3,5,7,8,10),
                lydian     = c(0,2,4,6,7,9,11),
                mixolydian = c(0,2,4,5,7,9,10),
                aeolian    = c(0,2,3,5,7,8,10),
                locrian    = c(0,1,3,5,6,8,10) )

midiRandomNote <- function( scale=scales$ionian, start_note=0x3c, note_weights=c( 0.4 , 0.05, 0.2, 0.05, 0.2, 0.05, 0.05 ), octave_weights=c( 0 , 0, 0, 0.25, 0.5, 0.25, 0, 0, 0 )  ) {
  note_value <- scale[ sample( 1:length(scale), 1, replace=TRUE, prob=note_weights ) ] + start_note
  octave <- sample( 1:length(octave_weights), 1, replace=TRUE, prob=octave_weights )-5 
  return( octave*12 + note_value )
}

midiNote <- function( octave=4, note_name='C', note_value=NA ) {
    if ( is.na( note_value ) ) {
      note_value <- note_values[ note_names==note_name ]
    }
  return( octave*12 + note_value + 12 )
}

createMidiEvent <- function( track=0, time = 0, channel=0, event=NA, subEvent=NA, data.int1=NA, data.int2=NA, data.int3=NA, data.int4=NA, data.int5=NA, data.string=NA, data.raw=NA ) {
  midi_event <- blank.midi_event
  midi_event$track <- track
  midi_event$time = time
  midi_event$channel=channel
  midi_event$event=event
  midi_event$subEvent=subEvent
  midi_event$event_name <- midiEventName( event )
  if ( !is.na(data.int1) ) { midi_event$data.int1 <- as.integer(data.int1) }
  if ( !is.na(data.int2) ) { midi_event$data.int1 <- as.integer(data.int2) }
  if ( !is.na(data.int3) ) { midi_event$data.int1 <- as.integer(data.int3) }
  if ( !is.na(data.int4) ) { midi_event$data.int1 <- as.integer(data.int4) }
  if ( !is.na(data.int5) ) { midi_event$data.int1 <- as.integer(data.int5) }
  if ( !is.na(data.string) ) { midi_event$data.int1 <- data.string }
  if ( !is.na(data.raw) ) { midi_event$data.int1 <- data.raw }
  return( midi_event )
}

createMidiNote <- function( track=0, channel=0, start_time = 0, end_time=96, note = 0x3c, velocity_on = 0x40, velocity_off = 0x40 ) {
  event <- as.raw( 0x90 )
  midi_event1 <- createMidiEvent( track=track, channel=channel, event=event, time=start_time, data.int1 = note, data.int2 = velocity_on )
  event <- as.raw( 0x80 )
  midi_event2 <- createMidiEvent( track=track, channel=channel, event=event, time=end_time, data.int1 = note, data.int2 = velocity_off )
  return( rbind( midi_event1, midi_event2 ) )
}

createMidiKeyPressure <- function( track=0, channel=0, time=0, note = 0x3c, pressure = 0x40 ) {
  event <- as.raw( 0xa0 )
  midi_event1 <- createMidiEvent( track=track, channel=channel, event=event, time=time, data.int1 = note, data.int2 = pressure )
  return( midi_event1 )
}

createMidiControlChange <- function( track=0, channel=0, time=0, controller = 0x00, value = 0x00 ) {
  event <- as.raw( 0xb0 )
  midi_event <- NULL
  midi_event1 <- createMidiEvent( track=track, channel=channel, event=event, time=time, data.int1 = controller, data.int2 = value )
  midi_event <- midi_event1
  if (controller < 0x20) {
    midi_event1$data.int2 <- bitwShiftR( value , 7 )
    midi_event2 <- createMidiEvent( track=track, channel=channel, event=event, time=time, data.int1 = controller+0x20, data.int2 = value && 0x7f )
    midi_event <- rbind( midi_event1 , midi_event2 )
  }
  if (controller >= 0x40 && controller <=0x45 ) {
    if (value) {
      midi_event$data.int2 <- as.integer( 0x40 )
    }
  }
  if (controller == 0x62) { # Non-Registered Parameter Number
    midi_event1$data.int2 <- value && 0x7f
    midi_event2 <- createMidiEvent( track=track, channel=channel, event=event, time=time, data.int1=controller + 1, data.int2=bitwShiftR( value , 7 ) )
    midi_event <- rbind( midi_event1 , midi_event2 )
  }
  if (controller == 0x64) { # Registered Parameter Number
    midi_event1$data.int2 <- value && 0x7f
    midi_event2 <- createMidiEvent( track=track, channel=channel, event=event, time=time, data.int1=controller + 1, data.int2=bitwShiftR( value , 7 ) )
    midi_event2 <- createMidiEvent( track=track, channel=channel, event=event )
    midi_event <- rbind( midi_event1 , midi_event2 )
  }
  return( midi_event )
}

createMidiProgramChange <- function( track=0, channel=0, time=0, program = 0x00 ) {
  event <- as.raw( 0xc0 )
  midi_event1 <- createMidiEvent( track=track, channel=channel, event=event )
  midi_event1$time <- time
  midi_event1$data.int1 <- program
  midi_event1$event_name <- midiEventName( event )
  return( midi_event1 )
}

createMidiChannelPressure <- function( track=0, channel=0, time=0, pressure = 0x40 ) {
  event <- as.raw( 0xd0 )
  midi_event1 <- createMidiEvent( track=track, channel=channel, event=event )
  midi_event1$time <- time
  midi_event1$data.int1 <- pressure
  midi_event1$event_name <- midiEventName( event )
  return( midi_event1 )
}

createMidiPitchWheel <- function( track=0, channel=0, time=0, value = 0x2000 ) {
  event <- as.raw( 0xa0 )
  midi_event1 <- createMidiEvent( track=track, channel=channel, event=event )
  midi_event1$time <- time
  midi_event1$data.int1 <- value
  midi_event1$event_name <- midiEventName( event )
  return( midi_event1 )
}

createMidiSysEx <- function( track=0, time=0, id=0, value = NA ) {
  event <- as.raw( 0xf0 )
  midi_event1 <- createMidiEvent( track=track, event=event )
  midi_event1$time <- time
  if (length( id )>1) {
    midi_event1$data.int1 <- as.integer( id[1] )
    midi_event1$data.int2 <- as.integer( id[2] )
    if (length( id )>2) {
      midi_event1$data.int3 <- as.integer( id[3] ) 
    }
  } else {
    midi_event1$data.int1 <- as.integer( id )
  }
  midi_event1$data.raw <- as.raw( value )
  midi_event1$event_name <- midiEventName( event )
  return( midi_event1 )
}

createMidi <- function( number_tracks=1, format=0, ticksPerQN=96, ticksPerFrame=NA, framesPerSecond=NA ) {
  midi_header <- data.frame( format, number_tracks, ticksPerQN, ticksPerFrame, framesPerSecond, length=6 )
  midi_tracks <- NULL
  for (i in 1:number_tracks) {
    midi_event <- createMidiEvent()
    midi_tracks[[i]] <- midi_event
  }
  return( list( header=midi_header, tracks=midi_tracks ) )
}

updateDeltaTime <- function( midiObject) {
  for (i in 1:midiObject$header$number_tracks) {
    x <- order( midiObject$tracks[[i]]$time )
    midiObject$tracks[[i]] <- midiObject$tracks[[i]][ x , ]
    for ( j in 1:nrow( midiObject$tracks[[ i ]] ) ) {
      if (j==1) {
        midiObject$tracks[[ i ]]$delta.time[ j ] <- midiObject$tracks[[ i ]]$time[ j ]
      } else {
        midiObject$tracks[[ i ]]$delta.time[ j ] <- midiObject$tracks[[ i ]]$time[ j ] - midiObject$tracks[[ i ]]$time[ j-1 ]
      }
    }
  }
  return( midiObject )
}

readMidiFile <- function( midiFile ) {
  #midiFile = midiFile
  file.con <- file( description = midiFile, open = "rb" )
  on.exit( close( file.con ) )
  
  ###############################-
  # MIDI header
  # chunk type: 4 chars == 'MThd'
  # length: 32 bit number == 6
  # format: 16 bit number == 0, 1, or 2
  # ntrks: 16 bit number == number of tracks
  # division: 16 bit coded number
  
  midi_header <- data.frame( format=NA, number_tracks=NA, ticksPerQN=NA, ticksPerFrame=NA, framesPerSecond=NA )
  midi_tracks <- NULL
    
  MThd_head <- readChar( file.con, 4 )
  if( MThd_head != "MThd" ) 
    stop("No Header Chunk")
  
  midi_header$length <- readBin( file.con, integer(0), n = 1, size = 4, endian = "big" )
  if(midi_header$length < 6) 
    stop("Unexpected Header Chunk size") 
  
  midi_format <- c("single","simultaneous","sequential")
  # FORMAT - 16 bit integer
  # 0-the file contains a single multi-channel track
  # 1-the file contains one or more simultaneous tracks (or MIDI outputs) of a sequence
  # 2-the file contains one or more sequentially independent single-track patterns
  midi_header$format <- readBin( file.con, integer(0), n = 1, size = 2, endian = "big", signed = FALSE )
  if(!(midi_header$format %in% 0:2)) 
    stop("Unexpected Midi file format") 
  
  midi_header$number_tracks <- readBin( file.con, integer(0), n = 1, size = 2, endian = "big", signed = FALSE )
  
  # DIVISION - 16 bit integer
  # +ve:	ticks per quarter-note
  # -ve: low byte - ticks per frame, high byte - the number of frames per second (one of the four values -24, -25, -29, or -30, corresponding to the four standard SMPTE and MIDI Time Code formats)
  MThd_division <- readBin( file.con, integer(0), n = 1, size = 2, signed = TRUE, endian = "big" )
  midi_header$ticksPerQN <- ifelse( MThd_division>0, MThd_division, NA )
  midi_header$ticksPerFrame <- ifelse( MThd_division<0, bitwAnd( MThd_division , 255 ), NA )
  midi_header$framesPerSecond <- ifelse( MThd_division<0, bitwAnd( MThd_division , 32767-255 ) / 2^8  , NA )

  # read any extra header
  if ( midi_header$length > 6 ) {
    padding <-  readBin( file.con, raw(), n = MThd_length - 6, size=1 )
  }
  
  #############################
  # read tracks
  
  
  track <- 1
  for( track in 1:midi_header$number_tracks ) {
    #if (track==12) { break }
    MTrk <- readChar(file.con, 4)
    MTrk_length <- readBin(file.con, integer(0), n = 1, size = 4, endian = "big")
    if (MTrk != "MTrk") { # skip to next track, something weird
      padding <-  readBin( file.con, character(), size = MTrk_length  )
      next 
      } 
    midi_events <- blank.midi_event[ rep ( 1 , 1000 ) , ]
    bytes <- 0
    i <- 0
    time <- 0
    while( bytes < MTrk_length ) {
      i <- i+1
      if ( i>nrow( midi_events ) ) { midi_events <- rbind( midi_events , blank.midi_event[ rep( 1 , 1000 ) , ] ) }
      delta_time <- readVarLength( file.con )
      event <- readBin( file.con, raw(0), n = 1 )
      event.code <- substring( event, 1, 1 )
      event.int <- strtoi( event.code, base = 16 )
      #print( paste( "track=" , track , "event=" , i , "delta_time=" , delta_time[ 1 ] , "event=" , event , "bytes=" , bytes ) )
      bytes <- bytes + delta_time[ 2 ] + 1
      midi_events$track[ i ] <- track
      midi_events$delta.time[ i ] <- delta_time[ 1 ]
      time <- time + midi_events$delta.time[ i ]
      midi_events$time[ i ] <- time
      midi_events$event[ i ] <- event
      midi_events$event_name[ i ] <- midiEventName( event.code )
      midiData <- NA
      if ( event.code == "f" ) { # special
          switch( as.character(event) , 
            "f0" = { # sysex event
              midi_events$subevent_name[i] <- midiSystemEventName( event )
              eventLength <- readVarLength( file.con )
              midi_events$data.int1[i] <- readBin(file.con, integer(0), n = 1, size = 1, endian = "big", signed = FALSE)
              midiData  <- readBin( file.con, raw(), n=eventLength[1]-1 , size=1 )
              #               length_bytes + length
              bytes <- bytes + eventLength[2] + eventLength[1]
            } ,
            "f2" = { # song position
              x <- readBin( file.con, raw(), n=2 , size=1 )
              midiData <- x[2]*2^7 + x[1] # 14 bit integer
              bytes <- bytes + length( x )
            } ,
            "f3" = { # song select
              midiData <- readBin(file.con, integer(0), n = 1, size = 1, endian = "big", signed = FALSE)
              bytes <- bytes + length( midiData )
            } ,
            "f7" = { # sysex event
              eventLength <- readVarLength( file.con )
              midiData <- readBin( file.con, raw(), n=eventLength[1] , size=1 )
              #               delta bytes + event + length_bytes + length
              bytes <- bytes + eventLength[2] + eventLength[1]
            } ,
            "ff" = { # meta event
              eventType <- readBin( file.con, raw(0), n = 1 )
              midi_events$subEvent[i] <- eventType
              midi_events$subevent_name[i] <- midiMetaEventName( eventType )
              eventVarLength <- readVarLength( file.con )
              eventLength <- eventVarLength[ 1 ]
              midiData <- switch( as.character( eventType ) ,
                                  "00" = readBin( file.con, integer(0), n = 1, size = eventLength-1, endian = "big", signed = FALSE ), # sequence number
                                  "01" = readBin( file.con, character(0), size=eventLength ), #  text event
                                  "02" = readBin( file.con, character(0), size=eventLength ), #  copyright notice
                                  "03" = readBin( file.con, character(0), size=eventLength ), #  track name
                                  "04" = readBin( file.con, character(0), size=eventLength ), #  instrument name
                                  "05" = readBin( file.con, character(0), size=eventLength ), #  lyric
                                  "06" = readBin( file.con, character(0), size=eventLength ), #  marker
                                  "07" = readBin( file.con, character(0), size=eventLength ), #  cue point
                                  "08" = readBin( file.con, character(0), size=eventLength ), #  program name
                                  "09" = readBin( file.con, character(0), size=eventLength ), #  device name
                                  "20" = readBin(file.con, integer(0), n = 1, size = eventLength, endian = "big", signed = FALSE), #  MIDI channel prefix
                                  "21" = readBin(file.con, integer(0), n = 1, size = eventLength, endian = "big", signed = FALSE), #  MIDI port
                                  #"2f" - end of track, nothing else to read
                                  "51" = (readBin(file.con, integer(0), n = 1, size = 1, endian = "big", signed = FALSE) * 256^2) + readBin(file.con, integer(0), n = 1, size = 2, endian = "big", signed = FALSE), # set tempo
                                  "54" = readBin(file.con, integer(0), n = 5, size = 1, endian = "big", signed = FALSE), #  SMPTE offset
                                  "58" = readBin(file.con, integer(0), n = 4, size = 1, endian = "big", signed = FALSE), #  time signature
                                  "59" = readBin(file.con, integer(0), n = 2, size = 1, endian = "big", signed = TRUE), #  key signature
                                  "7f" = readBin( file.con, raw(), n=eventLength-1, size=1 ) #  meta-event
              )
              
              #               event type + length_bytes + length
              bytes <- bytes + 1 + eventVarLength[2] + eventVarLength[1]
            }
          )
      } else {
        channel <- strtoi( substr( event, 2, 2 ), base=16 )
        midi_events$channel <- channel
        midiData <- switch( event.code , 
                            "8" = readBin(file.con, integer(0), n = 2, size = 1, endian = "big", signed = FALSE), # note off
                            "9" = readBin(file.con, integer(0), n = 2, size = 1, endian = "big", signed = FALSE), # note on
                            "a" = readBin(file.con, integer(0), n = 2, size = 1, endian = "big", signed = FALSE), # after touch
                            "b" = readBin(file.con, integer(0), n = 2, size = 1, endian = "big", signed = FALSE), # control change
                            "c" = readBin(file.con, integer(0), n = 1, size = 1, endian = "big", signed = FALSE), # program change
                            "d" = readBin(file.con, integer(0), n = 1, size = 1, endian = "big", signed = FALSE), # channel pressure
                            "e" = { readBin(file.con, integer(0), n = 1, size = 1, endian = "big", signed = FALSE) +  
                                    readBin(file.con, integer(0), n = 2, size = 1, endian = "big", signed = FALSE) * 2^7 } # pitch wheel
        ) 
        bytes <- bytes <- bytes + length( midiData )
      }
      switch( typeof(midiData) ,
              "character" = { midi_events$data.string[i] <- midiData } ,
              "raw" = { midi_events$data.raw[i] <- paste(midiData,collapse=" ") } ,
              "integer" = { 
                if ( length(midiData) >=1 ) { midi_events$data.int1[i] <- midiData[1] } 
                if ( length(midiData) >=2 ) { midi_events$data.int2[i] <- midiData[2] } 
                if ( length(midiData) >=3 ) { midi_events$data.int3[i] <- midiData[3] } 
                if ( length(midiData) >=4 ) { midi_events$data.int4[i] <- midiData[4] } 
                if ( length(midiData) >=5 ) { midi_events$data.int5[i] <- midiData[5] } 
              }
      )
    }
    midi_tracks[[track]] <- midi_events[ 1:i , ]
  }
  
  return( list( header=midi_header, tracks=midi_tracks ) )
}

writeMidiFile <- function( midiObject, midiFile ) {
  #midiObject=midi
  #midiFile=midi.out
  #midiObject=x
  #midiFile="test.mid"
  file.con <- file( description = midiFile, open = "wb" )

  midiObject <- updateDeltaTime( midiObject )
  
  midi_header <- midiObject$header
  midi_tracks <- midiObject$tracks
  
  ###############################-
  # MIDI header
  # chunk type: 4 chars == 'MThd'
  # length: 32 bit number == 6
  # format: 16 bit number == 0, 1, or 2
  # ntrks: 16 bit number == number of tracks
  # division: 16 bit coded number
  
  writeChar( "MThd", file.con, eos=NULL )
  writeBin( as.integer(6), file.con, size = 4, endian = "big" )
  writeBin( as.integer(midi_header$format), file.con, size = 2, endian = "big" )
  writeBin( length(midi_tracks), file.con, size = 2, endian = "big" )
  
  # DIVISION - 16 bit integer
  # +ve:	ticks per quarter-note
  # -ve: low byte - ticks per frame, high byte - the number of frames per second (one of the four values -24, -25, -29, or -30, corresponding to the four standard SMPTE and MIDI Time Code formats)
  MThd_division <- ifelse( 
                      midi_header$ticksPerQN>0, 
                      midi_header$ticksPerQN, 
                      midi_header$ticksPerFrame + framesPerSecond*2^8 )
  writeBin( as.integer(MThd_division), file.con, size = 2, endian = "big" )
  close( file.con )
  
  #############################
  # write tracks

  track <- 1
  for( track in 1:midi_header$number_tracks ) {
    print(paste("writing track",track))
    midi_events <- midi_tracks[[ track ]]
    bytes <- 0
    track.con <- file( description = "_track.mid", open = "wb" )
    i <- 1
    for ( i in 1:nrow(midi_events) ) {
      if ( is.na( midi_events$event[ i ] ) ) { next }
      delta_time <- writeVarLength( midi_events$delta.time[i], track.con )
      event.code <- substring( midi_events$event[i], 1, 1 )
      event.int <- strtoi( event.code, base = 16 )
      print( paste( "track=" , track , "event=" , i , "delta_time=" , midi_events$delta.time[i] , "event_code=" , midi_events$event[i] , "bytes=" , bytes ) )
      bytes <- bytes + delta_time[ 2 ] + 1
      if ( event.code == "f" ) { # special
        writeBin( midi_events$event[i] , track.con )
        midiData <- NA
        switch( as.character( midi_events$event[i] ) , 
                "f0" = { # sysex event
                  varLength <- length(midiData) + 1
                  writeBin( as.integer( midi_events$data.int1[i] ) , track.con, size = 1, endian = "big" )
                  if ( !is.na( midi_events$data.int2[i] ) ) {
                    writeBin( as.integer( midi_events$data.int2[i] ) , track.con, size = 1, endian = "big" )
                    varLength <- varLength + 1
                  }
                  if ( !is.na( midi_events$data.int3[i] ) ) {
                    writeBin( as.integer( midi_events$data.int3[i] ) , track.con, size = 1, endian = "big" )
                    varLength <- varLength + 1
                  }
                  midiData <- as.raw( strtoi( strsplit( midi_events$data.raw[i], " " )[[1]], base=16 ) ) 
                  eventLength <- writeVarLength( varLength , track.con )
                  writeBin( midiData , track.con )
                  #               length_bytes + length
                  bytes <- bytes + eventLength[2] + eventLength[1]
                } ,
                "f2" = { # song position
                  writeBin( bitwAnd( midi_events$data.int1[i] , 2^7 - 1 ) , track.con, size = 1, endian = "big" )
                  writeBin( bitwAnd( midi_events$data.int1[i] , 2^14 - (2^7 - 1)  ) / 2^7 , track.con, size = 1, endian = "big" )
                  bytes <- bytes + 2
                } ,
                "f3" = { # song select
                  writeBin( midi_events$data.int1[i] , track.con, size = 1, endian = "big" )
                  bytes <- bytes + length( 1 )
                } ,
                "f7" = { # sysex event
                  midiData <- as.raw( strtoi( strsplit( midi_events$data.raw[i], " " )[[1]], base=16 ) ) 
                  eventLength <- writeVarLength( length(midiData) , track.con )
                  writeBin( midiData , track.con )
                  #               length_bytes + length
                  bytes <- bytes + eventLength[2] + eventLength[1]
                } ,
                "ff" = { # meta event
                  writeBin( midi_events$subEvent[i], track.con )
                  switch( as.character( midi_events$subEvent[i] ) ,
                                      "00" = { eventLength <- 2
                                               midiData <- as.integer( midi_events$data.int1[i] ) }, # sequence number
                                      "01" = { eventLength <- nchar(midi_events$data.string[i])+1
                                               midiData <- midi_events$data.string[i] }, # sequence number
                                      "02" = { eventLength <- nchar(midi_events$data.string[i])+1
                                               midiData <- midi_events$data.string[i] }, #  copyright notice
                                      "03" = { eventLength <- nchar(midi_events$data.string[i])+1
                                               midiData <- midi_events$data.string[i] }, #  track name
                                      "04" = { eventLength <- nchar(midi_events$data.string[i])+1
                                               midiData <- midi_events$data.string[i] }, #  instrument name
                                      "05" = { eventLength <- nchar(midi_events$data.string[i])+1
                                               midiData <- midi_events$data.string[i] }, #  lyric
                                      "06" = { eventLength <- nchar(midi_events$data.string[i])+1
                                               midiData <- midi_events$data.string[i] }, #  marker
                                      "07" = { eventLength <- nchar(midi_events$data.string[i])+1
                                               midiData <- midi_events$data.string[i] }, #  cue point
                                      "08" = { eventLength <- nchar(midi_events$data.string[i])+1
                                               midiData <- midi_events$data.string[i] }, #  program name
                                      "09" = { eventLength <- nchar(midi_events$data.string[i])+1
                                               midiData <- midi_events$data.string[i] }, #  device name
                                      "20" = { eventLength <- 1
                                               midiData <- as.integer( midi_events$data.int1[i] ) }, #  MIDI channel prefix
                                      "21" = { eventLength <- 1
                                               midiData <- as.integer( midi_events$data.int1[i] ) }, #  MIDI port
                                      "2f" = { eventLength <- 1
                                               midiData <- NA },# end of track
                                      "51" = { eventLength <- 3
                                               midiData <- as.integer( midi_events$data.int1[i] ) }, # set tempo
                                      "54" = { eventLength <- 5
                                               midiData <- c( as.integer( midi_events$data.int1[i] ) , as.integer( midi_events$data.int2[i] ) , as.integer( midi_events$data.int3[i] ) , as.integer( midi_events$data.int4[i] ) , as.integer( midi_events$data.int5[i] ) ) }, #  SMPTE offset
                                      "58" = { eventLength <- 4
                                               midiData <- c( as.integer( midi_events$data.int1[i] ) , as.integer( midi_events$data.int2[i] ) , as.integer( midi_events$data.int3[i] ) , as.integer( midi_events$data.int4[i] ) ) }, #  time signature
                                      "59" = { eventLength <- 2
                                               midiData <- c( as.integer( midi_events$data.int1[i] ) , as.integer( midi_events$data.int2[i] ) ) }, #  key signature
                                      "7f" = { eventLength <- length(midi_events$data.raw[i])
                                               midiData <- as.raw( midi_events$data.raw[i] ) } #  meta-event
                  )
                  if (length(midiData) == 1 && is.na( midiData ) ) {
                    writeBin( as.integer(0), track.con, size = 1, endian = "big" ) 
                    eventVarLength <- c(0,1)
                  } else {
                    eventVarLength <- writeVarLength( eventLength, track.con )
                    if ( length( midiData ) == 1 ) {
                      writeBin( midiData, track.con, size = eventLength, endian = "big" ) 
                    } else {
                      j = 1
                      for (j in 1:length(midiData)) {
                        writeBin( midiData[j], track.con, size = eventLength/length(midiData), endian = "big" ) 
                      }
                    }
                  }
                  bytes <- bytes + 1 + eventVarLength[2] + eventVarLength[1]
                }
        )
      } else {
        channel <- substring( midi_events$event[i], 2, 2 )
        if ( channel!='0' && midi_events$channel[i]==0 ) {
          midi_events$channel[i] <- strtoi( channel, base=16 )
        }
        writeBin( as.raw( strtoi( paste( "0x", event.code, midi_events$channel[i], sep="" ) ) ), track.con, size=1, endian = "big" )
        midiData <- switch( event.code , 
                  "8" = { midiData <- c( as.integer( midi_events$data.int1[i] ) , as.integer( midi_events$data.int2[i] ) ) }, # note off
                  "9" = { midiData <- c( as.integer( midi_events$data.int1[i] ) , as.integer( midi_events$data.int2[i] ) ) }, # note on
                  "a" = { midiData <- c( as.integer( midi_events$data.int1[i] ) , as.integer( midi_events$data.int2[i] ) ) }, # after touch
                  "b" = { midiData <- c( as.integer( midi_events$data.int1[i] ) , as.integer( midi_events$data.int2[i] ) ) }, # control change
                  "c" = { midiData <- as.integer( midi_events$data.int1[i] ) }, # program change
                  "d" = { midiData <- as.integer( midi_events$data.int1[i] ) }, # channel pressure
                  "e" = { midiData <- c( bitwAnd( as.integer( midi_events$data.int1[i], 2^7-1 ) ) , bitwShiftR( as.integer( midi_events$data.int2[i], 7 ) ) ) } # pitch wheel
        ) 
        if ( length( midiData ) >0  ) {
            if ( length( midiData ) ==1 ) {
            writeBin( midiData, track.con, size = 1, endian = "big" ) 
          } else {
            for (j in 1:length(midiData)) {
              writeBin( midiData[j], track.con, size = 1, endian = "big" ) 
            }
          }
        }
        bytes <- bytes + 1 + length(midiData)
      }
    }
    close( track.con )
    
    # write track header
    file.con <- file( description = midiFile, open = "ab" )
    MTrk <- writeChar( "MTrk", file.con, eos=NULL )
    #eventVarLength <- writeVarLength( bytes, file.con )
    writeBin( as.integer(file.size( "_track.mid ")), file.con, size = 4, endian = "big" )
    close( file.con )
    
    # add track
    file.append( midiFile , "_track.mid" )
    #file.remove( "_track.mid" )
  }
  
  return( 0 )
}

################################################
# controller mapping


