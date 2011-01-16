## Functions for producing DTMF tones and using them to "listen" to
## Benford's Law analysis
## All code is released as GPL >= 2.
## By Ethan Brown except where noted below

require(tuneR)

DTMFtone <- function(DTMFchar, duration = 0.5, bit = 16,
                     samp.rate = 44100, waveform="sine") {
  ## Returns a Wave object of *duration* seconds
  ## for the given DTMF character

  ## Frequencies taken from
  ## http://en.wikipedia.org/wiki/Dual-tone_multi-frequency_signaling
  
  ## Create "keypad" to look up values in

  keypad <- cbind(matrix(c(1:9, "*", "0", "#"),
                         nrow = 4, ncol = 3, byrow = TRUE),
                  LETTERS[1:4])

  ## Assign frequencies
  
  lowfreqs <- c(697, 770, 852, 941)
  hifreqs <- c(1209, 1336, 1477, 1633)

  ## Find coordinates in matrix of input value
  ## and find the corresponding frequencies
  ## or an error if nothing present
    
  ind <- which(DTMFchar == keypad, arr.ind = TRUE)
  if(is.na(ind[1])) stop("Invalid DTMF string value.")

  freqs <- c(lowfreqs[ind[1]], hifreqs[ind[2]])

  ## Choose waveform
  if(waveform == "sine") (wavetype <- sine)
  if(waveform == "square") (wavetype <- square)
  if(waveform == "sawtooth") (wavetype <- sawtooth)
  
  ## Generate waves and add together
  
  tone <- 0.5 * wavetype(freqs[1], duration = duration,
                     bit = bit, samp.rate = samp.rate,
                     xunit = "time") + 0.5 * wavetype(
                       freqs[2], duration = duration,
                       bit = bit, samp.rate = samp.rate,
                       xunit = "time")
  return(tone)
}

## Just for fun--create a wave object "dialing" a string
## each with *duration* and *gap* seconds in between each tone

dial <- function(DTMFstring, duration = 0.5, gap = 0.2) {
  DTMFvect <- strsplit(DTMFstring, split = c())[[1]]
  silencegap <- silence(duration = gap, samp.rate = 44100,
                           bit = 16, xunit = "time")

  DTMFout <- DTMFtone(DTMFvect[1], duration)
  
  for(i in 2:length(DTMFvect)) {
    DTMFout <- bind(DTMFout, silencegap)
    DTMFout <- bind(DTMFout, DTMFtone(DTMFvect[i], duration))
  }

  return(DTMFout)
}
    
# Benford code below is based on Drew Conway's Wikileaks_Analysis
# Available at http://github.com/drewconway/WikiLeaks_Analysis/blob/master/wikileaks_analysis.R

# Given an integer and a base, returns the leading "digit" in a base.
leading.dig <- function(x, base = 10) {
  highest.power <- floor(log(x, base = base))
  largest.multiple <- floor(x / base^highest.power)
  return(largest.multiple)
}

CountDigits <- function(x, base = 10) {
  ## Count digits and store as data frame
  ## Elements equal to zero are not counted.

  if(all(x==0)) stop("All elements of input are equal to zero, so leading digits are not defined.")
  dig.count <- as.data.frame(table(sapply(as.vector(x),leading.dig, base = base)))
  names(dig.count) <- c("Digit", "DigitCount")
  dig.count$Digit <- as.numeric(levels(dig.count$Digit))

  ## Check to see whether dig.count has all digits and
  ## if not, add the missing ones to the data frame:

  if(length(dig.count$Digit) == (base - 1)) return(dig.count) else {
    dig.missing <- (1:(base - 1))[-dig.count$Digit]
    dig.count[(length(dig.count$Digit) + 1):(base -1), "Digit"] <- dig.missing
    dig.count$DigitCount[is.na(dig.count$DigitCount)] <- 0
    reordered <- dig.count[order(dig.count$Digit),]  
    rownames(reordered) <- NULL
    return(reordered)
  }
}



# Benford's distribution
dbenford<-function(d, base) {
    return(log(1+(1/d),base=base))
}

DTMFBenford <- function(x = NA, total.length = 10) {
  ## Takes an input numeric vector, calculates the
  ## leading digit (in base 10) and returns
  ## a Wave object corresponding to the DTMF realization
  ## of those proportions, where each leading digit is played
  ## for the proportion of *total.length* that it appears in the
  ## dataset.

  ## If no input vector, or input vector all NAs,
  ## return mono DTMF Benford distribution realization
  
  if(all(is.na(x))) {
    DTMFBenf <- DTMFtone(1, duration = dbenford(1, 10)*10)
    for(i in 2:9) {
      DTMFBenf <- bind(DTMFBenf,
                          DTMFtone(i, dbenford(i,10)*10))
    }
    return(DTMFBenf)
  }
      
  lengths <- (CountDigits(x)[,2]/sum(CountDigits(x)[,2])
                                    ) * total.length

  ## Some tasty inefficient looping to generate
  ## Wave objects corresponding to the sample
  ## and Benford's distribution

  
  DTMFsamp <- DTMFtone(1, duration = lengths[1])
  DTMFBenf <- DTMFtone(1, duration = dbenford(1, 10)*10)
  
  for(i in 2:9) {
    DTMFsamp <- bind(DTMFsamp, DTMFtone(i, lengths[i]))
    DTMFBenf <- bind(DTMFBenf,
                     DTMFtone(i, dbenford(i,10)*10))
  }

  ## The lengths may be slightly off due to numerical
  ## imprecision; so we need to add silence at the very end
  ## (probably a tiny fraction of a second) to make the two
  ## files exactly equal
  
  diff.length <- length(DTMFsamp) - length(DTMFBenf)
  if(diff.length > 0 ) {
    DTMFBenf <- bind(DTMFBenf, silence(duration = diff.length,
                                             bit=16, samp.rate = 44100))
  } else if(diff.length < 0) {
    DTMFsamp <- bind(DTMFBenf, silence(duration = diff.length,
                                          bit=16, samp.rate = 44100))
  }

  ## Return a stereo Wave object with the sample proportions
  ## in the left channel, and the ideal Benford proportions
  ## on the right
  
  return(stereo(DTMFsamp, DTMFBenf))
}


  

  

  
  
