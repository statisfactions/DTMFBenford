source("DTMFBenfordFunctions.R")

## Writes a 10-second WAV file where the proportional
## length of each tone is the Benford's Law frequency of
## the corresponding digit

writeWave(DTMFBenford(), "Benford.wav")

## Estonian web site metrics scraped from http://metrix.station.ee/
## by ScraperWiki user intgr
## May take time to download... is 37211 rows of 12 variables
Estonia <- read.csv("http://scraperwiki.com/scrapers/export/metrixstation/")


## Left ear is the observed frequencies of each digit;
## Right ear is the same as above, the theoretical Benford's Law
## frequency.
## Page Views--pretty close to Benford!

writeWave(DTMFBenford(Estonia$pageviews), "EstoniaPageViews.wav")

## Percentage of New Visitors--not so close to benford
## ... not in any way count data, so we wouldn't expect it.

writeWave(DTMFBenford(Estonia$newvisitors), "EstoniaNewVisitors_Percent.wav")

## But! If we transform the percentage to get the absolute
## number of new visitors to the web site, we're back to Benfordland.

writeWave(DTMFBenford((Estonia$newvisitors/100 * Estonia$visitors)
                      ), "EstoniaNewVisitors_Abs.wav")


