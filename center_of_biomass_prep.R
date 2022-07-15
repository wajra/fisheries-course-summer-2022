# 2022-07-02
# Preparing dataset for the summer course
# This is not important from the view of a studnet
# I'm putting this here for completeness
library(mgcv)
library(dismo)
library(beepr)


# This is the dataset named 'dat'
load('data/master_hauls_March7_2017.RData') # import master hauls file
# This is the dataset named 'hauls'
load('data/dat_selectedspp_Feb_1_2017.Rdata')# load species catch data


hauls <- hauls[!(hauls$region %in% c("VIMS_NEAMAP","SEFSC_GOMex")),]


# Here they simply say that all other rows apart from those having a 'wtcpue' value of 0 and 'region'
# value of 'DFO_SoGulf' are actual absences therefore can be removed from the dataframe
dat <- dat[!(dat$wtcpue == 0 & dat$region == 'DFO_SoGulf'),] # the zeros in SoGulf are actual zeros (ie not just a scale issue) and thus are true absences
# And then the remaining zero values of 'wtcpue' are set to 0.0002 because they do have a value. They are not absences.
# They are barely there
dat$wtcpue[dat$wtcpue == 0] <- 0.0002 # 'zeros' in dat are now species too light to register on scales_here a value below the lowest non-zero value is assigned_for transforming data
# Then we check for the species ('sppocean' column) named 'calappa sulcata in Atlantic Ocean' and
# check if there 'wtcpue' is missing. If so we set it to 0.13.
dat$wtcpue[dat$sppocean=="calappa sulcata_Atl" & is.na(dat$wtcpue)] <- 0.13 # the median weight of this species when observed
# Then a new column is created named 'logwtcpue' and log values of 'wtcpue' is set to it
dat$logwtcpue <- log(dat$wtcpue)

# drop dubious GMex observations of temperate species or species that have a Gulf of Mexico endemic congener that closely resembles the Atlantic coast species
# So what we have here is a list or vector of a set of species that are in both Gulf of Mexico and Atlantic Ocean
drops <- c('alosa pseudoharengus_Gmex', 'brevoortia tyrannus_Gmex', 'clupea harengus_Gmex', 'dipturus laevis_Gmex', 'paralichthys dentatus_Gmex',
           'hippoglossoides platessoides_Gmex', 'pseudopleuronectes americanus_Gmex', 'scomber scombrus_Gmex', 'cryptacanthodes maculatus_Gmex',
           'echinarachnius parma_Gmex', 'illex illecebrosus_Gmex', 'melanostigma atlanticum_Gmex', 'menidia menidia_Gmex', 'ovalipes ocellatus_Gmex','placopecten magellanicus_Gmex')
# 'gsub' is a function that simply substitutes a substring for another substring in a string or vector
# USAGE
# gsub(pattern, replacement, x, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
drops <- gsub('_Gmex', '_Atl', drops)

# OK. So here we need to understand some context. 'SEFSC_GOMexFall' is the Gulf of Mexico survey
# This survey went from 1982-2014 and occured in the Summer and Fall.
# So what needs to be done is see whether a row has 'region' belonging to these surveys and seasons and
# see whether the 'sppocean' value is in the 'drops' vector
# Rows that fall into this classification are filtered out using !
dat <- dat[!(dat$region=='SEFSC_GOMexFall' & dat$sppocean %in% drops),]
# Same for Summery surveys
dat <- dat[!(dat$region=='SEFSC_GOMexSummer' & dat$sppocean %in% drops),]

# trim columns that are already in master hauls file, which will be merged in below with the hauls data
# Creating a new data frame using columns and data from 'dat' data frame
dat <- data.frame(haulid = dat$haulid, sppocean = dat$sppocean, Freq = dat$Freq, wtcpue = dat$wtcpue, logwtcpue = dat$logwtcpue, presfit = TRUE, stringsAsFactors = F)
# Here the rows that have'sppocean'  value of 'NO CATCH' will be filtered out
dat <- dat[!dat$sppocean=='NO CATCH',]
#dat <- dat[!is.na(dat$wtcpue),] # drop NAs as it creates errors for some species. May want to go back and manually do 'oncorhynchus tshawytscha_Pac' as almost 10% of presence records have NA for wtcpue (tagging study?)
# Found a species naming error_correcting here
dat$sppocean[dat$sppocean=='heppasteria phygiana_Atl'] <- 'hippasteria phrygiana_Atl' # no need to aggregate as they never overlapped w/n hauls (only found in Newfoundland and they called it one way or the other)


# Merge everything
sp_data <- merge(dat, hauls, by='haulid', all.x = T, sort=F) # Add empty hauls

# From these we select only the Atlantic Species and the ones relevant to the North Atlantic
ne_regions <- c("NEFSC_NEUS","DFO_ScotianShelf","VIMS_NEAMAP")
sp_data <- sp_data[sp_data$region %in% ne_regions,]

# We only need a few columns - Species, Region, Year, Month, Longitude, Latitude, Depth, wtcpue
sp_data <- subset(sp_data,select=c("sppocean","region","year","month","lat","lon","depth","wtcpue"))
# Remove invalid entries (Missing values)
# Using 'complete.cases' function
sp_data <- sp_data[complete.cases(sp_data), ,]

# Save this as a CSV
write.csv(sp_data,file="data/summer_course_dataset.csv")
