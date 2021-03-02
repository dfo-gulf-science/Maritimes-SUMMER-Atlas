## generate the cumulative distribution function for catch and for the environmental variable specified by "env.var"
## currently supports depth, bottom temperature and bottom salinity
##
generate.cumul.dist <- function(catch.in, env.var) {
# fn <- file.path(path.ATLAS, "/Data/SS10_catch.csv")
# catch.in <- read.csv(fn, header=TRUE)

# strata statistics
## strata statistics
# qu <- paste("
# select 
# *
# from
# groundfish.gsstratum
# ", sep="")

# strata.stats.df <- sqlQuery(chan, qu)

# write.csv(strata.stats.df, file=file.path(main.path, "Figures-Data/DFO-strata-statistics.csv"), row.names=FALSE)
strata.stats.df <-read.csv(file=file.path(main.path, "Figures-Data/DFO-strata-statistics.csv")) ## static file, to do this outside of BIO

summer.strata.stats.df <- subset(
strata.stats.df, 
(STRAT == '440' | STRAT == '441' | STRAT == '442' | STRAT == '443' | STRAT == '444' | STRAT == '445' | STRAT == '446' | STRAT == '447' | STRAT == '448' | STRAT == '449' | 
STRAT == '450' | STRAT == '451' | STRAT == '452' | STRAT == '453' | STRAT == '454' | STRAT == '455' | STRAT == '456' | STRAT == '457' | STRAT == '458' | STRAT == '459' | 
STRAT == '460' | STRAT == '461' | STRAT == '462' | STRAT == '463' | STRAT == '464' | STRAT == '465' | STRAT == '466' | 
STRAT == '470' | STRAT == '471' | STRAT == '472' | STRAT == '473' | STRAT == '474' | STRAT == '475' | STRAT == '476' | STRAT == '477' | STRAT == '478' | 
STRAT == '480' | STRAT == '481' | STRAT == '482' | STRAT == '483' | STRAT == '484' | STRAT == '485' | 
STRAT == '490' | STRAT == '491' | STRAT == '492' | STRAT == '493' | STRAT == '494' | STRAT == '495' ) 
)

summer.strata.tt <- summer.strata.stats.df[order(summer.strata.stats.df$STRAT),]
summer.strat <- data.frame(Strata=summer.strata.tt$STRAT, area=summer.strata.tt$AREA, NH=summer.strata.tt$AREA/0.01)
summer.strat$prop.area <- summer.strat$area / sum(summer.strat$area)

# number of tows per year-stratum
n.tows.yr.strat <- table(catch.in$YEAR, catch.in$Strata)

my.yrs <- as.integer(attr(n.tows.yr.strat, "dimnames")[[1]])
my.strat <- as.integer(attr(n.tows.yr.strat, "dimnames")[[2]])

df.n.tows.yr.strat <- expand.grid(my.yrs, my.strat)
names(df.n.tows.yr.strat) <- c("YEAR","Strata")
df.n.tows.yr.strat$ntows <- c(n.tows.yr.strat)

catch.in.merged <- merge(merge(catch.in, summer.strat, by="Strata"), df.n.tows.yr.strat, by = c("YEAR","Strata"))

catch.in.merged$I.strat <- (catch.in.merged$prop.area / catch.in.merged$ntows)

##
switch(env.var,
	depth = {
	
		breaks <- seq(0,300)
		cuts <- cut(catch.in.merged$DEPTH, breaks, right=FALSE)
		freq <- table(cuts)
		cumfreq <- c(0, cumsum(freq))
		cumfreq <- (cumfreq/max(cumfreq))*100
		
		# include the stratified design
		catch.in.merged$depth.interval <- cuts
		samples.depth.weighted <- aggregate((prop.area/ntows)~depth.interval, data=catch.in.merged, sum)
		samples.depth.weighted$freq <- samples.depth.weighted[,2]/sum(samples.depth.weighted[,2])
		#samples.depth.weighted$cumfreq <- cumsum(samples.depth.weighted$freq) * 100

		# catch records
		cuts.catch <- cut(subset(catch.in.merged, totno > 0)$DEPTH, breaks, right=FALSE)
		freq.catch <- table(cuts.catch)
		cumfreq.catch <- c(0, cumsum(freq.catch))
		cumfreq.catch <- (cumfreq.catch/max(cumfreq.catch))*100
		
		catch.depth.weighted <- aggregate((prop.area/ntows)~depth.interval, data=subset(catch.in.merged, totno > 0), sum)
		catch.depth.weighted$freq <- catch.depth.weighted[,2]/sum(catch.depth.weighted[,2])
		#catch.depth.weighted$cumfreq <- cumsum(catch.depth.weighted$freq)*100
		
		#plot(cumfreq~depth.interval, data=samples.depth.weighted,type='b', pch=20, col='black')
		#t.df <- data.frame(depth.interval=factor(names(cumfreq), levels=names(cumfreq)), cumfreq=cumfreq)
		#points(cumfreq~depth.interval, data=t.df, type='b', col='red', pch=20)
		
		## generate table with 5%, 25%, 50%, 75% and 95% breaks
		# d.5 <- substring(noquote(strsplit(noquote(strsplit(names(which(cumfreq.catch>=5)[1]),","))[[1]]," ")[[1]]),2,4)
		# d.25 <- substring(noquote(strsplit(noquote(strsplit(names(which(cumfreq.catch>=25)[1]),","))[[1]]," ")[[1]]),2,4)
		# d.50 <- substring(noquote(strsplit(noquote(strsplit(names(which(cumfreq.catch>=50)[1]),","))[[1]]," ")[[1]]),2,4)
		# d.75 <- substring(noquote(strsplit(noquote(strsplit(names(which(cumfreq.catch>=75)[1]),","))[[1]]," ")[[1]]),2,4)
		# d.95 <- substring(noquote(strsplit(noquote(strsplit(names(which(cumfreq.catch>=95)[1]),","))[[1]]," ")[[1]]),2,4)
		# my.df <- data.frame(Freq=c("F5","F25","F50","F75","F95"), Depth=as.numeric(c(d.5, d.25, d.50, d.75, d.95)))
		

		both.df <- merge(merge(samples.depth.weighted, catch.depth.weighted, by="depth.interval", all.x=TRUE), data.frame(depth.interval = names(cumfreq)), by="depth.interval", all.y=TRUE)
		names(both.df) <- c("depth.interval","prop.area.samples","freq.samples","prop.area.catch","freq.catch")
		both.df$depth <- breaks
		both.df$freq.catch <- ifelse(is.na(both.df$freq.catch),0,both.df$freq.catch)
		both.df$freq.samples <- ifelse(is.na(both.df$freq.samples),0,both.df$freq.samples)
		
		both.df$cumfreq.samples <- (cumsum(both.df$freq.samples) / sum(both.df$freq.samples)) * 100
		both.df$cumfreq.catch <- (cumsum(both.df$freq.catch) / sum(both.df$freq.catch)) * 100
		
		d.5 <- substring(noquote(strsplit(noquote(strsplit(as.character(both.df[both.df$cumfreq.catch>=5,]$depth.interval[1]),",")[[1]])," ")[[1]]),2,4)
		d.25 <- substring(noquote(strsplit(noquote(strsplit(as.character(both.df[both.df$cumfreq.catch>=25,]$depth.interval[1]),",")[[1]])," ")[[1]]),2,4)
		d.50 <- substring(noquote(strsplit(noquote(strsplit(as.character(both.df[both.df$cumfreq.catch>=50,]$depth.interval[1]),",")[[1]])," ")[[1]]),2,4)
		d.75 <- substring(noquote(strsplit(noquote(strsplit(as.character(both.df[both.df$cumfreq.catch>=75,]$depth.interval[1]),",")[[1]])," ")[[1]]),2,4)
		d.95 <- substring(noquote(strsplit(noquote(strsplit(as.character(both.df[both.df$cumfreq.catch>=95,]$depth.interval[1]),",")[[1]])," ")[[1]]),2,4)
		my.df <- data.frame(Freq=c("F5","F25","F50","F75","F95"), Depth=as.numeric(c(d.5, d.25, d.50, d.75, d.95)))

		#		return(list(df.freq=data.frame(depth=breaks, cum.catch=cumfreq.catch, cum=cumfreq), df.tab=my.df))
		return(list(df.freq=data.frame(depth=both.df$depth, cum.catch=both.df$cumfreq.catch, cum=both.df$cumfreq.samples), df.tab=my.df))
		
	},
	
	temperature = {
		breaks <- seq(-1,25,0.1)
		cuts <- cut(catch.in$temperature, breaks, right=FALSE)
		freq <- table(cuts)
		cumfreq <- c(0, cumsum(freq))
		cumfreq <- (cumfreq/max(cumfreq))*100

		# include the stratified design
		catch.in.merged$temperature.interval <- cuts
		samples.temperature.weighted <- aggregate((prop.area/ntows)~temperature.interval, data=catch.in.merged, sum)
		samples.temperature.weighted$freq <- samples.temperature.weighted[,2]/sum(samples.temperature.weighted[,2])
		
		# catch records
		cuts.catch <- cut(subset(catch.in, totno > 0)$temperature, breaks, right=FALSE)
		freq.catch <- table(cuts.catch)
		cumfreq.catch <- c(0, cumsum(freq.catch))
		cumfreq.catch <- (cumfreq.catch/max(cumfreq.catch))*100

		catch.temperature.weighted <- aggregate((prop.area/ntows)~temperature.interval, data=subset(catch.in.merged, totno > 0), sum)
		catch.temperature.weighted$freq <- catch.temperature.weighted[,2]/sum(catch.temperature.weighted[,2])

		## generate table with 5%, 25%, 50%, 75% and 95% breaks
		# d.5 <- substring(noquote(strsplit(noquote(strsplit(names(which(cumfreq.catch>=5)[1]),","))[[1]]," ")[[1]]),2,4)
		# d.25 <- substring(noquote(strsplit(noquote(strsplit(names(which(cumfreq.catch>=25)[1]),","))[[1]]," ")[[1]]),2,4)
		# d.50 <- substring(noquote(strsplit(noquote(strsplit(names(which(cumfreq.catch>=50)[1]),","))[[1]]," ")[[1]]),2,4)
		# d.75 <- substring(noquote(strsplit(noquote(strsplit(names(which(cumfreq.catch>=75)[1]),","))[[1]]," ")[[1]]),2,4)
		# d.95 <- substring(noquote(strsplit(noquote(strsplit(names(which(cumfreq.catch>=95)[1]),","))[[1]]," ")[[1]]),2,4)
		# my.df <- data.frame(Freq=c("F5","F25","F50","F75","F95"), Depth=as.numeric(c(d.5, d.25, d.50, d.75, d.95)))
		
		both.df <- merge(merge(samples.temperature.weighted, catch.temperature.weighted, by="temperature.interval", all.x=TRUE), data.frame(temperature.interval = names(cumfreq)), by="temperature.interval", all.y=TRUE)
		names(both.df) <- c("temperature.interval","prop.area.samples","freq.samples","prop.area.catch","freq.catch")
		both.df$temperature <- breaks
		both.df$freq.catch <- ifelse(is.na(both.df$freq.catch),0,both.df$freq.catch)
		both.df$freq.samples <- ifelse(is.na(both.df$freq.samples),0,both.df$freq.samples)
		
		both.df$cumfreq.samples <- (cumsum(both.df$freq.samples) / sum(both.df$freq.samples)) * 100
		both.df$cumfreq.catch <- (cumsum(both.df$freq.catch) / sum(both.df$freq.catch)) * 100
		
		d.5 <- substring(noquote(strsplit(noquote(strsplit(as.character(both.df[both.df$cumfreq.catch>=5,]$temperature.interval[1]),",")[[1]])," ")[[1]]),2,4)
		d.25 <- substring(noquote(strsplit(noquote(strsplit(as.character(both.df[both.df$cumfreq.catch>=25,]$temperature.interval[1]),",")[[1]])," ")[[1]]),2,4)
		d.50 <- substring(noquote(strsplit(noquote(strsplit(as.character(both.df[both.df$cumfreq.catch>=50,]$temperature.interval[1]),",")[[1]])," ")[[1]]),2,4)
		d.75 <- substring(noquote(strsplit(noquote(strsplit(as.character(both.df[both.df$cumfreq.catch>=75,]$temperature.interval[1]),",")[[1]])," ")[[1]]),2,4)
		d.95 <- substring(noquote(strsplit(noquote(strsplit(as.character(both.df[both.df$cumfreq.catch>=95,]$temperature.interval[1]),",")[[1]])," ")[[1]]),2,4)
		my.df <- data.frame(Freq=c("F5","F25","F50","F75","F95"), Depth=as.numeric(c(d.5, d.25, d.50, d.75, d.95)))

#		return(list(df.freq=data.frame(temperature=breaks, cum.catch=cumfreq.catch, cum=cumfreq), df.tab=my.df))
		return(list(df.freq=data.frame(temperature=both.df$temperature, cum.catch=both.df$cumfreq.catch, cum=both.df$cumfreq.samples), df.tab=my.df))
	
	},
	
	salinity = {
		breaks <- seq(30,37,0.01)
		cuts <- cut(catch.in$salinity, breaks, right=FALSE)
		freq <- table(cuts)
		cumfreq <- c(0, cumsum(freq))
		cumfreq <- (cumfreq/max(cumfreq))*100

		# include the stratified design
		catch.in.merged$salinity.interval <- cuts
		samples.salinity.weighted <- aggregate((prop.area/ntows)~salinity.interval, data=catch.in.merged, sum)
		samples.salinity.weighted$freq <- samples.salinity.weighted[,2]/sum(samples.salinity.weighted[,2])

		# catch records
		cuts.catch <- cut(subset(catch.in, totno > 0)$salinity, breaks, right=FALSE)
		freq.catch <- table(cuts.catch)
		cumfreq.catch <- c(0, cumsum(freq.catch))
		cumfreq.catch <- (cumfreq.catch/max(cumfreq.catch))*100

		catch.salinity.weighted <- aggregate((prop.area/ntows)~salinity.interval, data=subset(catch.in.merged, totno > 0), sum)
		catch.salinity.weighted$freq <- catch.salinity.weighted[,2]/sum(catch.salinity.weighted[,2])

		## generate table with 5%, 25%, 50%, 75% and 95% breaks
		# d.5 <- substring(noquote(strsplit(noquote(strsplit(names(which(cumfreq.catch>=5)[1]),","))[[1]]," ")[[1]]),2,6)
		# d.25 <- substring(noquote(strsplit(noquote(strsplit(names(which(cumfreq.catch>=25)[1]),","))[[1]]," ")[[1]]),2,6)
		# d.50 <- substring(noquote(strsplit(noquote(strsplit(names(which(cumfreq.catch>=50)[1]),","))[[1]]," ")[[1]]),2,6)
		# d.75 <- substring(noquote(strsplit(noquote(strsplit(names(which(cumfreq.catch>=75)[1]),","))[[1]]," ")[[1]]),2,6)
		# d.95 <- substring(noquote(strsplit(noquote(strsplit(names(which(cumfreq.catch>=95)[1]),","))[[1]]," ")[[1]]),2,6)
		# my.df <- data.frame(Freq=c("F5","F25","F50","F75","F95"), Depth=as.numeric(c(d.5, d.25, d.50, d.75, d.95)))
		
		both.df <- merge(merge(samples.salinity.weighted, catch.salinity.weighted, by="salinity.interval", all.x=TRUE), data.frame(salinity.interval = names(cumfreq)), by="salinity.interval", all.y=TRUE)
		names(both.df) <- c("salinity.interval","prop.area.samples","freq.samples","prop.area.catch","freq.catch")
		both.df$salinity <- breaks
		both.df$freq.catch <- ifelse(is.na(both.df$freq.catch),0,both.df$freq.catch)
		both.df$freq.samples <- ifelse(is.na(both.df$freq.samples),0,both.df$freq.samples)
		
		both.df$cumfreq.samples <- (cumsum(both.df$freq.samples) / sum(both.df$freq.samples)) * 100
		both.df$cumfreq.catch <- (cumsum(both.df$freq.catch) / sum(both.df$freq.catch)) * 100
		
		d.5 <- substring(noquote(strsplit(noquote(strsplit(as.character(both.df[both.df$cumfreq.catch>=5,]$salinity.interval[1]),",")[[1]])," ")[[1]]),2,4)
		d.25 <- substring(noquote(strsplit(noquote(strsplit(as.character(both.df[both.df$cumfreq.catch>=25,]$salinity.interval[1]),",")[[1]])," ")[[1]]),2,6)
		d.50 <- substring(noquote(strsplit(noquote(strsplit(as.character(both.df[both.df$cumfreq.catch>=50,]$salinity.interval[1]),",")[[1]])," ")[[1]]),2,6)
		d.75 <- substring(noquote(strsplit(noquote(strsplit(as.character(both.df[both.df$cumfreq.catch>=75,]$salinity.interval[1]),",")[[1]])," ")[[1]]),2,6)
		d.95 <- substring(noquote(strsplit(noquote(strsplit(as.character(both.df[both.df$cumfreq.catch>=95,]$salinity.interval[1]),",")[[1]])," ")[[1]]),2,6)
		my.df <- data.frame(Freq=c("F5","F25","F50","F75","F95"), Depth=as.numeric(c(d.5, d.25, d.50, d.75, d.95)))

#		return(list(df.freq=data.frame(salinity=breaks, cum.catch=cumfreq.catch, cum=cumfreq), df.tab=my.df))
		return(list(df.freq=data.frame(salinity=both.df$salinity, cum.catch=both.df$cumfreq.catch, cum=both.df$cumfreq.samples), df.tab=my.df))
	},
	
	NULL = {
	stop("This environmental variable is not recognised!\n")
	}
	
	)
	
} # end function
