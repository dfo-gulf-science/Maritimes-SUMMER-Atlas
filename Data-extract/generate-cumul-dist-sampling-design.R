path.ATLAS=file.path("D:/Dropbox/ATLAS_poissons_SS") ## on my Acer notebook
 
fn <- file.path(path.ATLAS, "/Data/SS10_catch.csv")
catch.in <- read.csv(fn, header=TRUE)

strata.stats.df <-read.csv(file=file.path(path.ATLAS, "Data/DFO-strata-statistics.csv")) ## static file, to do this outside of BIO

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

oo <- order(catch.in.merged$Strata, catch.in.merged$setno)

catch.df <- subset(catch.in.merged, totno > 0)

samples.df <- subset(catch.in.merged[oo,], !(is.na(temperature)))

ss <- unique(samples.df$Strata)

## following Perry and Smith

# loop over depth breaks, the "t" in Perry and Smith
for(temp in seq(1:300)){

	# loop over strata, the "h" in Perry and Smith
	for(s in ss){
	tt <- unique(subset(samples.df, Strata==s)$setno) # tows in that stratum
	p.area <- subset(samples.df, Strata==s)$prop.area[1]
	n.tows <- dim(subset(samples.df, Strata==s))[1]
	ww <- p.area/n.tows
	
		# loop over tows, the "i" in Perry and Smith
		for(tt in tt){
		t.df <- subset(samples.df, Strata==s & setno==tt)
		rr <- dim(t.df)[1]
			# loop over records
			for(r in rr){
			ii.t <- ifelse(t.df$temperature<=temp,1,0) # indicator function
			ii.ww <- ww * ii.t # indicator function weighted by proportion area in stratum and number of tows
			print(c(temp, s, tt, r, ii.ww))
			} # end loop over records
		
		
		} # end loop over tows
	} # end loop over strata
} # end loop over depth breaks


		breaks <- seq(0,300)
		cuts <- cut(catch.in$DEPTH, breaks, right=FALSE)
		freq <- table(cuts)
		cumfreq <- c(0, cumsum(freq))
		cumfreq <- (cumfreq/max(cumfreq))*100

		cuts.catch <- cut(subset(catch.in, totno > 0)$DEPTH, breaks, right=FALSE)
		freq.catch <- table(cuts.catch)
		cumfreq.catch <- c(0, cumsum(freq.catch))
		cumfreq.catch <- (cumfreq.catch/max(cumfreq.catch))*100
		
		## generate table with 5%, 25%, 50%, 75% and 95% breaks
		d.5 <- substring(noquote(strsplit(noquote(strsplit(names(which(cumfreq.catch>=5)[1]),","))[[1]]," ")[[1]]),2,4)
		d.25 <- substring(noquote(strsplit(noquote(strsplit(names(which(cumfreq.catch>=25)[1]),","))[[1]]," ")[[1]]),2,4)
		d.50 <- substring(noquote(strsplit(noquote(strsplit(names(which(cumfreq.catch>=50)[1]),","))[[1]]," ")[[1]]),2,4)
		d.75 <- substring(noquote(strsplit(noquote(strsplit(names(which(cumfreq.catch>=75)[1]),","))[[1]]," ")[[1]]),2,4)
		d.95 <- substring(noquote(strsplit(noquote(strsplit(names(which(cumfreq.catch>=95)[1]),","))[[1]]," ")[[1]]),2,4)
		my.df <- data.frame(Freq=c("F5","F25","F50","F75","F95"), Depth=as.numeric(c(d.5, d.25, d.50, d.75, d.95)))
		
		return(list(df.freq=data.frame(depth=breaks, cum.catch=cumfreq.catch, cum=cumfreq), df.tab=my.df))
	},
	
	temperature = {
		breaks <- seq(-1,25,0.1)
		cuts <- cut(catch.in$temperature, breaks, right=FALSE)
		freq <- table(cuts)
		cumfreq <- c(0, cumsum(freq))
		cumfreq <- (cumfreq/max(cumfreq))*100

		cuts.catch <- cut(subset(catch.in, totno > 0)$temperature, breaks, right=FALSE)
		freq.catch <- table(cuts.catch)
		cumfreq.catch <- c(0, cumsum(freq.catch))
		cumfreq.catch <- (cumfreq.catch/max(cumfreq.catch))*100

		## generate table with 5%, 25%, 50%, 75% and 95% breaks
		d.5 <- substring(noquote(strsplit(noquote(strsplit(names(which(cumfreq.catch>=5)[1]),","))[[1]]," ")[[1]]),2,4)
		d.25 <- substring(noquote(strsplit(noquote(strsplit(names(which(cumfreq.catch>=25)[1]),","))[[1]]," ")[[1]]),2,4)
		d.50 <- substring(noquote(strsplit(noquote(strsplit(names(which(cumfreq.catch>=50)[1]),","))[[1]]," ")[[1]]),2,4)
		d.75 <- substring(noquote(strsplit(noquote(strsplit(names(which(cumfreq.catch>=75)[1]),","))[[1]]," ")[[1]]),2,4)
		d.95 <- substring(noquote(strsplit(noquote(strsplit(names(which(cumfreq.catch>=95)[1]),","))[[1]]," ")[[1]]),2,4)
		my.df <- data.frame(Freq=c("F5","F25","F50","F75","F95"), Depth=as.numeric(c(d.5, d.25, d.50, d.75, d.95)))
		
		return(list(df.freq=data.frame(temperature=breaks, cum.catch=cumfreq.catch, cum=cumfreq), df.tab=my.df))
	

