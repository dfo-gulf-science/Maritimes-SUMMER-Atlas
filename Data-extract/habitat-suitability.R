##
##
habitat.suitability <- function(species.num) {
	# base path
	path.Base1=path.ATLAS

	# data path
	path.Data=file.path(path.Base1,"Data")

	## data frame for storing the results to be plotted
	my.strata <- data.frame(stratum=c(440,441,442,443,444,445,446,447,448,449,450,451,452,453,454,455,456,457,458,459,460,461,462,463,464,465,466,470,471,472,473,474,475,476,477,478,480,481,482,483,484,485,490,491,492,493,494,495))
	
	# 
	nn <- length(species.num)
	my.sorting.crit <- data.frame(species=species.num, ratio=rep(-99,nn), slope=rep(-99,nn))
## for each species, identify the strata that represent high habitat suitability
cc <-1
	for(i in species.num){
	print(i)
	# load the data file containing the DDHS slope estimates
	f.path <- file.path(path.Data, paste("SS", i, "_DDHSslopes.csv", sep=""))
	ddhs.dat <- read.csv(f.path, header = TRUE)
	
	# load the data file containing the stratified estimates
	f.path <- file.path(path.Data, paste("SS", i, "_stratified.csv", sep=""))
	strat.dat <- read.csv(f.path, header = TRUE)

	## identify the strata with the highest mean abundance
	cut.50 <- quantile(ddhs.dat$mean.n)[3]
	ddhs.dat$abundance.high <- ifelse(ddhs.dat$mean.n>=cut.50,1,0)
	
	## identify the strata that are within 2* mean standard error of the slope estimates
	avg.stderr <- mean(ddhs.dat$slope.glm.poisson.stderr)
	ddhs.dat$slope.zero <- ifelse(abs(ddhs.dat$slope.glm.poisson) <= (2*avg.stderr),1,0)
	
	## identify high suitability
	ddhs.dat$high.suitability <- ifelse(ddhs.dat$slope.zero==1 & ddhs.dat$abundance.high==1,1,0)
	
	my.strata <- merge(my.strata, ddhs.dat[,c(1,11)], by="stratum", all.x=TRUE)
	
	## compute the ratio of average abundance in the last 3 years over the maximum abundance
	ll <- dim(strat.dat)[1]
	my.ratio <- mean(strat.dat$b[(ll-2):ll]) / max(strat.dat$b)
	
	
	## compute the slope for the last 5 years
	dd <- data.frame(xx=1:5,yy=strat.dat$b[(ll-4):ll])
	my.lm <- lm(yy~xx, data=dd)
	my.slope <- coef(my.lm)[2]
	
	my.sorting.crit[cc,2] <- my.ratio
	my.sorting.crit[cc,3] <- my.slope
	cc<- cc+1
	} # end loop over species
	my.list <- list(strat=my.strata, crit=my.sorting.crit)
return(my.list)	

} # end function

##############################################
# function call and generation of plots
##############################################

  spec.list <- read.csv(file.path(path.ATLAS,"Report/species-list-final.csv"),header=FALSE)
  
  # spec.list$lab.name <- strsplit(as.character(spec.list$V1), " ")
  
  species.L <- spec.list[spec.list$V9=='L',] # long timeseries

	species.num <- species.L$V4
	
my.list <-  habitat.suitability(species.num)

habitat.df <- my.list[[1]]
names(habitat.df) <- c("stratum", paste("s.",species.num, sep=""))

crit.df <- my.list[[2]]

## strata by species matrix
n.strat <- dim(habitat.df)[1]
n.spec <- length(species.num)

hab.mat <- as.matrix(habitat.df[,2:(n.spec+1)])
# is.na(hab.mat)

pdf("./Figure/species-by-strata.pdf", width=12, height=6)
par(mar=c(1,10,3,1))
image(x=seq(1,n.strat), y=seq(1,n.spec), z=hab.mat, col=c('white',grey(0.8)), axes=FALSE, ann=FALSE)
par(las=3)
axis(side=3,at=seq(1,n.strat),labels=habitat.df[,1])
par(las=1)
#axis(side=2,at=seq(1,n.spec),labels=species.L$V1)

species.list <- strsplit(as.character(species.L$V1), " ")
species.short <- sapply(1:length(species.list), function(i){paste(substring(species.list[[i]][1],1,1),". ",species.list[[i]][2],sep="")})
species.short[19] <- "Sebastes"
axis(side=2,at=seq(1,n.spec),labels=species.short)
box()
dev.off()


pdf("./Figure/strata-by-species.pdf", width=8, height=12)
par(mar=c(1,3,10,1))
image(y=seq(1,n.strat), x=seq(1,n.spec), z=t(hab.mat), col=c('white',grey(0.8)), axes=FALSE, ann=FALSE)
par(las=1)
axis(side=2,at=seq(1,n.strat), labels=habitat.df[,1])
par(las=3)
#axis(side=3,at=seq(1,n.spec),labels=species.L$V1)
axis(side=3,at=seq(1,n.spec),labels=species.short)
box()
dev.off()


## now add the ratio of mean biomass over the last 3 years over Bmax and the slope over the last 5 years
pdf("./Figure/species-by-strata-with-criteria.pdf", width=12, height=8)

#mm <- matrix(c(0,4,1,5,2,6,3,7), nrow=2, ncol=4)
#ll <- layout(mm, widths=c(5,2,2,25), heights=c(3,20), respect=TRUE)
#layout.show(ll)
mm <- matrix(c(0,6,1,7,2,8,3,9,4,10,5,11), nrow=2, ncol=6)
ll <- layout(mm, widths=c(5,2,2,2,2,25), heights=c(3,20), respect=TRUE)

par(mar=c(3,0,0,0))
plot(1,type='n',axes=FALSE,xlab="",ylab="")
axis(side=3,at=1,labels="func.group",line=-5)

par(mar=c(3,3,0,0))
plot(1,type='n',axes=FALSE,xlab="",ylab="")
axis(side=3,at=1,labels="Pearson",line=-5)

par(mar=c(3,3,0,0))
plot(1,type='n',axes=FALSE,xlab="",ylab="")
axis(side=3,at=1,labels="ratio",line=-5)

par(mar=c(3,3,0,0))
plot(1,type='n',axes=FALSE,xlab="",ylab="")
axis(side=3,at=1,labels="slope",line=-5)

par(mar=c(1,0,0,0))
plot(seq(1,n.strat),rep(0,n.strat),type='n',axes=FALSE,xlab="",ylab="",ylim=c(0,0))
par(las=3)
axis(side=3,at=seq(2,n.strat+1),labels=habitat.df[,1],line=-5)

par(mar=c(1,0,0,0))
plot(rep(1,n.spec), seq(1,n.spec),type='n',axes=FALSE,xlab="",ylab="",xlim=c(1,1))
par(las=1)
#axis(side=2,at=seq(1,n.spec),labels=species.L$V1)
axis(side=2,at=seq(1,n.spec)+0.5,labels=species.short,line=-12) #-0.5

## functional groups
par(mar=c(1,0,0,0))
plot(rep(1,n.spec), seq(1,n.spec),type='n',axes=FALSE,xlab="",ylab="",xlim=c(1,1))
text(rep(1,n.spec), seq(1,n.spec)+0.5, rep("C",n.spec))
abline(h=c(1,6,11,16,21,26))

## Pearson corr.
par(mar=c(1,0,0,0))
plot(rep(1,n.spec), seq(1,n.spec),type='n',axes=FALSE,xlab="",ylab="",xlim=c(1,1))
text(rep(1,n.spec), seq(1,n.spec)+0.5, rep("0.99**",n.spec))
abline(h=c(1,6,11,16,21,26))

## plot ratios
my.bins <- floor((crit.df$ratio*100)/10)
my.cols <- colorRampPalette(c('white','black'))(20)[my.bins]
par(mar=c(3,3,0,0))
image(x=1, y=seq(1,n.spec), z=t(as.matrix(crit.df$ratio)), col=my.cols, axes=FALSE, ann=FALSE)
abline(h=c(0.5,5.5,10.5,15.5,20.5,25.5))
#par(las=1)
#axis(side=2,at=seq(1,n.spec),labels=species.short)

breaks <- seq(range(crit.df$slope)[1],range(crit.df$slope)[2],length.out=20)
cuts <- cut(crit.df$slope, breaks, right=TRUE, include.lowest=TRUE)
my.bins <- as.numeric(cuts)

crit.df$slope.sign <- ifelse(crit.df$slope>=0, "+","-")

par(mar=c(1,0,0,0))
plot(rep(1,n.spec), seq(1,n.spec),type='n',axes=FALSE,xlab="",ylab="",xlim=c(1,1))
text(rep(1,n.spec)+0.25, seq(1,n.spec)+0.5, crit.df$slope.sign)
#abline(h=c(1,6,11,16,21,26))

# my.bins <- floor((crit.df$slope*100)/10)
my.cols <- colorRampPalette(c('red','white','blue'))(20)[my.bins]
#par(mar=c(3,3,0,0))
#image(x=1, y=seq(1,n.spec), z=t(as.matrix(crit.df$slope)), col=my.cols, axes=FALSE, ann=FALSE)
#abline(h=c(0.5,5.5,10.5,15.5,20.5,25.5))
#par(las=1)
#axis(side=2,at=seq(1,n.spec),labels=species.short)

par(mar=c(3,3,0,0))
image(x=seq(1,n.strat), y=seq(1,n.spec), z=(hab.mat), col=c('white',grey(0.8)), axes=FALSE, ann=FALSE)
#image(x=seq(1,n.strat), y=seq(1,n.spec), z=(hab.mat), col=c('white',colours()[625]), axes=FALSE, ann=FALSE)

abline(h=c(0.5,5.5,10.5,15.5,20.5,25.5))
abline(v=c(0.5,10.5,20.5,30.5,40.5))
dev.off()

## now experiment with ordering by different criterion

## ordered by functional group

## ordered by ratio

