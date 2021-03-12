## maps of stratum-level abundance

figure1.fct <- function(dat.in, cex.in, pos.ylabel=c(0,0)) {
#dat.in <- read.csv("C:/RProjects/FishInverAtlas_Ricard/Data/SS10_catch.csv", header=TRUE)

my.xlim <- c(291.25,303.75)
my.ylim <- c(41,47.5)

yrs.labels <- c("1970-1974","1975-1979","1980-1984","1985-1989","1990-1994","1995-1999","2000-2004","2005-2009","2010-2014")

my.cols.palette <- c('white','#FEF0D9', '#FDCC8A', '#FC8D59', '#E34A33', '#B30000')

## loop over 5-year periods
for (i in 1:9) {
yy <- strsplit(yrs.labels[i],"-")
tt <- subset(dat.in, YEAR >=as.numeric(yy[[1]][1]) & YEAR <= as.numeric(yy[[1]][2]) )
tt$occ <- ifelse(tt$totno.corr==0,0,1)
pr.occ <- round(mean(tt$occ), digits=3)

mean.num.strat <- tapply(tt$totno.corr, tt$Strata, mean)
my.bins <- ifelse(mean.num.strat==0,1,ifelse(mean.num.strat<5,2,ifelse(mean.num.strat<20,3,ifelse(mean.num.strat<50,4,ifelse(mean.num.strat<100,5,6)))))
my.cols <- my.cols.palette[my.bins]

plotMap(worldLLhigh, my.xlim, my.ylim, col=grey(0.9),plt=c(0.0,1.0,0.0,1.0),border=grey(0.7),axes=FALSE,tckLab=FALSE,xlab="",ylab="")
text(293.2,46.4,yrs.labels[i],cex=0.95)
text(293.2,45.9,paste("P(occ) = ",pr.occ,sep=""),cex=0.7)

my.df <- data.frame(stratum=names(mean.num.strat), mean.num=mean.num.strat, colour=my.cols)

# my.df2 <- merge(my.df, attr(SS.strata,"PolyData"), by.x="stratum", by.y="Stratum1")
my.df2 <- my.df
oo <- order(my.df2$stratum)
my.df.final <- my.df2[oo,]

dfo.summer.local <- subset(SS.strata.DRfixed.final, PID %in% my.df.final$stratum)
addPolys(dfo.summer.local, col=my.df.final$colour, 
         lty=ifelse(my.df.final$colour=='white',1,0), lwd=0.1)

if(i==1 | i==9) {legend('bottomright', c("0","<5","<20","<50","<100",">=100"), col='black', fill=my.cols.palette, bg='white',cex=0.45)}

}


} # end function
