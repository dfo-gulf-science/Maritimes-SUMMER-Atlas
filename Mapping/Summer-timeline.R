
qu <- paste0(
  "SELECT
m.YEAR,
m.VESEL,
m.PURPOSE,
count(*)
FROM
groundfish.GSMISSION_LIST m
WHERE
m.FK_SERIES_ID = 'SUMMER'
GROUP BY
m.YEAR,
m.VESEL,
m.PURPOSE
ORDER BY
m.YEAR
  "
)

##
##
## R script to produce a figure showing the vessel and gear changes in the Maritimes 

before <- ls()

fp <- mapping.path


y1 <- seq(1969.5, 1981.5, 1.0)
df1 <- data.frame(
  year=y1,
  southern.vessel=c(rep("A.T. Cameron Y36", length(y1)))
)

y2 <- seq(1981.5,1982.5,0.5)
df2 <- data.frame(
  year=y2,
  southern.vessel=c(rep("Lady Hammond WIIA", length(y2)))
)

y3 <- c(seq(1982.5,2006.5,0.5), seq(2009.5,2017.5,0.5), seq(2018.5,2020.5,0.5))
df3 <- data.frame(
  year=y3,
  southern.vessel=c(rep("Alfred Needler WIIA", length(y3)))
)

y4 <- c(2007.5,2008.5)
df4 <- data.frame(
  year=y4,
  southern.vessel=c(rep("Wilfred Templeman WIIA", 2))
)

y5 <- seq(2017.5,2018.5,0.5)
df5 <- data.frame(
  year=y5,
  southern.vessel=c(rep("Teleost WIIA", length(y5)))
)

my.cols <- c("#d73027","#fc8d59","#fee090","#e0f3f8","#91bfdb","#4575b4") # c("wheat1","#fdae61","tomato1","#abdda4","steelblue1") ##c("firebrick1","forestgreen","","dodgerblue","goldenrod")



## 

summer.timeline.fig.fct <- function(){
  plot(1969:2021, rep(1,length(1969:2021)), ylim=c(0.5,5.5), type="n", xlab="", ylab="", axes=FALSE)
  abline(v=seq(1970,2021,5), lwd=0.5, col=grey(0.8))
  
  #abline(h=c(1,2,3,4,6.5,7.5,8.5), col=grey(0.8), lwd=0.5)
  segments(1967,5,1969.5,5, col=grey(0.8), lwd=0.5)
  segments(1967,4,1981.5,4, col=grey(0.8), lwd=0.5)
  segments(1967,3,1982.5,3, col=grey(0.8), lwd=0.5)
  segments(1967,2,2007.5,2, col=grey(0.8), lwd=0.5)
  segments(1967,1,2017.5,1, col=grey(0.8), lwd=0.5)
  
  polygon(c(df1$year,rev(df1$year)), c(rep(4.6,nrow(df1)),rep(5.4,nrow(df1))), col=my.cols[1])#, border=my.cols[1])
  polygon(c(df2$year,rev(df2$year)), c(rep(3.6,nrow(df2)),rep(4.4,nrow(df2))), col=my.cols[2])#, border=my.cols[2])
  
  polygon(c(df3$year[1:49],rev(df3$year[1:49])), c(rep(2.6,length(1:49)),rep(3.4,length(1:49))), col=my.cols[3])#, border=my.cols[3])
  polygon(c(df3$year[50:66],rev(df3$year[50:66])), c(rep(2.6,length(50:66)),rep(3.4,length(50:66))), col=my.cols[3])#, border=my.cols[3])
  polygon(c(df3$year[67:71],rev(df3$year[67:71])), c(rep(2.6,length(67:71)),rep(3.4,length(67:71))), col=my.cols[3])#, border=my.cols[3])
  
  polygon(c(df4$year,rev(df4$year)), c(rep(1.6,nrow(df4)),rep(2.4,nrow(df4))), col=my.cols[4])#, border=my.cols[4])
  
  polygon(c(df5$year,rev(df5$year)), c(rep(0.6,nrow(df5)),rep(1.4,nrow(df5))), col=my.cols[5])#, border=my.cols[5])
  
  axis(side=1, at=seq(1970,2020,5), cex.axis=2.5, padj=1, tck=-0.02)
  axis(side=1, at=seq(1970,2020,1), labels=F, tck=-0.01)
  
  axis(side=2, at=c(1:5), labels=rev(c("A.T. Cameron","Lady Hammond","Alfred Needler","Wilfred Templeman","Teleost")), las=1, cex.axis=2.5)
  
  box()
  
  
  mtext(side=1, "Year", cex=2.7, line=4.5, at=1992.7, padj = 0.3)
  
  
}

tiff.fn <- file.path(fp, "Maritimes-SUMMER-survey-timeline-2020.tiff")

tiff(tiff.fn, width=1600, height=900, compression="lzw")
par(mar=c(6,22,1,1))
summer.timeline.fig.fct()
dev.off()

png.fn <- file.path(fp, "Maritimes-SUMMER-survey-timeline-2020.png")

png(png.fn, width=1600, height=900)
par(mar=c(6,22,1,1))
summer.timeline.fig.fct()
dev.off()

pdf.fn <- file.path(fp, "Maritimes-SUMMER-survey-timeline-2020.pdf")
pdf(pdf.fn, width=16*1.5, height=9*1.5)
par(mar=c(6,20,1,1))
summer.timeline.fig.fct()
dev.off()


file.copy(c(png.fn), file.path(actualreport.path, "figures"), overwrite=TRUE)


rm(list= ls()[!(ls() %in% before)]) ## clean up after ourselves
