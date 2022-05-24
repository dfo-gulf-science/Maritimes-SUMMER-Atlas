## Perry and Smith environmental covariates
##
##
# fn <- file.path(figdata.path, paste("SS",10,"_catch.csv",sep=""))
# catch.in <- read.csv(fn, header=TRUE)
## env.var <- "depth"

generate.cumul.dist <- function(catch.in, env.var) {

  ## switch statement for which environmental parameter to do
switch(env.var,
       depth = {## depth
         depth.df <- catch.in[which(!is.na(catch.in$dmin)),]
         depth.df$DEPTH <- (depth.df$dmin+depth.df$dmax) / 2
         
         st <- read.csv(file.path(actualreport.path, "strata-statistics.csv"))
         st <- st[which(st$STRAT %in% c(440:466, 470:478, 480:485, 490:495)),]
         
         x <- depth.df
         x$unique.id <- paste0(x$mission, "-", x$setno)
         
         ##
         ## following Table 1 in Perry and Smith (1994)
         # nh = number of hauls or sets in stratum h (h = 1, . . ., L)
         nh <- aggregate(unique.id~Strata, data=x, length)
         st <- merge(st, nh, by.x="STRAT" , by.y="Strata")
         st$nh <- st$unique.id
         
         
         ## n = total number of hauls
         n <- nrow(x)
         
         # Nh, = total number of possible sets in stratum h
         # a.k.a. number of trawlable units
         # swept area of a 30-minute WIIa set, 
         swept.area <- 4E-02 ## NOT THE REAL VALUE, check this
         st$Nh <- st$AREA / swept.area
         
         # N = total number of possible sets overall
         N <- sum(st$Nh)
         
         # Wh = proportion of the survey area in stratum h
         st$Wh <- st$AREA / sum(st$AREA)
         ## in Perry and Smith, the proportion is computed as Nh/N 
         ## (st$Wh - st$Nh / N) < 1E-12 ## the numbers numerically differ in R, but they are the same
         
         depths <- seq(min(x$DEPTH)-2, max(x$DEPTH)+2, length.out=200)
         #depths <- seq(0, max(x$DEPTH), length.out=200)
         cdf.df <- expand.grid(depth=depths, cdf=NA) ## CDF for sets
         catch.cdf.df <- expand.grid(depth=depths, cdf=NA) ## CDF for catch
         
         for(t in depths){
           # print(paste0("depth ", t))
           this.depth.cdf <- 0
           this.depth.catch.cdf <- 0
           
           ## loop over strata
           for(h in unique(x$Strata)){
             #print(paste0("stratum ", h))
             
             ## number of unique tows in this stratum
             t.df <- x[x$Strata==h,]
             
             ## loop over sets in each stratum
             for(i in 1:nrow(t.df)){
               this.set.cdf <- ifelse(t.df[i,"DEPTH"]<=t,1,0) * (st[st$STRAT==h,"Wh"] / st[st$STRAT==h,"nh"])
               this.depth.cdf <- this.depth.cdf + this.set.cdf
               
               # catch
               third.term <- (t.df[i,"totno.corr"] / mean(t.df[,"totno.corr"])) ## this term will return NA if no fish were ever caught in that stratum
               if(is.na(third.term)) third.term <- 0
               this.set.catch.cdf <- ifelse(t.df[i,"DEPTH"]<=t,1,0) * 
                 (st[st$STRAT==h,"Wh"] / st[st$STRAT==h,"nh"]) * 
                 third.term
               this.depth.catch.cdf <- this.depth.catch.cdf + this.set.catch.cdf
               #print(paste0("set ", i, " this.depth.catch.cdf: ", this.depth.catch.cdf))
             }## end loop over sets
             
             
             
           }## end loop over strata
           
           
           cdf.df[cdf.df$depth==t,"cdf"] <- this.depth.cdf
           catch.cdf.df[catch.cdf.df$depth==t,"cdf"] <- this.depth.catch.cdf
         }## end loop over depths
         
         merged.df <- merge(catch.cdf.df, cdf.df, by="depth")
         names(merged.df) <- c("depth","catch.cdf","cdf")
         
         merged.df$interval <- findInterval(merged.df$catch.cdf, c(0.05,0.25,0.5,0.75,0.95)) 
         agg.df <- aggregate(depth~interval, data=merged.df, max)
         quantiles.df <- data.frame(freq=c(0.05,0.25,0.5,0.75,0.95), depth=agg.df[1:5,"depth"])
         

       },
       temperature = { ## temperature
         
         temperature.df <- catch.in[which(!is.na(catch.in$temperature)),]
         
         st <- read.csv(file.path(actualreport.path, "strata-statistics.csv"))
         st <- st[which(st$STRAT %in% c(440:466, 470:478, 480:485, 490:495)),]
         
         x <- temperature.df
         x$unique.id <- paste0(x$mission, "-", x$setno)
         
         ##
         ## following Table 1 in Perry and Smith (1994)
         # nh = number of hauls or sets in stratum h (h = 1, . . ., L)
         nh <- aggregate(unique.id~Strata, data=x, length)
         st <- merge(st, nh, by.x="STRAT" , by.y="Strata")
         st$nh <- st$unique.id
         
         
         ## n = total number of hauls
         n <- nrow(x)
         
         # Nh, = total number of possible sets in stratum h
         # a.k.a. number of trawlable units
         # swept area of a 30-minute WIIa set, 
         swept.area <- 4E-02 ## NOT THE REAL VALUE, check this
         st$Nh <- st$AREA / swept.area
         
         # N = total number of possible sets overall
         N <- sum(st$Nh)
         
         # Wh = proportion of the survey area in stratum h
         st$Wh <- st$AREA / sum(st$AREA)
         ## in Perry and Smith, the proportion is computed as Nh/N 
         ## (st$Wh - st$Nh / N) < 1E-12 ## the numbers numerically differ in R, but they are the same
         
         temperatures <- seq(min(x$temperature)-2, max(x$temperature)+2, length.out=200)
         
         cdf.df <- expand.grid(temperature=temperatures, cdf=NA) ## CDF for sets
         catch.cdf.df <- expand.grid(temperature=temperatures, cdf=NA) ## CDF for catch
         
         for(t in temperatures){
           this.temperature.cdf <- 0
           this.temperature.catch.cdf <- 0
           
           ## loop over strata
           for(h in unique(x$Strata)){
             ## number of unique tows in this stratum
             t.df <- x[x$Strata==h,]
             ## loop over sets in each stratum
             for(i in 1:nrow(t.df)){
               this.set.cdf <- ifelse(t.df[i,"temperature"]<=t,1,0) * (st[st$STRAT==h,"Wh"] / st[st$STRAT==h,"nh"])
               this.temperature.cdf <- this.temperature.cdf + this.set.cdf
               
               # catch
               third.term <- (t.df[i,"totno.corr"] / mean(t.df[,"totno.corr"])) ## this term will return NA if no fish were ever caught in that stratum
               if(is.na(third.term)) third.term <- 0
               this.set.catch.cdf <- ifelse(t.df[i,"temperature"]<=t,1,0) * 
                 (st[st$STRAT==h,"Wh"] / st[st$STRAT==h,"nh"]) * 
                 third.term 
               this.temperature.catch.cdf <- this.temperature.catch.cdf + this.set.catch.cdf
               
               if(is.na(this.temperature.catch.cdf)) {stop(paste0("Stratum ", h, " set ", i,  " this.temperature.catch.cdf:", this.temperature.catch.cdf))}
             }## end loop over sets
           }## end loop over strata
           
           
           cdf.df[cdf.df$temperature==t,"cdf"] <- this.temperature.cdf
           catch.cdf.df[catch.cdf.df$temperature==t,"cdf"] <- this.temperature.catch.cdf
         }## end loop over temperatures
         
         merged.df <- merge(catch.cdf.df, cdf.df, by="temperature")
         names(merged.df) <- c("temperature","catch.cdf","cdf")
         
         merged.df$interval <- findInterval(merged.df$catch.cdf, c(0.05,0.25,0.5,0.75,0.95)) 
         agg.df <- aggregate(temperature~interval, data=merged.df, max)
         quantiles.df <- data.frame(freq=c(0.05,0.25,0.5,0.75,0.95), temperature=agg.df[1:5,"temperature"])
         
       },
       salinity = { ## salinity
         
         salinity.df <- catch.in[which(!is.na(catch.in$salinity) & catch.in$salinity>0),]
         
         
         st <- read.csv(file.path(actualreport.path, "strata-statistics.csv"))
         st <- st[which(st$STRAT %in% c(440:466, 470:478, 480:485, 490:495)),]
         
         x <- salinity.df
         x$unique.id <- paste0(x$mission, "-", x$setno)
         
         ##
         ## following Table 1 in Perry and Smith (1994)
         # nh = number of hauls or sets in stratum h (h = 1, . . ., L)
         nh <- aggregate(unique.id~Strata, data=x, length)
         st <- merge(st, nh, by.x="STRAT" , by.y="Strata")
         st$nh <- st$unique.id
         
         
         ## n = total number of hauls
         n <- nrow(x)
         
         # Nh, = total number of possible sets in stratum h
         # a.k.a. number of trawlable units
         # swept area of a 30-minute WIIa set, 
         swept.area <- 4E-02 ## NOT THE REAL VALUE, check this
         st$Nh <- st$AREA / swept.area
         
         # N = total number of possible sets overall
         N <- sum(st$Nh)
         
         # Wh = proportion of the survey area in stratum h
         st$Wh <- st$AREA / sum(st$AREA)
         ## in Perry and Smith, the proportion is computed as Nh/N 
         ## (st$Wh - st$Nh / N) < 1E-12 ## the numbers numerically differ in R, but they are the same
         
         salinities <- seq(min(x$salinity)-0.5, max(x$salinity)+0.5, length.out=200)
         
         cdf.df <- expand.grid(salinity=salinities, cdf=NA) ## CDF for sets
         catch.cdf.df <- expand.grid(salinity=salinities, cdf=NA) ## CDF for catch
         
         for(t in salinities){
           this.salinity.cdf <- 0
           this.salinity.catch.cdf <- 0
           
           ## loop over strata
           for(h in unique(x$Strata)){
             ## number of unique tows in this stratum
             t.df <- x[x$Strata==h,]
             ## loop over sets in each stratum
             for(i in 1:nrow(t.df)){
               this.set.cdf <- ifelse(t.df[i,"salinity"]<=t,1,0) * (st[st$STRAT==h,"Wh"] / st[st$STRAT==h,"nh"])
               this.salinity.cdf <- this.salinity.cdf + this.set.cdf
               
                              # catch
               third.term <- (t.df[i,"totno.corr"] / mean(t.df[,"totno.corr"])) ## this term will return NA if no fish were ever caught in that stratum
               if(is.na(third.term)) third.term <- 0
               this.set.catch.cdf <- ifelse(t.df[i,"salinity"]<=t,1,0) * 
                 (st[st$STRAT==h,"Wh"] / st[st$STRAT==h,"nh"]) * 
                 third.term
               this.salinity.catch.cdf <- this.salinity.catch.cdf + this.set.catch.cdf
               
             }## end loop over sets
           }## end loop over strata
           
           
           cdf.df[cdf.df$salinity==t,"cdf"] <- this.salinity.cdf
           catch.cdf.df[catch.cdf.df$salinity==t,"cdf"] <- this.salinity.catch.cdf           
           
         }## end loop over salinities
         
         
         merged.df <- merge(catch.cdf.df, cdf.df, by="salinity")
         names(merged.df) <- c("salinity","catch.cdf","cdf")
         
         merged.df$interval <- findInterval(merged.df$catch.cdf, c(0.05,0.25,0.5,0.75,0.95)) 
         agg.df <- aggregate(salinity~interval, data=merged.df, max)
         quantiles.df <- data.frame(freq=c(0.05,0.25,0.5,0.75,0.95), salinity=agg.df[1:5,"salinity"])
         
       },
       
       NULL = {
         stop("This environmental variable is not recognised!\n")
         
       }
) ## end of switch statement

  return(list(merged.df[,c(1:3)], quantiles.df))
  
} # end function

