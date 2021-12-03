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
         
         depths <- seq(min(x$DEPTH), max(x$DEPTH), length.out=200)
         
         cdf.df <- expand.grid(depth=depths, cdf=NA) ## CDF for sets
         catch.cdf.df <- expand.grid(depth=depths, cdf=NA) ## CDF for catch
         
         for(t in depths){
           print(paste0("depth ", t))
           this.depth.cdf <- 0
           this.depth.catch.cdf <- 0
           
           ## loop over strata
           for(h in unique(x$Strata)){
             ## number of unique tows in this stratum
             t.df <- x[x$Strata==h,]
             
             ## loop over sets in each stratum
             for(i in 1:nrow(t.df)){
               this.set.cdf <- ifelse(t.df[i,"DEPTH"]<=t,1,0) * (st[st$STRAT==h,"Wh"] / st[st$STRAT==h,"nh"])
               this.depth.cdf <- this.depth.cdf + this.set.cdf
               
               # catch
               this.set.catch.cdf <- ifelse(t.df[i,"DEPTH"]<=t,1,0) * 
                 (st[st$STRAT==h,"Wh"] / st[st$STRAT==h,"nh"]) * 
                 (t.df[i,"totno.corr"] / mean(t.df[,"totno.corr"]))
               this.depth.catch.cdf <- this.depth.catch.cdf + this.set.catch.cdf
               
             }## end loop over sets
             
             
             
           }## end loop over strata
           
           
           cdf.df[cdf.df$depth==t,"cdf"] <- this.depth.cdf
           catch.cdf.df[catch.cdf.df$depth==t,"cdf"] <- this.depth.catch.cdf
         }## end loop over depths
         
         merged.df <- merge(catch.cdf.df, cdf.df, by="depth")
         names(merged.df) <- c("depth","catch.cdf","cdf")
         
         # return(list(df.freq=data.frame(temperature=both.df$temperature, cum.catch=both.df$cumfreq.catch, cum=both.df$cumfreq.samples), df.tab=my.df))
         
         # depth.cdf <- cdf.df
         # depth.cdf$variable="Depth (m)"
         # names(depth.cdf)[1] <- "variable.value"
         # 
         # 
         # out.df <- cdf.df
         # names(out.df) <- c("depth","cdf.depth")
         
       },
       temperature = { ## temperature
         
         temperature.df <- catch.in[which(!is.na(catch.in$BOTTOM_TEMPERATURE)),]
         
         st <- read.csv(file.path(actualreport.path, "strata-statistics.csv"))
         st <- st[which(st$STRAT %in% c(440:466, 470:478, 480:485, 490:495)),]
         
         x <- temperature.df
         x$unique.id <- paste0(x$mission, "-", x$setno)
         
         ##
         ## following Table 1 in Perry and Smith (1994)
         # nh = number of hauls or sets in stratum h (h = 1, . . ., L)
         nh <- aggregate(unique.id~STRAT, data=x, length)
         st <- merge(st, nh, by="STRAT")
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
         
         temperatures <- seq(min(x$BOTTOM_TEMPERATURE), max(x$BOTTOM_TEMPERATURE), length.out=200)
         
         cdf.df <- expand.grid(temperature=temperatures, cdf=NA) ## CDF for sets
         catch.cdf.df <- expand.grid(depth=depths, cdf=NA) ## CDF for catch
         
         for(t in temperatures){
           this.temperature.cdf <- 0
           this.temperature.catch.cdf <- 0
           
           ## loop over strata
           for(h in unique(x$Strata)){
             ## number of unique tows in this stratum
             t.df <- x[x$Strata==h,]
             ## loop over sets in each stratum
             for(i in 1:nrow(t.df)){
               this.set.cdf <- ifelse(t.df[i,"BOTTOM_TEMPERATURE"]<=t,1,0) * (st[st$STRAT==h,"Wh"] / st[st$STRAT==h,"nh"])
               this.temperature.cdf <- this.temperature.cdf + this.set.cdf
               
               # catch
               this.set.catch.cdf <- ifelse(t.df[i,"DEPTH"]<=t,1,0) * 
                 (st[st$STRAT==h,"Wh"] / st[st$STRAT==h,"nh"]) * 
                 (t.df[i,"totno.corr"] / mean(t.df[,"totno.corr"]))
               this.temperature.catch.cdf <- this.temperature.catch.cdf + this.set.catch.cdf
               
             }## end loop over sets
           }## end loop over strata
           
           
           cdf.df[cdf.df$temperature==t,"cdf"] <- this.temperature.cdf
           
         }## end loop over temperatures
         
         
         merged.df <- merge(catch.cdf.df, cdf.df, by="depth")
         names(merged.df) <- c("depth","catch.cdf","cdf")
         
         # temp.cdf <- cdf.df
         # temp.cdf$variable="Bottom temperature (\u{B0}C)"
         # names(temp.cdf)[1] <- "variable.value"
         # out.df <- cdf.df
         # names(out.df) <- c("temperature","cdf.temperature")
         
       },
       salinity = { ## salinity
         
         salinity.df <- catch.in[which(!is.na(catch.in$BOTTOM_SALINITY) & catch.in$BOTTOM_SALINITY>0),]
         
         
         st <- read.csv(file.path(actualreport.path, "strata-statistics.csv"))
         st <- st[which(st$STRAT %in% c(440:466, 470:478, 480:485, 490:495)),]
         
         x <- salinity.df
         x$unique.id <- paste0(x$mission, "-", x$setno)
         
         ##
         ## following Table 1 in Perry and Smith (1994)
         # nh = number of hauls or sets in stratum h (h = 1, . . ., L)
         nh <- aggregate(unique.id~STRAT, data=x, length)
         st <- merge(st, nh, by="STRAT")
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
         
         salinities <- seq(min(x$BOTTOM_SALINITY), max(x$BOTTOM_SALINITY), length.out=200)
         
         cdf.df <- expand.grid(salinity=salinities, cdf=NA) ## CDF for sets
         catch.cdf.df <- expand.grid(depth=depths, cdf=NA) ## CDF for catch
         
         for(t in salinities){
           this.salinity.cdf <- 0
           this.salinity.catch.cdf <- 0
           
           ## loop over strata
           for(h in unique(x$Strata)){
             ## number of unique tows in this stratum
             t.df <- x[x$Strata==h,]
             ## loop over sets in each stratum
             for(i in 1:nrow(t.df)){
               this.set.cdf <- ifelse(t.df[i,"BOTTOM_SALINITY"]<=t,1,0) * (st[st$STRAT==h,"Wh"] / st[st$STRAT==h,"nh"])
               this.salinity.cdf <- this.salinity.cdf + this.set.cdf
               
               # catch
               this.set.catch.cdf <- ifelse(t.df[i,"DEPTH"]<=t,1,0) * 
                 (st[st$STRAT==h,"Wh"] / st[st$STRAT==h,"nh"]) * 
                 (t.df[i,"totno.corr"] / mean(t.df[,"totno.corr"]))
               this.salinity.catch.cdf <- this.salinity.catch.cdf + this.set.catch.cdf
               
             }## end loop over sets
           }## end loop over strata
           
           
           cdf.df[cdf.df$salinity==t,"cdf"] <- this.salinity.cdf
           
         }## end loop over salinities
         
         
         merged.df <- merge(catch.cdf.df, cdf.df, by="depth")
         names(merged.df) <- c("depth","catch.cdf","cdf")
         
         # sal.cdf <- cdf.df
         # sal.cdf$variable="Bottom salinity (psu)"
         # names(sal.cdf)[1] <- "variable.value"
         # 
         # out.df <- cdf.df
         # names(out.df) <- c("salinity","cdf.salinity")
         # 
         
       },
       
       NULL = {
         stop("This environmental variable is not recognised!\n")
         
       }
) ## end of switch statement


} # end function

