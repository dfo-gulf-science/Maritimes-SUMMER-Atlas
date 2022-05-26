

## make sure that all species with distribution indices
## have the same values for yearly area surveyed
species.numbers <- c(species.LF, species.LI)
all.df <- data.frame(year=1970:2020)
for(ss in species.numbers){
  fn <- file.path(figdata.path, paste0("SS",ss,"_distribution-usingbiomass.csv"))
  x <- read.csv(fn, header=TRUE)
  t.df <- data.frame(x$area.surveyed)
  names(t.df) <- paste0("species",ss)
  all.df <- cbind(all.df, t.df)
}

species.numbers <-species.LIn
for(ss in species.numbers){
  fn <- file.path(figdata.path, paste0("SS",ss,"_distribution-usingabundance.csv"))
  x <- read.csv(fn, header=TRUE)
  t.df <- data.frame(x$area.surveyed)
  names(t.df) <- paste0("species",ss)
  all.df <- cbind(all.df, t.df)
}

(dim(all.df)[2]-1) == length(c(species.LF, species.LI, species.LIn))
for(i in 3:(dim(all.df)[2]-1)){
  cc <- cor(all.df[,2], all.df[,i])
  
  print(paste0("column ", names(all.df)[i], " corr: ", cc))
}
