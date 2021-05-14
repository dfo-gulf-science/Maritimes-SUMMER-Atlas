before <- ls()

library(worms)

taxo.tree.fct <- function(spec.code, sci.name){
  my.df <- wormsbynames(sci.name)
  vars <- c("AphiaID", "url","kingdom","phylum","class","order","family","scientificname")
  return(cbind(spec.code, my.df[,vars]))
}## end function

## use the scientific name in the species list to obtain the APHIA ID from the World Registry of marine species
spec.tab <- read.csv("./species-list-for-report.csv", encoding = "UTF-8")
spec.tab$ACCEPTED_SCIENT_NAME <- as.character(spec.tab$ACCEPTED_SCIENT_NAME) #make sure is of class character
#taxo.tree.fct(10, "Gadus morhua")

taxo.list.out <- lapply(1:nrow(spec.tab), function(ii){aa1<-spec.tab[ii,"spec"];aa2<-spec.tab[ii,"ACCEPTED_SCIENT_NAME"];taxo.tree.fct(aa1,aa2)})
taxo.t <- do.call(rbind, taxo.list.out)

vars <- c("spec","FAO_E_COMMON_NAME","FAO_F_COMMON_NAME","nrecords","type")
taxo.df.out <- merge(spec.tab[,vars], taxo.t, by.x="spec", by.y="spec.code")

## order the taxonomic tree phylogenetically
taxo.final <- taxo.df.out
taxo.final$phylum <- factor(taxo.final$phylum, levels=c("Chordata","Mollusca","Arthropoda"), ordered=TRUE) ## this is used to order the table of species
taxo.final$class <- factor(taxo.final$class, levels=c("Myxini","Petromyzonti","Actinopterygii","Elasmobranchii","Cephalopoda","Malacostraca"), ordered=TRUE) ## this is used to order the table of species
taxo.final$order <-  factor(taxo.final$order, levels=c("Myxiniformes","Petromyzontiformes","Gadiformes","Scorpaeniformes","Pleuronectiformes","Perciformes","Clupeiformes","Osmeriformes","Aulopiformes","Myctophiformes","Stomiiformes","Argentiniformes","Lophiiformes","Anguilliformes","Zeiformes","Beloniformes","Rajiformes","Squaliformes","Oegopsida","Myopsida","Decapoda"), ordered=TRUE) ## this is used to order the table of species
  
oo1 <- order(taxo.final$phylum)
taxo.final <- taxo.final[oo1,]

oo2 <- order(taxo.final$class)
taxo.final <- taxo.final[oo2,]

oo3 <- order(taxo.final$order)
taxo.final <- taxo.final[oo3,]

## write to a file
csv.fn <- file.path(main.path, "species-list-for-report-APHIA.csv")
readr::write_csv(taxo.final,
                 file=csv.fn,
                 col_names=T
)

file.copy(csv.fn, actualreport.path, overwrite=TRUE)

rm(list= ls()[!(ls() %in% before)]) ## clean up after ourselves
