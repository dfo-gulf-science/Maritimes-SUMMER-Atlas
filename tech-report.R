## write the .Rmd file that will make up the Appendix of the report
##
## script to generate a single .Rmd file to be included in the TechReport
## - this is an alternative to the way I was doing the assembly of the Maritimes Atlas, and is tailored to work with csasdown
## - highly influenced by Sean Anderson's approach for his groundfish synoptic report

## generate a single .Rmd file containing all the bits and pieces required to generate the Appendix of the Atlas, 
## i.e. where all the figures appear: plot-pages.Rmd
library(tidyverse)

in.df <- readr::read_csv(file.path(here::here(), "species-list-for-report-APHIA.csv"))

## first class tickets, L species
taxo.final <- in.df
names(taxo.final)[1:3] <- c("species.code","comm.english","comm.fr")
taxo.final <- taxo.final[taxo.final$type %in% c("LF"),]
## 


tempLF <- lapply(taxo.final$species.code, function(x) {
  
  out <- list()
  ## figure files 
  spp_file1 <- paste0("RV-4VWX-",x,"-IDWbiomass.pdf")
  
  spp_file2 <- paste0("RV-4VWX-",x,"-stratifiedB.pdf")
  spp_file3 <- paste0("RV-4VWX-",x,"-distribution-indices-usingB.pdf")
  spp_file4 <- paste0("RV-4VWX-",x,"-BvsD75corr.pdf")
  
  spp_file5 <- paste0("RV-4VWX-",x,"-lengthfreqNAFO.pdf")
  
  spp_file6 <- paste0("RV-4VWX-",x,"-conditionNAFO.pdf")
  
  spp_file7 <- paste0("RV-4VWX-",x,"-depthpref.pdf")
  spp_file8 <- paste0("RV-4VWX-",x,"-bottomtemppref.pdf")
  spp_file9 <- paste0("RV-4VWX-",x,"-salinitypref.pdf")
  spp_table <- paste0("SS",x,"_alldist.tex")
  spp_file10 <- paste0("RV-4VWX-",x,"-DDHS.pdf")
  
  latin_name <- taxo.final$scientificname[taxo.final$species.code == x]
  english_name <- taxo.final$comm.english[taxo.final$species.code == x]
  french_name <- taxo.final$comm.fr[taxo.final$species.code == x]
  family_name <- taxo.final$family[taxo.final$species.code == x]
  worms_id <- taxo.final$AphiaID[taxo.final$species.code == x]
  worms_link <- taxo.final$url[taxo.final$species.code == x]
  category <- taxo.final$type[taxo.final$species.code == x]
  
  i <- 1
  out[[i]] <- "\\renewcommand\\thefigure{\\thesubsection\\Alph{figure}} \n"
  i <- i + 1
  out[[i]] <- "\\setcounter{figure}{0} \n"
  i <- i + 1
  out[[i]] <- paste0("## ", english_name, " (", french_name, ") - species code ", x, " (category ", category, ")"," {#sec:", x, "} \n")
  i <- i + 1
  out[[i]] <- paste0("\\index{", french_name, "} ",  "\\index{", english_name, "} ",  "\\index{", latin_name, "} \n")
  i <- i + 1
  out[[i]] <- paste0("\\index{", family_name, "!", latin_name, "} \n")
  i <- i + 1
  out[[i]] <- paste0(
    "Scientific name: [", latin_name, "](",worms_link,") \n \\newline")
  i <- i + 1
  #Figure 1
  out[[i]] <- "\\begin{minipage}{1.0\\textwidth}"
  i <- i + 1
  out[[i]] <- " \\begin{tabular}{c}"
  i <- i + 1
  out[[i]] <- paste0("\\includegraphics[width=6.75in]{../Figures-Actual/",
                     spp_file1, "} \\\\ ")
  i <- i + 1
  out[[i]] <- "\\end{tabular} "
  i <- i + 1
  out[[i]] <- paste0("\\captionof{figure}{Inverse distance weighted distribution of catch biomass (kg/tow) for ", english_name,". $P(occ)$ is the proportion of tows with catch records for each 5-year period.}")
  i <- i + 1
  out[[i]] <- "\\end{minipage}  \n"
  i <- i + 1
  #end of Figure 1
  out[[i]] <- "\\newline  \n"
  i <- i + 1
  out[[i]] <- "\\vspace{1cm}  \n"
  i <- i + 1
  
  
                                 
  #Figures 2,3 and 4
  out[[i]] <- "\\begin{minipage}{1.0\\textwidth}"
  i <- i + 1
  out[[i]] <- " \\begin{tabular}{ccc}"
  i <- i + 1
  out[[i]] <- paste0("\\includegraphics[width=2.0in]{../Figures-Actual/",
                     spp_file2, "} & ")
  i <- i + 1
  out[[i]] <- paste0("\\includegraphics[width=2.0in]{../Figures-Actual/",
                     spp_file3, "} & ")
  i <- i + 1
  out[[i]] <- paste0("\\includegraphics[width=2.0in]{../Figures-Actual/",
                     spp_file4, "} \\\\ ")
  i <- i + 1
  out[[i]] <- "\\end{tabular} "
  i <- i + 1
  out[[i]] <- paste0("\\captionof{figure}{Stratified random estimates of biomass (kg/tow), D75 and D95 and the correlation between D75 and biomass of ", english_name,". The predictions from a loess estimator are overlaid on the distribution indices in the middle panel.}")
  i <- i + 1
  out[[i]] <- "\\end{minipage} \n"
  i <- i + 1
  #end of Figure 2, 3 and 4
  out[[i]] <- "\\clearpage\n"
  i <- i + 1
  #Figure 5 
  out[[i]] <- "\\begin{minipage}{1.0\\textwidth}"
  i <- i + 1
  out[[i]] <- " \\begin{tabular}{c}"
  i <- i + 1
  out[[i]] <- paste0("\\includegraphics[width=6.5in]{../Figures-Actual/",
                     spp_file5, "} \\\\ ")
  i <- i + 1
  out[[i]] <- "\\end{tabular} "
  i <- i + 1
  out[[i]] <- paste0("\\captionof{figure}{Length frequency distribution in NAFO units 4X and 4VW for ", english_name,".}")
  i <- i + 1
  out[[i]] <- "\\end{minipage} \n"
  i <- i + 1
  #end of Figure 5 
  out[[i]] <- "\\newline  \n"
  i <- i + 1
  out[[i]] <- "\\vspace{1cm}  \n"
  i <- i + 1
  #Figure 6 
  out[[i]] <- "\\begin{minipage}{1.0\\textwidth}"
  i <- i + 1
  out[[i]] <- " \\begin{tabular}{c}"
  i <- i + 1
  out[[i]] <- paste0("\\includegraphics[width=6.5in]{../Figures-Actual/",
                     spp_file6, "} \\\\ ")
  i <- i + 1
  out[[i]] <- "\\end{tabular} "
  i <- i + 1
  out[[i]] <- paste0("\\captionof{figure}{Average fish condition in NAFO units 4X and 4VW for ", english_name,".}")
  i <- i + 1
  out[[i]] <- "\\end{minipage} \n"
  i <- i + 1
  #end of Figure 6
  out[[i]] <- "\\clearpage\n"
  i <- i + 1
  
  #Figures 7,8 and 9
  out[[i]] <- "\\begin{minipage}{1.0\\textwidth}"
  i <- i + 1
  out[[i]] <- " \\begin{tabular}[t]{m{3in}m{3in}}"
  i <- i + 1
  out[[i]] <- paste0("\\includegraphics[width=3in]{../Figures-Actual/",
                     spp_file7, "} & ")
  i <- i + 1
  out[[i]] <- paste0("\\includegraphics[width=3in]{../Figures-Actual/",
                     spp_file8, "} \\\\ ")
  i <- i + 1
  out[[i]] <- paste0("\\includegraphics[width=3in]{../Figures-Actual/",
                     spp_file9, "} & ")
  i <- i + 1
  out[[i]] <- paste0("\\input{../Figures-data/",
                     spp_table, "} \\\\ ")
  i <- i + 1
  out[[i]] <- "\\end{tabular} "
  i <- i + 1
  out[[i]] <- paste0("\\captionof{figure}{Catch distribution by depth, temperature and salinity of ", english_name,".}")
  i <- i + 1
  out[[i]] <- "\\end{minipage} \n"
  i <- i + 1
  #end of Figure 7,8 and 9
  out[[i]] <- "\\newline  \n"
  i <- i + 1
  out[[i]] <- "\\vspace{1cm}  \n"
  i <- i + 1
  
  
  #Figure 10
  out[[i]] <- "\\begin{minipage}{1.0\\textwidth}"
  i <- i + 1
  out[[i]] <- " \\begin{tabular}{c}"
  i <- i + 1
  out[[i]] <- paste0("\\includegraphics[width=6.5in]{../Figures-Actual/",
                     spp_file10, "} \\\\ ")
  i <- i + 1
  out[[i]] <- "\\end{tabular} "
  i <- i + 1
  out[[i]] <- paste0("\\captionof{figure}{DDHS slopes versus median stratum abundance. The last two digits of each stratum number is shown in the figure for ", english_name,". The red box indicates strata of particular importance for a species by identifying slopes that are within a standard error from zero and that are within the top 25\\% of median abundance.}")
  i <- i + 1
  out[[i]] <- "\\end{minipage} \n"
  i <- i + 1
  #end of Figure 10
  out[[i]] <- "\n"
  
  
  #End of Figures:
  out[[i]] <- "\\clearpage\n"
  out
})

tempLF <- lapply(tempLF, function(x) paste(x, collapse = "\n"))
tempLF <- paste(tempLF, collapse = "\n")

## first class tickets, L species
taxo.final <- in.df
names(taxo.final)[1:3] <- c("species.code","comm.english","comm.fr")
taxo.final <- taxo.final[taxo.final$type %in% c("LFn"),]
## 



## second class tickets, I species
taxo.final <- in.df
taxo.final <- taxo.final[taxo.final$type %in% c("LI"),]
names(taxo.final)[1:3] <- c("species.code","comm.english","comm.fr")


tempI <- lapply(taxo.final$species.code, function(x) {
  
  out <- list()
  ## figure files 
  spp_file1 <- paste0("RV-4VWX-",x,"-IDWbiomass.pdf")
  
  spp_file2 <- paste0("RV-4VWX-",x,"-stratifiedB.pdf")
  spp_file3 <- paste0("RV-4VWX-",x,"-distribution-indices-usingB.pdf")
  spp_file4 <- paste0("RV-4VWX-",x,"-BvsD75corr.pdf")

  latin_name <- taxo.final$scientificname[taxo.final$species.code == x]
  english_name <- taxo.final$comm.english[taxo.final$species.code == x]
  french_name <- taxo.final$comm.fr[taxo.final$species.code == x]
  family_name <- taxo.final$family[taxo.final$species.code == x]
  worms_id <- taxo.final$AphiaID[taxo.final$species.code == x]
  worms_link <- taxo.final$url[taxo.final$species.code == x]
  category <- taxo.final$type[taxo.final$species.code == x]
  
  i <- 1
  out[[i]] <- "\\renewcommand\\thefigure{\\thesubsection\\Alph{figure}} \n"
  i <- i + 1
  out[[i]] <- "\\setcounter{figure}{0} \n"
  i <- i + 1
  out[[i]] <- paste0("## ", english_name, " (", french_name, ") - species code ", x, " (category ", category, ")"," {#sec:", x, "} \n")
  i <- i + 1
  out[[i]] <- paste0("\\index{", french_name, "} ",  "\\index{", english_name, "} ",  "\\index{", latin_name, "} \n")
  i <- i + 1
  out[[i]] <- paste0("\\index{", family_name, "!", latin_name, "} \n")
  i <- i + 1
  out[[i]] <- paste0(
    "Scientific name: [", latin_name, "](",worms_link,") \n \\newline")
  i <- i + 1
  #Figure 1
  out[[i]] <- "\\begin{minipage}{1.0\\textwidth}"
  i <- i + 1
  out[[i]] <- " \\begin{tabular}{c}"
  i <- i + 1
  out[[i]] <- paste0("\\includegraphics[width=7in]{../Figures-Actual/",
                     spp_file1, "} \\\\ ")
  i <- i + 1
  out[[i]] <- "\\end{tabular} "
  i <- i + 1
  out[[i]] <- paste0("\\captionof{figure}{Inverse distance weighted distribution of catch biomass (kg/tow) for ", english_name,".}")
  i <- i + 1
  out[[i]] <- "\\end{minipage} \n"
  i <- i + 1
  #end of Figure 1
  out[[i]] <- "\\newline  \n"
  i <- i + 1
  out[[i]] <- "\\vspace{1cm}  \n"
  i <- i + 1
  
  #Figures 2,3 and 4
  out[[i]] <- "\\begin{minipage}{1.0\\textwidth}"
  i <- i + 1
  out[[i]] <- " \\begin{tabular}{ccc}"
  i <- i + 1
  out[[i]] <- paste0("\\includegraphics[width=2.0in]{../Figures-Actual/",
                     spp_file2, "} & ")
  i <- i + 1
  out[[i]] <- paste0("\\includegraphics[width=2.0in]{../Figures-Actual/",
                     spp_file3, "} & ")
  i <- i + 1
  out[[i]] <- paste0("\\includegraphics[width=2.0in]{../Figures-Actual/",
                     spp_file4, "} \\\\ ")
  i <- i + 1
  out[[i]] <- "\\end{tabular} "
  i <- i + 1
  out[[i]] <- paste0("\\captionof{figure}{Stratified random estimates of biomass (kg/tow), D75 and D95 and the correlation between D75 and biomass of ", english_name,".}")
  i <- i + 1
  out[[i]] <- "\\end{minipage} \n"
  i <- i + 1
  #end of Figure 2, 3 and 4
  
  #End of Figures:
  out[[i]] <- "\\clearpage\n"
  out
})

## 
tempI <- lapply(tempI, function(x) paste(x, collapse = "\n"))
tempI <- paste(tempI, collapse = "\n")


## second class tickets, I species
taxo.final <- in.df
taxo.final <- taxo.final[taxo.final$type %in% c("LIn"),]
names(taxo.final)[1:3] <- c("species.code","comm.english","comm.fr")


tempIn <- lapply(taxo.final$species.code, function(x) {
  
  out <- list()
  ## figure files 
  spp_file1 <- paste0("RV-4VWX-",x,"-IDWabundance.pdf")
  
  spp_file2 <- paste0("RV-4VWX-",x,"-stratifiedN.pdf")
  spp_file3 <- paste0("RV-4VWX-",x,"-distribution-indices-usingN.pdf")
  spp_file4 <- paste0("RV-4VWX-",x,"-NvsD75corr.pdf")
  
  latin_name <- taxo.final$scientificname[taxo.final$species.code == x]
  english_name <- taxo.final$comm.english[taxo.final$species.code == x]
  french_name <- taxo.final$comm.fr[taxo.final$species.code == x]
  family_name <- taxo.final$family[taxo.final$species.code == x]
  worms_id <- taxo.final$AphiaID[taxo.final$species.code == x]
  worms_link <- taxo.final$url[taxo.final$species.code == x]
  category <- taxo.final$type[taxo.final$species.code == x]
  
  i <- 1
  out[[i]] <- "\\renewcommand\\thefigure{\\thesubsection\\Alph{figure}} \n"
  i <- i + 1
  out[[i]] <- "\\setcounter{figure}{0} \n"
  i <- i + 1
  out[[i]] <- paste0("## ", english_name, " (", french_name, ") - species code ", x, " (category ", category, ")"," {#sec:", x, "} \n")
  i <- i + 1
  out[[i]] <- paste0("\\index{", french_name, "} ",  "\\index{", english_name, "} ",  "\\index{", latin_name, "} \n")
  i <- i + 1
  out[[i]] <- paste0("\\index{", family_name, "!", latin_name, "} \n")
  i <- i + 1
  out[[i]] <- paste0(
    "Scientific name: [", latin_name, "](",worms_link,") \n \\newline")
  i <- i + 1
  #Figure 1
  out[[i]] <- "\\begin{minipage}{1.0\\textwidth}"
  i <- i + 1
  out[[i]] <- " \\begin{tabular}{c}"
  i <- i + 1
  out[[i]] <- paste0("\\includegraphics[width=7in]{../Figures-Actual/",
                     spp_file1, "} \\\\ ")
  i <- i + 1
  out[[i]] <- "\\end{tabular} "
  i <- i + 1
  out[[i]] <- paste0("\\captionof{figure}{Inverse distance weighted distribution of catch abundance (N/tow) for ", english_name,".}")
  i <- i + 1
  out[[i]] <- "\\end{minipage} \n"
  i <- i + 1
  #end of Figure 1
  out[[i]] <- "\\newline  \n"
  i <- i + 1
  out[[i]] <- "\\vspace{1cm}  \n"
  i <- i + 1
  
  #Figures 2,3 and 4
  out[[i]] <- "\\begin{minipage}{1.0\\textwidth}"
  i <- i + 1
  out[[i]] <- " \\begin{tabular}{ccc}"
  i <- i + 1
  out[[i]] <- paste0("\\includegraphics[width=2.0in]{../Figures-Actual/",
                     spp_file2, "} & ")
  i <- i + 1
  out[[i]] <- paste0("\\includegraphics[width=2.0in]{../Figures-Actual/",
                     spp_file3, "} & ")
  i <- i + 1
  out[[i]] <- paste0("\\includegraphics[width=2.0in]{../Figures-Actual/",
                     spp_file4, "} \\\\ ")
  i <- i + 1
  out[[i]] <- "\\end{tabular} "
  i <- i + 1
  out[[i]] <- paste0("\\captionof{figure}{Stratified random estimates of biomass (kg/tow), D75 and D95 and the correlation between D75 and biomass of ", english_name,".}")
  i <- i + 1
  out[[i]] <- "\\end{minipage} \n"
  i <- i + 1
  #end of Figure 2, 3 and 4
  
  #End of Figures:
  out[[i]] <- "\\clearpage\n"
  out
})

## 
tempIn <- lapply(tempIn, function(x) paste(x, collapse = "\n"))
tempIn <- paste(tempIn, collapse = "\n")



## third class tickets, SF species
taxo.final <- in.df
names(taxo.final)[1:3] <- c("species.code","comm.english","comm.fr")
taxo.final <- taxo.final[taxo.final$type %in% c("SF"),]

tempS <- lapply(taxo.final$species.code, function(x) {
  
  out <- list()
  ## figure files 
  spp_file1 <- paste0("RV-4VWX-",x,"-IDWbiomassSspecies.pdf")
  spp_file2 <- paste0("RV-4VWX-",x,"-stratifiedB.pdf")
  spp_file3 <- paste0("RV-4VWX-",x,"-distribution-indices-usingB.pdf")
  spp_file4 <- paste0("RV-4VWX-",x,"-BvsD75corr.pdf")
  
  latin_name <- taxo.final$scientificname[taxo.final$species.code == x]
  english_name <- taxo.final$comm.english[taxo.final$species.code == x]
  french_name <- taxo.final$comm.fr[taxo.final$species.code == x]
  family_name <- taxo.final$family[taxo.final$species.code == x]
  worms_id <- taxo.final$AphiaID[taxo.final$species.code == x]
  worms_link <- taxo.final$url[taxo.final$species.code == x]
  category <- taxo.final$type[taxo.final$species.code == x]
  
  i <- 1
  out[[i]] <- "\\renewcommand\\thefigure{\\thesubsection\\Alph{figure}} \n"
  i <- i + 1
  out[[i]] <- "\\setcounter{figure}{0} \n"
  i <- i + 1
  out[[i]] <- paste0("## ", english_name, " (", french_name, ") - species code ", x, " (category ", category, ")"," {#sec:", x, "} \n")
  i <- i + 1
  out[[i]] <- paste0("\\index{", french_name, "} ",  "\\index{", english_name, "} ",  "\\index{", latin_name, "} \n")
  i <- i + 1
  out[[i]] <- paste0("\\index{", family_name, "!", latin_name, "} \n")
  i <- i + 1
  out[[i]] <- paste0(
    "Scientific name: [", latin_name, "](",worms_link,") \n \\newline")
  i <- i + 1
  #Figure 1
  out[[i]] <- "\\begin{minipage}{1.0\\textwidth}"
  i <- i + 1
  out[[i]] <- " \\begin{tabular}{c}"
  i <- i + 1
  out[[i]] <- paste0("\\includegraphics[width=6.5in]{../Figures-Actual/",
                     spp_file1, "} \\\\ ")
  i <- i + 1
  out[[i]] <- "\\end{tabular} "
  i <- i + 1
  out[[i]] <- paste0("\\captionof{figure}{Inverse distance weighted distribution of catch biomass (kg/tow) for ", english_name,".}")
  i <- i + 1
  out[[i]] <- "\\end{minipage} \n"
  i <- i + 1
  #end of Figure 1
  #end of Figure 1
  out[[i]] <- "\\newline  \n"
  i <- i + 1
  
  #Figures 2,3 and 4
  out[[i]] <- "\\begin{minipage}{1.0\\textwidth}"
  i <- i + 1
  out[[i]] <- " \\begin{tabular}{ccc}"
  i <- i + 1
  out[[i]] <- paste0("\\includegraphics[width=2.0in]{../Figures-Actual/",
                     spp_file2, "} & ")
  i <- i + 1
  out[[i]] <- paste0("\\includegraphics[width=2.0in]{../Figures-Actual/",
                     spp_file3, "} & ")
  i <- i + 1
  out[[i]] <- paste0("\\includegraphics[width=2.0in]{../Figures-Actual/",
                     spp_file4, "} \\\\ ")
  i <- i + 1
  out[[i]] <- "\\end{tabular} "
  i <- i + 1
  out[[i]] <- paste0("\\captionof{figure}{Stratified random estimates of biomass (kg/tow), D75 and D95 and the correlation between D75 and biomass of ", english_name,".}")
  i <- i + 1
  out[[i]] <- "\\end{minipage} \n"
  i <- i + 1
  #end of Figure 2, 3 and 4
  
  #End of Figures:
  out[[i]] <- "\\clearpage\n"
  out
})

## 
tempS <- lapply(tempS, function(x) paste(x, collapse = "\n"))
tempS <- paste(tempS, collapse = "\n")


## fourth and fifth class tickets, LR and SR species
taxo.final <- in.df
names(taxo.final)[1:3] <- c("species.code","comm.english","comm.fr")
taxo.final <- taxo.final[taxo.final$type %in% c("LR","SR"),]

tempR <- lapply(taxo.final$species.code, function(x) {
  
  out <- list()
  ## figure files 
  spp_file1 <- paste0("RV-4VWX-",x,"-presence.pdf")
  
  latin_name <- taxo.final$scientificname[taxo.final$species.code == x]
  english_name <- taxo.final$comm.english[taxo.final$species.code == x]
  french_name <- taxo.final$comm.fr[taxo.final$species.code == x]
  family_name <- taxo.final$family[taxo.final$species.code == x]
  worms_id <- taxo.final$AphiaID[taxo.final$species.code == x]
  worms_link <- taxo.final$url[taxo.final$species.code == x]
  category <- taxo.final$type[taxo.final$species.code == x]
  
  i <- 1
  out[[i]] <- "\\renewcommand\\thefigure{\\thesubsection\\Alph{figure}} \n"
  i <- i + 1
  out[[i]] <- "\\setcounter{figure}{0} \n"
  i <- i + 1
  out[[i]] <- paste0("## ", english_name, " (", french_name, ") - species code ", x, " (category ", category, ")"," {#sec:", x, "} \n")
  i <- i + 1
  out[[i]] <- paste0("\\index{", french_name, "} ",  "\\index{", english_name, "} ",  "\\index{", latin_name, "} \n")
  i <- i + 1
  out[[i]] <- paste0("\\index{", family_name, "!", latin_name, "} \n")
  i <- i + 1
  out[[i]] <- paste0(
    "Scientific name: [", latin_name, "](",worms_link,") \n \\newline")
  i <- i + 1
  #Figure 1
  out[[i]] <- "\\begin{minipage}{1.0\\textwidth}"
  i <- i + 1
  out[[i]] <- " \\begin{tabular}{c}"
  i <- i + 1
  out[[i]] <- paste0("\\includegraphics[width=6.0in]{../Figures-Actual/",
                     spp_file1, "} \\\\ ")
  i <- i + 1
  out[[i]] <- "\\end{tabular} "
  i <- i + 1
  out[[i]] <- paste0("\\captionof{figure}{Catch distribution for ", english_name,".}")
  i <- i + 1
  out[[i]] <- "\\end{minipage} \n"
  i <- i + 1
  #end of Figure 1
  
  #End of Figures:
  out[[i]] <- "\\clearpage\n"
  out
})

## 
tempR <- lapply(tempR, function(x) paste(x, collapse = "\n"))
tempR <- paste(tempR, collapse = "\n")

## put the 5 tickets classes together and write to the report folder
temp <- c(
"<!-- This page has been automatically generated: do not edit by hand -->

# Figures for all species analysed {#app:appendix}

The figures generated for each species are presented here and consist of up to six figures (Figure types A to F) per species, depending on their taxonomic level classification (as described in Section \\@ref(sec:taxo)). The figure types are used as a suffix in each figure number.

To facilitate navigation, use the PDF navigation panel. Alternatively, Table \\@ref(tab:tabspecies) contains the list of all species presented and includes a hyperlink to the first page of figures for each species. Finally an Index is included at the end of the document, containing hyperlinks to each species based on its scientific name, English common name or French common name.

\\pagebreak \n", 
  tempLF, 
  tempI, 
  tempIn,
  tempS, 
  tempR, 
  "\\printindex \n")

#if (!exists("N"))
  writeLines(temp, con = file.path("TechReport-EN", "plot-pages.Rmd"), useBytes=T)


########################################################################################################
