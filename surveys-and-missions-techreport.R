## script to produce the contents of the Maritimes surveys summary Tech Report 
##
##
library(sf)
library(Mar.datawrangling)

## gather all the potential surveys from the Oracle database

get_data(db='rv', usepkg = 'rodbc', data.dir = "c:/Users/RicardD/Documents/GitHub/Maritimes-SUMMER-Atlas/data", 
         fn.oracle.username = "RICARDD", fn.oracle.password = "1020wel99", fn.oracle.dsn = "PTRAN")



GSINF$LONGITUDE <- -1 * gulf::dmm2deg(GSINF$SLONG)
GSINF$LATITUDE <- gulf::dmm2deg(GSINF$SLAT)



source("load-AC.R")
## define a function to generate a map showing the cruise track
cruise.track.figure.fct <- function(survey, mission, dat.in){
  o1 <- order(dat.in$SETNO)
  dat.in <- dat.in[o1,]

  g <- ggplot(data = boundaries_simple) + 
    geom_sf(data=boundaries_simple, fill="cornsilk", color=grey(0.8)) +
    theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5), panel.background = element_rect(fill = "powderblue")) + #, panel.border=element_rect(linetype="solid")
    xlim(-68,-57) + ylim(41.9,47) +
    xlab("Longitude (\u{B0}W)") + ylab("Latitude (\u{B0}N)") +
    geom_line(data=dat.in, aes(x=LONGITUDE, y=LATITUDE)) +
    geom_point(data=dat.in, aes(x=LONGITUDE, y=LATITUDE)) 
  
  f1.n <- file.path(mapping.path, paste0(survey, "-mission-track-", mission, ".pdf"))
  ggsave(f1.n, g)
  
  
}# end function


## for each survey, obtain the data necessary to generate a cruise track and call the function to make a figure of the cruise track
vars <- c("MISSION","SETNO","SDATE","LONGITUDE","LATITUDE","TYPE")

get_survey(db='rv',survey="4X",keepBadSets = TRUE,data.dir = "c:/Users/RicardD/Documents/GitHub/Maritimes-SUMMER-Atlas/data")
missions <- unique(GSINF$MISSION)

lapply(missions, function(m){
  cruise.track.figure.fct("4X", m, GSINF[GSINF$MISSION==m,vars])
})
# cruise.track.figure.fct("4X","NED2017002", GSINF[GSINF$MISSION=="NED2017002",vars])



## make an Rmd file for the Report 

temp.4X <- lapply(missions, function(x) {
  out <- list()
  ## figure files 
  cruise.name <- x
  spp_file1 <- file.path(mapping.path, paste0("4X-mission-track-",x,".pdf"))
  
  i <- 1
  out[[i]] <- paste0("### ", x," {#sec:", x, "} \n")
  i <- i + 1
  out[[i]] <- "\\begin{minipage}{0.9\\textwidth}"
  i <- i + 1
  out[[i]] <- " \\begin{tabular}{c}"
  i <- i + 1
  out[[i]] <- paste0("\\includegraphics[width=6in]{",
                     spp_file1, "} \n")
  i <- i + 1
  out[[i]] <- "\\end{tabular} "
  i <- i + 1
  out[[i]] <- paste0("\\captionof{figure}{Cruise track for ", x, ".}")
  i <- i + 1
  out[[i]] <- "\\end{minipage} \n"
  i <- i + 1
  
  
  out[[i]] <- "\n"
  i <- i + 1
  out[[i]] <- "\\clearpage\n"
  
  out
})

temp.4X <- lapply(temp.4X, function(x) paste(x, collapse = "\n"))
temp.4X <- paste(temp.4X, collapse = "\n")





temp <- c("# Mission tracks {#mission-tracks} \n<!-- This page has been automatically generated: do not edit by hand -->\n ", 
          "## 4X survey {#sec:4x} \n", temp.4X, " \n")
if (!exists("N"))
  writeLines(temp, con = file.path("TechReport-surveys-and-missions","mission-track-pages.Rmd"), useBytes=T)

