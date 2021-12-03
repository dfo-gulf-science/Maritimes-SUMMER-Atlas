## average fish condition plot, separately for NAFO 4X and 4VW

conditionNAFO.fct <- function(spec.code, file.name) {
  
  fn <- file.path(figdata.path, paste0("SS",spec.code,"_lw.csv"))
  lw <- read.csv(fn, header=TRUE)
  
  all.df <- lw

	all.df$nafo <- factor(all.df$nafo, levels=c("4X","4VW"), ordered=TRUE)
	all.df$year <- factor(all.df$YEAR, levels=1969:2021, ordered=TRUE)
	
	p <- ggplot(data=all.df, aes(x=year, y=condition, group=nafo)) + 
	  # geom_violin(fill=grey(0.8)) + facet_grid(.~nafo) +
	  geom_jitter(position=position_jitter(width=0.3, height=0.2)) +
	  #geom_boxplot(fill=grey(0.8)) + 
	  facet_grid(.~nafo) + 
	  geom_hline(yintercept=1, col=grey(0.7), lty=2) +
	  stat_summary(fun=mean, geom="line", color="black") +
	  stat_summary(fun=mean, geom="point", color="black", pch=19) +
	  stat_summary(fun=function(z){quantile(z,0.75)}, geom="line", color=grey(0.5)) +
	  stat_summary(fun=function(z){quantile(z,0.25)}, geom="line", color=grey(0.5)) +
	  #stat_summary(fun.min= function(z){quantile(z,0.25)}, fun.max= function(z){quantile(z,0.75)}, fun=median) + 
	  #stat_summary(fun=mean, geom="point", shape=20, size=2, color="red", fill="red") +
	  theme_bw() +
	  theme(panel.grid.major = element_blank(),
	        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
	  scale_y_log10() + 
	  scale_x_discrete(breaks=seq(1970,2020,10), limits=factor(1969:2021, levels=1969:2021, ordered=TRUE)) +
	  xlab("Year") + ylab("Fish condition")
	  
	p
	
	ggsave(filename=file.path(fig.path, file.name), p, width=12, height=6, units="in")
	

	} # end function

