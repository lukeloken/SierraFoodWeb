

# Plot temporal and vertical variability in 13C POM from literature values
# input is google drive folder of literature 13C POM or phytoplankton
# Each figure also includes data from our paper

#Code to read in isotope data


library(readxl)
library(plyr)
library(dplyr)
library(tidyr)
library(viridis)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(RColorBrewer)
library(ggplot2)

source('R/read_excel_allsheets.R')
source('R/g_legend.R')
source('R/PlotErrorBar.R')


# Box folder where data and outputs are stored
box_dir<-'C:/Users/lcloken/Box/SadroLab/SierraFoodWeb'

files<-list.files(paste(box_dir,"Data", sep='/'))
files

fig7_list<-read_excel_allsheets(paste(box_dir,"Data", "Fig7.xlsx", sep='/')) 

space_df<-fig7_list[[2]]
time_df<-fig7_list[[1]]


colorset<-'Dark2'
colors<-brewer.pal(3, colorset_incubation)[c(1,3,2)]

space_df2<-space_df %>%
  mutate(Depth_strata = factor(space_df$Depth_strata, c('Epilimnion', "Metalimnion", 'Hypolimnion'))) %>%
  drop_na(Depth_strata) %>%
  group_by(LakeName, Depth_strata) %>%
  arrange(LakeName, Depth_strata)

name_table<-space_df2 %>%
  select(LakeName, Depth_strata, C13mean) %>%
  group_by(LakeName) %>%
  summarize(mean=mean(C13mean), n=n())

  name_table$LakeName <- gsub('Lake', '', name_table$LakeName)
  name_table$LakeName <- gsub('Reservoir', '', name_table$LakeName)
  name_table$LakeName <- gsub(' ', '', name_table$LakeName)
  
  
space_df2$plot_nu<-1:nrow(space_df2)
space_df2$colour_nu<-match(space_df2$Depth_strata, levels(space_df2$Depth_strata))
space_df2$colour <- colors[space_df2$colour_nu]

ggplot(aes(x=LakeName, y=C13mean, colour=Depth_strata), data=space_df2) + 
  # geom_boxplot(aes(colour=Depth_strata)) + 
  geom_point(aes(colour=Depth_strata), size=2, pch=15) +
  geom_errorbar(aes(ymin=C13mean-C13sd, ymax=C13mean+C13sd), width=.3) + 
  theme_bw()



png(paste0(box_dir, '/Figures/Compare13CLiterature_Vertical.png'), units='in', width=5.5, height=3, res=300, bg='white')

par(mfrow=c(1,1),mar=c(3,3,1,0.5), xpd=F, mgp=c(1.5,0.3,0), tck=-0.02)

plot(space_df2$plot_nu, space_df2$C13mean, col=space_df2$colour, pch=15, ylim=c(-51, -23), xlim=c(0.5, max(space_df2$plot_nu)+0.5), xaxs='i', axes=F, ylab='', xlab='', cex=1)
error.bar(x=space_df2$plot_nu, y=space_df2$C13mean, upper.y=space_df2$C13sd, col=space_df2$colour, lwd=1.5, length=0.05)
abline(v=space_df2$plot_nu[which(space_df2$Depth_strata=='Epilimnion')]-0.5, lty=3)
box(which='plot')

axis(2, las=1, cex.axis=0.7)
axis(1, at=c(1.5, 4, 6.5, 9, 12, 14.5, 16.5), labels=name_table$LakeName, cex.axis=0.7)

mtext( expression(paste(delta^{13}, "C POM (\u2030)")), side=2, line=1.5)
mtext( "Lake", side=1, line=1.5)

legend('bottomright', c(levels(space_df2$Depth_strata)), text.col=colors, bty='n')

dev.off()
