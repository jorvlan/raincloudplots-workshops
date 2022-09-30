# running on MacOS 12.5 Rstudio 2022.07.01 on 22-10-2022 by Nicholas Judd
# original source: https://github.com/RainCloudPlots/RainCloudPlots/tree/master/tutorial_R

packages <- c("patchwork", "tidyverse", "lavaan")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))
}

# Load packages ----
library(tidyverse); library(patchwork)

getwd() # this tells you the current path of R
# we need to direct R to the folder with the scripts & data
# we set the path by using setwd()
# for example:
# setwd("/Users/njudd/projects/rain/raincloudplots-workshops/tutorial/")

source("geom_flat_violin.R") #this only works if you set the right path!
source("fn_summary_SE.r") # This does the summary. For each group's data frame, return a vector with
# N, mean, median, and sd


# first we will start by simulating some data! :)
m <- 50 # mean
s <- 25 # sd
sim_n <- 250 # draws

# Calculate log-normal parameters ----
location <- log(m^2 / sqrt(s^2 + m^2))
shape <- sqrt(log(1 + (s^2 / m^2)))

# Set seed to get same data everytime ----
set.seed(42)

# Create data by hand ----
simdat_group1 <- rlnorm(sim_n, location, shape)
simdat_group2 <- rnorm(sim_n, m, s)

simdat <- c(simdat_group1, simdat_group2) %>% as_tibble() %>% dplyr::rename(score = 1)
simdat <- simdat %>% mutate(group = 
                              fct_inorder(c(rep("Group1", times = sim_n),
                                            rep("Group2", times = sim_n))))

# Calculate summary stats ----
summary_simdat <- fn_summary_SE(simdat, score, group)

# lets look at the data
head(simdat)
str(simdat)
summary_simdat

# now we will have a basic example of a single raincloud plot
p0_h <- ggplot(simdat, aes(x = 1, y=score)) +
  geom_flat_violin(position = position_nudge(x = .23, y = 0),adjust = 2, width = .5, fill = "red", color = NA)+
  geom_boxplot(position = position_nudge(x = .17, y = 0), width = .1, outlier.colour = NA, fill = "red") +
  geom_point(position = position_jitter(width = .10), size = .3, color = "red", alpha = .4)+
  coord_cartesian(xlim = c(0.2, 2)) + # change to .75 & 1.5 to zoom in
  labs(x = "Group", y = "Score") + 
  theme_minimal() + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

p0_h # printing the plot

p0_v <- p0_h + coord_flip() #flipping the plot
p0_v
#try to save the plot with ggsave()!

# Plot with colours and coordinate flip
p3 <- ggplot(simdat, aes(x=group,y=score, fill = group))+
  #geom_flat_violin(position = position_nudge(x = .2, y = 0),adjust = 2)+
  geom_point(position = position_jitter(width = .15), size = .25)+
  ylab('Score')+xlab('Group')+theme_classic()+guides(fill = "none")+
  coord_flip() + # remove coord flip to change orientation
  ggtitle('Figure 3: The Basic Raincloud with Colour')
# here we have a raincloud with two groups
p3 + geom_flat_violin(position = position_nudge(x = .2, y = 0),adjust = 2)

# we can also change the smoothing with adjust
# here is one with reduced smoothing
p3 + geom_flat_violin(position = position_nudge(x = .2, y = 0),adjust = .1)

#Rainclouds with boxplots
p5 <- ggplot(simdat, aes(x=group,y=score, fill = group))+
  geom_flat_violin(position = position_nudge(x = .25, y = 0),adjust =2)+
  geom_point(position = position_jitter(width = .15), size = .25)+
  #note that here we need to set the x-variable to a numeric variable and bump it to get the boxplots to line up with the rainclouds. 
  geom_boxplot(aes(x = as.numeric(group)+0.25, y = score), outlier.shape = NA, alpha = 0.3, width = .1, colour = "BLACK") +
  ylab('Score')+xlab('Group')+coord_flip()+theme_classic()+guides(fill = "none", colour = "none")
  
p5 + ggtitle("Figure 5: Raincloud Plot w/Boxplots")

# we can change the colors!
p6 <- p5 + ggtitle("Figure 6: Change in Colour Palette") +
  scale_colour_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2")
p6

##### now lets try the package
# https://github.com/jorvlan/raincloudplots
# the package can make 4 plots
# 1. a two group raincloud
# 2. a two repeated measures raincloud
# 3. a two group two repeated measures raincloud
# 4. a two group 3 repeated measures raincloud

if (!require(remotes)) {
  install.packages("remotes")
}
remotes::install_github('jorvlan/raincloudplots')

library(raincloudplots)

# the data structure is fixed, x & y must be labeled x_axis & y_axis
# the data also need to a pre-jittered
# there are two functions to do this: 1) data_1x1() & 2) data_2x2()


simdat_package <- data_1x1(
  array_1 = simdat[1:250,]$score, # grabbing a vector of the scores from the first group
  array_2 = simdat[251:500,]$score, # grabbing a vector 
  jit_distance = .09, # the specifies the amount of jitter
  jit_seed = 321)

raincloud_1_h <- raincloud_1x1(
  data = simdat_package, 
  colors = (c('dodgerblue','darkorange')), 
  fills = (c('dodgerblue','darkorange')), 
  size = 1, 
  alpha = .6, 
  ort = 'h') +
  scale_x_continuous(breaks=c(1,2), labels=c("Group1", "Group2"), limits=c(0, 3)) +
  labs(x = "Groups", y = "Score") +
  theme_classic()

# now try to change the orientation!

# here we will make a repeated measures raincloud
raincloud_2 <- raincloud_1x1_repmes(
  data = simdat_package,
  colors = (c('dodgerblue', 'darkorange')),
  fills = (c('dodgerblue', 'darkorange')),
  line_color = 'gray',
  line_alpha = .3,
  size = 1,
  alpha = .6,
  align_clouds = FALSE) +
  
  scale_x_continuous(breaks=c(1,2), labels=c("Pre", "Post"), limits=c(0, 3)) +
  xlab("Time") + 
  ylab("Score") +
  theme_classic()

raincloud_2


# advanced rainclouds!




#Rainclouds with mean and confidence interval
p7 <- ggplot(simdat,aes(x=group,y=score, fill = group, colour = group))+
  geom_flat_violin(position = position_nudge(x = .25, y = 0),adjust =2)+
  geom_point(position = position_jitter(width = .15), size = .25)+
  geom_point(data = summary_simdat, aes(x = group, y = score_mean), position = position_nudge(.25), colour = "BLACK")+
  geom_errorbar(data = summary_simdat, aes(x = group, y = score_mean, ymin = score_mean-score_sem_ci, ymax = score_mean+score_sem_ci), position = position_nudge(.25), colour = "BLACK", width = 0.1, size = 0.8)+
  ylab('Score')+xlab('Group')+coord_flip()+theme_classic()+guides(fill = "none", colour = "none") +
  scale_colour_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+
  ggtitle("Figure 7: Raincloud Plot with Mean ± 95% CI")
p7


# see if you can get stat_summary to do the same! (check out the lines commented off)
# If you have issues installing Hmisc just move on

p7_c <- ggplot(simdat,aes(x=group,y=score, fill = group, colour = group))+
  geom_flat_violin(position = position_nudge(x = .25, y = 0),adjust =2)+
  geom_point(position = position_jitter(width = .15), size = .25) +
  #stat_summary(aes(y = score), fun = "mean", position = position_nudge(.25), colour = "BLACK", geom = "point") +
  #stat_summary(aes(y = score), fun.data  = "mean_cl_normal", position = position_nudge(.25), colour = "BLACK", geom = "errorbar", width = .1) +
  geom_point(data = summary_simdat, aes(x = group, y = score_mean), position = position_nudge(.25), colour = "BLACK")+
  geom_errorbar(data = summary_simdat, aes(x = group, y = score_mean, ymin = score_mean-score_sem_ci, ymax = score_mean+score_sem_ci), position = position_nudge(.25), colour = "BLACK", width = 0.1, size = 0.8)+
  ylab('Score')+xlab('Group')+coord_flip()+theme_classic()+guides(fill = "none", colour = "none") +
  scale_colour_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+
  ggtitle("Figure 7: Raincloud Plot with Mean ± 95% CI")
p7_c


#Rainclouds with striated data

#Round data
simdat_round<-simdat
simdat_round$score<-round(simdat$score,0) 

#Striated/grouped when no jitter applied
ap1 <- ggplot(simdat_round,aes(x=group,y=score,fill=group,col=group))+
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .6,adjust =4)+
  geom_point(size = 1, alpha = 0.6)+ylab('Score')+xlab("")+
  scale_fill_brewer(palette = "Dark2")+
  scale_colour_brewer(palette = "Dark2")+
  guides(fill = "none", col = "none")+
  ggtitle('Striated')

#Added jitter helps
ap2 <- ggplot(simdat_round,aes(x=group,y=score,fill=group,col=group))+
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .4,adjust =4)+
  geom_point(position=position_jitter(width = .15),size = 1, alpha = 0.4)+ylab('Score')+ xlab("")+
  scale_fill_brewer(palette = "Dark2")+
  scale_colour_brewer(palette = "Dark2")+
  guides(fill = "none", col = "none")+
  ggtitle('Added jitter')


# with patchwork you can add ggplots together!
p8 <- ap1 + ap2 + plot_annotation("Figure 8: Jittering Ordinal Data", tag_levels = "a")
p8


#Add additional factor/condition
simdat$gr2<-as.factor(c(rep('high',125),rep('low',125),rep('high',125),rep('low',125)))

p9 <- ggplot(simdat,aes(x=group,y=score, fill = group, colour = group))+
  geom_flat_violin(position = position_nudge(x = .25, y = 0),adjust =2, trim = TRUE)+
  geom_point(position = position_jitter(width = .15), size = .25)+
  geom_boxplot(aes(x = as.numeric(group)+0.25, y = score),outlier.shape = NA, alpha = 0.3, width = .1, colour = "BLACK") +
  ylab('Score')+xlab('')+coord_flip()+theme_classic()+guides(fill = "none", colour = "none") + facet_wrap(~gr2)+
  scale_colour_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+
  ggtitle("Figure 9: Complex Raincloud Plots with Facet Wrap")
p9

# p10 needs a csv in the same folder
# loading the data: make sure to get getwd() and do setwd()
rep_data <- read_csv("repeated_measures_data.csv", 
                     col_types = cols(group = col_factor(levels = c("1", "2")), 
                                      time = col_factor(levels = c("1", "2", "3"))))

sumrepdat <- fn_summary_SE(rep_data, "score", c("group", "time"))
head(sumrepdat); str(sumrepdat)

# Rainclouds for repeated measures, continued 
p10 <- ggplot(rep_data, aes(x = time, y = score, fill = group)) +
  geom_flat_violin(aes(fill = group),position = position_nudge(x = .1, y = 0), adjust = 1.5, trim = FALSE, alpha = .5, colour = NA)+
  geom_point(aes(x = as.numeric(time)-.15, y = score, colour = group),position = position_jitter(width = .05), size = 1, shape = 20)+
  geom_boxplot(aes(x = time, y = score, fill = group),outlier.shape = NA, alpha = .5, width = .1, colour = "black")+
  scale_colour_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+
  ggtitle("Figure 10: Repeated Measures Factorial Rainclouds")

p10

#Rainclouds for repeated measures, additional plotting options 

p11 <- ggplot(rep_data, aes(x = time, y = score, fill = group)) +
  geom_flat_violin(aes(fill = group),position = position_nudge(x = .1, y = 0), adjust = 1.5, trim = FALSE, alpha = .5, colour = NA)+
  geom_point(aes(x = as.numeric(time)-.15, y = score, colour = group),position = position_jitter(width = .05), size = .25, shape = 20)+
  geom_boxplot(aes(x = time, y = score, fill = group),outlier.shape = NA, alpha = .5, width = .1, colour = "black")+
  geom_line(data = sumrepdat, aes(x = as.numeric(time)+.1, y = score_mean, group = group, colour = group), linetype = 3)+
  geom_point(data = sumrepdat, aes(x = as.numeric(time)+.1, y = score_mean, group = group, colour = group), shape = 18) +
  geom_errorbar(data = sumrepdat, aes(x = as.numeric(time)+.1, y = score_mean, group = group, colour = group, ymin = score_mean-score_sem_ci, ymax = score_mean+score_sem_ci), width = .05)+
  scale_colour_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+
  ggtitle("Figure 11: Repeated Measures - Factorial (Extended)")
p11

#Rainclouds for repeated measures, additional plotting options 

p12 <- ggplot(rep_data, aes(x = group, y = score, fill = time)) +
  geom_flat_violin(aes(fill = time),position = position_nudge(x = .1, y = 0), adjust = 1.5, trim = FALSE, alpha = .5, colour = NA)+
  geom_point(aes(x = as.numeric(group)-.15, y = score, colour = time),position = position_jitter(width = .05), size = .25, shape = 20)+
  geom_boxplot(aes(x = group, y = score, fill = time),outlier.shape = NA, alpha = .5, width = .1, colour = "black")+
  geom_line(data = sumrepdat, aes(x = as.numeric(group)+.1, y = score_mean, group = time, colour = time), linetype = 3)+
  geom_point(data = sumrepdat, aes(x = as.numeric(group)+.1, y = score_mean, group = time, colour = time), shape = 18) +
  geom_errorbar(data = sumrepdat, aes(x = as.numeric(group)+.1, y = score_mean, group = time, colour = time, ymin = score_mean-score_sem_ci, ymax = score_mean+score_sem_ci), width = .05)+
  scale_colour_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+
  ggtitle("Figure 12: Repeated Measures - Factorial (Extended)") +
  coord_flip()
p12