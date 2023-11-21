# running on MacOS 12.5 Rstudio 2022.07.01 on 22-10-2022 by Nicholas Judd
# updated for ggrain on 2023-02-11
# original source: https://github.com/RainCloudPlots/RainCloudPlots/tree/master/tutorial_R

# R package https://github.com/njudd/ggrain
# R package vingette https://www.njudd.com/raincloud-ggrain/

if (!require(pacman)) {
  install.packages("pacman")
}
pacman::p_load(patchwork, tidyverse, lavaan, ggpp, plyr,
               ggrain) #for rainclouds

getwd() # this tells you the current path of R
# we need to direct R to the folder with the scripts & data
# we set the path by using setwd()
# for example:
# setwd("/Users/njudd/projects/rain/raincloudplots-workshops/tutorial/R_tutorial")
#or select Session->set working directory

#setwd("/Users/njudd/surfdrive/Shared/NWO Open Science Fund/Workshops/Workshop4 - 17 feb-2023/Materials/R_tutorial/")

source("geom_flat_violin.R") #this only works if you set the right path!
source("fn_summary_SE.r") # This does the summary. For each group's data frame, return a vector with
# N, mean, median, and sd

#################################################
# first we will start by simulating some data! :)
#################################################
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


################################################# 
# Example 1: Two group plot with colours and coordinate flip
#################################################

# lets plot the individual data points of each group jittered
p1 <- ggplot(simdat, aes(x=group,y=score, fill = group))+
  geom_point(position = position_jitter(width = .15), size = .25) +
  labs(title = 'Figure 1: The Basic Raincloud with Colour', y = 'Score', x = 'Group') + 
  theme_classic()+guides(fill = "none")
p1
# lets flip the points
p1 <- p1 + coord_flip()
p1

# here we have a raincloud with two groups
p1 + geom_flat_violin(position = position_nudge(x = .2, y = 0),adjust = 2)

# we can also change the smoothing with adjust
# here is one with reduced smoothing
p1 + geom_flat_violin(position = position_nudge(x = .2, y = 0), adjust = .1)

################################################# 
# Example 2: now we will have a basic example of a single raincloud plot
#################################################
p2_h <- ggplot(simdat, aes(x = 1, y=score)) +
  geom_flat_violin(position = position_nudge(x = .23, y = 0),adjust = 2, width = .5, fill = "red", color = NA)+
  geom_boxplot(position = position_nudge(x = .17, y = 0), width = .1, outlier.colour = NA, fill = "red") +
  geom_point(position = position_jitter(width = .10), size = .3, color = "red", alpha = .4)+
  coord_cartesian(xlim = c(0.2, 2)) + # change to .75 & 1.5 to zoom in
  labs(x = "Group", y = "Score") + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  theme_minimal()

p2_h # printing the plot
p2_v <- p2_h + coord_flip() #flipping the plot
p2_v

# lets do it with ggrain; it doesn't have to look the exact same
ggplot(simdat, aes(x = 1, y=score)) +
  geom_rain() +
  coord_flip() +
  theme_minimal()

# can you make them red?
#*answer








#try to save the plot with ggsave()!
#ggsave("~/Desktop/p0_v.png", p0_v, bg = "white") # need to specify bg white for theme_minimal now


# can you modify the violin to have similar smoothing?
#answer*
ggplot(simdat, aes(x = 1, y=score)) +
  geom_rain(point.args = list(color = "red"),
            boxplot.args = list(fill = "red", outlier.color = NA),
            violin.args = list(fill = "red", color = NA, adjust = 2)) +
  coord_flip() +
  theme_minimal()

################################################# 
# Example 3: Two group raincloud
#################################################

# lets make two group raincloud
p3 <- ggplot(simdat, aes(x=group,y=score, fill = group))+
  geom_flat_violin(position = position_nudge(x = .25, y = 0),adjust =2)+
  geom_point(position = position_jitter(width = .15), size = .25)+
  #note that here we need to set the x-variable to a numeric variable and bump it to get the boxplots to line up with the rainclouds. 
  geom_boxplot(aes(x = as.numeric(group)+0.25, y = score), outlier.shape = NA, alpha = 0.3, width = .1, colour = "BLACK") +
  ylab('Score')+xlab('Group')+coord_flip()+theme_classic()+guides(fill = "none", colour = "none") +
  labs(title = 'Figure 3: Raincloud Plot w/Boxplots', x = 'Group', y = 'Score')
p3

# we can change the colors!
p3c <- p3 + ggtitle("Figure 3c: Change in Colour Palette") +
  scale_colour_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2")
p3c

# lets do it with ggrain; it doesn't have to look the exact same
ggplot(simdat, aes(x=group,y=score, fill = group)) +
  geom_rain()

# position arguements are given as a list for each element
# when you give them the defaults are written over
ggplot(simdat, aes(x=group,y=score, fill = group)) +
  geom_rain(violin.args.pos = list(side = "r", width = 0.7, position = position_nudge(x =0.3)))

# can you change the side with rain.side arguement?
#*answer



# can you change the smoothing?
#answer*



# can you flip them?
#answer*




################################################# 
# Example 4: advanced rainclouds!
################################################# 

#Rainclouds with mean and confidence interval
p4 <- ggplot(simdat,aes(x=group,y=score, fill = group, colour = group))+
  geom_flat_violin(position = position_nudge(x = .25, y = 0),adjust =2)+
  geom_point(position = position_jitter(width = .15), size = .25)+
  geom_point(data = summary_simdat, aes(x = group, y = score_mean), position = position_nudge(.25), colour = "BLACK")+
  geom_errorbar(data = summary_simdat, aes(x = group, y = score_mean, ymin = score_mean-score_sem_ci, ymax = score_mean+score_sem_ci), position = position_nudge(.25), colour = "BLACK", width = 0.1, size = 0.8)+
  ylab('Score')+xlab('Group')+coord_flip()+theme_classic()+guides(fill = "none", colour = "none") +
  scale_colour_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+
  ggtitle("Figure 7: Raincloud Plot with Mean ± 95% CI")
p4

# see if you can get stat_summary to do the same! (check out the lines commented off)
# If you have issues installing Hmisc just move on

p4_c <- ggplot(simdat,aes(x=group,y=score, fill = group, colour = group))+
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
p4_c

# Now try to add stat_summary to geom_rain()
# see vingette https://www.njudd.com/raincloud-ggrain/ for help :)
#answer*
ggplot(simdat,aes(x=group, y=score, fill = group))+
  geom_rain(boxplot.args = list(color = NA, fill = NA), violin.args = list(color = NA)) + 
  stat_summary(fun = mean, geom = "point", aes(color = group), color = "black", position = position_nudge(x = .15, y = 0)) +
  stat_summary(fun = mean, geom = "errorbar", fun.data  = "mean_cl_normal", color = "black", position = position_nudge(x = .15, y = 0),
               aes(color = group, width = .1)) +
  coord_flip() +
  scale_colour_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")


#################################################
#Example 5: Rainclouds with striated data
#################################################

#Round data
simdat_round<-simdat
simdat_round$score<-round(simdat$score,0) 

#Striated/grouped when no jitter applied
ap5.1 <- ggplot(simdat_round,aes(x=group,y=score,fill=group,col=group))+
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .6,adjust =4)+
  geom_point(size = 1, alpha = 0.6)+ylab('Score')+xlab("")+
  scale_fill_brewer(palette = "Dark2")+
  scale_colour_brewer(palette = "Dark2")+
  guides(fill = "none", col = "none")+
  ggtitle('Striated')

#Added jitter helps
ap5.2 <- ggplot(simdat_round,aes(x=group,y=score,fill=group,col=group))+
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .4,adjust =4)+
  geom_point(position=position_jitter(width = .15),size = 1, alpha = 0.4)+ylab('Score')+ xlab("")+
  scale_fill_brewer(palette = "Dark2")+
  scale_colour_brewer(palette = "Dark2")+
  guides(fill = "none", col = "none")+
  ggtitle('Added jitter')


# with patchwork you can add ggplots together!
p5 <- ap5.1 + ap5.2 + plot_annotation("Figure 8: Jittering Ordinal Data", tag_levels = "a")
p5

# can you make two similar ones quickly with geom_rain and patch them together?
#*answer
#*














#################################################
#Example 6: Add additional factor/condition
#################################################

simdat$gr2<-as.factor(c(rep('high',125),rep('low',125),rep('high',125),rep('low',125)))

p6 <- ggplot(simdat,aes(x=group,y=score, fill = group, colour = group))+
  geom_flat_violin(position = position_nudge(x = .25, y = 0),adjust =2, trim = TRUE)+
  geom_point(position = position_jitter(width = .15), size = .25)+
  geom_boxplot(aes(x = as.numeric(group)+0.25, y = score),outlier.shape = NA, alpha = 0.3, width = .1, colour = "BLACK") +
  ylab('Score')+xlab('')+coord_flip()+theme_classic()+guides(fill = "none", colour = "none") + facet_wrap(~gr2)+
  scale_colour_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+
  ggtitle("Figure 9: Complex Raincloud Plots with Facet Wrap")
p6

# can you make a similar one with ggrain?
#*answer







# can you see the median and the IQR of the boxplot?
# if you can't try to fix it with ggrain while also having the violin without a boarder
#*answer







# p10 needs a csv in the same folder
# loading the data: make sure to get getwd() and do setwd()
rep_data <- read_csv("repeated_measures_data.csv", 
                     col_types = cols(group = col_factor(levels = c("1", "2")), 
                                      time = col_factor(levels = c("1", "2", "3"))))

sumrepdat <- fn_summary_SE(rep_data, "score", c("group", "time"))
head(sumrepdat); str(sumrepdat)

#################################################
#Example 7: Rainclouds for repeated measures, continued 
#################################################

p7 <- ggplot(rep_data, aes(x = time, y = score, fill = group)) +
  geom_flat_violin(aes(fill = group),position = position_nudge(x = .1, y = 0), adjust = 1.5, trim = FALSE, alpha = .5, colour = NA)+
  geom_point(aes(x = as.numeric(time)-.15, y = score, colour = group),position = position_jitter(width = .05), size = 1, shape = 20)+
  geom_boxplot(aes(x = time, y = score, fill = group),outlier.shape = NA, alpha = .5, width = .1, colour = "black")+
  scale_colour_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+
  ggtitle("Figure 10: Repeated Measures Factorial Rainclouds")

p7

# can you make a similar graph with geom_rain? hint ggpp::position_jitternudge & ggpp::position_dodgenudge exist
#*answer











#################################################
#Example 8: Rainclouds for repeated measures, additional plotting options 
#################################################

p8 <- ggplot(rep_data, aes(x = time, y = score, fill = group)) +
  geom_flat_violin(aes(fill = group),position = position_nudge(x = .1, y = 0), adjust = 1.5, trim = FALSE, alpha = .5, colour = NA)+
  geom_point(aes(x = as.numeric(time)-.15, y = score, colour = group),position = position_jitter(width = .05), size = .25, shape = 20)+
  geom_boxplot(aes(x = time, y = score, fill = group),outlier.shape = NA, alpha = .5, width = .1, colour = "black")+
  geom_line(data = sumrepdat, aes(x = as.numeric(time)+.1, y = score_mean, group = group, colour = group), linetype = 3)+
  geom_point(data = sumrepdat, aes(x = as.numeric(time)+.1, y = score_mean, group = group, colour = group), shape = 18) +
  geom_errorbar(data = sumrepdat, aes(x = as.numeric(time)+.1, y = score_mean, group = group, colour = group, ymin = score_mean-score_sem_ci, ymax = score_mean+score_sem_ci), width = .05)+
  scale_colour_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+
  ggtitle("Figure 11: Repeated Measures - Factorial (Extended)")
p8

# can you replicate this with geom_rain? hint use some of the code in the last examples
#*answer














#################################################
##################   advanced   #################
#################################################
#Example 9: can you connect the individual data points using id.long.var arg in geom_rain?
#################################################

#*answer













# can you subset the first two timepoints and make a flanking raincloud with connected points?
# hint check side argument in geom_rain
#*answer








