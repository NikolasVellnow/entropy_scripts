rm(list=ls())
library(lattice)
library(ggplot2)
library(ggpubr)
library(dplyr)
library(svglite)

axis_title_size_neutral <- 15
text_size_neutral <- 12

axis_title_size_rest <- 24
text_size_rest <- 16

###################### neutral case ~ recombination rate, N=400 #########

#path <- "/Users/niko/sciebo/Bioinformatik Bachelor Bielefeld/6_WS_2022_2023/Trajectories_project/"
#out_path <- "/Users/niko/sciebo/Bioinformatik Bachelor Bielefeld/6_WS_2022_2023/entropy_manuscript/"

path <- "C:/Users/nvellnow/sciebo7/6_WS_2022_2023/Trajectories_project/"
out_path <- "C:/Users/nvellnow/sciebo7/6_WS_2022_2023/entropy_manuscript/"

setwd(out_path)

dat1 <- read.delim(paste(path, "pseudoEntropySimsNeutralModel_n400_entropy_delta_entropy.txt", sep=""),
                  head=T)
dat1$run <- as.factor(dat1$run)
dat1$r <- dat1$recombination_rate

numLoc <- mean(dat1$num_trajectories)
tapply(dat1$num_trajectories, dat1$r, mean)

variable_labeller <- c(
  '0' = expression(paste(italic(r),"=0")) ,
  '1e-10' = expression(paste(italic(r), "=",10^{-10})) ,
  '1e-09' = expression(paste(italic(r), "=",10^{-9}))  ,
  '1e-08' = expression(paste(italic(r), "=",10^{-8}))  ,
  '1e-07' = expression(paste(italic(r), "=",10^{-7}))  ,
  '1e-06' = expression(paste(italic(r), "=",10^{-6}))  ,
  '1e-05' = expression(paste(italic(r), "=",10^{-5}))  ,
  '1e-04' = expression(paste(italic(r), "=",10^{-4}))  ,
  '0.001' = expression(paste(italic(r), "=",10^{-3}))  ,
  '0.01' = expression(paste(italic(r), "=",10^{-2}))
)

dat1 <- mutate_at(dat1, .vars = "r", .funs = factor, labels = variable_labeller)

theme_set(theme_bw() + theme(legend.position = "top"))

neutral <- ggplot(dat1,(aes(x=generation, y=delta_pseudo_entropy, color=run)))+
    geom_line()+
    theme_linedraw()+
    ylab(expression(paste("change in entropy, ", Delta, italic(H[L](t))))) +
    xlab("time (generations)") +
    theme(legend.position = "none",
          axis.text.x = element_text(angle=90,
                                     vjust=0.5, 
                                     hjust = 0.5,
                                     size=text_size_neutral),
          axis.title.x = element_text(size=axis_title_size_neutral),
          axis.text.y = element_text(size=text_size_neutral),
          axis.title.y = element_text(size=axis_title_size_neutral))+
    stat_summary(fun=mean, colour="black", geom="line", aes(group = 1), linewidth=1.1)+
    facet_wrap(~r,ncol=2,labeller=label_parsed) +
    theme(strip.text.x = element_text(size=text_size_neutral, margin=margin(.5,.5,.5,.5, "pt")),
          strip.text.y = element_text(size=text_size_neutral, margin=margin(.5,.5,.5,.5, "pt")))
neutral

# use relative sizes here:
# width: 0.7 (ylabs) + 2 * 2.65 (width/col)
# height: 0.83 (xlabs) + 5 * 1.43 (height/row)
ggsave(file="neutral_evol_sim.svg", plot=neutral, width=6, height=8)
ggsave(file="neutral_evol_sim.pdf", plot=neutral, width=6, height=8)


#################### positive Selection ####################################

#path <- "/Users/niko/sciebo/Bioinformatik Bachelor Bielefeld/6_WS_2022_2023/Trajectories_project/"
#out_path <- "/Users/niko/sciebo/Bioinformatik Bachelor Bielefeld/6_WS_2022_2023/entropy_manuscript/"

path <- "C:/Users/nvellnow/sciebo7/6_WS_2022_2023/Trajectories_project/"
out_path <- "C:/Users/nvellnow/sciebo7/6_WS_2022_2023/entropy_manuscript/"

setwd(out_path)

dat2 <- read.delim(paste(path, "posSelModel_all_s_entropy_delta_entropy.txt", sep=""), head=T)

dat2$run <- as.factor(dat2$run)
dat2 <- dat2[dat2$recombination_rate==1e-10 |
               dat2$recombination_rate==1e-08|
               dat2$recombination_rate==1e-06|
               dat2$recombination_rate==1e-04,]

dat2$r <- dat2$recombination_rate
dat2$s <- dat2$selection_coeff
mean(dat2$num_trajectories)


r_labeller <- c(
  '1e-10' = expression(paste(italic(r), "=",10^{-10})) ,
  '1e-08' = expression(paste(italic(r), "=",10^{-8}))  ,
  '1e-06' = expression(paste(italic(r), "=",10^{-6}))  ,
  '1e-04' = expression(paste(italic(r), "=",10^{-4}))
)
dat2 <- mutate_at(dat2, .vars = "r", .funs = factor, labels = r_labeller)

s_labeller <- c(
  '1e-04' = expression(paste(italic(s), "=",10^{-4})) ,
  '0.001' = expression(paste(italic(s), "=",10^{-3}))  ,
  '0.01' = expression(paste(italic(s), "=",10^{-2}))  ,
  '0.1' = expression(paste(italic(s), "=",10^{-1}))
)
dat2 <- mutate_at(dat2, .vars = "s", .funs = factor, labels = s_labeller)

theme_set(
    theme_bw()
)

positive <- ggplot(dat2,(aes(x=generation, y=delta_pseudo_entropy, color=run)))+
    geom_line()+
    theme_linedraw()+
    theme(legend.position = "none",
          axis.text.x = element_text(angle=90,
                                     vjust=0.5, 
                                     hjust = 0.5,
                                     size=text_size_rest),
          axis.title.x = element_text(size=axis_title_size_rest),
          axis.text.y = element_text(size=text_size_rest),
          axis.title.y = element_text(size=axis_title_size_rest))+
    ylab(expression(paste("change in entropy, ", Delta, italic(H[L](t))))) +
    xlab("time (generations)") +
    stat_summary(fun=mean, colour="black", geom="line", aes(group = 1), linewidth=1.1)+
    facet_grid(r ~ s, labeller=label_parsed) +
    theme(strip.text.x = element_text(size=text_size_rest, margin=margin(.5,.5,.5,.5, "pt")),
          strip.text.y = element_text(size=text_size_rest, margin=margin(.5,.5,.5,.5, "pt")))
positive

# use relative sizes here:
# width: 0.7 (ylabs) + 4 * 2.65 (width/col) = 11.3
# height: 0.83 (xlabs) + 4 * 1.43 (height/row) = 6.55
ggsave(file="positive_sel_sim.svg", plot=positive, width=11.3, height=6.55)
ggsave(file="positive_sel_sim.pdf", plot=positive, width=11.3, height=6.55)

###################### negative selection #########

#path <- "/Users/niko/sciebo/Bioinformatik Bachelor Bielefeld/6_WS_2022_2023/Trajectories_project/"
#out_path <- "/Users/niko/sciebo/Bioinformatik Bachelor Bielefeld/6_WS_2022_2023/entropy_manuscript/"

path <- "C:/Users/nvellnow/sciebo7/6_WS_2022_2023/Trajectories_project/"
out_path <- "C:/Users/nvellnow/sciebo7/6_WS_2022_2023/entropy_manuscript/"

setwd(out_path)

dat3 <- read.delim(paste(path, "negSelModel_n400_entropy_delta_entropy.txt", sep=""), head=T)

dat3 <- dat3[dat3$selection_coeff !=0,]
dat3$r <- dat3$recombination_rate
dat3$s <- dat3$selection_coeff
dat3$run <- as.factor(dat3$run)

mean(dat3$num_trajectories)
numLoc <- mean(dat3$num_trajectories[dat3$selection_coeff !=0 ]) # only values with negative selection

r_labeller <- c(
  '1e-10' = expression(paste(italic(r), "=",10^{-10})) ,
  '1e-08' = expression(paste(italic(r), "=",10^{-8}))  ,
  '1e-06' = expression(paste(italic(r), "=",10^{-6}))  ,
  '1e-04' = expression(paste(italic(r), "=",10^{-4}))
)
dat3 <- mutate_at(dat3, .vars = "r", .funs = factor, labels = r_labeller)

s_labeller <- c(
  '-0.1' = expression(paste(italic(s), "=",-10^{-1})) ,
  '-0.01' = expression(paste(italic(s), "=",-10^{-2}))  ,
  '-0.001' = expression(paste(italic(s), "=",-10^{-3}))  ,
  '-1e-04' = expression(paste(italic(s), "=",-10^{-4}))
)
dat3 <- mutate_at(dat3, .vars = "s", .funs = factor, labels = s_labeller)


theme_set(theme_bw())

negative <- ggplot(dat3,(aes(x=generation, y=delta_pseudo_entropy, color=run)))+
    geom_line()+
    theme_linedraw()+
    theme(legend.position = "none",
          axis.text.x = element_text(angle=90,
                                     vjust=0.5, 
                                     hjust = 0.5,
                                     size=text_size_rest),
          axis.title.x = element_text(size=axis_title_size_rest),
          axis.text.y = element_text(size=text_size_rest),
          axis.title.y = element_text(size=axis_title_size_rest))+
    ylab(expression(paste("change in entropy, ", Delta, italic(H[L](t))))) +
    xlab("time (generations)") +
    stat_summary(fun=mean, colour="black", geom="line", aes(group = 1), linewidth=1.1)+
    facet_grid(r ~ s, labeller=label_parsed) +
    theme(strip.text.x = element_text(size=text_size_rest, margin=margin(.5,.5,.5,.5, "pt")),
          strip.text.y = element_text(size=text_size_rest, margin=margin(.5,.5,.5,.5, "pt")))
negative

# use relative sizes here:
# width: 0.7 (ylabs) + 4 * 2.65 (width/col) = 11.3
# height: 0.83 (xlabs) + 4 * 1.43 (height/row) = 6.55
ggsave(file="negative_sel_sim.svg", plot=negative, width=11.3, height=6.55)
ggsave(file="negative_sel_sim.pdf", plot=negative, width=11.3, height=6.55)


###################### overdominance selection #########

#path <- "/Users/niko/sciebo/Bioinformatik Bachelor Bielefeld/6_WS_2022_2023/Trajectories_project/"
#out_path <- "/Users/niko/sciebo/Bioinformatik Bachelor Bielefeld/6_WS_2022_2023/entropy_manuscript/"

path <- "C:/Users/nvellnow/sciebo7/6_WS_2022_2023/Trajectories_project/"
out_path <- "C:/Users/nvellnow/sciebo7/6_WS_2022_2023/entropy_manuscript/"

setwd(out_path)


dat4 <- read.delim(paste(path, "20221010_172934_entropy_delta_entropy.txt", sep=""), head=T)

dat4$run <- as.factor(dat4$run)
dat4 <- dat4[dat4$dominance_coeff==-0.5,]

dat4$r <- dat4$recombination_rate
dat4$s <- dat4$selection_coeff

numLoc <- mean(dat4$num_trajectories)

r_labeller <- c(
  '1e-10' = expression(paste(italic(r), "=",10^{-10})) ,
  '1e-08' = expression(paste(italic(r), "=",10^{-8}))  ,
  '1e-06' = expression(paste(italic(r), "=",10^{-6}))  ,
  '1e-04' = expression(paste(italic(r), "=",10^{-4}))
)
dat4 <- mutate_at(dat4, .vars = "r", .funs = factor, labels = r_labeller)

s_labeller <- c(
  '-0.1' = expression(paste(italic(s), "=",-10^{-1})) ,
  '-0.01' = expression(paste(italic(s), "=",-10^{-2}))  ,
  '-0.001' = expression(paste(italic(s), "=",-10^{-3}))  ,
  '-1e-04' = expression(paste(italic(s), "=",-10^{-4}))
)
dat4 <- mutate_at(dat4, .vars = "s", .funs = factor, labels = s_labeller)


theme_set(
    theme_bw()
)

overdominance <- ggplot(dat4,(aes(x=generation, y=delta_pseudo_entropy, color=run)))+
    geom_line()+
    theme_linedraw()+
    theme(legend.position = "none",
          axis.text.x = element_text(angle=90,
                                     vjust=0.5, 
                                     hjust = 0.5,
                                     size=text_size_rest),
          axis.title.x = element_text(size=axis_title_size_rest),
          axis.text.y = element_text(size=text_size_rest),
          axis.title.y = element_text(size=axis_title_size_rest))+
    ylab(expression(paste("change in entropy, ", Delta, italic(H[L](t))))) +
    xlab("time (generations)") +
    stat_summary(fun=mean, colour="black", geom="line", aes(group = 1), linewidth=1.1)+
    facet_grid(r ~ s, labeller=label_parsed) +
    theme(strip.text.x = element_text(size=text_size_rest, margin=margin(.5,.5,.5,.5, "pt")),
          strip.text.y = element_text(size=text_size_rest, margin=margin(.5,.5,.5,.5, "pt")))
overdominance

# use relative sizes here:
# width: 0.7 (ylabs) + 4 * 2.65 (width/col) = 11.3
# height: 0.83 (xlabs) + 4 * 1.43 (height/row) = 6.55
ggsave(file="overdominance_sel_sim.svg", plot=overdominance, width=11.3, height=6.55)
ggsave(file="overdominance_sel_sim.pdf", plot=overdominance, width=11.3, height=6.55)



###################### soft sweep #########

#path <- "/Users/niko/sciebo/Bioinformatik Bachelor Bielefeld/6_WS_2022_2023/Trajectories_project/"
#out_path <- "/Users/niko/sciebo/Bioinformatik Bachelor Bielefeld/6_WS_2022_2023/entropy_manuscript/"

path <- "C:/Users/nvellnow/sciebo7/6_WS_2022_2023/Trajectories_project/"
out_path <- "C:/Users/nvellnow/sciebo7/6_WS_2022_2023/entropy_manuscript/"

setwd(out_path)

dat5 <- read.delim(paste(path, "pseudoEntropySimsSoftSweep_neutral_sel_n400_entropy_delta_entropy.txt", sep=""), head=T)

dat5$run <- as.factor(dat5$run)
dat5 <- dat5[dat5$selection_coeff!=0,]

dat5$r <- dat5$recombination_rate
dat5$s <- dat5$selection_coeff

numLoc <- mean(dat5$num_trajectories[dat5$selection_coeff!=0])
tapply(dat5$num_trajectories, dat5$recombination_rate, mean)


r_labeller <- c(
  '1e-10' = expression(paste(italic(r), "=",10^{-10})) ,
  '1e-08' = expression(paste(italic(r), "=",10^{-8}))  ,
  '1e-06' = expression(paste(italic(r), "=",10^{-6}))  ,
  '1e-04' = expression(paste(italic(r), "=",10^{-4}))
)
dat5 <- mutate_at(dat5, .vars = "r", .funs = factor, labels = r_labeller)

s_labeller <- c(
  '1e-04' = expression(paste(italic(s), "=",10^{-4})) ,
  '0.001' = expression(paste(italic(s), "=",10^{-3}))  ,
  '0.01' = expression(paste(italic(s), "=",10^{-2}))  ,
  '0.1' = expression(paste(italic(s), "=",10^{-1}))
)
dat5 <- mutate_at(dat5, .vars = "s", .funs = factor, labels = s_labeller)

theme_set(
    theme_bw()
)

soft_sweep <- ggplot(dat5,(aes(x=generation, y=delta_pseudo_entropy, color=run)))+
    geom_line()+
    theme_linedraw()+
    theme(legend.position = "none",
          axis.text.x = element_text(angle=90,
                                     vjust=0.5, 
                                     hjust = 0.5,
                                     size=text_size_rest),
          axis.title.x = element_text(size=axis_title_size_rest),
          axis.text.y = element_text(size=text_size_rest),
          axis.title.y = element_text(size=axis_title_size_rest))+
    ylab(expression(paste("change in entropy, ", Delta, italic(H[L](t))))) +
    xlab("time (generations)") +
    stat_summary(fun=mean, colour="black", geom="line", aes(group = 1), linewidth=1.1)+
    facet_grid(r ~ s, labeller=label_parsed) +
    theme(strip.text.x = element_text(size=text_size_rest, margin=margin(.5,.5,.5,.5, "pt")),
          strip.text.y = element_text(size=text_size_rest, margin=margin(.5,.5,.5,.5, "pt")))
soft_sweep

# use relative sizes here:
# width: 0.7 (ylabs) + 4 * 2.65 (width/col) = 11.3
# height: 0.83 (xlabs) + 4 * 1.43 (height/row) = 6.55
ggsave(file="soft_sweep_sim.svg", plot=soft_sweep, width=11.3, height=6.55)
ggsave(file="soft_sweep_sim.pdf", plot=soft_sweep, width=11.3, height=6.55)

################# negative frequency dependent selection all loci all the time #########

#path <- "/Users/niko/sciebo/Bioinformatik Bachelor Bielefeld/6_WS_2022_2023/Trajectories_project/"
#out_path <- "/Users/niko/sciebo/Bioinformatik Bachelor Bielefeld/6_WS_2022_2023/entropy_manuscript/"

path <- "C:/Users/nvellnow/sciebo7/6_WS_2022_2023/Trajectories_project/"
out_path <- "C:/Users/nvellnow/sciebo7/6_WS_2022_2023/entropy_manuscript/"

setwd(out_path)

dat6 <- read.delim(paste(path,"pseudoEntropySimsNegFreqDepSweep_neutral_sel_n400_entropy_delta_entropy.txt", sep=""), header=T)

dat6$r <- dat6$recombination_rate
dat6$c <- dat6$scaling_constant

dat6$run <- as.factor(dat6$run)

numLoc <- mean(dat6$num_trajectories)
tapply(dat6$num_trajectories, dat6$recombination_rate, mean)
table(dat6$scaling_constant, dat6$recombination_rate)


r_labeller <- c(
  '1e-10' = expression(paste(italic(r), "=",10^{-10})) ,
  '1e-08' = expression(paste(italic(r), "=",10^{-8}))  ,
  '1e-06' = expression(paste(italic(r), "=",10^{-6}))  ,
  '1e-04' = expression(paste(italic(r), "=",10^{-4}))
)
dat6 <- mutate_at(dat6, .vars = "r", .funs = factor, labels = r_labeller)

c_labeller <- c(
  '0.25' = expression(paste(italic(c), "=",0.25)) ,
  '0.5' = expression(paste(italic(c), "=",0.5))  ,
  '0.75' = expression(paste(italic(c), "=",0.75))  ,
  '1' = expression(paste(italic(c), "=",1.0))
)
dat6 <- mutate_at(dat6, .vars = "c", .funs = factor, labels = c_labeller)

theme_set(theme_bw())

one_locus_NFDS <- ggplot(dat6,(aes(x=generation, y=delta_pseudo_entropy, color=run)))+
    geom_line()+
    theme_linedraw()+
    theme(legend.position = "none",
          axis.text.x = element_text(angle=90,
                                     vjust=0.5, 
                                     hjust = 0.5,
                                     size=text_size_rest),
          axis.title.x = element_text(size=axis_title_size_rest),
          axis.text.y = element_text(size=text_size_rest),
          axis.title.y = element_text(size=axis_title_size_rest))+
    stat_summary(fun=mean, colour="black", geom="line", aes(group = 1), linewidth=1.1)+
    ylab(expression(paste("change in entropy, ", Delta, italic(H[L](t))))) +
    xlab("time (generations)") +
    facet_grid(r ~ c, labeller=label_parsed) +
    theme(strip.text.x = element_text(size=text_size_rest, margin=margin(.5,.5,.5,.5, "pt")),
          strip.text.y = element_text(size=text_size_rest, margin=margin(.5,.5,.5,.5, "pt")))
one_locus_NFDS

# use relative sizes here:
# width: 0.7 (ylabs) + 4 * 2.65 (width/col) = 11.3
# height: 0.83 (xlabs) + 4 * 1.43 (height/row) = 6.55
ggsave(file="one_locus_NFDS_sim.svg", plot=one_locus_NFDS, width=11.3, height=6.55)
ggsave(file="one_locus_NFDS_sim.pdf", plot=one_locus_NFDS, width=11.3, height=6.55)


############ negative frequency dependent selection all loci after burn-in ###############

#path <- "/Users/niko/sciebo/Bioinformatik Bachelor Bielefeld/6_WS_2022_2023/Trajectories_project/"
#out_path <- "/Users/niko/sciebo/Bioinformatik Bachelor Bielefeld/6_WS_2022_2023/entropy_manuscript/"

path <- "C:/Users/nvellnow/sciebo7/6_WS_2022_2023/Trajectories_project/"
out_path <- "C:/Users/nvellnow/sciebo7/6_WS_2022_2023/entropy_manuscript/"

setwd(out_path)

dat7 <- read.delim(paste(path, "pseudoEntropySimsNegFreqDep_neutral_sel_all_loci_n400_entropy_delta_entropy.txt", sep=""), head=T)

dat7$run <- as.factor(dat7$run)

dat7$r <- dat7$recombination_rate
dat7$c <- dat7$scaling_constant

numLoc <- mean(dat7$num_trajectories)
tapply(dat7$num_trajectories, dat7$recombination_rate, mean)


r_labeller <- c(
  '1e-10' = expression(paste(italic(r), "=",10^{-10})) ,
  '1e-08' = expression(paste(italic(r), "=",10^{-8}))  ,
  '1e-06' = expression(paste(italic(r), "=",10^{-6}))  ,
  '1e-04' = expression(paste(italic(r), "=",10^{-4}))
)
dat7 <- mutate_at(dat7, .vars = "r", .funs = factor, labels = r_labeller)

c_labeller <- c(
  '0.25' = expression(paste(italic(c), "=",0.25)) ,
  '0.5' = expression(paste(italic(c), "=",0.5))  ,
  '0.75' = expression(paste(italic(c), "=",0.75))  ,
  '0.99' = expression(paste(italic(c), "=",0.99))
)
dat7 <- mutate_at(dat7, .vars = "c", .funs = factor, labels = c_labeller)


all_loci_NFDS <- ggplot(dat7,(aes(x=generation, y=delta_pseudo_entropy, color=run)))+
    geom_line()+
    theme_linedraw()+
    theme(legend.position = "none",
          axis.text.x = element_text(angle=90,
                                     vjust=0.5, 
                                     hjust = 0.5,
                                     size=text_size_rest),
          axis.title.x = element_text(size=axis_title_size_rest),
          axis.text.y = element_text(size=text_size_rest),
          axis.title.y = element_text(size=axis_title_size_rest))+
    stat_summary(fun=mean, colour="black", geom="line", aes(group = 1), linewidth=1.1)+
    ylab(expression(paste("change in entropy, ", Delta, italic(H[L](t))))) +
    xlab("time (generations)") +
    facet_grid(r ~ c, labeller=label_parsed) +
    theme(strip.text.x = element_text(size=text_size_rest, margin=margin(.5,.5,.5,.5, "pt")),
          strip.text.y = element_text(size=text_size_rest, margin=margin(.5,.5,.5,.5, "pt")))
all_loci_NFDS

# use relative sizes here:
# width: 0.7 (ylabs) + 4 * 2.65 (width/col) = 11.3
# height: 0.83 (xlabs) + 4 * 1.43 (height/row) = 6.55
ggsave(file="all_loci_NFDS_sim.svg", plot=all_loci_NFDS, width=11.3, height=6.55)
ggsave(file="all_loci_NFDS_sim.pdf", plot=all_loci_NFDS, width=11.3, height=6.55)



