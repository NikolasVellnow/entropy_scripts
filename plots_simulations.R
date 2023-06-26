###################### neutral case ~ recombination rate, N=400 #########
rm(list=ls())
library(lattice)
library(ggplot2)
library(ggpubr)
library(svglite)

#path <- "/Users/niko/sciebo/Bioinformatik Bachelor Bielefeld/6_WS_2022_2023/Trajectories_project/"
#out_path <- "/Users/niko/sciebo/Bioinformatik Bachelor Bielefeld/6_WS_2022_2023/entropy_manuscript/"

path <- "C:/Users/nvellnow/sciebo7/6_WS_2022_2023/Trajectories_project/"
out_path <- "C:/Users/nvellnow/sciebo7/6_WS_2022_2023/entropy_manuscript/"

setwd(out_path)

dat <- read.delim(paste(path, "pseudoEntropySimsNeutralModel_n400_entropy_delta_entropy.txt", sep=""),
                  head=T)
dat$run <- as.factor(dat$run)
dat$r <- dat$recombination_rate

numLoc <- mean(dat$num_trajectories)
tapply(dat$num_trajectories, dat$r, mean)


theme_set(theme_bw() + theme(legend.position = "top"))

dh <- ggplot(dat,(aes(x=generation, y=delta_pseudo_entropy, color=run)))+
    geom_line()+
    theme_linedraw()+
    ylab(expression(paste("change in entropy, ", Delta, italic(H[L](t))))) +
    xlab("time(generations)") +
    theme(legend.position = "none",
          axis.text.x = element_text(angle=90,
                                     vjust=0.5, 
                                     hjust = 0.5,
                                     size=12),
          axis.title.x = element_text(size=15),
          axis.text.y = element_text(size=12),
          axis.title.y = element_text(size=15))+
    stat_summary(fun=mean, colour="black", geom="line", aes(group = 1), linewidth=1.1)+
    facet_wrap(~r,ncol=2,labeller=label_both) +
    theme(strip.text.x = element_text(size=12, margin=margin(.5,.5,.5,.5, "pt")),
          strip.text.y = element_text(size=12, margin=margin(.5,.5,.5,.5, "pt")))
dh

# use relative sizes here:
# width: 0.7 (ylabs) + 2 * 2.65 (width/col)
# height: 0.83 (xlabs) + 5 * 1.43 (height/row)
ggsave(file="neutral_evol_sim.svg", plot=dh, width=6, height=8)


#################### positive Selection ####################################
rm(list=ls())
library(lattice)
library(ggplot2)
library("ggpubr")
library(svglite)

#path <- "/Users/niko/sciebo/Bioinformatik Bachelor Bielefeld/6_WS_2022_2023/Trajectories_project/"
#out_path <- "/Users/niko/sciebo/Bioinformatik Bachelor Bielefeld/6_WS_2022_2023/entropy_manuscript/"

path <- "C:/Users/nvellnow/sciebo7/6_WS_2022_2023/Trajectories_project/"
out_path <- "C:/Users/nvellnow/sciebo7/6_WS_2022_2023/entropy_manuscript/"

setwd(out_path)

dat <- read.delim(paste(path, "posSelModel_all_s_entropy_delta_entropy.txt", sep=""), head=T)

dat$run <- as.factor(dat$run)
dat <- dat[dat$recombination_rate==1e-10 |
               dat$recombination_rate==1e-08|
               dat$recombination_rate==1e-06|
               dat$recombination_rate==1e-04,]

dat$r <- dat$recombination_rate
dat$s <- dat$selection_coeff
mean(dat$num_trajectories)


theme_set(
    theme_bw()
)

p <- ggplot(dat,(aes(x=generation, y=delta_pseudo_entropy, color=run)))+
    geom_line()+
    theme_linedraw()+
    theme(legend.position = "none",
          axis.text.x = element_text(angle=90,
                                     vjust=0.5, 
                                     hjust = 0.5,
                                     size=12),
          axis.title.x = element_text(size=15),
          axis.text.y = element_text(size=12),
          axis.title.y = element_text(size=15))+
    ylab(expression(paste("change in entropy, ", Delta, italic(H[L](t))))) +
    xlab("time(generations)") +
    stat_summary(fun=mean, colour="black", geom="line", aes(group = 1), linewidth=1.1)+
    facet_grid(r ~ s, labeller=label_both) +
    theme(strip.text.x = element_text(size=12, margin=margin(.5,.5,.5,.5, "pt")),
          strip.text.y = element_text(size=12, margin=margin(.5,.5,.5,.5, "pt")))
p


# use relative sizes here:
# width: 0.7 (ylabs) + 4 * 2.65 (width/col) = 11.3
# height: 0.83 (xlabs) + 4 * 1.43 (height/row) = 6.55
ggsave(file="positive_sel_sim.svg", plot=p, width=11.3, height=6.55)

###################### negative selection #########
rm(list=ls())
library(lattice)
library(ggplot2)
library("ggpubr")
library(svglite)

#path <- "/Users/niko/sciebo/Bioinformatik Bachelor Bielefeld/6_WS_2022_2023/Trajectories_project/"
#out_path <- "/Users/niko/sciebo/Bioinformatik Bachelor Bielefeld/6_WS_2022_2023/entropy_manuscript/"

path <- "C:/Users/nvellnow/sciebo7/6_WS_2022_2023/Trajectories_project/"
out_path <- "C:/Users/nvellnow/sciebo7/6_WS_2022_2023/entropy_manuscript/"

setwd(out_path)

dat <- read.delim(paste(path, "negSelModel_n400_entropy_delta_entropy.txt", sep=""), head=T)

dat <- dat[dat$selection_coeff !=0,]
dat$r <- dat$recombination_rate
dat$s <- dat$selection_coeff
dat$run <- as.factor(dat$run)

mean(dat$num_trajectories)
numLoc <- mean(dat$num_trajectories[dat$selection_coeff !=0 ]) # only values with negative selection


theme_set(theme_bw())

dh <- ggplot(dat,(aes(x=generation, y=delta_pseudo_entropy, color=run)))+
    geom_line()+
    theme_linedraw()+
    theme(legend.position = "none",
          axis.text.x = element_text(angle=90,
                                     vjust=0.5, 
                                     hjust = 0.5,
                                     size=12),
          axis.title.x = element_text(size=15),
          axis.text.y = element_text(size=12),
          axis.title.y = element_text(size=15))+
    ylab(expression(paste("change in entropy, ", Delta, italic(H[L](t))))) +
    xlab("time(generations)") +
    stat_summary(fun=mean, colour="black", geom="line", aes(group = 1), linewidth=1.1)+
    facet_grid(r ~ s, labeller = label_both) +
    theme(strip.text.x = element_text(size=12, margin=margin(.5,.5,.5,.5, "pt")),
          strip.text.y = element_text(size=12, margin=margin(.5,.5,.5,.5, "pt")))
dh

# use relative sizes here:
# width: 0.7 (ylabs) + 4 * 2.65 (width/col) = 11.3
# height: 0.83 (xlabs) + 4 * 1.43 (height/row) = 6.55
ggsave(file="negative_sel_sim.svg", plot=dh, width=11.3, height=6.55)


###################### overdominance selection #########
rm(list=ls())
library(lattice)
library(ggplot2)
library("ggpubr")
library(svglite)

#path <- "/Users/niko/sciebo/Bioinformatik Bachelor Bielefeld/6_WS_2022_2023/Trajectories_project/"
#out_path <- "/Users/niko/sciebo/Bioinformatik Bachelor Bielefeld/6_WS_2022_2023/entropy_manuscript/"

path <- "C:/Users/nvellnow/sciebo7/6_WS_2022_2023/Trajectories_project/"
out_path <- "C:/Users/nvellnow/sciebo7/6_WS_2022_2023/entropy_manuscript/"

setwd(out_path)


dat <- read.delim(paste(path, "20221010_172934_entropy_delta_entropy.txt", sep=""), head=T)

dat$run <- as.factor(dat$run)
dat_1 <- dat[dat$dominance_coeff==-0.5,]

dat_1$r <- dat_1$recombination_rate
dat_1$s <- dat_1$selection_coeff

numLoc <- mean(dat_1$num_trajectories)


theme_set(
    theme_bw()
)

p <- ggplot(dat_1,(aes(x=generation, y=delta_pseudo_entropy, color=run)))+
    geom_line()+
    theme_linedraw()+
    theme(legend.position = "none",
          axis.text.x = element_text(angle=90,
                                     vjust=0.5, 
                                     hjust = 0.5,
                                     size=12),
          axis.title.x = element_text(size=15),
          axis.text.y = element_text(size=12),
          axis.title.y = element_text(size=15))+
    ylab(expression(paste("change in entropy, ", Delta, italic(H[L](t))))) +
    xlab("time(generations)") +
    stat_summary(fun=mean, colour="black", geom="line", aes(group = 1), linewidth=1.1)+
    facet_grid(r ~ s, labeller = label_both) +
    theme(strip.text.x = element_text(size=12, margin=margin(.5,.5,.5,.5, "pt")),
          strip.text.y = element_text(size=12, margin=margin(.5,.5,.5,.5, "pt")))
p

# use relative sizes here:
# width: 0.7 (ylabs) + 4 * 2.65 (width/col) = 11.3
# height: 0.83 (xlabs) + 4 * 1.43 (height/row) = 6.55
ggsave(file="overdominance_sel_sim.svg", plot=p, width=11.3, height=6.55)



###################### soft sweep #########
rm(list=ls())
library(lattice)
library(ggplot2)
library("ggpubr")
library(svglite)

#path <- "/Users/niko/sciebo/Bioinformatik Bachelor Bielefeld/6_WS_2022_2023/Trajectories_project/"
#out_path <- "/Users/niko/sciebo/Bioinformatik Bachelor Bielefeld/6_WS_2022_2023/entropy_manuscript/"

path <- "C:/Users/nvellnow/sciebo7/6_WS_2022_2023/Trajectories_project/"
out_path <- "C:/Users/nvellnow/sciebo7/6_WS_2022_2023/entropy_manuscript/"

setwd(out_path)

dat <- read.delim(paste(path, "pseudoEntropySimsSoftSweep_neutral_sel_n400_entropy_delta_entropy.txt", sep=""), head=T)

dat$run <- as.factor(dat$run)
dat <- dat[dat$selection_coeff!=0,]

dat$r <- dat$recombination_rate
dat$s <- dat$selection_coeff

numLoc <- mean(dat$num_trajectories[dat$selection_coeff!=0])
tapply(dat$num_trajectories, dat$recombination_rate, mean)

theme_set(
    theme_bw()
)

p <- ggplot(dat,(aes(x=generation, y=delta_pseudo_entropy, color=run)))+
    geom_line()+
    theme_linedraw()+
    theme(legend.position = "none",
          axis.text.x = element_text(angle=90,
                                     vjust=0.5, 
                                     hjust = 0.5,
                                     size=12),
          axis.title.x = element_text(size=15),
          axis.text.y = element_text(size=12),
          axis.title.y = element_text(size=15))+
    ylab(expression(paste("change in entropy, ", Delta, italic(H[L](t))))) +
    xlab("time(generations)") +
    stat_summary(fun=mean, colour="black", geom="line", aes(group = 1), linewidth=1.1)+
    facet_grid(r ~ s, labeller = label_both) +
    theme(strip.text.x = element_text(size=12, margin=margin(.5,.5,.5,.5, "pt")),
          strip.text.y = element_text(size=12, margin=margin(.5,.5,.5,.5, "pt")))
p

# use relative sizes here:
# width: 0.7 (ylabs) + 4 * 2.65 (width/col) = 11.3
# height: 0.83 (xlabs) + 4 * 1.43 (height/row) = 6.55
ggsave(file="soft_sweep_sim.svg", plot=p, width=11.3, height=6.55)

################# negative frequency dependent selection all loci all the time #########
rm(list=ls())
library(lattice)
library(ggplot2)
library("ggpubr")
library(svglite)

#path <- "/Users/niko/sciebo/Bioinformatik Bachelor Bielefeld/6_WS_2022_2023/Trajectories_project/"
#out_path <- "/Users/niko/sciebo/Bioinformatik Bachelor Bielefeld/6_WS_2022_2023/entropy_manuscript/"

path <- "C:/Users/nvellnow/sciebo7/6_WS_2022_2023/Trajectories_project/"
out_path <- "C:/Users/nvellnow/sciebo7/6_WS_2022_2023/entropy_manuscript/"

setwd(out_path)

dat <- read.delim(paste(path,"pseudoEntropySimsNegFreqDepSweep_neutral_sel_n400_entropy_delta_entropy.txt", sep=""), header=T)

dat$r <- dat$recombination_rate
dat$c <- dat$scaling_constant

dat$run <- as.factor(dat$run)

numLoc <- mean(dat$num_trajectories)
tapply(dat$num_trajectories, dat$recombination_rate, mean)
table(dat$scaling_constant, dat$recombination_rate)

theme_set(theme_bw())


dh <- ggplot(dat,(aes(x=generation, y=delta_pseudo_entropy, color=run)))+
    geom_line()+
    theme_linedraw()+
    theme(legend.position = "none",
          axis.text.x = element_text(angle=90,
                                     vjust=0.5, 
                                     hjust = 0.5,
                                     size=12),
          axis.title.x = element_text(size=15),
          axis.text.y = element_text(size=12),
          axis.title.y = element_text(size=15))+
    stat_summary(fun=mean, colour="black", geom="line", aes(group = 1), linewidth=1.1)+
    ylab(expression(paste("change in entropy, ", Delta, italic(H[L](t))))) +
    xlab("time(generations)") +
    facet_grid(r ~ c, labeller = label_both) +
    theme(strip.text.x = element_text(size=12, margin=margin(.5,.5,.5,.5, "pt")),
          strip.text.y = element_text(size=12, margin=margin(.5,.5,.5,.5, "pt")))
dh

# use relative sizes here:
# width: 0.7 (ylabs) + 4 * 2.65 (width/col) = 11.3
# height: 0.83 (xlabs) + 4 * 1.43 (height/row) = 6.55
ggsave(file="one_locus_NFDS_sim.svg", plot=dh, width=11.3, height=6.55)


############ negative frequency dependent selection all loci after burn-in ###############
rm(list=ls())
library(lattice)
library(ggplot2)
library("ggpubr")
library(svglite)

#path <- "/Users/niko/sciebo/Bioinformatik Bachelor Bielefeld/6_WS_2022_2023/Trajectories_project/"
#out_path <- "/Users/niko/sciebo/Bioinformatik Bachelor Bielefeld/6_WS_2022_2023/entropy_manuscript/"

path <- "C:/Users/nvellnow/sciebo7/6_WS_2022_2023/Trajectories_project/"
out_path <- "C:/Users/nvellnow/sciebo7/6_WS_2022_2023/entropy_manuscript/"

setwd(out_path)

dat <- read.delim(paste(path, "pseudoEntropySimsNegFreqDep_neutral_sel_all_loci_n400_entropy_delta_entropy.txt", sep=""), head=T)

dat$run <- as.factor(dat$run)

dat$r <- dat$recombination_rate
dat$c <- dat$scaling_constant

numLoc <- mean(dat$num_trajectories)
tapply(dat$num_trajectories, dat$recombination_rate, mean)


p <- ggplot(dat,(aes(x=generation, y=delta_pseudo_entropy, color=run)))+
    geom_line()+
    theme_linedraw()+
    theme(legend.position = "none",
          axis.text.x = element_text(angle=90,
                                     vjust=0.5, 
                                     hjust = 0.5,
                                     size=12),
          axis.title.x = element_text(size=15),
          axis.text.y = element_text(size=12),
          axis.title.y = element_text(size=15))+
    stat_summary(fun=mean, colour="black", geom="line", aes(group = 1), linewidth=1.1)+
    ylab(expression(paste("change in entropy, ", Delta, italic(H[L](t))))) +
    xlab("time(generations)") +
    facet_grid(r ~ c, labeller = label_both) +
    theme(strip.text.x = element_text(size=12, margin=margin(.5,.5,.5,.5, "pt")),
          strip.text.y = element_text(size=12, margin=margin(.5,.5,.5,.5, "pt")))
p

# use relative sizes here:
# width: 0.7 (ylabs) + 4 * 2.65 (width/col) = 11.3
# height: 0.83 (xlabs) + 4 * 1.43 (height/row) = 6.55
ggsave(file="all_loci_NFDS_sim.svg", plot=p, width=11.3, height=6.55)



