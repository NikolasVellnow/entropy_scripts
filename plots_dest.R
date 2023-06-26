rm(list=ls())
library(lattice)
library(ggplot2)
library(ggpubr)
library(svglite)

alph <- 0.6
window_line <- 0.5
mean_line <- 0.6

violin_line <- 0.3
violin_median <- 0.4

##### 50kb windows #######

path <- "/Users/niko/sciebo/Bioinformatik Bachelor Bielefeld/6_WS_2022_2023/dest_project/"
out_path <- "/Users/niko/sciebo/Bioinformatik Bachelor Bielefeld/6_WS_2022_2023/entropy_manuscript/"

#path <- "C:/Users/nvellnow/sciebo7/6_WS_2022_2023/dest_project/"
#out_path <- "C:/Users/nvellnow/sciebo7/6_WS_2022_2023/entropy_manuscript/"

setwd(out_path)


dat1 <- read.csv(paste(path, "Entropy_Linvilla_agg_chromosome2L_fixed_win_50000_threshold100.csv", sep=""),
                  head=T)

dat1$window <- as.factor(dat1$window)


# spaghetti plot
theme_set(theme_bw())

p1 <- ggplot(dat1,(aes(x=time_point, y=delta_entropy, color=window)))+
  geom_line(linewidth=window_line, alpha=alph)+
  theme_linedraw()+
  ylab(expression(paste("change in entropy, ", Delta, italic(H[L](t))))) +
  xlab("time points (half-yearly)") +
  ylim(-0.27, 0.25) +
  scale_x_continuous(breaks = round(seq(0, 14, by = 2))) +
  theme(legend.position = "none",
        axis.text.x = element_text(vjust=0.5, hjust = 0.5, size=12),
        axis.title.x = element_text(size=15),
        axis.text.y = element_text(size=12),
        axis.title.y = element_text(size=15),
        strip.text.x = element_text(size=12, margin=margin(.5,.5,.5,.5, "pt")),
        strip.text.y = element_text(size=12, margin=margin(.5,.5,.5,.5, "pt")))+
  stat_summary(fun=mean, colour="black", geom="line", aes(group = 1), linewidth=mean_line)
p1


# violin plot
theme_set(theme_bw())

p2 <- ggplot(dat1,(x=aes(factor(time_point), y=delta_entropy)))+
  geom_violin(fill = "grey", colour = "black", linewidth=violin_line)+
  theme_linedraw()+
  ylab(expression(paste("change in entropy, ", Delta, italic(H[L](t))))) +
  xlab("time points (half-yearly)") +
  ylim(-0.27, 0.25) +
  scale_x_discrete(breaks = round(seq(0, 14, by = 2))) +
  theme(legend.position = "none",
        axis.text.x = element_text(vjust=0.5,hjust = 0.5,size=12),
        axis.title.x = element_text(size=15),
        axis.text.y = element_text(size=12),
        axis.title.y = element_text(size=15),
        strip.text.x = element_text(size=12, margin=margin(.5,.5,.5,.5, "pt")),
        strip.text.y = element_text(size=12, margin=margin(.5,.5,.5,.5, "pt"))
        )+
  stat_summary(fun=median, geom="point", size=violin_median, color="black")
p2

###### 5kb windows #####

path <- "/Users/niko/sciebo/Bioinformatik Bachelor Bielefeld/6_WS_2022_2023/dest_project/"
out_path <- "/Users/niko/sciebo/Bioinformatik Bachelor Bielefeld/6_WS_2022_2023/entropy_manuscript/"

#path <- "C:/Users/nvellnow/sciebo7/6_WS_2022_2023/dest_project/"
#out_path <- "C:/Users/nvellnow/sciebo7/6_WS_2022_2023/entropy_manuscript/"

setwd(out_path)


dat2 <- read.csv(paste(path, "Entropy_Linvilla_agg_chromosome2L_fixed_win_5000_threshold10.csv", sep=""),
                head=T)

dat2$window <- as.factor(dat2$window)


# spaghetti plot
theme_set(theme_bw())

p3 <- ggplot(dat2,(aes(x=time_point, y=delta_entropy, color=window)))+
  geom_line(linewidth=window_line, alpha=alph)+
  theme_linedraw()+
  ylab(expression(paste("change in entropy, ", Delta, italic(H[L](t))))) +
  xlab("time points (half-yearly)") +
  ylim(-0.26, 0.25) +
  scale_x_continuous(breaks = round(seq(0, 14, by = 2))) +
  theme(legend.position = "none",
        axis.text.x = element_text(vjust=0.5, hjust = 0.5, size=12),
        axis.title.x = element_text(size=15),
        axis.text.y = element_text(size=12),
        axis.title.y = element_text(size=15),
        strip.text.x = element_text(size=12, margin=margin(.5,.5,.5,.5, "pt")),
        strip.text.y = element_text(size=12, margin=margin(.5,.5,.5,.5, "pt"))
        )+
  stat_summary(fun=mean, colour="black", geom="line", aes(group = 1), linewidth=mean_line)
p3


# violin plot
theme_set(theme_bw())

p4 <- ggplot(dat2,(x=aes(factor(time_point), y=delta_entropy)))+
  geom_violin(fill = "grey", colour = "black", linewidth=violin_line)+
  theme_linedraw()+
  ylab(expression(paste("change in entropy, ", Delta, italic(H[L](t))))) +
  xlab("time points (half-yearly)") +
  ylim(-0.27, 0.25) +
  scale_x_discrete(breaks = round(seq(0, 14, by = 2))) +
  theme(legend.position = "none",
        axis.text.x = element_text(vjust=0.5, hjust = 0.5, size=12),
        axis.title.x = element_text(size=15),
        axis.text.y = element_text(size=12),
        axis.title.y = element_text(size=15),
        strip.text.x = element_text(size=12, margin=margin(.5,.5,.5,.5, "pt")),
        strip.text.y = element_text(size=12, margin=margin(.5,.5,.5,.5, "pt"))
        )+
  stat_summary(fun=median, geom="point", size=violin_median, color="black")

p4


##### combine subplots ######
figure <- ggarrange(p1 + rremove("ylab") + rremove("xlab"),
                    p2 + rremove("ylab") + rremove("xlab"),
                    p3 + rremove("ylab") + rremove("xlab"),
                    p4 + rremove("ylab") + rremove("xlab"),
                    labels = "AUTO",
                    vjust=1.0,
                    hjust=-1,
                    ncol = 2, nrow = 2)

require(grid)
figure <- annotate_figure(figure, left = textGrob(expression(paste("change in entropy, ", Delta, italic(H[L](t)))), rot = 90, vjust = 1, gp = gpar(cex = 1.3)),
                          bottom = textGrob("time points (half-yearly)", gp = gpar(cex = 1.3)))

# use relative sizes here:
# width: 0.7 (ylabs) + 2 * 2.65 (width/col)
# height: 0.83 (xlabs) + 2 * 1.43 (height/row)
ggsave(file="dest_data_2L.svg", plot=figure, width=6, height=3.69)




###########################################################################
###################### plot the rest for appendix #########################
###########################################################################

rm(list=ls())
library(lattice)
library(ggplot2)
library(ggpubr)
library(svglite)

alph <- 0.6
window_line <- 0.5
mean_line <- 0.6

violin_line <- 0.3
violin_median <- 0.4

##### 5 kb windows chromosome 2R #######

path <- "/Users/niko/sciebo/Bioinformatik Bachelor Bielefeld/6_WS_2022_2023/dest_project/"
out_path <- "/Users/niko/sciebo/Bioinformatik Bachelor Bielefeld/6_WS_2022_2023/entropy_manuscript/"

#path <- "C:/Users/nvellnow/sciebo7/6_WS_2022_2023/dest_project/"
#out_path <- "C:/Users/nvellnow/sciebo7/6_WS_2022_2023/entropy_manuscript/"

setwd(out_path)

dat_2R <- read.csv(paste(path, "Entropy_Linvilla_agg_chromosome2R_fixed_win_5000_threshold10.csv", sep=""),
                 head=T)

dat_2R$window <- as.factor(dat_2R$window)


# spaghetti plot
theme_set(theme_bw())

p5 <- ggplot(dat_2R,(aes(x=time_point, y=delta_entropy, color=window)))+
    geom_line(linewidth=window_line, alpha=alph)+
    theme_linedraw()+
    ylab(expression(paste("change in entropy, ", Delta, italic(H[L](t))))) +
    xlab("time points (half-yearly)") +
    ylim(-0.27, 0.25) +
    scale_x_continuous(breaks = round(seq(0, 14, by = 2))) +
    theme(legend.position = "none",
          axis.text.x = element_text(vjust=0.5, hjust = 0.5, size=12),
          axis.title.x = element_text(size=15),
          axis.text.y = element_text(size=12),
          axis.title.y = element_text(size=15),
          strip.text.x = element_text(size=12, margin=margin(.5,.5,.5,.5, "pt")),
          strip.text.y = element_text(size=12, margin=margin(.5,.5,.5,.5, "pt")))+
    stat_summary(fun=mean, colour="black", geom="line", aes(group = 1), linewidth=mean_line)
p5


# violin plot
theme_set(theme_bw())

p6 <- ggplot(dat_2R,(x=aes(factor(time_point), y=delta_entropy)))+
    geom_violin(fill = "grey", colour = "black", linewidth=violin_line)+
    theme_linedraw()+
    ylab(expression(paste("change in entropy, ", Delta, italic(H[L](t))))) +
    xlab("time points (half-yearly)") +
    ylim(-0.27, 0.25) +
    scale_x_discrete(breaks = round(seq(0, 14, by = 2))) +
    theme(legend.position = "none",
          axis.text.x = element_text(vjust=0.5,hjust = 0.5,size=12),
          axis.title.x = element_text(size=15),
          axis.text.y = element_text(size=12),
          axis.title.y = element_text(size=15),
          strip.text.x = element_text(size=12, margin=margin(.5,.5,.5,.5, "pt")),
          strip.text.y = element_text(size=12, margin=margin(.5,.5,.5,.5, "pt"))
    )+
    stat_summary(fun=median, geom="point", size=violin_median, color="black")
p6

##### 5 kb windows chromosome 3L #######

dat_3L <- read.csv(paste(path, "Entropy_Linvilla_agg_chromosome3L_fixed_win_5000_threshold10.csv", sep=""),
                   head=T)

dat_3L$window <- as.factor(dat_3L$window)


# spaghetti plot
theme_set(theme_bw())

p7 <- ggplot(dat_3L,(aes(x=time_point, y=delta_entropy, color=window)))+
    geom_line(linewidth=window_line, alpha=alph)+
    theme_linedraw()+
    ylab(expression(paste("change in entropy, ", Delta, italic(H[L](t))))) +
    xlab("time points (half-yearly)") +
    ylim(-0.27, 0.25) +
    scale_x_continuous(breaks = round(seq(0, 14, by = 2))) +
    theme(legend.position = "none",
          axis.text.x = element_text(vjust=0.5, hjust = 0.5, size=12),
          axis.title.x = element_text(size=15),
          axis.text.y = element_text(size=12),
          axis.title.y = element_text(size=15),
          strip.text.x = element_text(size=12, margin=margin(.5,.5,.5,.5, "pt")),
          strip.text.y = element_text(size=12, margin=margin(.5,.5,.5,.5, "pt")))+
    stat_summary(fun=mean, colour="black", geom="line", aes(group = 1), linewidth=mean_line)
p7


# violin plot
theme_set(theme_bw())

p8 <- ggplot(dat_3L,(x=aes(factor(time_point), y=delta_entropy)))+
    geom_violin(fill = "grey", colour = "black", linewidth=violin_line)+
    theme_linedraw()+
    ylab(expression(paste("change in entropy, ", Delta, italic(H[L](t))))) +
    xlab("time points (half-yearly)") +
    ylim(-0.27, 0.25) +
    scale_x_discrete(breaks = round(seq(0, 14, by = 2))) +
    theme(legend.position = "none",
          axis.text.x = element_text(vjust=0.5,hjust = 0.5,size=12),
          axis.title.x = element_text(size=15),
          axis.text.y = element_text(size=12),
          axis.title.y = element_text(size=15),
          strip.text.x = element_text(size=12, margin=margin(.5,.5,.5,.5, "pt")),
          strip.text.y = element_text(size=12, margin=margin(.5,.5,.5,.5, "pt"))
    )+
    stat_summary(fun=median, geom="point", size=violin_median, color="black")
p8


##### 5 kb windows chromosome 3R #######

dat_3R <- read.csv(paste(path, "Entropy_Linvilla_agg_chromosome3R_fixed_win_5000_threshold10.csv", sep=""),
                   head=T)

dat_3R$window <- as.factor(dat_3R$window)


# spaghetti plot
theme_set(theme_bw())

p9 <- ggplot(dat_3R,(aes(x=time_point, y=delta_entropy, color=window)))+
    geom_line(linewidth=window_line, alpha=alph)+
    theme_linedraw()+
    ylab(expression(paste("change in entropy, ", Delta, italic(H[L](t))))) +
    xlab("time points (half-yearly)") +
    ylim(-0.27, 0.25) +
    scale_x_continuous(breaks = round(seq(0, 14, by = 2))) +
    theme(legend.position = "none",
          axis.text.x = element_text(vjust=0.5, hjust = 0.5, size=12),
          axis.title.x = element_text(size=15),
          axis.text.y = element_text(size=12),
          axis.title.y = element_text(size=15),
          strip.text.x = element_text(size=12, margin=margin(.5,.5,.5,.5, "pt")),
          strip.text.y = element_text(size=12, margin=margin(.5,.5,.5,.5, "pt")))+
    stat_summary(fun=mean, colour="black", geom="line", aes(group = 1), linewidth=mean_line)
p9


# violin plot
theme_set(theme_bw())

p10 <- ggplot(dat_3R,(x=aes(factor(time_point), y=delta_entropy)))+
    geom_violin(fill = "grey", colour = "black", linewidth=violin_line)+
    theme_linedraw()+
    ylab(expression(paste("change in entropy, ", Delta, italic(H[L](t))))) +
    xlab("time points (half-yearly)") +
    ylim(-0.27, 0.25) +
    scale_x_discrete(breaks = round(seq(0, 14, by = 2))) +
    theme(legend.position = "none",
          axis.text.x = element_text(vjust=0.5,hjust = 0.5,size=12),
          axis.title.x = element_text(size=15),
          axis.text.y = element_text(size=12),
          axis.title.y = element_text(size=15),
          strip.text.x = element_text(size=12, margin=margin(.5,.5,.5,.5, "pt")),
          strip.text.y = element_text(size=12, margin=margin(.5,.5,.5,.5, "pt"))
    )+
    stat_summary(fun=median, geom="point", size=violin_median, color="black")
p10


##### combine subplots ######

figure2 <- ggarrange(p5 + rremove("ylab") + rremove("xlab"),
                    p6 + rremove("ylab") + rremove("xlab"),
                    p7 + rremove("ylab") + rremove("xlab"),
                    p8 + rremove("ylab") + rremove("xlab"),
                    p9 + rremove("ylab") + rremove("xlab"),
                    p10 + rremove("ylab") + rremove("xlab"),
                    labels = "AUTO",
                    vjust=1.0,
                    hjust=-1,
                    ncol = 2, nrow = 3)
figure2


require(grid)
figure2 <- annotate_figure(figure2, left = textGrob(expression(paste("change in entropy, ", Delta, italic(H[L](t)))), rot = 90, vjust = 1, gp = gpar(cex = 1.3)),
                          bottom = textGrob("time points (half-yearly)", gp = gpar(cex = 1.3)))

figure2

# use relative sizes here:
# width: 0.7 (ylabs) + 2 * 2.65 (width/col)
# height: 0.83 (xlabs) + 3 * 1.43 (height/row)
ggsave(file="dest_data_5kb.svg", plot=figure2, width=6, height=5.12)



####################################################

##### 50 kb windows chromosome 2R #######

path <- "/Users/niko/sciebo/Bioinformatik Bachelor Bielefeld/6_WS_2022_2023/dest_project/"
out_path <- "/Users/niko/sciebo/Bioinformatik Bachelor Bielefeld/6_WS_2022_2023/entropy_manuscript/"

#path <- "C:/Users/nvellnow/sciebo7/6_WS_2022_2023/dest_project/"
#out_path <- "C:/Users/nvellnow/sciebo7/6_WS_2022_2023/entropy_manuscript/"

setwd(out_path)

dat_2R_50kb <- read.csv(paste(path, "Entropy_Linvilla_agg_chromosome2R_fixed_win_50000_threshold100.csv", sep=""),
                   head=T)

dat_2R_50kb$window <- as.factor(dat_2R_50kb$window)


# spaghetti plot
theme_set(theme_bw())

p11 <- ggplot(dat_2R_50kb,(aes(x=time_point, y=delta_entropy, color=window)))+
    geom_line(linewidth=window_line, alpha=alph)+
    theme_linedraw()+
    ylab(expression(paste("change in entropy, ", Delta, italic(H[L](t))))) +
    xlab("time points (half-yearly)") +
    ylim(-0.27, 0.25) +
    scale_x_continuous(breaks = round(seq(0, 14, by = 2))) +
    theme(legend.position = "none",
          axis.text.x = element_text(vjust=0.5, hjust = 0.5, size=12),
          axis.title.x = element_text(size=15),
          axis.text.y = element_text(size=12),
          axis.title.y = element_text(size=15),
          strip.text.x = element_text(size=12, margin=margin(.5,.5,.5,.5, "pt")),
          strip.text.y = element_text(size=12, margin=margin(.5,.5,.5,.5, "pt")))+
    stat_summary(fun=mean, colour="black", geom="line", aes(group = 1), linewidth=mean_line)
p11


# violin plot
theme_set(theme_bw())

p12 <- ggplot(dat_2R_50kb,(x=aes(factor(time_point), y=delta_entropy)))+
    geom_violin(fill = "grey", colour = "black", linewidth=violin_line)+
    theme_linedraw()+
    ylab(expression(paste("change in entropy, ", Delta, italic(H[L](t))))) +
    xlab("time points (half-yearly)") +
    ylim(-0.27, 0.25) +
    scale_x_discrete(breaks = round(seq(0, 14, by = 2))) +
    theme(legend.position = "none",
          axis.text.x = element_text(vjust=0.5,hjust = 0.5,size=12),
          axis.title.x = element_text(size=15),
          axis.text.y = element_text(size=12),
          axis.title.y = element_text(size=15),
          strip.text.x = element_text(size=12, margin=margin(.5,.5,.5,.5, "pt")),
          strip.text.y = element_text(size=12, margin=margin(.5,.5,.5,.5, "pt"))
    )+
    stat_summary(fun=median, geom="point", size=violin_median, color="black")
p12

##### 50 kb windows chromosome 3L #######

dat_3L_50kb <- read.csv(paste(path, "Entropy_Linvilla_agg_chromosome3L_fixed_win_50000_threshold100.csv", sep=""),
                   head=T)

dat_3L_50kb$window <- as.factor(dat_3L_50kb$window)


# spaghetti plot
theme_set(theme_bw())

p13 <- ggplot(dat_3L_50kb,(aes(x=time_point, y=delta_entropy, color=window)))+
    geom_line(linewidth=window_line, alpha=alph)+
    theme_linedraw()+
    ylab(expression(paste("change in entropy, ", Delta, italic(H[L](t))))) +
    xlab("time points (half-yearly)") +
    ylim(-0.27, 0.25) +
    scale_x_continuous(breaks = round(seq(0, 14, by = 2))) +
    theme(legend.position = "none",
          axis.text.x = element_text(vjust=0.5, hjust = 0.5, size=12),
          axis.title.x = element_text(size=15),
          axis.text.y = element_text(size=12),
          axis.title.y = element_text(size=15),
          strip.text.x = element_text(size=12, margin=margin(.5,.5,.5,.5, "pt")),
          strip.text.y = element_text(size=12, margin=margin(.5,.5,.5,.5, "pt")))+
    stat_summary(fun=mean, colour="black", geom="line", aes(group = 1), linewidth=mean_line)
p13


# violin plot
theme_set(theme_bw())

p14 <- ggplot(dat_3L_50kb,(x=aes(factor(time_point), y=delta_entropy)))+
    geom_violin(fill = "grey", colour = "black", linewidth=violin_line)+
    theme_linedraw()+
    ylab(expression(paste("change in entropy, ", Delta, italic(H[L](t))))) +
    xlab("time points (half-yearly)") +
    ylim(-0.27, 0.25) +
    scale_x_discrete(breaks = round(seq(0, 14, by = 2))) +
    theme(legend.position = "none",
          axis.text.x = element_text(vjust=0.5,hjust = 0.5,size=12),
          axis.title.x = element_text(size=15),
          axis.text.y = element_text(size=12),
          axis.title.y = element_text(size=15),
          strip.text.x = element_text(size=12, margin=margin(.5,.5,.5,.5, "pt")),
          strip.text.y = element_text(size=12, margin=margin(.5,.5,.5,.5, "pt"))
    )+
    stat_summary(fun=median, geom="point", size=violin_median, color="black")
p14


##### 50 kb windows chromosome 3R #######

dat_3R_50kb <- read.csv(paste(path, "Entropy_Linvilla_agg_chromosome3R_fixed_win_50000_threshold100.csv", sep=""),
                   head=T)

dat_3R_50kb$window <- as.factor(dat_3R_50kb$window)


# spaghetti plot
theme_set(theme_bw())

p15 <- ggplot(dat_3R_50kb,(aes(x=time_point, y=delta_entropy, color=window)))+
    geom_line(linewidth=window_line, alpha=alph)+
    theme_linedraw()+
    ylab(expression(paste("change in entropy, ", Delta, italic(H[L](t))))) +
    xlab("time points (half-yearly)") +
    ylim(-0.27, 0.25) +
    scale_x_continuous(breaks = round(seq(0, 14, by = 2))) +
    theme(legend.position = "none",
          axis.text.x = element_text(vjust=0.5, hjust = 0.5, size=12),
          axis.title.x = element_text(size=15),
          axis.text.y = element_text(size=12),
          axis.title.y = element_text(size=15),
          strip.text.x = element_text(size=12, margin=margin(.5,.5,.5,.5, "pt")),
          strip.text.y = element_text(size=12, margin=margin(.5,.5,.5,.5, "pt")))+
    stat_summary(fun=mean, colour="black", geom="line", aes(group = 1), linewidth=mean_line)
p15


# violin plot
theme_set(theme_bw())

p16 <- ggplot(dat_3R_50kb,(x=aes(factor(time_point), y=delta_entropy)))+
    geom_violin(fill = "grey", colour = "black", linewidth=violin_line)+
    theme_linedraw()+
    ylab(expression(paste("change in entropy, ", Delta, italic(H[L](t))))) +
    xlab("time points (half-yearly)") +
    ylim(-0.27, 0.25) +
    scale_x_discrete(breaks = round(seq(0, 14, by = 2))) +
    theme(legend.position = "none",
          axis.text.x = element_text(vjust=0.5,hjust = 0.5,size=12),
          axis.title.x = element_text(size=15),
          axis.text.y = element_text(size=12),
          axis.title.y = element_text(size=15),
          strip.text.x = element_text(size=12, margin=margin(.5,.5,.5,.5, "pt")),
          strip.text.y = element_text(size=12, margin=margin(.5,.5,.5,.5, "pt"))
    )+
    stat_summary(fun=median, geom="point", size=violin_median, color="black")
p16


##### combine subplots ######

figure3 <- ggarrange(p11 + rremove("ylab") + rremove("xlab"),
                    p12 + rremove("ylab") + rremove("xlab"),
                    p13 + rremove("ylab") + rremove("xlab"),
                    p14 + rremove("ylab") + rremove("xlab"),
                    p15 + rremove("ylab") + rremove("xlab"),
                    p16 + rremove("ylab") + rremove("xlab"),
                    labels = "AUTO",
                    vjust=1.0,
                    hjust=-1,
                    ncol = 2, nrow = 3)
figure3

require(grid)
figure3 <- annotate_figure(figure3, left = textGrob(expression(paste("change in entropy, ", Delta, italic(H[L](t)))), rot = 90, vjust = 1, gp = gpar(cex = 1.3)),
                           bottom = textGrob("time points (half-yearly)", gp = gpar(cex = 1.3)))

figure3

# use relative sizes here:
# width: 0.7 (ylabs) + 2 * 2.65 (width/col)
# height: 0.83 (xlabs) + 3 * 1.43 (height/row)
ggsave(file="dest_data_50kb.svg", plot=figure3, width=6, height=5.12)


