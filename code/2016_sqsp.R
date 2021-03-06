#!/usr/bin/env Rscript
# ------------------------------------------------------------------------------
# Title:        Fiscal Conservatism Index
# Filename:     test.R
# Description:  TODO: (write me)
# Version:      0.0.0.000
# Created:      2016-04-15 11:48:37
# Modified:     2017-04-08 17:56:35
# Author:       Mickael Temporão < mickael.temporao.1 at ulaval.ca >
# ------------------------------------------------------------------------------
# Copyright (C) 2016 Mickael Temporão
# Licensed under the GPL-2 < https://www.gnu.org/licenses/gpl-2.0.txt >
# ------------------------------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(ggthemes)
path <- '../data/20160417_wordscores/'
files <- list.files('../data/20160417_wordscores', pattern='*.csv')

# Read files in separate objects
ab <- read.csv(paste0(path, files[1]), stringsAsFactors=F)
bc <- read.csv(paste0(path, files[2]), stringsAsFactors=F)
mb <- read.csv(paste0(path, files[3]), stringsAsFactors=F)
nb <- read.csv(paste0(path, files[4]), stringsAsFactors=F)
on <- read.csv(paste0(path, files[5]), stringsAsFactors=F)
qc <- read.csv(paste0(path, files[6]), stringsAsFactors=F)
sk <- read.csv(paste0(path, files[7]), stringsAsFactors=F)

#data2 <- read.csv('data/data_idb.csv', stringsAsFactors=F)

# merge files in one data frame
db <- data.frame(year = 1970:2015)
db <- db %>% left_join(ab) %>%
  rename(AB = trans_score)%>%
  left_join(bc) %>%
  rename(BC = trans_score) %>%
  left_join(mb) %>%
  rename(MB = trans_score) %>%
  left_join(nb) %>%
  rename(NB = trans_score) %>%
  left_join(on) %>%
  rename(ON = trans_score) %>%
  left_join(qc) %>%
  rename(QC = trans_score) %>%
  left_join(sk) %>%
  rename(SK = trans_score) %>%
  gather(prov, ws_2016, AB:SK)# %>%
#  left_join(data2)

test <- db[!is.na(db$ws_2016),]
test <- dplyr::filter(test, prov %in% c('AB', 'BC', 'MB', 'NB', 'ON', 'QC', 'SK')) %>%
  dplyr::filter(year, year %in% c(1970:2015)) #%>%

test$label_fr[test$prov=='AB'] <- 'Alberta'
test$label_fr[test$prov=='BC'] <- 'Colombie-Britannique'
test$label_fr[test$prov=='MB'] <- 'Manitoba'
test$label_fr[test$prov=='NB'] <- 'Nouveau-Brunswick'
test$label_fr[test$prov=='ON'] <- 'Ontario'
test$label_fr[test$prov=='QC'] <- 'Québec'
test$label_fr[test$prov=='SK'] <- 'Saskatchewan'

ggplot(test, aes(x = year, y = ws_2016)) +
  geom_point(size=3.5, color='grey', alpha = 0.4) +
  geom_smooth(span=0.8, se=F, color='grey46', aes(linetype = label_fr, group = label_fr)) +
  theme_classic() +
  scale_x_continuous("", limits=c(1970,2015)) +
  scale_y_continuous("", limits=c(-.3,1)) +
  scale_colour_grey() +
  theme(legend.title=element_blank(),
        legend.position='bottom',
        legend.text=element_text(size=18),
        legend.key.width=unit(1, "cm"),
        text = element_text(size=20),
        strip.background = element_rect(colour="black", fill="white",
                                       size=1.5, linetype="solid"))

ggsave(filename="../figures/can_wordscores_FR.png", width=12, height=8)

test$label_fr[test$prov=='AB'] <- '1. Alberta'
test$label_fr[test$prov=='BC'] <- '2. Colombie-Britannique'
test$label_fr[test$prov=='MB'] <- '3. Manitoba'
test$label_fr[test$prov=='NB'] <- '4. Nouveau-Brunswick'
test$label_fr[test$prov=='ON'] <- '5. Ontario'
test$label_fr[test$prov=='QC'] <- '6. Québec'
test$label_fr[test$prov=='SK'] <- '7. Saskatchewan'

# Wraped Figures Plot
db_2 <- test
test2 <- test
test2$prov <- 'ALL'
test2$label_fr <- '8. Toutes les provinces'

test <- test %>% bind_rows(test2)

ggplot(test, aes(x = year, y = ws_2016)) +
  geom_point(size=3.5, shape=21, fill='grey', alpha = 0.4) +
  geom_smooth(span=0.8, se=F, color='black', linetype=2) +
  theme(strip.background = element_rect(colour="black", fill="white",
                                       size=1.5, linetype="solid")) +
  # annotate("rect", xmin=1985, xmax=1995, ymin=-Inf, ymax=Inf, alpha=0.2, fill='grey') +
  # geom_vline(xintercept=1985, linetype='dotdash', alpha = .6)+
  # geom_vline(xintercept=1995, linetype='dotdash', alpha = .6)+
  facet_wrap(~label_fr, ncol=4, nrow=2, scales='free') +
  theme_classic() +
  scale_x_continuous("", limits=c(1970,2015)) +
  scale_y_continuous("", limits=c(-.3,1)) +
  annotate('text', x=2005, y=-.3, label = paste0('n = ',c(43,42,45,43,34,45,43, 295)))
  # annotate('text', x=2005, y=-.3, label = paste0('n = ',c(43,42,45,43,34,45,43,295)))

ggsave(filename="../figures/can_prov_wordscores_fr.png", width=12, height=6, scale =1)

# Base Plot
plot(test$year, test$ws_2016, col = "black", pch = 21, bg = "grey", cex = 2,
     xlim = c(1970,2020), ylim = c(-.5,1.1), ylab = "", xlab = "", axes = FALSE)
axis(1)
axis(2)
fit_loess = loess.smooth(test$year, test$ws_2016, family="gaussian", span=.2, degree=1)
lines(fit_loess, col=1, lwd=3)
lines(lowess(test$ws_2016~test$year, f=.2), col=2, lwd=3)
legend(1980, -0.4, c(paste("f = ", c("2/3", ".2"))), lty = 1, col = 2:3)
#reg1 <- scatter.smooth(test$ws_2016~test$year)
#ablineclip(reg1)
par(las = 0)
mtext("Presidential Height Ratio", side = 1, line = 2.5, cex = 1.5)
mtext("Relative Support for President", side = 2, line = 3.7, cex = 1.5)
text(1.15, .65, "r = .39", cex = 1.5)

options(OutDec= ".")
