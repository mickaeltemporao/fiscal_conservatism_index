#!/usr/bin/env Rscript
# ------------------------------------------------------------------------------
# Title:        Fiscal Conservatism Index
# Filename:     test.R
# Description:  TODO: (write me)
# Version:      0.0.0.000
# Created:      2016-04-15 11:48:37
# Modified:     2016-06-02 16:42:29
# Author:       Mickael Temporão < mickael.temporao.1 at ulaval.ca >
# ------------------------------------------------------------------------------
# Copyright (C) 2016 Mickael Temporão
# Licensed under the GPL-2 < https://www.gnu.org/licenses/gpl-2.0.txt >
# ------------------------------------------------------------------------------
library(dplyr)
library(tidyr)
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
test <- filter(test, prov %in% c('AB', 'BC', 'MB', 'NB', 'ON', 'QC', 'SK')) %>%
  filter(year, year %in% c(1970:2015)) #%>%
#  select(-locuteur, -poste, -party) %>% select(ws_2016, everything())

test$prov[test$prov=='AB'] <- '1. Alberta'
test$prov[test$prov=='BC'] <- '2. British-Columbia'
test$prov[test$prov=='MB'] <- '3. Manitoba'
test$prov[test$prov=='NB'] <- '4. New-Brunswick'
test$prov[test$prov=='ON'] <- '5. Ontario'
test$prov[test$prov=='QC'] <- '6. Quebec'
test$prov[test$prov=='SK'] <- '7. Saskatchewan'

db_2 <- test
test2 <- test
test2$prov <- '8.All Provinces'
test <- test %>% bind_rows(test2)

ggplot(test, aes(x = year, y = ws_2016)) +
  geom_point(size=3.5, shape=21, fill='grey', alpha = 0.4) +
  geom_smooth(span=0.8, se=F, color='black', linetype=2) +
  theme(strip.background = element_rect(colour="black", fill="white",
                                       size=1.5, linetype="solid")) +
  annotate("rect", xmin=1985, xmax=1995, ymin=-Inf, ymax=Inf, alpha=0.2, fill='grey') +
  geom_vline(xintercept=1985, linetype='dotdash', alpha = .6)+
  geom_vline(xintercept=1995, linetype='dotdash', alpha = .6)+
  facet_wrap(~prov, ncol=4, nrow=2, scales='free') +
  theme_classic() +
  scale_x_continuous("", limits=c(1970,2015)) +
  scale_y_continuous("", limits=c(-.3,1)) +
  annotate('text', x=2005, y=-.3, label = paste0('n = ',c(43,42,45,43,34,45,43,295)))

ggsave(filename="../figures/can_prov_wordscores.pdf", width=12, height=6, scale =1)

ggplot(db_2, aes(x = year, y = ws_2016, color = prov)) +
  geom_point(size=3.5, color='grey', alpha = 0.4) +
  geom_smooth(span=0.8, se=F, linetype = 2) +
  theme(strip.background = element_rect(colour="black", fill="white",
                                       size=1.5, linetype="solid")) +
  annotate("rect", xmin=1985, xmax=1995, ymin=-Inf, ymax=Inf, alpha=0.2, fill='grey') +
  geom_vline(xintercept=1985, linetype='dotdash', alpha = .6)+
  geom_vline(xintercept=1995, linetype='dotdash', alpha = .6)+
#  facet_wrap(~prov, ncol=4, nrow=2, scales='free') +
  theme_classic() +
  scale_x_continuous("", limits=c(1970,2015)) +
  scale_y_continuous("", limits=c(-.3,1)) +
  theme(legend.title=element_blank(), legend.position='bottom')

ggsave(filename="../figures/can_wordscores.pdf", width=10, height=7, scale =1)

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

# dev.off()
# For comparison, consider the default plot:
# par(op) # reset to default "par" settings
# plot(height.ratio, pop.vote) #yuk!


# for (i in names(test)) {
#   if (sum(is.na(test[,i]))> 40) {
#    test[,i] <- NULL
#   }
# }

# test$gdpUsa <- as.numeric(gsub(",","",test$gdpUsa))
# test$gdpCan <- as.numeric(gsub(",","",test$gdpCan))
# test$year <- as.numeric(test$year)
# test$uniqueWords <- gsub(",","",test$uniqueWords)
# test$uniqueWords <- gsub("[.]","",test$uniqueWords)
# test$uniqueWords <- as.numeric(test$uniqueWords)
# test$deficit1<- as.numeric(gsub(",","",test$deficit1))
# test$deficit2<- as.numeric(gsub(",","",test$deficit2))
# test$depenses2<- as.numeric(gsub(",","",test$depenses2))
# test$vbb1 <- as.numeric(test$vbb1)
# test$prov <- as.factor(test$prov)

# for (i in names(test)) {
#   if (i!='prov') {
#    test[,i] <- as.numeric(test[,i])
#   }
# }

# for (i in names(test)) {
#   if (sum(is.na(test[,i]))> 40) {
#    test[,i] <- NULL
#   }
# }

# dv <- 'ws_2016'
# iv <- c(names(test)[2:length(names(test))])

# model <- as.formula(paste(dv, paste(iv, collapse=" + "), sep=" ~ "))
# rf <- randomForest::randomForest(model, data=test, importance = TRUE, ntree = 2500, do.trace = 250,   na.action=na.roughfix)

# pred.rf <- predict(rf, test)
# RMSE.rf <- sqrt(mean((pred.rf-test$rri)^2, na.rm=T))





# plot(ab$year, ab$trans_score , type="l", col="red")
# lines(cb$year, cb$trans_score, lty=2)
# lines(mb$year, mb$trans_score, lty=3)
# lines(nb$year, nb$trans_score, lty=4)
# lines(on$year, on$trans_score, lty=5)
# lines(qc$year, qc$trans_score, lty=6)
# lines(sk$year, sk$trans_score, lty=7)
