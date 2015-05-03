getwd()

reddit <- read.csv('reddit.csv')

table(reddit$employment.status)
summary(reddit)
str(reddit)
levels(reddit$age.range)


library(ggplot2)
qplot(data = reddit, x = age.range)
qplot(data = reddit, x=income.range)

reddit$age.range <- ordered(reddit$age.range, levels = c('Under 18', '18-24', '25-34', '35-44', '45-54', '55-64', '65 or Above'))

pf <- read.delim('pseudo_facebook.tsv')

qplot(data = pf, x = friend_count, xlim = c(0,1000), binwidth =25)
qplot(data = subset(pf, !is.na(gender)), x = friend_count, binwidth =10) +
  scale_x_continuous(limits = c(0,1000), breaks = seq(0,1000,50)) +
  facet_wrap(~gender)
table(pf$gender)
by(pf$friend_count, pf$gender, summary)

qplot(x = tenure/365, data = pf, binwidth = .25, color = I('black'), fill = I('#099DD9')) +
  scale_x_continuous (breaks = seq(1,7,1), lim = c(0,7))

qplot(x = age, data = pf, binwidth = 1, color = I('black'), fill = I('#099DD9'))+
  scale_x_continuous (lim = c(0,120))

summary(pf$age)

install.packages('gridExtra')
library(gridExtra)

p1 <- qplot(x = friend_count, data = pf)
p2 <- qplot(x = log10(friend_count + 1), data = pf)
p3 <- qplot(x = sqrt(friend_count), data=pf)
grid.arrange(p1, p2, p3, ncol = 1)

p1 <- ggplot(aes(x= friend_count), data = pf) + geom_histogram()
p2 <- p1 + scale_x_log10()
p3 <- p1 + scale_x_sqrt()
grid.arrange(p1, p2, p3, ncol = 1)

qplot(x = www_likes, data = subset(pf, !is.na(gender)),
      geom = 'freqpoly', color = gender)+
  scale_x_continuous()+
  scale_x_log10()

by(pf$www_likes, pf$gender, sum)

qplot(x = gender, y = friend_count, data = subset(pf, !is.na(gender)), geom = 'boxplot', ylim = c(0,1000))
qplot(x = gender, y = friend_count, data = subset(pf, !is.na(gender)), geom = 'boxplot') +
  scale_y_continuous(limits = c(0, 1000))
qplot(x = gender, y = friend_count, data = subset(pf, !is.na(gender)), geom = 'boxplot') +
  coord_cartesian(ylim = c(0,1000))
by(pf$friend_count, pf$gender, summary)

names(pf)

by(pf$friendships_initiated, pf$gender, summary)
qplot(x = gender, y=f)

mobile_check_in <- NA
pf$mobile_check_in <- ifelse(pf$mobile_likes > 0, 1, 0)
pf$mobile_check_in <- factor(pf$mobile_check_in)
summary(pf$mobile_check_in)
sum(pf$mobile_check_in == 1)/length(pf$mobile_check_in)
library(ggplot2)
data(diamonds)
summary(diamonds)
?diamonds
diamonds$color
summary(diamonds)
qplot(x = price, data = diamonds)
summary(diamonds$price)
diamonds$price < 500
diamonds$price <- NA
diamonds$price <- ifelse(diamonds$price < 500, 1, 0)
diamonds$price <- factor(diamonds$price)
summary(diamonds$price)
diamonds$price <- ifelse(diamonds$price < 250, 1, 0)
diamonds$price <- factor(diamonds$price)
summary(diamonds$price_250)
diamonds$price <- ifelse(diamonds$price >= 15000, 1, 0)
diamonds$price <- factor(diamonds$price)
summary(diamonds$price_15001)
diamonds$price_15001 <- ifelse(diamonds$price > 15000, 1, 0)
sum(diamonds$price == 1)
price1 <- NA
diamonds$price <- NA
diamonds$price <- ifelse(diamonds$price < 500, 1, 0)
diamonds$price <- factor(diamonds$price)
summary(diamonds$price)
length(diamonds$price) 
summary(diamonds$price == 1)
d_250 <- subset(diamonds$price < 250)
d_500 <- subset(diamonds, diamonds$price < 500)
d_15000 <- subset(diamonds, diamonds$price > 15000)
d_250 <- factor(d_250)
dim(d_250)
dim(d_500)
dim(d_15000)
data(diamonds)
qplot(x = price, data = diamonds, binwidth = 20, color = I('blue'), fill = I('#099DD9')) +
scale_x_continuous (lim = c(0,18000)) + facet_wrap(~cut)
ggsave('priceHistrogram.png')
summary(diamonds)
qplot(y = carat, x = price, data = diamonds) +
  facet_wrap(~cut)

q2 <- qplot(x = log10(friend_count + 1), data = pf)
q3 <- qplot(x = sqrt(friend_count), data=pf)
summary(diamonds$cut)
summary(diamonds$price)
by(diamonds$price, diamonds$cut, max)
by(diamonds$price, diamonds$cut, min)
qplot(x = price, data = diamonds) + facet_wrap(~cut)
qplot(data=diamonds, x=log10(price/carat),
      binwidth=0.05qplot(x = X2008, data = CA, geom = 'boxplot', color = I('blue'), fill = I('#099DD9')) +
  facet_wrap(~color, scales="free_y"),
      xlab="Price by Carat by log10",
      ylab="Number of diamonds in sample",
      color=I("black"), fill=I("#099DD9")) +
  facet_wrap(~cut, scales="free_y")
by(diamonds$price, diamonds$color, summary)
qplot(y = price, x = cut, data = diamonds, geom = 'boxplot', color = I('blue'), fill = I('#099DD9'))
ggsave('priceBoxplot.png')
by(diamonds$price, diamonds$color, summary)
IQR(subset(diamonds, color == "D")$price) 
IQR(subset(diamonds, color = D)$color) 
IQR(subset(diamonds, color = D)$price) 
IQR(subset(diamonds, color = J)$price) 
IQR(subset(diamonds, color == "J")$price) 
qplot(y = price, x = carat, data = diamonds, geom = 'boxplot', color = I('blue'), fill = I('#099DD9')) +
  facet_wrap(~color, scales="free_y")
qplot(x = price, y = carat, data = diamonds, binwidth=0.05,
      geom = 'freqpoly')
qplot(carat, data = diamonds,geom = 'freqpoly',xlim = c(0.0,5.5),binwidth = 0.1)
library(dplyr)
install.packages("devtools")
devtools::install_github("rstudio/EDAWR")
library(EDAWR)
?storms
?cases
?pollution
?tb
install.packages("tidyr")
library(tidyr)
?gather
?spread
install.packages("dplyr")
?select
?filter
?arrange
?mutate
?summarise
?group_by
library(dplyr)
install.packages("nycflights13")
?airlines
library(nycflights13)
?airports
?flights
?planes
?weather
dir
cd
CA <- read.csv('Child-All.csv')
qplot(x = Gapminder.name, y = X2008, data = CA, color = I('blue'), fill = I('#099DD9'))
AH <- read.csv('alcohol.csv')
qplot(X = Year, data = AH, color = I('blue'), fill = I('#099DD9'))
em <- read.csv('employ.csv')
name(em)
summary(em)
qplot(age,friend_count,data=pf)
ggplot(aes(x=age, y=friend_initiated),data=pf) + geom_point(alpha = 1/20, position = position_jitter(h=0)) + xlim(13,90) + coord_trans(y = 'sqrt')
?pf
summary(pf)
ggplot(aes(x=age, y=friendships_initiated),data=pf) + geom_point(alpha = 1/20, position = position_jitter(h=0)) + xlim(13,90) + coord_trans(y = 'sqrt')
library(dplyr)
filter()
age_groups <- group_by(pf,age)
pf.fc_by_age <- summarise(age_groups, friend_count_mean = mean(friend_count),
          friend_count_median = median(friend_count),
          n=n())
pf.fc_by_age <- arrange(pf.fc_by_age, age)
head(pf.fc_by_age)

pf.fc_by_age <- pf %.%
group_by(age)%.%
  summarise(age_groups, friend_count_mean = mean(friend_count),
            friend_count_median = median(friend_count),
            n=n()) %.%
  arrange(age)
head(pf.fc_by_age, 20)
ggplot(aes(age, friend_count_mean), data = pf.fc_by_age) + geom_line()
ggplot(aes(x=age, y=friendships_initiated),data=pf) + 
  geom_point(alpha = 0.05, position = position_jitter(h=0), color ='orange') + 
  xlim(13,90) + 
  coord_trans(y = 'sqrt') +
  geom_line(stat= 'summary', fun.y = mean) +
  geom_line(stat= 'summary', fun.y = quantile, probs = .1,
            linetype = 2, color = 'blue') +
  geom_line(stat= 'summary', fun.y = quantile, probs = .9,
            linetype = 2, color = 'blue') + 
  geom_line(stat= 'summary', fun.y = quantile, probs = .5,
            color = 'blue')
?cor.test
cor.test(pf$age, pf$friend_count, method = 'pearson')
with(pf,cor.test(pf$age, pf$friend_count, method = 'pearson'))
ggplot(aes(x=www_likes_received, y=likes_received),data=pf)  + geom_point() +
  xlim(0, quantile(pf$www_likes_received, 0.95)) +
  ylim(0, quantile(pf$likes_received, 0.95)) +
  geom_smooth(method = 'lm', color = 'red')
cor.test(pf$www_likes_received, pf$likes_received, method='pearson')
install.packages('alr3')
library(alr3)
data(Mitchell)
?Mitchell
summary(Mitchell)
names(Mitchell)
ggplot(aes(x=Month, y = Temp),data = Mitchell) + 
  geom_point() +
  scale_x_discrete(breaks = seq(0,203,12))
cor.test(Mitchell$Month,Mitchell$Temp)
pf$age_with_months <- pf$age + (12 - pf$dob_month) / 12
library(dplyr)
pf.fc_by_age_months <- pf %.%
  group_by(age_with_months) %.%
  summarise(friend_count_mean = mean(friend_count),
            friend_count_median = median(friend_count),
            n = n()) %.%
  arrange(age_with_months)
head(pf.fc_by_age_months)
age_with_months_groups <- group_by(pf, age_with_months)
pf.fc_by_age_months2 <- summarise(age_with_months_groups,
                                  friend_count_mean = mean(friend_count),
                                  friend_count_median = median(friend_count),
                                  n = n())
pf.fc_by_age_months2 <- arrange(pf.fc_by_age_months2, age_with_months)
head(pf.fc_by_age_months2)

p1 <- ggplot(aes(x= age, y = friend_count_mean),
       data = subset(pf.fc_by_age, age<71)) +
  geom_line() + geom_smooth()
p2 <- ggplot(aes(x=age_with_months, y = friend_count_mean),
       data = subset(pf.fc_by_age_months, age_with_months < 71)) +
  geom_line() + geom_smooth()
p3 <- ggplot(aes(x=round(age/5) * 5, y = friend_count),
             data = subset(pf, age < 71)) +
  geom_line(stat = 'summary', fun.y = mean)
library(gridExtra)
grid.arrange(p2, p1, p3, ncol = 1)
ggplot(aes(x=price, y = x), data = diamonds) + geom_point()
cor.test(diamonds$price, diamonds$depth, method = 'pearson')
ggplot(aes(x=price, y = x * y * z), data = diamonds) + geom_point()  +
  xlim(0, quantile(diamonds$price, 0.99)) + 
  ylim(0, 500)
ggplot(aes(x=table, y = price), data = diamonds) + geom_point(aes(color = cut)) +
  xlim(0, quantile(diamonds$price, 0.99)) +
  ylim(0, quantile(diamonds$carat, 0.99)) 

cor.test(diamonds$price, diamonds$depth, method = 'pearson')

with(subset(diamonds, x*y*z <= 800), cor.test(diamonds$price, diamonds$x*y*z), method = 'pearson')

diamondSub <- subset(diamonds, x*y*z > 0 & x*y*z < 800)

clarity_Groups <- group_by(diamonds, clarity)
diamondsByClarity <- summarize(clarity_Groups,
                               mean_price = mean(price),
                               median_price = median(price),
                               min_price = min(price),
                               max_price = max(price),
                               n = n())
diamonds_by_clarity <- group_by(diamonds, clarity)
diamonds_mp_by_clarity <- summarise(diamonds_by_clarity, mean_price = mean(price))
install gridExtra
ggplot(aes(x=price, y=table), data = diamonds) +
  geom_point(aes(color = cut, stat = 'summary', fun.y = median))


ggplot(aes(x=age, y=friend_count), data=subset(pf, !is.na(year_joined.bucket))) +
  geom_line(aes(color = year_joined.bucket, stat = 'summary', fun.y = median))

install.packages("dplyr")
library(dplyr)
pf.fc_by_age_gender <- pf %>%
  filter(!is.na(gender)) %>%
  group_by(age, gender) %>%
  summarise(mean_friend_count = mean(as.numeric(friend_count)),
            median_friend_count = median(as.numeric(friend_count)),
            n = n()) %>%
  ungroup() %>%
  arrange(age)

head(pf.fc_by_age_gender)

ggplot(aes(x=age, y = median_friend_count), data = pf.fc_by_age_gender) +
  geom_line(aes(color=gender))
install.packages('reshape2')
library(reshape2)
pf.fc_by_age_gender.wide <- dcast(pf.fc_by_age_gender, age ~ gender,
                                  value.var = 'median_friend_count')
head(pf.fc_by_age_gender.wide)

ggplot(aes(x=age, y = female/male), data = pf.fc_by_age_gender.wide) +
  geom_line() +
  geom_hline(yintercept = 1, alpha = 0.3, linetype = 2)
pf$year_joined <- floor(2014 - pf$tenure/365)
summary(pf$year_joined)
table(pf$year_joined)
pf$year_joined.bucket <- cut(pf$year_joined, c(2004,2009,2011,2012,2014))
table(pf$year_joined.bucket, useNA = 'ifany')
ggplot(aes(x=age, y=friend_count), data=subset(pf, !is.na(year_joined.bucket))) +
  geom_line(aes(color = year_joined.bucket), stat = 'summary', fun.y = median)

ggplot(aes(x=age, y=friend_count), data=subset(pf, !is.na(year_joined.bucket))) +
  geom_line(aes(color = year_joined.bucket), stat = 'summary', fun.y = mean)+
  geom_line(stat = 'summary', fun.y = mean, linetype = 2)
with(subset(pf, tenure >= 1), summary(friend_count/tenure))
ggplot(aes(x = 30* round(tenure/30), y = friendships_initiated / tenure), 
       data = subset(pf, tenure >= 1)) +
  geom_line(aes(color = year_joined.bucket))
ggplot(aes(x = 7 * round(tenure / 7), y = friendships_initiated / tenure),
       data = subset(pf, tenure > 0)) +
  geom_smooth(aes(color = year_joined.bucket))
yo <- read.csv('yogurt.csv')
str(yo)
yo$id <- factor(yo$id)
str(yo)
qplot(data = yo, x = price, fill = I('#F79420'))
yo <- transform(yo, all.purchases = strawberry + blueberry + 
                  pina.colada + plain + mixed.berry)
summary(yo$all.purchases)

ggplot(aes(x=time, y=price), data = yo) +
  geom_jitter(alpha = 1/4, shape = 21, fill = I('#F79420'))

set.seed(4111)
sample.ids <- sample(levels(yo$id), 16)
sample.ids

ggplot(aes(x=time, y=price),
       data = subset(yo, id %in% sample.ids)) +
  facet_wrap(~id) +
  geom_line ()+
  geom_point(aes(size=all.purchases), pch = 1)
install.packages('GGally')
library(GGally)
theme_set(theme_minimal(20))

set.seed(1836)
pf_subset <- pf[, c(2:15)]
names(pf_subset)
ggpairs(pf_subset[sample.int(nrow(pf_subset), 1000),])
nci <- read.table('nci.tsv')
library(reshape2)
nci.long.samp <- melt(as.matrix(nci[1:200,]))
names(nci.long.samp) <- c('gene','case','value')
head(nci.long.samp)

ggplot(aes(y = gene, x = case, fill = value),
       data = nci.long.samp) +
  geom_tile() +
  scale_fill_gradientn(colours = colorRampPalette(c('blue', 'red')) (100))

ggplot(aes(x=table, y = price), data = diamonds) + geom_point(aes(color = cut))

ggplot(aes(x=table, y = price), data = diamonds) + geom_point(aes(color = cut))

ggplot(aes(y= log10(price), x = x*y*z), data = diamonds) + geom_point(aes(color = clarity)) +
  xlim(0, quantile(diamonds$volume, 0.01)) +
  ylim(0, quantile(diamonds$price, 0.01))

ggplot(diamonds, aes(x = price, fill = cut)) +
  geom_histogram() +
  facet_wrap(~ color) +
  scale_fill_brewer(type = 'qual') +
  scale_x_log10(expression(paste(Log[10], " of Price"))) +
  ylab("Count")

ggplot(diamonds, aes(x = table, y = price, color = cut)) + 
  geom_jitter(size = 3) +
  scale_x_continuous(breaks = seq(50, 80, 2),
                     limits = c(50, 80)) +
  scale_color_brewer(type = 'qual') +
  theme_minimal()

library (dplyr)
diamonds <- diamonds %>% 
  mutate(volume = x * y *z)

ggplot(subset(diamonds, volume <= quantile(volume, 0.99) & volume > 0 ), aes(x = volume, y = price, color = clarity)) +
  geom_jitter(size = 3) + 
  scale_y_log10() +
  scale_color_brewer(type = 'div') + 
  theme_minimal()

pf <- pf %>% mutate(pf, prop_initiated = friendships_initiated/friend_count)
summary (pf)

fb <- tbl_df(read.table("pseudo_facebook.tsv", header=TRUE))
fb <- fb %>%
  mutate(prop_initiated = friendships_initiated/friend_count)

pf <- pf %>%
  mutate(year_joined = floor(2014 - tenure/365),
         year_joined_bucket = cut(year_joined, breaks=c(2004, 2009, 2011, 2012, 2014))) 

pf2 <- pf %>%
  mutate(prop_initiated = ifelse(friend_count > 0, friendships_initiated/friend_count, 0))

ggplot(subset(pf2, tenure > 0), aes(x=tenure, y=prop_initiated)) +
  geom_line(aes(color=year_joined_bucket), stat='summary', fun.y=mean) + 
  theme_minimal()

ggplot(subset(pf, tenure > 0), aes(x=tenure, y=prop_initiated)) +
  #geom_line(aes(color=year_joined_bucket), stat='summary', fun.y=mean) +
  geom_smooth(aes(color = year_joined_bucket))

pf %>%
  filter(year_joined_bucket == "(2012,2014]") %>%
  summarise(avg = mean(prop_initiated, na.rm=TRUE))

ggplot(diamonds, aes(x = cut, y = price/carat, color = color)) + 
  geom_jitter() + 
  facet_wrap(~clarity) + 
  scale_color_brewer(type = 'div')

qplot(data = diamonds, x = carat, y = price, 
      xlim = c(0, quantile(diamonds$carat, .99)),
      ylim = c(0, quantile(diamonds$price, .99))) +
  geom_point(fill = I('#F79420'), color = I('black'), shape = 21)

ggplot(data = diamonds, aes(x = carat, y = price)) + 
  geom_point(color = I('#F79420'), alpha = 1/4) +
  stat_smooth(method = 'lm') +
  scale_x_continuous(lim = c(0, quantile(diamonds$carat, .99))) +
  scale_y_continuous(lim = c(0, quantile(diamonds$price, .99)))

install.packages('GGally')
install.packages('scales')
install.packages('memisc')
install.packages('lattice')
install.packages('MASS')
install.packages('car')
install.packages('reshape')
install.packages('plyr')

library(ggplot2)
library(GGally)
library(scales)
library(memisc)
set.seed(20022012)
diamond_samp <- diamonds[sample(1:length(diamonds$price), 10000),]
ggpairs(diamond_samp, params = c(shape = I('-'), outlier.shape = I('.')))

library(gridExtra)

plot1 <- qplot(data = diamonds, x = price, binwidth = 100, fill = I('#099DD9')) + 
  ggtitle('Price')

plot2 <- qplot(data = diamonds, x = price, binwidth = 0.01, fill = I('#F79420')) + 
  ggtitle('Price (log10)') + scale_x_log10()

grid.arrange(plot1, plot2, ncol = 2)

qplot(carat, price, data=diamonds) +
  scale_y_continuous(trans = log10_trans())+
  ggtitle('Price(log10) by Carat')

cuberoot_trans = function() trans_new('cuberoot', transform = function(x) x^(1/3),
                                      inverse = function (x) x^3)
ggplot(aes(carat, price), data = diamonds) +
  geom_point(alpha = 0.5, size = 0.75, position = 'jitter') +
  scale_x_continuous(trans = cuberoot_trans(), limits = c(0.2, 3),
                     breaks = c(0.2, 0.5, 1, 2, 3)) +
  scale_y_continuous(trans = log10_trans(), limits = c(350, 15000),
                     breaks = c(350, 1000, 5000, 10000, 15000)) +
  ggtitle('Price (log10) by Cube-Root of Carat')

head(sort(table(diamonds$carat), decreasing = T))
head(sort(table(diamonds$price), decreasing = T))

install.packages('RColorBrewer', dependencies = TRUE) 
library(RColorBrewer) 

ggplot(aes(x = carat, y = price, colour = color), data = diamonds) + 
  geom_point(alpha = 0.5, size = 1, position = 'jitter') +
  scale_color_brewer(type = 'div',
                     guide = guide_legend(title = 'Color',
                                          override.aes = list(alpha = 1, size = 2))) +  
  scale_x_continuous(trans = cuberoot_trans(), limits = c(0.2, 3),
                     breaks = c(0.2, 0.5, 1, 2, 3)) + 
  scale_y_continuous(trans = log10_trans(), limits = c(350, 15000),
                     breaks = c(350, 1000, 5000, 10000, 15000)) +
  ggtitle('Price (log10) by Cube-Root of Carat and Color')

m1 <- lm(I(log(price))~ I(carat ^ (1/3)), data = diamonds)
m2 <- update(m1, ~ . + carat)
m3 <- update(m2, ~ . + cut)
m4 <- update(m3, ~ . + color)
m5 <- update(m4, ~ . + clarity)
mtable(m1, m2, m3, m4, m5)

install.packages('RCurl')
install.packages('bitops')
library('RCurl')
library('bitops')
diamondsurl = getBinaryURL("https://github.com/solomonm/diamonds-data")
load("BigDiamonds.rda")

diamndsbig$logprice = log(diamondsbig$price)
m1 <- lm(I(log(price))~ I(carat ^ (1/3)), 
         data = diamondsbig[diamondsbig$price < 10000 & diamondsbig$cert == "GIA",])
m2 <- update(m1, ~ . + carat)
m3 <- update(m2, ~ . + cut)
m4 <- update(m3, ~ . + color)
m5 <- update(m4, ~ . + clarity)
mtable(m1, m2, m3, m4, m5)

thisDiamond = data.frame(carat = 0.96, cut = "V.Good", color = "E", clarity = "VS1")

modelEstimate = predict(m5, newdata = thisDiamond, interval = "prediction", level = .95)
exp(modelEstimate)

install.packages("ggplot2", dependencies = T) 
install.packages("knitr", dependencies = T)
install.packages("dplyr", dependencies = T)

library(GGally)
pair_1 <- wine[,2:5]
pair_1$quality <- wine$quality
ggpairs(pair_1)

pair_2 <- wine[,6:9]
pair_2$quality <- wine$quality
ggpairs(pair_2)

pair_3 <- wine[,10:12]
pair_3$quality <- wine$quality
ggpairs(pair_3)
