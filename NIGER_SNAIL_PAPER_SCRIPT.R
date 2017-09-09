############
setwd("C:\\Users\\murr\\Documents\\R_FILES-2015")
library(tidyverse)
library(reshape2)
library(tidyr)
library(dplyr)
######### EDA- COLLECTION DATA
new <- read.csv("niger_snail_coll_clean_2011-2016.csv")
dim(new)
head(new)
names(new)
## count, species, inf_stat, site_type, month, year### LOG COUNTS ### SPECIES CONFIRM
plot(new$month, new$count) ### seasonality- august lower. OUTLIERS
plot(new$count, new$site_type) ### warnings - INF
plot(new$year, new$count) ## outliers




##survey data cleanup
#gather()#aggregate() #slice() #select() #mutate() #spread
test3 <- dcast(data = new, site_name + coll_date ~ species + inf_stat, 
               fun.aggregate = sum, value.var = "count")
write.csv(test3, "test_spread_coll_data_090917.csv")

test <- dcast(data, site + date ~ species + status, length)

new2 <-
new %>%
  unite(sp_status, species, inf_stat) %>%
  group_by(site_type, month, year, coll_date, inf_stat, species) %>%
  summarise(tot = sum(count)) %>%
  spread(key = inf_stat, value = TOT, fill = 0)

test3 <- dcast(data, site + date ~ species + status, fun.aggregate = sum, value.var = "TOT")
write.csv(test, "Niger_test.csv")

test <-
  survey %>%
  gather(species, no, BP_tot:L_tot)

######RESHAPE- ADD SPECIES
test <-
  survey %>%
  gather(species, no, BP_tot:L_tot)
########



############ this works but shouls more look like boxplot
ggplot(data = new) + stat_summary(mapping = aes(x = species, y = count),
fun.ymin = min, fun.ymax = max, fun.y = median)
##### good plot
ggplot(data = new) + geom_bar(mapping = aes(x = month, fill = site_type))
ggplot(data = new) + geom_bar(mapping = aes(x = species, fill = site_type))
ggplot(data = new) + geom_bar(mapping = aes(x = site_type, fill = species))#better display
ggplot(data = new) + geom_bar(mapping = aes(x = month, fill = species))
ggplot(data = new) + geom_bar(mapping = aes(x = year, fill = species)) ### DO month,colour-species
ggplot(data = new) + geom_boxplot(mapping = aes(x = species, y = count))## do logs
ggplot(data = new, mapping = aes(x = species, y = count, fill = species)) + geom_violin()
ggplot(data = new, mapping = aes(x = site_type, y = count, fill = site_type)) +
  geom_boxplot() + coord_flip() 
##
new <-
  new %>%
  mutate(log_count = log(count))

############# EDA BY SURVEY DATA- MERGED W WATER CHEM DATA
survey <- merge(new1, new2, by = "ID", all.x = T)
merger_para <- merge(new1, new2, by = "ID", all.y = T)
new1 <- read.csv("Niger_survey.csv")
new2 <- read.csv("niger_params.csv")
write.csv(survey, "niger_merge_survey.csv")  ### this on pretty clean not much missing data
survey <- read.csv("niger_merge_survey.csv")
surv <- read.csv("niger_merge_no_outliers.csv")

#sampling_point, date, village, passage, month # Bulinus_sp, BP_tot, L_tot ##water
plot(survey$Temp_Eau, survey$Temp_Air) ## LINEAR RELSHIP
plot(survey$PH, survey$Bulinus_tot) ## peak- 7 then 9 and 10 much smaller- artifact of data
plot(survey$Conductivite, survey$Bulinus_tot) ##peak at 50-100 appr
plot(survey$PPM, survey$Bulinus_tot)#no tail-few outliers +200 (w low snail nos) 20/50 peak
plot(survey$Profondeur_cm, survey$Bulinus_tot) ## categorical, most0-20, 50
plot(survey$Vitesse_ms, survey$L_tot) ### most zeros
plot(survey$Stagnante, survey$Bulinus_tot) ##yes higher- lot of blanks 
plot(survey$Mise_Eau, survey$Bulinus_tot) ##debut high
plot(survey$Temp_Air, survey$Bulinus_tot) ## one outlier, peak at 30, and little tail40+
plot(survey$Temp_Eau, survey$Bulinus_tot) ## outlier, lower vals than air temp
plot(survey$sampling_point_cl, survey$Bulinus_tot)## boxplot, bulinus- canals, lymnaea- mare
plot(survey$month, survey$Bulinus_tot) ## seasonality- low in aug, more obv in coll data. do as boxplot
plot(survey$month, survey$Bulinus_pos) ## seasonality- low in aug, 
##more obv in coll data. do as boxplot. BP seems less seasonal- i.e. high in jan-jun
plot(surv$month, surv$Temp_Eau)
plot(surv$Altitude_m, surv$snail_tot) 
plot(surv$passage, surv$snail_tot) 
plot(survey$year, surv$Bulinus_tot) 
plot(surv$year, surv$snail_tot) ##2012=2014, 2013=2015
plot(survey$passage, survey$Bulinus_tot) ## very cyclical, bp not- later peak
plot(survey$passage, survey$L_tot)  #cyclical
plot(surv$village, surv$Bulinus_pos) ##
plot(survey$village, survey$L_tot) ## gantchi high
plot(survey$village, survey$BP_tot) ##NG, diam

##counts
survey %>%
  count(sampling_point_cl)
range(surv$Bulinus_tot)
##pp 99 PLOTTING COUNTS
ggplot(survey) + geom_count(mapping = aes(x = village, y = sampling_point_cl))
survey %>%
  count(village, sampling_point_cl) %>%
  ggplot(mapping = aes(x = village, y = sampling_point_cl)) +
  geom_tile(mapping = aes(fill = n))

### EXPLORE DISTRIBUTIONS of continuous variables
###coord_cartesian, xlim, ylim= optimise display
ggplot(data = surv)+ geom_histogram(mapping = aes(x = snail_tot), binwidth = 0.6) +
coord_cartesian((ylim = c(0, 50)))
ggplot(data = surv)+ geom_histogram(mapping = aes(x = BP_tot), binwidth = 0.5)
ggplot(data = surv)+ geom_histogram(mapping = aes(x = Bulinus_pos), binwidth = 0.1)
ggplot(data = surv)+ geom_histogram(mapping = aes(x = PPM), binwidth = 5)
ggplot(data = surv)+ geom_histogram(mapping = aes(x = Conductivite), binwidth = 5)
ggplot(data = surv)+ geom_histogram(mapping = aes(x = Temp_Air), binwidth = 0.5)
ggplot(data = surv)+ geom_histogram(mapping = aes(x = Temp_Eau), binwidth = 0.5)
ggplot(data = survey)+ geom_histogram(mapping = aes(x = PH), binwidth = 0.1)
ggplot(data = survey, mapping = aes(x = Bulinus_tot, colour = sampling_point_cl)) +
geom_freqpoly(binwidth = 100)
ggplot(data = surv, mapping = aes(x = sampling_point_cl, y = Bulinus_tot, 
fill = sampling_point_cl)) + geom_boxplot() + coord_flip()
ggplot(data = survey) + geom_bar(mapping = aes(x = sampling_point_cl))

length(survey$surveynotes[DRY])
#geom_point()
######do logs 

cleaning
new <- survey %>%
  filter(PH < 4 | PH > 14) %>%
  arrange(PH)
new$PH


##STATS ##from bbsrc course code 4- basic stats

plot(surv$Temp_Air ~ surv$Temp_Eau)
#plot(growth$plant.growth.rate ~ growth$soil.moisture.content) moisture- x axis
model1 <- lm(Temp_Air ~ Temp_Eau, data = surv)
#model1 <- lm(plant.growth.rate ~ soil.moisture.content, data = growth)
autoplot(model1, smooth.colour = NA)
anova(model1)
summary(model1)
##add reg line
newX <- expand.grid(Temp_Eau = seq(from = 12, to = 40, length = 100))
#newX <- expand.grid(soil.moisture.content = seq(from = 0.25, to = 2, length = 100))
head(newX)
newY <- predict(model1, newdata = newX, interval = "confidence")
head(newY)
addThese <- data.frame(newX, newY)
head(addThese)
addThese <- rename(addThese,Temp_Air= fit)
#addThese <- rename(addThese, plant.growth.rate = fit)
head(addThese)
ggplot(surv, aes(x = Temp_Eau, y = Temp_Air)) + 
       geom_point(col = "cornflowerblue", size = 3) +
       labs(x = "temp eau", y = "temp air") +
       geom_smooth(data = addThese, aes(ymin = lwr, ymax = upr), stat = "identity")

newX <- expand.grid(Temp_Eau = 32)
prediction <- predict(model1, newdata = newX, interval = "confidence")

# Look at the prediction
prediction
```






