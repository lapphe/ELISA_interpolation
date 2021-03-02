# Interpolating cort measurements from samples using Enzo Cortisol Kit
library(drc)
library(tidyverse)
library(ggrepel)

# before running this, format your exported data into columns: sample, Read1, read2, average, concentration

df<- read.csv("mm_cort.csv")

#log transform the cort concentrations
df$logCort<- log10(df$concentration)

#plot to look at curve of standards

ggplot(df, aes(x=logCort, y= AVERAGE))+
  geom_point()+
  theme_minimal()

plot(df$logCort, df$AVERAGE, main="log standard curve", xlab="x=log(conc)", ylab="y=OD")

#fit 4 parameter standard curve
fit<-drm(formula = AVERAGE ~ logCort , data = df, fct = LL.4())

#plot the curve
plot(fit, main="log standard curve", xlab="logCort(pg/mL)", ylab="OD")


df$interpolated<- fit$coefficients[4]*(((-1* fit$coefficients[3]+df$AVERAGE)/( fit$coefficients[2]-df$AVERAG))^(1/ fit$coefficients[1]))

plot(df$interpolated, df$AVERAGE)

#undo the log transformation to get cort in (pg/mL)
df$cort_pg_mL <- 10^(df$interpolated)

#convert to ug/dL

df$cort_ug_dL <- df$cort_pg_mL* 0.0001


#final plot
ggplot(df, aes(x = cort_pg_mL, y = AVERAGE, label = Sample))+
  geom_point()+
  geom_point(aes(x= concentration, y = AVERAGE), color = "red", size = 4, alpha= .6)+
  geom_text_repel(size = 3, alpha = .8)



