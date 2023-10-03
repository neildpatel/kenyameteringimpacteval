#Load required libraries
library(dplyr)
library(ggplot2)
library(tidyr)

data <- read.csv('FILE PATH')

#PART 1: DATA TRANSFORMATIONS

#Transform the dataset into long format
df <- data|> pivot_longer(cols = c('MayBill','JunBill','JulBill','AugBill','SepBill','OctBill'),
                              names_to='month',
                              values_to='bill')

#Convert treatment status into a binary variable
df$TreatmentStatus <- df$Treatment
df$Treatment <- ifelse(df$Treatment == 'Replaced', 1, 0)

#Quick balance checks
aggregate(df$Treatment, list(df$newcustcode), FUN=mean)
aggregate(df$Treatment, list(df$zn_name), FUN=mean)
aggregate(df$Treatment, list(df$usageclusters), FUN=mean)

#Since no DOD, School, or Kiosks were affected, drop these rows
df_new <- subset(df, newcustcode != "DOD" & newcustcode != "Public Schools" & newcustcode != "Water-Kiosks")

#Drop inactive accounts
df_new <-df_new[!(df_new$month == "OctBill" & df_new$bill == 0),]
df_new <-df_new[!(df_new$month == "MayBill" & df_new$bill == 0),]

#Group bill amounts by Treatment Status, Month, Customer Type
df_grouped <- df_new %>%
    group_by(TreatmentStatus, month, newcustcode) %>%
    summarise(bill = mean(bill),
              month = month,
              newcustcode = newcustcode)

df_grouped$Treatment <- as.factor(df_grouped$Treatment)

#PART 2: PARALLEL TRENDS

#Domestic Trends Over Time
ggplot(data=subset(df_grouped, newcustcode == "Domestic"), aes(x=month, y=log(bill), group=TreatmentStatus))+
  geom_line(aes(color=TreatmentStatus))+geom_point()+
  scale_x_discrete(limits = c('MayBill','JunBill','JulBill','AugBill','SepBill','OctBill'))+
  annotate("rect", fill = "red", alpha = 0.05,
           xmin = "AugBill", xmax = "SepBill",
           ymin = 6, ymax = Inf) +
  scale_fill_discrete(labels=c('Control', 'Replaced'))+
  labs(x = "Month", y = "Average Monthly Bill (Log)", title = "NAWASCO Billing by Month (Domestic)")


#Commercial Trends Over Time
ggplot(data=subset(df_grouped, newcustcode == "Commercial"), aes(x=month, y=log(bill), group=TreatmentStatus))+
  geom_line(aes(color=TreatmentStatus))+geom_point()+
  scale_x_discrete(limits = c('MayBill','JunBill','JulBill','AugBill','SepBill','OctBill'))+
  annotate("rect", fill = "red", alpha = 0.05,
           xmin = "AugBill", xmax = "SepBill",
           ymin = 6, ymax = Inf) +
  scale_fill_discrete(labels=c('Control', 'Replaced'))+
  labs(x = "Month", y = "Average Monthly Bill (Log)", title = "NAWASCO Billing by Month (Commercial)")

#PART 3: DIFFERENCES IN DIFFERENCES

#Drop all months but baseline (May) and endline (Oct) for the DD regression
df_reg <- subset(df_new, month == "MayBill" | month=="OctBill")

#Generate a post binary variable that equals 1 for the October bill at endline
df_reg$post <- ifelse(df_reg$month == "OctBill", 1,0)

#Generate a diff-in-diff indicator that captures the treatment effect of the meter installations
#Interaction between df$post and df$Treatment differences out time trends and baseline differences 
df_reg$did <- df_reg$post * df_reg$Treatment

#Reg 1: Log transformation of the y-variable to interpret in % (a) Commercial
lm_com <- lm(log(bill) ~ Treatment + post + did + zn_name + usageclusters, data=subset(df_reg, newcustcode == "Commercial"))
summary(lm_com)

#Reg 2: Log transformation of the y-variable to interpret in % (b) Domestic
lm_dom <- lm(log(bill) ~ Treatment + post + did + zn_name + usageclusters, data=subset(df_reg, newcustcode == "Domestic"))
summary(lm_dom)
