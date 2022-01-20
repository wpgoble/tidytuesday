setwd("/Volumes/Work Space/tidytuesday/r/height_weight/")
library(ggplot2)
library(reshape2)
library(plyr)

ht_weight_df <- read.csv(file = "01_heights_weights_genders.csv")

# str is short for structure(). It reports what is in the data.frame
# str(ht_weight_df)

plot(x = ht_weight_df$Height, y=ht_weight_df$Weight)

# This looks like a linear relationship between 
# height and weight, lets execute an lm fit to see

lm_ht_weight <- lm(Weight ~ Height, data = ht_weight_df)

# summary(lm_ht_weight)

# how do height and weight depend on gender?

male_df <- subset(ht_weight_df, Gender == "Male")
female_df <- subset(ht_weight_df, Gender == "Female")

#summary(male_df$Height)
#summary(female_df$Height)

ddply(ht_weight_df, .(Gender), function(df) summary(df$Height))

plot(density(male_df$Height))
plot(density(female_df$Height))

dens_by_gender <- ggplot(data = ht_weight_df, aes(x = Height, color = Gender)) +
  geom_density()

dens_by_gender

qq_by_gender <- ggplot(data = ht_weight_df, aes(sample = Height)) + 
  geom_point(stat = "qq") + 
  facet_wrap(~ Gender)

qq_by_gender

ht_weight_pt_gender <- ggplot(data = ht_weight_df, aes(x = Height, y = Weight, color = Gender)) +
  geom_point(alpha = 0.2)

ht_weight_pt_gender

lm_ht_wt_gender <- lm(Weight ~ Height * Gender, data = ht_weight_df)
summary(lm_ht_wt_gender)

# while the means are different, the slopes of these two genders are the same