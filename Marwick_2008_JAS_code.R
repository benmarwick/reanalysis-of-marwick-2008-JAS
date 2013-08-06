# Reanalysis of data from:

# Marwick, B. 2008. What attributes are important 
# for the measurement of assemblage reduction intensity? 
# Results from an experimental stone artefact assemblage 
# with relevance to the Hoabinhian of mainland Southeast 
# Asia. Journal of Archaeological Science 35(5): 1189-1200
# http://dx.doi.org/10.1016/j.jas.2007.08.007

### get data
# where is the data file?
setwd("E:/My Documents/My Papers/Journal of Anthropological Archaeology/Revise and resubmit")
dat <- read.csv("Marwick_2008_JAS_data.csv")

### prepae data
# get flake sequence numbers
dat$flake_seq <- as.numeric(gsub("\\D", "", dat$HAPP.artefact.number))
# get core IDs
dat$core_id <- gsub("[[:digit:]]", "", dat$HAPP.artefact.number)
# recode core_ids
# just keep the first two characters of core id
# other characters denote ties in flaking sequence
dat$core_id <- substring(dat$core_id, 1, 2)
# if the first character is not 'A' then drop second character
dat$core_id <- ifelse(substring(dat$core_id, 1, 1) != 'A', substring(dat$core_id, 1, 1), dat$core_id)
# subset to use only complete flakes
dat <- dat[dat$Flake.Type =='Complete', ]

### analyse data

## overhang removal
# recode OHR as 0 or 1
unique(dat$Overhang.removal)
dat$Overhang.removal <- with(dat, ifelse(Overhang.removal == 'Present', 1, 0 ))
# use logistic regression to investigate
# relationship between presence of OHR
# and position of flake in removal sequence
library(plyr)
# Break up dat by core_id, then fit the 
# specified model to each piece and
# return a list
models <- dlply(dat, "core_id ", function(df) 
  glm(Overhang.removal ~ flake_seq, family=binomial(logit), data=df)   )

# Print the summary of each model
l_ply(models, summary, .print = TRUE)

# plot all models
lapply(models, function(i) plot(i$fitted) )

# Apply coef to each model and return a data frame
ldply(models, coef)

# Apply p-values to each model and return a data frame
tmp <- ldply(models, function(i) summary(i)$coef[, "Pr(>|z|)"][2])

# subset where p < 0.05
tmp1 <- tmp[ tmp$flake_seq < 0.5, ]
# none! so subset where p < 0.1
tmp2 <- tmp[ tmp$flake_seq < 0.5, ]
models_p <- models[na.omit(tmp2$core_id)]
# plot models where p < 0.1
windows()
par(ask = TRUE)
lapply(models_p, function(i) plot(i$fitted) )
dev.off()

# result:
# we get 7 cores, with 6 out of 7 showing 
# positive correlation between overhang removal
# and position is flake sequence

## interior platform angle
# use linear regression to investigate
# relationship between interior platform angle
# and position of flake in removal sequence
library(plyr)
# Break up dat by core_id, then fit the 
# specified model to each piece and
# return a list
models <- dlply(dat, "core_id ", function(df) 
  lm(Int.Plat.Angle ~ flake_seq, data=df)   )

# Print the summary of each model
l_ply(models, summary, .print = TRUE)

# Apply coef to each model and return a data frame
ldply(models, coef)

# Apply p-values to each model and return a data frame
tmp <- ldply(models, function(i) summary(i)$coef[, "Pr(>|t|)"][2])
# subset where p < 0.05
tmp <- tmp[ tmp$flake_seq < 0.05, ]
# subset models where p < 0.05
models_p <- models[na.omit(tmp$core_id)]

# plot models where p < 0.05
lapply(models_p, function(i) plot(i$fitted) )

# result:
# We get 6 cores with linear model where p < 0.05
# all six cores show increase of angle with 
# increase in position in flake sequence 


## percentage of flake dorsal cortex
# use linear regression to investigate
# relationship between  percentage of flake dorsal cortex
# and position of flake in removal sequence
library(plyr)
# Break up dat by core_id, then fit the 
# specified model to each piece and
# return a list
models <- dlply(dat, "core_id ", function(df) 
  lm(Dorsal.Cortex....~ flake_seq, data=df)   )

# Print the summary of each model
l_ply(models, summary, .print = TRUE)

# Apply coef to each model and return a data frame
ldply(models, coef)

# Apply p-values to each model and return a data frame
tmp <- ldply(models, function(i) summary(i)$coef[, "Pr(>|t|)"][2])
# subset where p < 0.05
tmp <- tmp[ tmp$flake_seq < 0.05, ]
# subset models where p < 0.05
models_p <- models[na.omit(tmp$core_id)]

# plot models where p < 0.05
windows()
par(ask = TRUE)
lapply(models_p, function(i) plot(i$fitted) )
dev.off()

# result:
# We get 16 cores with linear model where p < 0.05
# all 16 cores show decrease of dorsal cortext % with 
# increase in position in flake sequence 

## mean number of flake scars on the dorsal surface
# use linear regression to investigate
# relationship between  percentage of flake dorsal cortex
# and position of flake in removal sequence
library(plyr)
# Break up dat by core_id, then fit the 
# specified model to each piece and
# return a list
models <- dlply(dat, "core_id ", function(df) 
  lm(Dorsal.Scars ~ flake_seq, data=df)   )

# Print the summary of each model
l_ply(models, summary, .print = TRUE)

# Apply coef to each model and return a data frame
ldply(models, coef)

# Apply p-values to each model and return a data frame
tmp <- ldply(models, function(i) summary(i)$coef[, "Pr(>|t|)"][2])
# subset where p < 0.05
tmp <- tmp[ tmp$flake_seq < 0.05, ]
# subset models where p < 0.05
models_p <- models[na.omit(tmp$core_id)]

# plot models where p < 0.05
windows()
par(ask = TRUE)
lapply(models_p, function(i) plot(i$fitted) )
dev.off()

# result:
# We get 9 cores with linear model where p < 0.05
# all 9 cores show increase of dorsal flake scars with 
# increase in position in flake sequence




dorsal cortex location

