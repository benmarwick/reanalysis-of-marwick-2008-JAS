Reanalysis of data from Marwick (2008)
====

Here I reanalyse data that was first presented in:

Marwick, B. 2008. What attributes are important for the measurement of assemblage reduction intensity? Results from an experimental stone artefact assemblage with relevance to the Hoabinhian of mainland Southeast Asia. Journal of Archaeological Science 35(5): 1189-1200 http://dx.doi.org/10.1016/j.jas.2007.08.007

The key difference between this analysis and the 2008 analysis is that in this analysis I consider each flakes from each core as a separate unit of analysis. In the 2008 paper I grouped all flakes from all cores into a single large group, now I want to see if changing the unit of analysis gives different results.

In brief, I'm looking to answer these questions within each group of flakes from a single core:

1. Does the frequency of overhang removal on flakes increase towards the latter part of the flake removal sequence?
2. Does the interior platform angle of flake increase towards the latter part of the flake removal sequence?
3. Does the percentage of flake dorsal cortex per flake decrease towards the latter part of the flake removal sequence? percentage of flake dorsal cortex?
4. Does the number of flake dorsal flake scars per flake increase towards the latter part of the flake removal sequence? percentage of flake dorsal cortex?

This document comes from https://github.com/benmarwick/reanalysis-of-marwick-2008-JAS and the data for the analysis is also downloadable from that location as a CSV file. All of the code for loading, processing, analysing and visualising the data is contained in this document.

First step is loading the data.

```{r, echo=TRUE, message=FALSE}
### load data
# where is the data file?
wd <- "C:/Users/marwick/Documents/GitHub/reanalysis-of-marwick-2008-JAS"
setwd(wd)
dat <- read.csv("Marwick_2008_JAS_data.csv")
```

Second is preparing the data for analysis. 

```{r, echo=TRUE, message=FALSE, warning=FALSE}
### prepare data
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
```

Third is the analysis, starting with overhang removal. Overhang removal is a presence/absence variable, so I computed logistic regression models for each core, then plotted the fitted values for the models where p < 0.1 (since no models had p < 0.05). Of the seven cores that have p < 0.1, six show that overhang removal tends to be more frequent at later parts of the flake removal sequence (core AG is the odd one out). 

```{r, echo=TRUE, message=FALSE, warning=FALSE, fig.cap="Overhang removal" }
## overhang removal
# recode OHR as 0 or 1
# unique(dat$Overhang.removal)
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
# Apply p-values to each model and return a data frame
tmp <- ldply(models, function(i) summary(i)$coef[, "Pr(>|z|)"][2])
# subset where p < 0.05
tmp1 <- tmp[ tmp$flake_seq < 0.5, ]
# none! so subset where p < 0.1
tmp2 <- tmp[ tmp$flake_seq < 0.5, ]
models_p <- models[na.omit(tmp2$core_id)]
# plot models where p < 0.1
fit <- lapply(models_p, function(i) data.frame(i$fitted) )
require(ggplot2)
require(reshape2)
h.melt <- melt(fit)
# add sequence numbers
h.melt$seq <- unlist(sapply(rle(h.melt$L1)$lengths, seq))
# plot
ggplot(h.melt,aes(x=value,y=seq))+
  geom_point() +
  facet_grid(.~L1) + 
  theme_grey(base_size = 10) 
```
Next, for interior platform angle I computed linear models for each core. For the models where p < 0.05 I plotted the fitted values. The plots show that six cores give models where p < 0.05. All six cores show that interior platform angle tends to increase with the position of a flake in the removal sequence. 
```{r,  echo=TRUE, message=FALSE, warning=FALSE, fig.cap="Interior Platform Angle"}
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
# Apply p-values to each model and return a data frame
tmp <- ldply(models, function(i) summary(i)$coef[, "Pr(>|t|)"][2])
# subset where p < 0.05
tmp <- tmp[ tmp$flake_seq < 0.05, ]
# subset models where p < 0.05
models_p <- models[na.omit(tmp$core_id)]
# plot models where p < 0.05
fit <- lapply(models_p, function(i) data.frame(i$fitted) )
require(ggplot2)
require(reshape2)
h.melt <- melt(fit)
# add sequence numbers
h.melt$seq <- unlist(sapply(rle(h.melt$L1)$lengths, seq))
# plot
ggplot(h.melt,aes(x=value,y=seq))+
  geom_point() +
  facet_grid(.~L1) + 
  theme_grey(base_size = 10)  
```

Next is flake dorsal cortex. I used the same method as for interior platform angle. The result is 16 cores, all showing that dorsal cortex percentages tend to decrease along the sequence of flake removal. 


```{r, echo=TRUE, message=FALSE, warning=FALSE, fig.cap="Flake Dorsal Cortex"}
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
# Apply p-values to each model and return a data frame
tmp <- ldply(models, function(i) summary(i)$coef[, "Pr(>|t|)"][2])
# subset where p < 0.05
tmp <- tmp[ tmp$flake_seq < 0.05, ]
# subset models where p < 0.05
models_p <- models[na.omit(tmp$core_id)]
fit <- lapply(models_p, function(i) data.frame(i$fitted) )
require(ggplot2)
require(reshape2)
h.melt <- melt(fit)
# add sequence numbers
h.melt$seq <- unlist(sapply(rle(h.melt$L1)$lengths, seq))
# plot models where p < 0.05
ggplot(h.melt,aes(x=value,y=seq))+
  geom_point() +
  facet_grid(.~L1) + 
  theme_grey(base_size = 5)  
```

Next is the number of flake scars, the method is the same as for the previous variable. The result is nine cores that have models where p < 0.05 and they all indicate that the number of flake scars per flake tends to increase as the position along the removal sequence increases.

```{r, echo=TRUE, message=FALSE, warning=FALSE, fig.cap="Number of Flake Scars"}
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
# Apply p-values to each model and return a data frame
tmp <- ldply(models, function(i) summary(i)$coef[, "Pr(>|t|)"][2])
# subset where p < 0.05
tmp <- tmp[ tmp$flake_seq < 0.05, ]
# subset models where p < 0.05
models_p <- models[na.omit(tmp$core_id)]
fit <- lapply(models_p, function(i) data.frame(i$fitted) )
require(ggplot2)
require(reshape2)
h.melt <- melt(fit)
# add sequence numbers
h.melt$seq <- unlist(sapply(rle(h.melt$L1)$lengths, seq))
# plot models where p < 0.05
ggplot(h.melt,aes(x=value,y=seq))+
  geom_point() +
  facet_grid(.~L1) + 
  theme_grey(base_size = 10)
```

To conclude, this reanalysis has examined some of the key variables of flake manufacture to see how they vary along the sequence of core reduction. The difference between this anlysis and the 2008 analysis is that this analysis has considered the flakes from each core as a discrete unit of analysis. The 2008 analysis grouped all the flakes from all the cores together. The results of this reanalysis support the conclusions of the 2008 paper by showing that overhang removal, interior platform angle, percentage of flake dorsal cortex and the number of flake dorsal flake scars vary predictably along the flake removal sequence. 

```{r, echo=TRUE, message=FALSE, eval=FALSE}
# This chunck is to run the code and generate the PDF
# Load packages
setwd(wd) # assumes wd has been set earlier in the doc
require(knitr)
require(markdown)
 
# process .md and .pdf files (including smart punctuation and grey background of code blocks)
filen <- "Marwick_2008_JAS_code" # name of this markdown file without suffix
knit(paste0(filen,".md"))
system(paste0("pandoc -s ", paste0(filen,"-out.md"), " -t latex -o ", paste0(filen,".pdf"), " --highlight-style=tango  -S"))
```
Ben Marwick, August 2013.