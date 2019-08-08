rm(list = ls())
library(dplyr)
library(plyr)
library(yaml)
library(tidyLPA)
lookup <- yaml::read_yaml("P:\\ECHO Deliverables\\inattentive-responses-adhd\\file lookup.yaml")


## Establish functions
normalize <- function(x) {
  sd = sd(x, na.rm = T)
  mean = mean(x, na.rm = T)
  x <- (x - mean) / sd
  return(x)
}


## Read data
academic <- read.csv(lookup$raw_pathways$academic, stringsAsFactors = F)
child <- read.csv(lookup$raw_pathways$child, stringsAsFactors = F)
parent <- read.csv(lookup$raw_pathways$parent, stringsAsFactors = F)
PHP <- read.csv(lookup$raw_pathways$PHP, stringsAsFactors = F) 



## Merge
#prepare matching IDs
academic$childID <- paste0(substr(academic$childid, 1, 1), "-", 
                           sprintf("%02d", academic$schoolid), "-",
                           substr(academic$childid, 2, 5), "-1")
child$childID <- child$ID
parent$childID <- paste0(substr(parent$childid, 1, 1), "-", 
                         sprintf("%02d", parent$schoolid), "-",
                         substr(parent$childid, 2, 5), "-1")
PHP$childID <- PHP$ID

#since `child` already contains all variables in PHP save for calculated data quality scores, just save those scores and the ID
PHP <- dplyr::select(PHP, childID, evenOdd:polyGuttmanErrors.byScale.unidirectional)

#remove ID variables no longer used
academic <- dplyr::select(academic, -schoolid, -childid)
parent <- dplyr::select(parent, -schoolid, -childid)

#remove duplicates
child <- child[!duplicated(child$childID),]

pathways <- plyr::join_all(list(academic, child, parent, PHP), by = "childID", type = "inner")


## Select variables for analysis
df <- pathways %>%
  dplyr::mutate(
    age = AGE + 6,
    GPA = rowMeans(dplyr::select(., ENQ1Y3:SSQ4Y3), na.rm = T),
    LD = HE23P == 3,
    ADHD = HE24P == 3,
    SHCN = (HE1P == 1 & HE2P == 1 & HE3P == 1) | (HE4P == 1 & HE5P == 1 & HE6P == 1) | (HE7P == 1 & HE8P == 1 & HE9P == 1) | (HE10P == 1 & HE11P == 1 & HE12P == 1) | (HE13P == 1 & HE14P == 1), #Any special healthcare needs
    CHC = HE19P == 3 | HE20P == 3 | HE21P == 3 | HE23P == 3 | HE24P == 3 #Any chronic health condition
  ) %>%
  dplyr::select(
    age,
    lang = std_LA, math = std_M, daysAbsent = absent, GPA,
    LD, ADHD, SHCN, CHC,
    LS = longstring,
    IR = resampledConsistency.unidirectional,
    PS = psychSyn.unidirectional,
    SD = interItemSD.unidirectional,
    RS = reversedItemDifference.unidirectional,
    PT = personTotalCor.unidirectional,
    MD = mahalanobisDist.unidirectional,
    GE = polyGuttmanErrors.byScale.unidirectional
  ) %>%
  dplyr::mutate_at(.vars = c("LS", "IR", "PS", "SD", "RS", "PT", "MD", "GE"), .fun = normalize)

df <- as.data.frame(sapply(df, as.numeric))

#limit to complete cases only
df <- na.omit(df)


## Latent profile analysis - need to align to Katherine
LPA <- tidyLPA::estimate_profiles(dplyr::select(df, LS:GE), 3)
plot_profiles(LPA, rawdata = F, sd = F)

df$class <- LPA$model_1_class_3$dff$Class


## Export clean data with LPA classes
save(df, file = lookup$analysis_ready)