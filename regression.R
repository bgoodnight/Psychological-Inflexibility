# Read in Mplus tab-delimited data file
data <- read.table(file = "data.dat", sep="\t", header=T)

#Add column names for variables
colnames(data) <- c("Age", "Gender", "SexOr", 
                    "MAAS01", "MAAS02", "MAAS03", "MAAS04", "MAAS05", "MAAS06", "MAAS07","MAAS08", 
                    "MAAS09", "MAAS10", "MAAS11", "MAAS12", "MAAS13", "MAAS14", "MAAS15", 
                    "GHQ01", "GHQ02", "GHQ03", "GHQ04", "GHQ05", "GHQ06", "GHQ07", "GHQ08", "GHQ09", 
                    "GHQ10", "GHQ11", "GHQ12", 
                    "BSI_01", "BSI_02", "BSI_03", "BSI_04", "BSI_05", "BSI_06", "BSI_07", 
                    "BSI_08", "BSI_09", "BSI_10", "BSI_11", "BSI_12", "BSI_13", "BSI_14", 
                    "BSI_15", "BSI_16", "BSI_17", "BSI_18", 
                    "AAQII01", "AAQII02", "AAQII03", "AAQII04", "AAQII05", "AAQII06", "AAQII07")

#Create vectors of variables used to compute scale scores
MAAS <- c("MAAS01", "MAAS02", "MAAS03", "MAAS04", "MAAS05", "MAAS06", "MAAS07","MAAS08", 
            "MAAS09", "MAAS10", "MAAS11", "MAAS12", "MAAS13", "MAAS14", "MAAS15")
GHQ <- c("GHQ01", "GHQ02", "GHQ03", "GHQ04", "GHQ05", "GHQ06", "GHQ07", "GHQ08", "GHQ09", 
         "GHQ10", "GHQ11", "GHQ12")
SOM <- c("BSI_01", "BSI_04", "BSI_07", "BSI_10", "BSI_13", "BSI_16")
DEP <- c("BSI_02", "BSI_05", "BSI_08", "BSI_11", "BSI_14", "BSI_17")
ANX <- c("BSI_03", "BSI_06", "BSI_09", "BSI_12", "BSI_15", "BSI_18")
AAQII <- c("AAQII01", "AAQII02", "AAQII03", "AAQII04", "AAQII05", "AAQII06", "AAQII07")

#Create new variables of scale scores of all variables listed in vectors
data$MAAS <- rowMeans(data[,MAAS])
data$GHQ <- rowMeans(data[,GHQ])
data$SOM <- rowMeans(data[,SOM])
data$DEP <- rowMeans(data[,DEP])
data$ANX <- rowMeans(data[,ANX])
data$AAQII <- rowMeans(data[,AAQII])

#Plot variables to visually inspect for normality
#library("ggplot2")
#qplot(x = MAAS, data = data)
#qplot(x = GHQ, data = data)
#qplot(x = SOM, data = data)
#qplot(x = DEP, data = data)
#qplot(x = ANX, data = data)
#qplot(x = AAQII, data = data)

#Conduct path analysis
library("lavaan")

model <- '
  # regressions
    GHQ + SOM + DEP + ANX ~ AAQII + MAAS + Age + Gender + SexOr
  # residual correlations
    SOM ~~ GHQ
    DEP ~~ GHQ + SOM
    ANX ~~ GHQ + SOM + DEP
    AAQII ~~ MAAS
'

fit <- sem(model, data=data)
summary(fit)
