library(psych)
library(ggplot2)
library(doBy)
library(stats)
library(data.table)
library(plyr)
#library(RColorBrewer)

#### SETUP ####

setwd("/mnt/adrc/ec2/R")

# If you reorder these make sure to change myColors to match
instances <- factor(levels = c("workstation",
                          "C4.large", "C4.xlarge", "C4.4xlarge", "C4.8xlarge", 
                          "M4.large", "M4.xlarge", "M4.4xlarge", "G2.2xlarge"),
                    ordered=TRUE)
order.large <- as.factor(levels(instances))

# COLORS
myColors <- c("#69CCC5", 
              "#FEE0B6", "#FDB863", "#E08214", "#B35806", 
              "#D8DAEB", "#B2ABD2", "#8073AC", "#A6DBA0")
names(myColors) <- order.large
colScale <- scale_color_manual(name="instance", values = myColors)

pie(rep(1, length(myColors)), col=myColors, label=names(myColors))

#### FUNCTIONS ####

size.order <- function(df)
{
  order.instances <- as.factor(as.character(
    order.large[sort(match(as.factor(df$instance), order.large))]
    ))
  
  # as.factor(as.character(y[sort(match(x, y))]))
  temp <- df[order.instances, ]
  temp$order <- 1:dim(temp)[1]
  
  return(temp)
}

summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE, 
                      conf.interval=.95) 
{
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # Collapse the data
  formula <- as.formula(paste(measurevar, paste(groupvars, collapse=" + "), 
                              sep=" ~ "))
  datac <- summaryBy(formula, data=data, FUN=c(length2,mean,sd), na.rm=na.rm)
  
  # Rename columns
  names(datac)[names(datac)==paste(measurevar, ".mean", sep="")] <- measurevar
  names(datac)[names(datac)==paste(measurevar, ".sd", sep="")] <- "sd"
  names(datac)[names(datac)==paste(measurevar, ".length2", sep="")] <- "N"
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(as.data.frame(datac))
}

plot.x <- function(data.set, title, x.label, y.label, y.lim)
{
  ordered.data.set <- size.order(data.set)
  
  plot <- ggplot(ordered.data.set, aes(x=order, y=price.per, fill=instance)) +
    geom_bar(position=position_dodge(), stat="identity") +
    geom_errorbar(aes(ymin=price.per-price.ci, ymax=price.per+price.ci, 
                      width=0.2)) +
    ggtitle(title) +
    ylab(y.label) + 
    xlab(x.label) +
    ylim(y.lim) + 
    theme(text = element_text(size=35),
          legend.position = "none") +
    scale_fill_manual(values = myColors) + 
    xlim(labels = as.character(ordered.data.set$instance))
  
  return(plot)
}

get.price <- function(S)
{
  if (S == "C4.large")
    return(0.105)
  else if (S == "C4.xlarge")
    return(0.209)
  else if (S == "C4.4xlarge")
    return(0.838)
  else if (S == "C4.8xlarge")
    return(1.675)
  else if (S == "M4.large")
    return(0.120)
  else if (S == "M4.xlarge")
    return(0.239)
  else if (S == "M4.4xlarge")
    return(0.958)
  else 
    return(NA)
}

get.prices <- function(v)
{
  out <- rep(NA, length(v))
  
  for (i in 1:length(v))
  {
    out[i] <- get.price(v[i])
  }
  
  return(out)
}

get.cpus <- function(v)
{
  out <- rep(NA, length(v))
  
  cpus <- NA
  for (i in 1:length(v))
  {
    if (v[i] == "C4.large")
      cpus <- 2
    else if (v[i] == "C4.xlarge")
      cpus <- 4
    else if (v[i] == "C4.4xlarge")
      cpus <- 16
    else if (v[i] == "C4.8xlarge")
      cpus <- 36
    else if (v[i] == "M4.large")
      cpus <- 2
    else if (v[i] == "M4.xlarge")
      cpus <- 4
    else if (v[i] == "M4.4xlarge")
      cpus <- 16
    
    out[i] <- cpus
  }
  
  return(out)
}


price.summary <- function(summary)
{
  temp <- summary
  
  temp$hours <- temp$real / 3600
  temp$price.per <- temp$hours * get.prices(temp$instance) / temp$ncpus
  temp$hours.ci <- temp$ci / 3600
  temp$price.ci <- temp$hours.ci * get.prices(temp$instance) / temp$ncpus
  
  return(temp)
}

my.png <- function(filename)
{
  png(filename, width=1200, height=1200)
}

#### FREESURFER ####
freesurfer <- read.csv("final-data/freesurfer_AWSADRC_SD.csv")
freesurfer.summary <- summarySE(freesurfer, measurevar="real", 
                               groupvars=c("instance", "sameness"))
freesurfer.summary$ncpus <- get.cpus(freesurfer.summary$instance)
freesurfer.costs <- price.summary(freesurfer.summary)

# compare times, same only
freesurfer.costs.aws <- freesurfer.costs[grepl("^[MC]4.", 
                                               freesurfer.costs$instance), ]

freesurfer.costs.aws.same <- freesurfer.costs.aws[
  freesurfer.costs.aws$sameness=="same", ]
freesurfer.costs.aws.diff <- freesurfer.costs.aws[
  freesurfer.costs.aws$sameness=="different", ]

# plot
my.png("figures/freesurfer_same.png")
plot.x(freesurfer.costs.aws.same, "Freesurfer (same)", x.label="Instance Type",
       y.label="Price ($)", y.lim=c(0, 1.1))
dev.off()

my.png("figures/freesurfer_diff.png")
plot.x(freesurfer.costs.aws.diff, "Freesurfer (different)", 
       x.label="Instance Type", y.label="Price ($)", y.lim=c(0, 1.1))
dev.off()

## FREESURFER HALF 
half <- read.csv("final-data/freesurfer-half_AWSonly_S.csv")
half.summary <- summarySE(half, measurevar="real", 
                                groupvars=c("instance", "sameness"))
half.summary$ncpus <- get.cpus(half.summary$instance)
half.costs <- price.summary(half.summary)
half.costs[is.na(half.costs)] <- 0

# because we only ran half, they cost more
half.costs$price.per <- 2 * half.costs$price.per

my.png("figures/freesurfer_half_same.png")
plot.x(half.costs, "Half-capacity (same)", x.label="Instance Type", 
       y.label="Price ($)", y.lim=c(0, 1.1))
dev.off()

#### TRACTOGRAPHY ####

plot.t <- function(data.set, title, x.label, y.label, y.lim)
{
  ordered.data.set <- size.order(data.set)
  
  plot <- ggplot(ordered.data.set, aes(x=order, y=hours, fill=instance)) +
    geom_bar(position=position_dodge(), stat="identity") +
    geom_errorbar(aes(ymin=hours-hours.ci, ymax=hours+hours.ci, 
                      width=0.2)) +
    ggtitle(title) +
    xlab(x.label) +
    ylab(y.label) +
    ylim(y.lim) + 
    theme(text = element_text(size=35),
          legend.position = "none") +
    scale_fill_manual(values = myColors) + 
    xlim(labels = as.character(ordered.data.set$instance))
  
  return(plot)
}

price.per.gpu <- data.frame(matrix(NA, ncol=2, nrow=2))
colnames(price.per.gpu) <- c("bpx", "ptx")
rownames(price.per.gpu) <- c("g2.2xl", "g2.8xl")

g2.2xl.price <- 0.650
g2.8xl.price <- 2.600

### BEDPOSTX 
bpx <- read.csv("final-data/bedpostx_AWSADRC_SD.csv")

bpx <- bpx[bpx$instance!="G2.8xlarge", ]

bpx.summary <- summarySE(bpx, measurevar="real", groupvars=c("instance", 
                                                             "sameness"))
bpx.summary$hours <- bpx.summary$real / 3600
bpx.summary$hours.ci <- bpx.summary$ci / 3600

# set the column name for adrc appropriately
bpx.summary$instance <- as.factor(rep(c("workstation", "G2.2xlarge"), each=2))

bpx.summary.same <- bpx.summary[bpx.summary$sameness=="same", ]
bpx.summary.diff <- bpx.summary[bpx.summary$sameness=="different", ]

my.png("figures/bedpostx_same.png")
plot.t(bpx.summary.same, "bedpostx (same)", x.label="Instance Type", 
       y.label="Time (h)", y.lim=c(0, 51))
dev.off()

my.png("figures/bedpostx_diff.png")
plot.t(bpx.summary.diff, "bedpostx (different)", x.label="Instance Type", 
       y.label="Time (h)", y.lim=c(0, 51))
dev.off()

bpx.adrc <- mean(bpx[bpx$instance=="adrc", "real"])
bpx.g2 <- mean(bpx[bpx$instance!="adrc", "real"]) 
bpx.speedup <- bpx.adrc / bpx.g2 ## 80.9268

price.per.gpu$bpx[1] <- mean(bpx[bpx$instance=="G2.2xlarge", "real"]) / 3600 * 
  g2.2xl.price
price.per.gpu$bpx[2] <- mean(bpx[bpx$instance=="G2.8xlarge", "real"]) / 3600 * 
  g2.2xl.price / 4


### PROBTRACKX
ptx <- read.csv("final-data/probtrackx_AWSADRC_S.csv")
ptx.summary <- summarySE(ptx, measurevar="real", groupvars="instance")
ptx.summary$hours <- ptx.summary$real / 3600
ptx.summary$hours.ci <- ptx.summary$ci / 3600

# set the column name for adrc appropriately
ptx.summary$instance <- c("workstation", "G2.2xlarge")

my.png("figures/probtrackx.png")
plot.t(ptx.summary, "probtrackx", x.label="Instance Type", y.label="Time (h)", 
       y.lim=c(0, 25))
dev.off()

ptx.adrc <- mean(ptx[ptx$instance=="adrc", "real"])
ptx.g2 <- mean(ptx[ptx$instance!="adrc", "real"])
ptx.speedup <- ptx.adrc / ptx.g2 # 7.7454926

price.per.gpu$ptx[1] <- mean(ptx[ptx$instance=="G2.2xlarge", "real"]) / 3600 * 
  g2.2xl.price

#### NEUROPOINT ####
npt <- read.csv("final-data/neuropoint_AWSADRC_SD.csv")
npt.summary <- summarySE(npt, measurevar="real", 
                         groupvars=c("instance", "sameness"))
npt.summary$hours <- npt.summary$real / 3600
npt.summary$hours.ci <- npt.summary$ci / 3600

npt.summary <- data.frame(npt.summary[npt.summary$instance!="adrc",])
npt.summary$ncpus <- get.cpus(npt.summary$instance)

npt.costs <- price.summary(npt.summary)
npt.costs.same <- npt.costs[npt.costs$sameness=="same", ]
npt.costs.diff <- npt.costs[npt.costs$sameness=="different", ]

my.png("figures/neuropoint_same.png")
plot.x(npt.costs.same, title="Neuropoint (same)", x.label="Instance Type", 
       y.label="Price ($)", y.lim=c(0, 0.05))
dev.off()

my.png("figures/neuropoint_diff.png")
plot.x(npt.costs.diff, title="Neuropoint (different)", x.label="Instance Type", 
       y.label="Price ($)",  y.lim=c(0, 0.05))
dev.off()

#### FREESURFER HIRES ####
## Downsampled
fs.h.dd <- read.csv("final-data/freesurfer.hires.I_AWSADRC_sD.csv")
fs.h.dd.summary <- summarySE(fs.h.dd, measurevar="real", 
                             groupvars=c("instance", "sameness"))
fs.h.dd.summary$hours <- fs.h.dd.summary$real / 3600
fs.h.dd.summary$hours.ci <- fs.h.dd.summary$ci / 3600
fs.h.dd.summary.aws <- fs.h.dd.summary[fs.h.dd.summary$instance!="adrc" & 
                                     fs.h.dd.summary$instance!="panuc", ]
fs.h.dd.summary.aws$ncpus <- get.cpus(fs.h.dd.summary.aws$instance)

fs.h.dd.costs.aws <- price.summary(fs.h.dd.summary.aws)

# Different
my.png("figures/freesurfer-hires-downsampled-diff.png")
plot.x(fs.h.dd.costs.aws, "Freesurfer hires pipeline, downsampled (different)", 
      x.label="Instance Type", y.label="Price ($)", y.lim=c(0, 1.7))
dev.off()

# Same
# FSH same was not executed on AWS instances

## Hires
# Same 
fs.h.sh <- read.csv("final-data/freesurfer.hires.II_AWSonly_S.csv")
fs.h.sh.summary <- summarySE(fs.h.sh, measurevar="real", groupvars="instance")
fs.h.sh.summary$hours <- fs.h.sh.summary$real / 3600
fs.h.sh.summary$hours.ci <- fs.h.sh.summary$ci / 3600
fs.h.sh.summary <- fs.h.sh.summary[fs.h.sh.summary$instance!="adrc",]
fs.h.sh.summary$ncpus <- get.cpus(fs.h.sh.summary$instance)

fs.h.sh.costs <- price.summary(fs.h.sh.summary)

my.png("figures/freesurfer-hires-hires-same.png")
plot.x(fs.h.sh.costs, "Freesurfer hires pipeline, hires (same)",
       x.label="Instance Type", y.label="Price ($)", y.lim=c(0, 3.5))
dev.off()

## different hires
hires <- read.csv("final-data/freesurfer.hires.II_AWSonly_D.csv")
hires.a <- hires[hires$part=="A", ]
hires.b <- hires[hires$part=="B", ]

hires.a.summary <- summarySE(hires.a, measurevar="real", groupvars="instance")
hires.b.summary <- summarySE(hires.b, measurevar="real", groupvars="instance")

hires.summary <- data.frame(hires.a.summary$instance)
colnames(hires.summary) <- "instance"

hires.summary$N <- hires.a.summary$N + hires.b.summary$N
hires.summary$N.adj <- hires.summary$N / 2

hires.summary$real <- hires.a.summary$real + hires.b.summary$real

# to sum the SDs, we take the sum of the variances, and square toot that.
hires.summary$sd <- (hires.a.summary$sd ** 2 + hires.a.summary$sd ** 2) ** 0.5

# calculate the 95% confidence interval from summed CIs
hires.summary$ci <- (hires.a.summary$ci ** 2 + hires.b.summary$ci ** 2) ** 0.5

fs.h.dh.summary <- hires.summary
fs.h.dh.summary$ncpus <- get.cpus(hires.summary$instance)
fs.h.dh.costs <- price.summary(fs.h.dh.summary)

my.png("figures/freesurfer-hires-hires-different.png")
plot.x(fs.h.dh.costs, "Freesurfer hires pipeline, hires (different)",
       x.label="Instance Type", y.label="Price ($)", y.lim=c(0, 3.5))
dev.off()

# ## sum part II
# fsh.diff.II <- data.frame(fs.h.dh.summary$real / 3600, fs.h.dh.summary$N.adj)
# colnames(fsh.diff.II) <- c("hours", "N")
# fsh.same.II <- data.frame(fs.h.sh.summary$real / 3600, fs.h.sh.summary$N)
# colnames(fsh.same.II) <- c("hours", "N")
# 
# fsh.II <- rbind(fsh.diff.II, fsh.same.II)
# fsh.II$weight <- fsh.II$hours * fsh.II$N
# 
# 
# ## same part II
# fs.h.dh.adrc <- read.csv("../freesurfer.hires/adrc_same_II.csv")
# fs.h.dh.a.summary <- summarySE(fs.h.dh.adrc, measurevar="real", 
#                                groupvars="instance")
# fs.h.dh.a.summary$hours <- fs.h.dh.a.summary$real / 3600
# fs.h.dh.a.summary$hours.ci <- fs.h.dh.a.summary$ci / 3600
