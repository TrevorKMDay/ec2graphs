###########
## SETUP ##
###########

setwd("/mnt/adrc/ec2/R")

compare.times <- data.frame(matrix(NA, ncol=7, nrow=6))
colnames(compare.times) <- c("neuron", "aws", "C4", "M4", "ratio.all", 
                             "ratio.C4", "ratio.M4")
rownames(compare.times) <- c("fs", "bpx", "ptx", "npt", "fsh1", "fsh2")

###############
## FUNCTIONS ##
###############

# Match a list of instances to which ones are neuron-class: adrc, panuc, tpp
neuron <- function(df)
{
  v <- df$instance
  is.neuron <- ifelse(v=="adrc", TRUE, 
                      ifelse(v=="panuc", TRUE,
                             ifelse(v=="tpp", TRUE, 
                                    FALSE)))
  
  return(df[is.neuron, ])
}

# Anything that isn't neuron-class has to be AWS
aws <- function(df)
{
  v <- df$instance
  is.aws <- grepl("^[CMG][24]", v)
  
  return(df[is.aws, ])
}

# Get one class only
x4 <- function(df, str)
{
  regex <- paste0("^", str)
  return(df[grepl(regex, df$instance), ])
}


# Get number of hours from real column (in sec)
get.hours <- function(df)
{
  return(mean(df$real) / 3600)
}

##############
## GET DATA ##
##############

# FREESURFER
fs <- read.csv("final-data/freesurfer_AWSADRC_SD.csv")
fs.same <- fs[fs$sameness=="same", ]

compare.times["fs", "neuron"] <- get.hours(neuron(fs.same))
compare.times["fs", "aws"] <- get.hours(aws(fs.same))
compare.times["fs", "C4"] <- get.hours(x4(fs.same, "C4"))
compare.times["fs", "M4"] <- get.hours(x4(fs.same, "M4"))


# BEDPOSTX
bpx <- read.csv("final-data/bedpostx_AWSADRC_SD.csv")
bpx.same <- bpx[bpx$sameness=="same", ]

compare.times["bpx", "neuron"] <- get.hours(neuron(bpx.same))
compare.times["bpx", "aws"] <- get.hours(x4(bpx.same, "G2.2xlarge"))


# PROBTRACKX
ptx <- read.csv("final-data/probtrackx_AWSADRC_S.csv")
# ptx is same only

compare.times["ptx", "neuron"] <- get.hours(neuron(ptx))
compare.times["ptx", "aws"] <- get.hours(x4(ptx, "G2.2xlarge"))


# NEUROPOINT
npt <- read.csv("final-data/neuropoint_AWSADRC_SD.csv")
npt.same <- npt[npt$sameness=="same", ]

compare.times["npt", "neuron"] <- get.hours(neuron(npt.same))
compare.times["npt", "aws"] <- get.hours(aws(npt.same))
compare.times["npt", "C4"] <- get.hours(x4(npt.same, "C4"))
compare.times["npt", "M4"] <- get.hours(x4(npt.same, "M4"))

# FREESURFER HIRES, I
fsh1 <- read.csv("final-data/freesurfer.hires.I_AWSADRC_sD.csv")
# can't do same only: no same AWS

compare.times["fsh1", "neuron"] <- get.hours(neuron(fsh1))
compare.times["fsh1", "aws"] <- get.hours(aws(fsh1))
compare.times["fsh1", "C4"] <- get.hours(x4(fsh1, "C4"))
compare.times["fsh1", "M4"] <- get.hours(x4(fsh1, "M4"))


# FREESURFER HIRES, II

fsh2.aws.same <- read.csv("final-data/freesurfer.hires.II_AWSonly_S.csv")

fsh2.aws.diff <- read.csv("final-data/freesurfer.hires.II_AWSonly_D.csv")
fsh2.aws.diff.A <- fsh2.aws.diff[fsh2.aws.diff$part=="A", ]
fsh2.aws.diff.B <- fsh2.aws.diff[fsh2.aws.diff$part=="B", ]

compare.times["fsh2", "aws"] <- get.hours(fsh2.aws.diff.A) + 
  get.hours(fsh2.aws.diff.B)

compare.times["fsh2", "C4"] <- get.hours(x4(fsh2.aws.diff.A, "C4")) + 
  get.hours(x4(fsh2.aws.diff.B, "C4"))
compare.times["fsh2", "M4"] <- get.hours(x4(fsh2.aws.diff.A, "M4")) + 
  get.hours(x4(fsh2.aws.diff.B, "M4"))

fsh2.neuron <- read.csv("final-data/freesurfer.hires.II_ADRConly_SD.csv")

compare.times["fsh2", "neuron"] <- get.hours(fsh2.neuron)

######################
## CALCULATE RATIOS ##
######################

compare.times$ratio.all <- compare.times$neuron / compare.times$aws
compare.times$ratio.C4 <- compare.times$neuron / compare.times$C4
compare.times$ratio.M4 <- compare.times$neuron / compare.times$M4

###################
## WRITE TO FILE ##
###################

# round to the same number of sigfigs as original hours: about 7
ct.r <- signif(compare.times, 7)
write.csv(ct.r, file="final-data/adrc-aws_ratio.csv", quote=FALSE)
