library(ggplot2)

args <- commandArgs(trailingOnly = TRUE)

# if (length(args) != 2){
#   stop("Arguments: in out")
# } else {
#   png.out <- args[2]
# }

# pricing <- read.csv("~/scratch/AWS-estimator/last-week/pricing.txt", sep="")
# pricing <- read.csv("~/scratch/AWS-estimator/last-week/pricing_sunstart.txt",
#                     sep="")

# pricing <- read.csv(args[1], sep="")
pricing <- read.table("~/scratch/AWS-estimator/last-week/pricing_start1023_usonly.txt",
                      header=TRUE)

ptx.favorable <- 7.754926 * 2

pricing$iXz <- as.factor(paste(pricing$instance, pricing$zone, sep="."))

price.per <- function(df, col)
{
  if (! col %in% colnames(df))
    stop(paste(col, "not in column names."))

  num <-  NA

  if (grepl("m4.large", col))
    num <- 2 # n vCPU
  else if (grepl("m4.xlarge", col))
    num <- 4
  else if (grepl("m4.2xlarge", col))
    num <- 8
  else if (grepl("m4.4xlarge", col))
    num <- 16
  else if (grepl("m4.10xlarge", col))
    num <- 40
  else if (grepl("c4.large", col))
    num <- 2
  else if (grepl("c4.xlarge", col))
    num <- 4
  else if (grepl("c4.2xlarge", col))
    num <- 8
  else if (grepl("c4.4xlarge", col))
    num <- 16
  else if (grepl("c4.8xlarge", col))
    num <- 36
  else if (grepl("g2.2xlarge", col))
    num <- 1 # n GPU
  else if (grepl("g2.8xlarge", col))
    num <- 4

  return(df[, col] / num)
}

prices.by.hour <- data.frame(matrix(ncol=length(levels(pricing$iXz)),
                                    nrow=168))

colnames(prices.by.hour) <- levels(pricing$iXz)

n <- 0
for (f in levels(pricing$iXz))
{
  for (h in 0:167)
  {
    price <- mean(pricing[pricing$iXz==f & pricing$hour==h, ]$price)
    prices.by.hour[h + 1, f] <- price
    n = n + 1
    print(n / (length(levels(pricing$iXz)) * 168))
  }
}

# just memory/compute
mc.pbh <- prices.by.hour[, grepl("^[mc]4", colnames(prices.by.hour))]

mc.pbh.adj <- mc.pbh
for (col in colnames(mc.pbh))
{
  mc.pbh.adj[, col] <- price.per(mc.pbh, col)
}

mc.min <- apply(mc.pbh.adj, 1, min, na.rm=TRUE)
mc.mean <- apply(mc.pbh.adj, 1, mean, na.rm=TRUE)

# just gpu-enabled
g.pbh <- prices.by.hour[, grepl("^g2", colnames(prices.by.hour))]

g.pbh.adj <- g.pbh
for (col in colnames(g.pbh))
{
  g.pbh.adj[, col] <- price.per(g.pbh, col)
}

g.min <- apply(g.pbh.adj, 1, min, na.rm=TRUE)
g.mean <- apply(g.pbh.adj, 1, mean, na.rm=TRUE)

prices <- data.frame(cbind(mc.min, g.min))
p.mean <- data.frame(cbind(mc.mean, g.mean))

prices$ratio <- prices$g.min / prices$mc.min
prices$good <- prices$ratio < ptx.favorable

p.mean$ratio <- p.mean$g.mean / p.mean$mc.mean
p.mean$good <- p.mean$ratio < ptx.favorable
sum(p.mean$good) #15 = 9%

png.out <- "start1023_usonly-2.png"
png(filename=png.out, width=180, height=90, units="mm", res=300)

ggplot(prices, aes(x=1:168, y=ratio)) +
  geom_line(size=0.8) +
  geom_point(aes(color=prices$good)) +
#  geom_smooth() +
  geom_hline(yintercept=ptx.favorable) +
  labs(title="Ratio of cost per GPU to cost per vCPU, hourly over a week") +
  ylab("GPU/vCPU") +
  xlab("Day") +
  scale_x_continuous(breaks=c(24*0:7),
                     limits=c(0, 168),
                     labels=c("S", "M", "T", "W", "Th", "F", "Sa", "S")) +
  theme(legend.position="none")

dev.off()

# 111 unfavorable, 57 favorable
sum(prices$ratio >= ptx.favorable) / length(prices$ratio) # 0.6607143

# longest run of T

favorable <- prices$ratio < ptx.favorable

lengths <- c()
n <- 0
for (i in 1:(length(favorable) - 1))
{
  if (favorable[i]==favorable[i + 1])
    n <- n + 1
  else
  {
    lengths <- append(lengths, n + 1)
    n <- 0
  }
}
