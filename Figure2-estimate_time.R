library(ggplot2)
library(reshape2)

n <- 1:1000

############################################################################
# Freesurfer - 6 hours
time <- 6 

desktop <- ceiling(n / 4) * time # 4 cores - like a dell (desktop)
workstation <- ceiling(n / 24) * time # 24 cores - a neuron (workstation)
cluster <- ceiling(n / 240) * time # 240 cores - 10 neurons (cluster)

#join data
plotter <- data.frame(cbind(n, desktop, workstation, cluster))
colnames(plotter) <- c("n", "Desktop", "Workstation", "Cluster")
melt.plotter <- melt(plotter, "n")

# set line intervals
time.1 <- 7 * 24 # one week
time.2 <- 30 * 24 # one month
time.3 <- time.2 * 2 # two months

# text corresponding to time.[123]
y.labels <- c("A week", "A month", "Two months")

# open print device
png("Figure2_freesurfer_cluster_times.png", width=1200, height=1200)

ggplot(melt.plotter, aes(n, value, color=variable)) + 
  geom_line() + 
  labs(x="# Subjects") +
  theme(axis.title.y=element_blank(),
        legend.position="bottom",
        legend.title=element_blank(),
        text=element_text(size=18)) + 
  scale_y_continuous(breaks=c(time.1, time.2, time.3),
                     labels=y.labels) +
  geom_hline(aes(yintercept=time.1)) +      # 1 day
  geom_hline(aes(yintercept=time.2)) +  # 
  geom_hline(aes(yintercept=time.3)) 

dev.off()

###########################################################################
# whole brain tractography of HCP data - 36 hours, but fully parallelizable
time <- 36

desktop <- (n / 4) * time # 4 cores - like a dell (desktop)
workstation <- (n / 24) * time # 24 cores - a neuron (workstation)
cluster <- (n / 240) * time # 240 cores - 10 neurons (cluster)

#join data
plotter <- data.frame(cbind(n, desktop, workstation, cluster))
colnames(plotter) <- c("n", "Desktop", "Workstation", "Cluster")
melt.plotter <- melt(plotter, "n")

# set line intervals
time.1 <- 7 * 24 # one week
time.2 <- 30 * 24 *2 # two months
time.3 <- 30* 24 * 6 # six months
time.4 <- 30* 24 * 12 # a year

# text corresponding to time.[123]
y.labels <- c("A week", "A month", "Six months", "One year")

# open print device
png("Figure2_wholebraintractography_cluster_times.png", width=1200, height=1200)

ggplot(melt.plotter, aes(n, value, color=variable)) + 
  geom_line() + 
  labs(x="# Subjects") +
  theme(axis.title.y=element_blank(),
        legend.position="bottom",
        legend.title=element_blank(),
        text=element_text(size=18)) + 
  scale_y_continuous(breaks=c(time.1, time.2, time.3, time.4),
                     labels=y.labels) +
  geom_hline(aes(yintercept=time.1)) + 
  geom_hline(aes(yintercept=time.2)) + 
      geom_hline(aes(yintercept=time.3)) +
  geom_hline(aes(yintercept=time.4))

dev.off()
