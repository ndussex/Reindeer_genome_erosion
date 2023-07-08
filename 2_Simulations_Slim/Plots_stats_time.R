library("RColorBrewer")

#summary across runs for each simulation combination

# idea
# 1. import all runs 

setwd("~/Founder effect/rec_1000/output")
#loading files for each simulation set (total of 6)
K_25_Kbot1800 <- list.files(pattern="Reindeer_K2=25_Kbot=1800")
K_250_Kbot1800 <- list.files(pattern="Reindeer_K2=250_Kbot=1800")
.
.
.
K_XXX_Kbot1800 <- list.files(pattern="Reindeer_K2=XXX_Kbot=1800")


#2. make dataframes for each dataset
df_K_25_Kbot1800 <- data.frame(matrix(nrow = 0, ncol = 12))
for(rep in seq_along(K_25_Kbot1800)){
  data <- read.table(K_25_Kbot1800[rep], sep=",", header=T)
  df_K_25_Kbot1800 <- rbind(df_K_25_Kbot1800,cbind(data,rep))
}

df_K_250_Kbot1800 <- data.frame(matrix(nrow = 0, ncol = 12))
for(rep in seq_along(K_250_Kbot1800)){
  data <- read.table(K_250_Kbot1800[rep], sep=",", header=T)
  df_K_250_Kbot1800 <- rbind(df_K_250_Kbot1800,cbind(data,rep))
}


## get mean of all reps(i.e. file) for each time point
# for temporal figure below (changes in indices through time)

#25
Year <- unique(df_K_25_Kbot1800$Year)
means_K_25_Kbot1800 <- data.frame(matrix(nrow = 0, ncol = 12))

for(Years in Year){
  data_this_Year <- df_K_25_Kbot1800[which(df_K_25_Kbot1800$Year==Years),]
  means_K_25_Kbot1800 <- rbind(means_K_25_Kbot1800,colMeans(data_this_Year))
  
}
colnames(means_K_25_Kbot1800) <- colnames(df_K_25_Kbot1800)

tail(means_K_250_Kbot1800)


#250
Year <- unique(df_K_250_Kbot1800$Year)
means_K_250_Kbot1800 <- data.frame(matrix(nrow = 0, ncol = 12))

for(Years in Year){
  data_this_Year <- df_K_250_Kbot1800[which(df_K_250_Kbot1800$Year==Years),]
  means_K_250_Kbot1800 <- rbind(means_K_250_Kbot1800,colMeans(data_this_Year))
  
}
colnames(means_K_250_Kbot1800) <- colnames(df_K_250_Kbot1800)


## set timing for limits of plots
start <- 500000 
end <- 507000 

####################################################################################################
# 1. plot temporal changes of indices for a given run (Mig rate) to show variation among runs
####################################################################################################

xmin <- start
xmax <- end

#25_1800
pdf(paste("../Plots/Main_K_25_Kbot1800_N_FROH_Load.pdf", sep=""), width=6, height=8)

colors = brewer.pal(n = 8, name = "BrBG")[1:8]
lwd = 1
lab = 1.5
axis=1.2

par(mfrow=c(5,1), mar = c(4.5,5,0.5,0.5), oma=c(2,0,1,0), mai = c(0.05, 1, 0.2, 0.2)) #, bty = "n"

#N
#plot the average for N
plot(means_K_25_Kbot1800$Year, means_K_25_Kbot1800$popSizeP1, type = "l", ylab = "Population size", lwd = 1, cex.lab=lab, xlab = "",xlim = c(xmin+5,xmax),
     #ylim=c((min(means_K_1000_Kbot500$popSizeP1) - 1000),max(means_K_1000_Kbot500$popSizeP1) + 1000), 
     ylim=c(0,51000),
     col="black", xaxt = "n", cex.axis=axis) #ylim=c(0,550)
#plot all runs
for(file in seq_along(K_25_Kbot1800)){
  
  data <- df_K_25_Kbot1800[which(df_K_25_Kbot1800$rep==file),]
  lines(data$Year,data$popSizeP1, col=colors[3])
}
lines(means_K_25_Kbot1800$Year,means_K_25_Kbot1800$popSizeP1, lwd=2, col="black")

abline(v=506800, col="black",lwd=1, lty=3)
abline(v=506925, col="black",lwd=1, lty=3)

#Het
#plot the average 
plot(means_K_25_Kbot1800$Year, means_K_25_Kbot1800$meanHetP1, type = "l", ylab = "Heterozygosity", lwd = 1, cex.lab=lab, xlab = "",xlim = c(xmin+5,xmax),
     ylim=c((min(means_K_25_Kbot1800$meanHetP1)-0.000005),max(means_K_25_Kbot1800$meanHetP1)+0.00001), 
     col="black", xaxt = "n", cex.axis=axis) #ylim=c(0,550)

#plot all runs
for(file in seq_along(K_25_Kbot1800)){
  
  data <- df_K_25_Kbot1800[which(df_K_25_Kbot1800$rep==file),]
  lines(data$Year,data$meanHetP1, col=colors[3])
}
lines(means_K_25_Kbot1800$Year,means_K_25_Kbot1800$meanHetP1, lwd=2, col="black")

abline(v=506800, col="black",lwd=1, lty=3)
abline(v=506925, col="black",lwd=1, lty=3)

#FROH
#plot the average
plot(means_K_25_Kbot1800$Year, means_K_25_Kbot1800$FROHP1, type = "l", ylab = "FROH", lwd = 1, cex.lab=lab, xlab = "",xlim = c(xmin+5,xmax),
     #ylim=c((min(means_K_1000_Kbot500$FROHP1)),max(means_K_1000_Kbot500$FROHP1)), 
     ylim=c(0.0,1),
     col="black", xaxt = "n", cex.axis=axis) #ylim=c(0,550)

#plot all runs
for(file in seq_along(K_25_Kbot1800)){
  
  data <- df_K_25_Kbot1800[which(df_K_25_Kbot1800$rep==file),]
  lines(data$Year,data$FROHP1, col=colors[3])
}
lines(means_K_25_Kbot1800$Year,means_K_25_Kbot1800$FROHP1, lwd=2, col="black")
abline(v=506800, col="black",lwd=1, lty=3)
abline(v=506925, col="black",lwd=1, lty=3)


#FitnessP1 = realised load
#plot the average
plot(means_K_25_Kbot1800$Year, 1-means_K_25_Kbot1800$FitnessP1, type = "l", ylab = "Realised load", lwd = 1, cex.lab=lab, xlab = "",xlim = c(xmin+5,xmax),
     ylim=c((min(1-means_K_25_Kbot1800$FitnessP1-0.002)),max(1-means_K_25_Kbot1800$FitnessP1+0.017)), 
     col="black", xaxt = "n")#, cex.axis=axis)

#plot all runs
for(file in seq_along(K_25_Kbot1800)){
  
  data <- df_K_25_Kbot1800[which(df_K_25_Kbot1800$rep==file),]
  lines(data$Year,1-data$FitnessP1, col=colors[3])
}
lines(means_K_25_Kbot1800$Year,1-means_K_25_Kbot1800$FitnessP1, lwd=2, col="black")

abline(v=506800, col="black",lwd=1, lty=3)
abline(v=506925, col="black",lwd=1, lty=3)

#Masked load
#plot the average
plot(means_K_25_Kbot1800$Year, means_K_25_Kbot1800$B_P1, type = "l", ylab = "Masked load", lwd = 1, cex.lab=lab, xlab = "Years (BP)",xlim = c(xmin+5,xmax),
     ylim=c((min(means_K_25_Kbot1800$B_P1-0.02)),max(means_K_25_Kbot1800$B_P1+0.3)), 
     col="black", xaxt = "n", cex.axis=axis)

#plot all runs
for(file in seq_along(K_25_Kbot1800)){
  
  data <- df_K_25_Kbot1800[which(df_K_25_Kbot1800$rep==file),]
  lines(data$Year,data$B_P1, col=colors[3])
}
lines(means_K_25_Kbot1800$Year,means_K_25_Kbot1800$B_P1, lwd=2, col="black")

abline(v=506800, col="black",lwd=1, lty=3)
abline(v=506925, col="black",lwd=1, lty=3)

axis(1, at=seq(xmin,xmax, by=500), labels=seq(7000,0, by = -500), cex.axis=axis)


dev.off()


#250_1800
pdf(paste("../Plots/Main_K_250_Kbot1800_N_FROH_Load.pdf", sep=""), width=6, height=8)

colors = brewer.pal(n = 8, name = "BrBG")[1:8]
lwd = 1
lab = 1.5
axis=1.2

par(mfrow=c(5,1), mar = c(4.5,5,0.5,0.5), oma=c(2,0,1,0), mai = c(0.05, 1, 0.2, 0.2)) #, bty = "n"

#N
#plot the average
plot(means_K_250_Kbot1800$Year, means_K_250_Kbot1800$popSizeP1, type = "l", ylab = "Population size", lwd = 1, cex.lab=lab, xlab = "",xlim = c(xmin+5,xmax),
     #ylim=c((min(means_K_2500_Kbot500$popSizeP1) - 1000),max(means_K_2500_Kbot500$popSizeP1) + 1000), 
     ylim=c(0,51000),
     col="black", xaxt = "n", cex.axis=axis) #ylim=c(0,550)


#plot all runs
for(file in seq_along(K_250_Kbot1800)){
  
  data <- df_K_250_Kbot1800[which(df_K_250_Kbot1800$rep==file),]
  lines(data$Year,data$popSizeP1, col=colors[3])
}
lines(means_K_250_Kbot1800$Year,means_K_250_Kbot1800$popSizeP1, lwd=2, col="black")

abline(v=506800, col="black",lwd=1, lty=3)
abline(v=506925, col="black",lwd=1, lty=3)

# Het
#plot the average
plot(means_K_250_Kbot1800$Year, means_K_250_Kbot1800$meanHetP1, type = "l", ylab = "Heterozygosity", lwd = 1, cex.lab=lab, xlab = "",xlim = c(xmin+5,xmax),
     ylim=c((min(means_K_250_Kbot1800$meanHetP1)-0.000005),max(means_K_250_Kbot1800$meanHetP1)+0.00001), 
     col="black", xaxt = "n", cex.axis=axis) #ylim=c(0,550)

#plot all runs
for(file in seq_along(K_250_Kbot1800)){
  
  data <- df_K_250_Kbot1800[which(df_K_250_Kbot1800$rep==file),]
  lines(data$Year,data$meanHetP1, col=colors[3])
}
lines(means_K_250_Kbot1800$Year,means_K_250_Kbot1800$meanHetP1, lwd=2, col="black")

abline(v=506800, col="black",lwd=1, lty=3)
abline(v=506925, col="black",lwd=1, lty=3)

#FROH
#plot the average
plot(means_K_250_Kbot1800$Year, means_K_250_Kbot1800$FROHP1, type = "l", ylab = "FROH", lwd = 1, cex.lab=lab, xlab = "",xlim = c(xmin+5,xmax),
     #ylim=c((min(means_K_2500_Kbot500$FROHP1)),max(means_K_2500_Kbot500$FROHP1)), 
     ylim=c(0.0,1),
     col="black", xaxt = "n", cex.axis=axis) #ylim=c(0,550)

#plot all runs
for(file in seq_along(K_250_Kbot1800)){
  
  data <- df_K_250_Kbot1800[which(df_K_250_Kbot1800$rep==file),]
  lines(data$Year,data$FROHP1, col=colors[3])
}
lines(means_K_250_Kbot1800$Year,means_K_250_Kbot1800$FROHP1, lwd=2, col="black")
abline(v=506800, col="black",lwd=1, lty=3)
abline(v=506925, col="black",lwd=1, lty=3)

#FitnessP1 = Realised load
#plot the average
plot(means_K_250_Kbot1800$Year, 1-means_K_250_Kbot1800$FitnessP1, type = "l", ylab = "Realised load", lwd = 1, cex.lab=lab, xlab = "",xlim = c(xmin+5,xmax),
     ylim=c((min(1-means_K_250_Kbot1800$FitnessP1-0.002)),max(1-means_K_250_Kbot1800$FitnessP1+0.017)), 
     col="black", xaxt = "n")#, cex.axis=axis)

#plot all runs
for(file in seq_along(K_250_Kbot1800)){
  
  data <- df_K_250_Kbot1800[which(df_K_250_Kbot1800$rep==file),]
  lines(data$Year,1-data$FitnessP1, col=colors[3])
}
lines(means_K_250_Kbot1800$Year,1-means_K_250_Kbot1800$FitnessP1, lwd=2, col="black")

abline(v=506800, col="black",lwd=1, lty=3)
abline(v=506925, col="black",lwd=1, lty=3)

#Masked load
#plot the average
plot(means_K_250_Kbot1800$Year, means_K_250_Kbot1800$B_P1, type = "l", ylab = "Masked load", lwd = 1, cex.lab=lab, xlab = "Years (BP)",xlim = c(xmin+5,xmax),
     ylim=c((min(means_K_250_Kbot1800$B_P1-0.02)),max(means_K_250_Kbot1800$B_P1+0.28)), 
     col="black", xaxt = "n", cex.axis=axis)

#plot all runs
for(file in seq_along(K_250_Kbot1800)){
  
  data <- df_K_250_Kbot1800[which(df_K_250_Kbot1800$rep==file),]
  lines(data$Year,data$B_P1, col=colors[3])
}
lines(means_K_250_Kbot1800$Year,means_K_250_Kbot1800$B_P1, lwd=2, col="black")

abline(v=506800, col="black",lwd=1, lty=3)
abline(v=506925, col="black",lwd=1, lty=3)

axis(1, at=seq(xmin,xmax, by=500), labels=seq(7000,0, by = -500), cex.axis=axis)


dev.off()
