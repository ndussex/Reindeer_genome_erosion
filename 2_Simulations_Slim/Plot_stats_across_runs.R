library("RColorBrewer")

#summary across runs 

setwd("~/Founder effect/rec_1000/output")
#loading files for each simulation set (total of 6)
K_25_Kbot1800 <- list.files(pattern="Reindeer_K2=25_Kbot=1800")
K_500_Kbot1800 <- list.files(pattern="Reindeer_K2=500_Kbot=1800")
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




################################################################
### 2. plot comparing allele counts before and after bottleneck
################################################################
colors = brewer.pal(n = 8, name = "BrBG")[1:8]

## timing
t7000BP <- 500000 #only need to call once
t6800BP <- 500200 #only need to call once
t6000BP <- 501000 #only need to call once
t200BP <- 506800 #only need to call once
t0BP <- 507000 #only need to call once

###
#K_25_Kbot1800
K_25_Kbot1800_t7000BP <- df_K_25_Kbot1800[which(df_K_25_Kbot1800$Year ==t7000BP) ,]
K_25_Kbot1800_t6800BP <- df_K_25_Kbot1800[which(df_K_25_Kbot1800$Year ==t6800BP) ,]
K_25_Kbot1800_t6000BP <- df_K_25_Kbot1800[which(df_K_25_Kbot1800$Year == t6000BP) ,]
K_25_Kbot1800_t200BP <- df_K_25_Kbot1800[which(df_K_25_Kbot1800$Year ==t200BP) ,]
K_25_Kbot1800_t0BP <- df_K_25_Kbot1800[which(df_K_25_Kbot1800$Year == t0BP) ,]


pdf("../Plots/Main_K_25_Kbot1800_Load_boxplot_Time_stratified.pdf", width = 10, height = 10)
par(mfrow=c(2,2))

lab = 1.5
axis = 1
Title = 1.5

boxplot(K_25_Kbot1800_t7000BP$avgWkDelP1,K_25_Kbot1800_t6800BP$avgWkDelP1, K_25_Kbot1800_t6000BP$avgWkDelP1, K_25_Kbot1800_t200BP$avgWkDelP1, K_25_Kbot1800_t0BP$avgWkDelP1, col=c(colors[1],colors[3],colors[3],colors[3],colors[3]), names = c("Mainland","6800 BP","6000 BP","200 BP","0 BP"), ylab="avg. N. alleles / ind.", main = "Weakly deleterious", cex.lab=lab, cex.axis=axis, cex.main=Title)
boxplot(K_25_Kbot1800_t7000BP$avgModDelP1,K_25_Kbot1800_t6800BP$avgModDelP1,K_25_Kbot1800_t6000BP$avgModDelP1, K_25_Kbot1800_t200BP$avgModDelP1, K_25_Kbot1800_t0BP$avgModDelP1, col=c(colors[1],colors[3],colors[3],colors[3],colors[3]), names = c("Mainland","6800 BP","6000 BP","200 BP","0 BP"), ylab="", main = "Moderately deleterious", cex.lab=lab, cex.axis=axis, cex.main=Title)
boxplot(K_25_Kbot1800_t7000BP$avgStrDelP1,K_25_Kbot1800_t6800BP$avgStrDelP1,K_25_Kbot1800_t6000BP$avgStrDelP1, K_25_Kbot1800_t200BP$avgStrDelP1, K_25_Kbot1800_t0BP$avgStrDelP1, col=c(colors[1],colors[3],colors[3],colors[3],colors[3]), names = c("Mainland","6800 BP","6000 BP","200 BP","0 BP"), ylab="avg. N. alleles / ind.", main = "Strongly deleterious deleterious", cex.lab=lab, cex.axis=axis, cex.main=Title)
boxplot(K_25_Kbot1800_t7000BP$avgvStrDelP1,K_25_Kbot1800_t6800BP$avgvStrDelP1,K_25_Kbot1800_t6000BP$avgvStrDelP1, K_25_Kbot1800_t200BP$avgvStrDelP1, K_25_Kbot1800_t0BP$avgvStrDelP1, col=c(colors[1],colors[3],colors[3],colors[3],colors[3]), names = c("Mainland","6800 BP","6000 BP","200 BP","0 BP"), ylab="", main = "Very Strongly deleterious", cex.lab=lab, cex.axis=axis, cex.main=Title)

dev.off()


#K_250_Kbot1800
K_250_Kbot1800_t6800BP <- df_K_250_Kbot1800[which(df_K_250_Kbot1800$Year ==t6800BP) ,]
K_250_Kbot1800_t7000BP <- df_K_250_Kbot1800[which(df_K_250_Kbot1800$Year ==t7000BP) ,]
K_250_Kbot1800_t6000BP <- df_K_250_Kbot1800[which(df_K_250_Kbot1800$Year == t6000BP) ,]
K_250_Kbot1800_t200BP <- df_K_250_Kbot1800[which(df_K_250_Kbot1800$Year ==t200BP) ,]
K_250_Kbot1800_t0BP <- df_K_250_Kbot1800[which(df_K_250_Kbot1800$Year == t0BP) ,]


pdf("../Plots/Main_K_250_Kbot1800_Load_boxplot_Time_stratified.pdf", width = 10, height = 10)
par(mfrow=c(2,2))

lab = 1.5
axis = 1
Title = 1.5

boxplot(K_250_Kbot1800_t7000BP$avgWkDelP1,K_250_Kbot1800_t6800BP$avgWkDelP1, K_250_Kbot1800_t6000BP$avgWkDelP1, K_250_Kbot1800_t200BP$avgWkDelP1, K_250_Kbot1800_t0BP$avgWkDelP1, col=c(colors[1],colors[3],colors[3],colors[3],colors[3]), names = c("Mainland","6800 BP","6000 BP","200 BP","0 BP"), ylab="avg. N. alleles / ind.", main = "Weakly deleterious", cex.lab=lab, cex.axis=axis, cex.main=Title)
boxplot(K_250_Kbot1800_t7000BP$avgModDelP1,K_250_Kbot1800_t6800BP$avgModDelP1,K_250_Kbot1800_t6000BP$avgModDelP1, K_250_Kbot1800_t200BP$avgModDelP1, K_250_Kbot1800_t0BP$avgModDelP1, col=c(colors[1],colors[3],colors[3],colors[3],colors[3]), names = c("Mainland","6800 BP","6000 BP","200 BP","0 BP"), ylab="", main = "Moderately deleterious", cex.lab=lab, cex.axis=axis, cex.main=Title)
boxplot(K_250_Kbot1800_t7000BP$avgStrDelP1,K_250_Kbot1800_t6800BP$avgStrDelP1,K_250_Kbot1800_t6000BP$avgStrDelP1, K_250_Kbot1800_t200BP$avgStrDelP1, K_250_Kbot1800_t0BP$avgStrDelP1, col=c(colors[1],colors[3],colors[3],colors[3],colors[3]), names = c("Mainland","6800 BP","6000 BP","200 BP","0 BP"), ylab="avg. N. alleles / ind.", main = "Strongly deleterious deleterious", cex.lab=lab, cex.axis=axis, cex.main=Title)
boxplot(K_250_Kbot1800_t7000BP$avgvStrDelP1,K_250_Kbot1800_t6800BP$avgvStrDelP1,K_250_Kbot1800_t6000BP$avgvStrDelP1, K_250_Kbot1800_t200BP$avgvStrDelP1, K_250_Kbot1800_t0BP$avgvStrDelP1, col=c(colors[1],colors[3],colors[3],colors[3],colors[3]), names = c("Mainland","6800 BP","6000 BP","200 BP","0 BP"), ylab="", main = "Very Strongly deleterious", cex.lab=lab, cex.axis=axis, cex.main=Title)

dev.off()
