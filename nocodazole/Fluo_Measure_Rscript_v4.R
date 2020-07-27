#### parametres ###
cond.name <- "siWDR62_sum_areaT_outlierT_svg"
remove.outlivers = T;
multiply.by.area = T;
  input.file = "../2020_Nocodazol/siCTRL_siWDR62/ANALYSIS_STRICT/RESULT_sum/ANALYSIS_SUM.csv"

#### code ####
noc <- read.table(input.file, header=T, sep=",", skip = 1)
#names(noc)[5] <- "mean.fluo"
str(noc)

#val fluorecence depending to area
if(multiply.by.area){
  noc$mean.fluo <- noc$fluo.value * noc$area
}else{
  noc$mean.fluo <- noc$fluo.value
}

#get the max value per cell:condition to normalize
a <- aggregate(mean.fluo ~ condition:cell, FUN=max, data=noc)
names(a)[3] <- "max.fluo"

#merge les df
noc <- merge(noc, a)
#normalize fluo by max
noc$norm.fluo <- noc$mean.fluo/noc$max.fluo*100

#plot the curves
library(ggplot2)
g1 <- ggplot(data=noc, aes(x=time, y=norm.fluo, color=condition, group=condition:cell))+ 
  geom_line(alpha=0.1) + labs(y="normalized fluo") + theme_bw(); 
g1 + geom_line(alpha=0.5) + scale_color_manual(values = c("black", "red"))

        if(remove.outlivers){
          #remove outliers ????
          rmv <- noc[noc$norm.fluo < 100 & noc$time == 1, 3]
          noc2 <- noc[!noc$img %in% rmv,]
          noc <- noc2
        }

g1 <- ggplot(data=noc, aes(x=time, y=norm.fluo, color=condition, group=condition:cell))+ 
  geom_line(alpha=0.1) + labs(y="normalized fluo") + theme_bw(); 
g1 + geom_line(alpha=0.5) + scale_color_manual(values = c("black", "red"))

#compute the mean/median and display a plot
a2_median <- aggregate(norm.fluo~condition:time, FUN=median, data=noc)
names(a2_median)[3] <- "median"
a2_mean <- aggregate(norm.fluo~condition:time, FUN=mean, data=noc)
names(a2_mean)[3] <- "mean"

g1_median <- g1 + geom_line(data=a2_median, aes(x=time, y=median, color=condition, group=condition), lty=2, lwd=1.2) + 
  scale_color_manual(values = c("black", "red"))
g1_mean <- g1 + geom_line(data=a2_mean, aes(x=time, y=mean, color=condition, group=condition), lty=2, lwd=1.2) +
  scale_color_manual(values = c("black", "red"))
g1_median
g1_mean

#calc nb replicate per condition
  a3 <- aggregate(norm.fluo~condition:time, FUN=length, data=noc)[1:2, c(1,3)]
  names(a3)[2] <- "count"
  a3

#calc sd
a4 <- aggregate(norm.fluo~condition:time, FUN=sd, data=noc)
names(a4)[3] <- "sd"
a4

#merge all stats in a single df
agg <- merge(a2_mean, a2_median)
agg <- merge(agg, a4)
agg <- agg[order(agg$time),]
agg

####K compute CI for the mean and median ####
IC_data <- data.frame(time=NULL, diff_mean=NULL, lwr_mean=NULL, upr_mean=NULL, 
                      diff_median=NULL, lwr_median=NULL, upr_median=NULL)
for(t in 1:16){ #loops over each time
  agg_t <- agg[agg$time==t,] #get data for the time
  agg_t
  #nb replicat for ctrl and siWDR
  n_c <- a3$count[1]
  n_w <- a3$count[2]
  #sd for control and siwdr
  sd_c <- agg_t$sd[1]
  sd_w <- agg_t$sd[2]
  #mean and median for ctrl and wdr
  mu_c_median <- agg_t$median[1]
  mu_w_median <- agg_t$median[2]
  mu_c_mean <- agg_t$mean[1]
  mu_w_mean <- agg_t$mean[2]
  
  #calcl the CI 95% for independant groups assuming unequal variance: 
  sp <- sqrt( (n_c*sd_c^2 + n_w*sd_w^2) / (n_c+n_w-2) )
  IC_mean <- (mu_w_mean-mu_c_mean) + c(-1,1) * qt(.975, n_c+n_w-2) * sp * (1/n_c+1/n_w)
  IC_median <- (mu_w_median-mu_c_median) + c(-1,1) * qt(.975, n_c+n_w-2) * sp * (1/n_c+1/n_w)
  
  #bind result into data frame for each time t
  IC_data <- rbind(IC_data, data.frame(time=t, diff_mean=mu_w_mean-mu_c_mean, 
                                       lwr_mean=IC_mean[1], upr_mean=IC_mean[2],
                                       diff_median=mu_w_median-mu_c_median, 
                                       lwr_median=IC_median[1], upr_median=IC_median[2]))
}
IC_data

# save data sheet of IC_data
#write.csv(IC_data, paste0("Resultats/", cond.name, ".csv"), row.names = F)

#plot the diff and lwr/upr CI values for mean and median
g2_mean <- ggplot(data=IC_data, aes(x=time, y=diff_mean)) + geom_line()+
  geom_ribbon(aes(ymin=lwr_mean, ymax=upr_mean), alpha=0.2) + 
  labs(y="mean wdr - mean ctrl") + theme_bw() +
  geom_hline(yintercept = 0, color="orange", lty=2)
g2_median <- ggplot(data=IC_data, aes(x=time, y=diff_median)) + geom_line()+
  geom_ribbon(aes(ymin=lwr_median, ymax=upr_median), alpha=0.2) + 
  labs(y="median wdr - median ctrl") + theme_bw() +
  geom_hline(yintercept = 0, color="orange", lty=2)

#arrange graphs into grids
#library(gridExtra)
#grid.arrange(g1_mean, g2_mean, top = "mean")
#grid.arrange(g1_median, g2_median, top = "median")

#final and save as png
#g_final <- grid.arrange(g1_mean+labs(title="mean"), g1_median+labs(title="median"), g2_mean+labs(title="95% CI"), g2_median+labs(title="95% CI"), ncol=2)
#ggsave(g_final, filename = paste0("Resultats/", cond.name, ".svg"), w=11, h=7)


#### save graph ####
library(gridExtra)
g_final <- grid.arrange(g1_mean+labs(title="mean"), g1_median+labs(title="median"), ncol=2)
ggsave(g_final, filename = paste0("Resultats/", cond.name, ".svg"), w=11, h=7)

#### p-val mean ####
test <- merge(a2_mean, a4)
test <- test[order(test$time),]
cond <- levels(test$condition)
test1 <- test[test$condition==cond[1],]
test2 <- test[test$condition==cond[2],]
  test1; test2;identical(test1$time, test2$time)

diff2 <- merge(test1, test2, by = "time")
diff <- data.frame(time = diff2$time, diff = diff2$mean.y - diff2$mean.x)
diff


cond <- levels(test$condition)
pval <- sapply(levels(factor(noc$time[noc$time!=1])), FUN = function(x){
  t.test(noc$norm.fluo[noc$time==x & noc$condition==cond[1]], 
         noc$norm.fluo[noc$time==x & noc$condition==cond[2]])$p.value
  })
pval <- data.frame(p.value = pval)
pval$signif = ""
pval$signif = sapply(pval$p.value, function(x){if(x < 0.001){return("***")}else if(x<0.01){return("**")}else if(x < 0.05){return("*")}else{return("n.s.")}})
pval$diff.fluo = diff$diff[-1]
pval$time = diff$time[-1]
pval <- pval[,c(4,3,1,2)]
names(pval)[2] <- paste0("fluo.", cond[2], ".vs.", cond[1])
pval_mean <- pval

#### p-val median ####
test <- merge(a2_median, a4)
test <- test[order(test$time),]
cond <- levels(test$condition)
test1 <- test[test$condition==cond[1],]
test2 <- test[test$condition==cond[2],]
test1; test2;identical(test1$time, test2$time)

diff2 <- merge(test1, test2, by = "time")
diff <- data.frame(time = diff2$time, diff = diff2$median.y - diff2$median.x)
diff

cond <- levels(test$condition)
pval <- sapply(levels(factor(noc$time[noc$time!=1])), FUN = function(x){
  t.test(noc$norm.fluo[noc$time==x & noc$condition==cond[1]], 
         noc$norm.fluo[noc$time==x & noc$condition==cond[2]])$p.value
})
pval <- data.frame(p.value = pval)
pval$signif = ""
pval$signif = sapply(pval$p.value, function(x){if(x < 0.001){return("***")}else if(x<0.01){return("**")}else if(x < 0.05){return("*")}else{return("n.s.")}})
pval$diff.fluo = diff$diff[-1]
pval$time = diff$time[-1]
pval <- pval[,c(4,3,1,2)]
names(pval)[2] <- paste0("fluo.", cond[2], ".vs.", cond[1])
pval_median <- pval


pval_mean$type = "mean"
pval_mean <- pval_mean[,c(5,1:4)]
pval_median$type = "median"
pval_median <- pval_median[,c(5,1:4)]

write.csv(rbind(pval_mean, pval_median), paste0("Resultats/", cond.name, "_pval.csv"), row.names = F)
