### STRATOSPHERE DISTRUBTIONS #### Fire paper

library(ggplot2)
library(gridExtra)
library(egg)
library(pracma)
library(corrplot)
library(xtable)
library(tidyr)
library(RColorBrewer)



mainDir <- "C:/Users/dgudy/Dropbox/PhD_UTAS/Data_Analysis/run_025/ice_core_DSS"
#make directory
folder = "stratospheric_vortex"
#dir.create(file.path(mainDir,folder))
in_dir  <- mainDir
setwd(in_dir)

f_name = "ONDJ_SOMcount_LDsss2k_S-T_SVbreakdown_ECFI_SAM_ENSO_regional_rainfall.csv"

Variable = "LDsss2k_ST"
Season = "ONDJ"
Season_Corr = "ONDJ_LDsss2k_ST"

#load file
data = read.csv(f_name, header = TRUE)
df = data.frame(data)


## stratospheric vortex breakdown
SVbreak_avg = mean(df$SV_breakdown)
SVbreak_sd = sd(df$SV_breakdown)
df$SVbreak_avg = SVbreak_avg
df$SVbreak_sd = SVbreak_sd

SVbreak_66terc = quantile(df$SV_breakdown, c(0.66))
SVbreak_33terc = quantile(df$SV_breakdown, c(0.33))
df$SVbreak_terc66 = SVbreak_66terc
df$SVbreak_terc33 = SVbreak_33terc

df$SVbreak_ID = ifelse(df$SV_breakdown > SVbreak_66terc,"late",ifelse(df$SV_breakdown<SVbreak_33terc,"early","mid"))

ST_avg = mean(df$S.Tmode)
ST_sd = sd(df$S.Tmode)
df$SVbreak_avg = ST_avg
df$SVbreak_sd = ST_sd

ST_66terc = quantile(df$S.Tmode, c(0.66))
ST_33terc = quantile(df$S.Tmode, c(0.33))
df$ST_terc66 = ST_66terc
df$ST_terc33 = ST_33terc

df$ST_ID = ifelse(df$S.Tmode > ST_66terc,"a_weak",ifelse(df$S.Tmode<ST_33terc,"b_strong","mid"))

### plot - stratospheric vortex breakdown
par(mfrow = c(1,1))
plot(df$JanYear, df$SV_breakdown, type = "l", lty = 1, col = "black", lwd = 3)
abline(h = df$SVbreak_avg, col = "grey", lty = 1, lwd = 2)
abline(h = df$SVbreak_terc66, col = "grey", lty = 2, lwd = 2)
abline(h = df$SVbreak_terc33, col = "grey", lty = 2, lwd = 2)

#### Upper/Lower terciles LDsss average (1979-2016) ##  
#Smaller dataset - less power. 
upper = subset(df,df$SVbreak_ID == "late")
lower = subset(df,df$SVbreak_ID == "early")

#Non parametric test - Wilcox test
SOM1 = wilcox.test(upper$SOM1,lower$SOM1, alternative = "two.sided")
SOM2 = wilcox.test(upper$SOM2,lower$SOM2, alternative = "two.sided")
SOM3 = wilcox.test(upper$SOM3,lower$SOM3, alternative = "two.sided")
SOM4 = wilcox.test(upper$SOM4,lower$SOM4, alternative = "two.sided")
SOM5 = wilcox.test(upper$SOM5,lower$SOM5, alternative = "two.sided")
SOM6 = wilcox.test(upper$SOM6,lower$SOM6, alternative = "two.sided")
SOM7 = wilcox.test(upper$SOM7,lower$SOM7, alternative = "two.sided")
SOM8 = wilcox.test(upper$SOM8,lower$SOM8, alternative = "two.sided")
SOM9 = wilcox.test(upper$SOM9,lower$SOM9, alternative = "two.sided")

Summary_Wilcox_terc= data.frame(SOM1$p.value,
                                SOM2$p.value,
                                SOM3$p.value,
                                SOM4$p.value,
                                SOM5$p.value,
                                SOM6$p.value,
                                SOM7$p.value,
                                SOM8$p.value,
                                SOM9$p.value)


#export table to csv
write.csv(Summary_Wilcox_terc,paste(Season, "_", Variable,"_SOMs_UpperLower_terciles_WilcoxTest_pvalue_SVbreakdown.csv",sep=""))


#### Upper/Lower terciles ST mode (1979-2016) ##  
#Smaller dataset - less power. 
upper = subset(df,df$ST_ID == "a_weak")
lower = subset(df,df$ST_ID == "b_strong")

#Non parametric test - Wilcox test
SOM1 = wilcox.test(upper$SOM1,lower$SOM1, alternative = "two.sided")
SOM2 = wilcox.test(upper$SOM2,lower$SOM2, alternative = "two.sided")
SOM3 = wilcox.test(upper$SOM3,lower$SOM3, alternative = "two.sided")
SOM4 = wilcox.test(upper$SOM4,lower$SOM4, alternative = "two.sided")
SOM5 = wilcox.test(upper$SOM5,lower$SOM5, alternative = "two.sided")
SOM6 = wilcox.test(upper$SOM6,lower$SOM6, alternative = "two.sided")
SOM7 = wilcox.test(upper$SOM7,lower$SOM7, alternative = "two.sided")
SOM8 = wilcox.test(upper$SOM8,lower$SOM8, alternative = "two.sided")
SOM9 = wilcox.test(upper$SOM9,lower$SOM9, alternative = "two.sided")

Summary_Wilcox_terc= data.frame(SOM1$p.value,
                                SOM2$p.value,
                                SOM3$p.value,
                                SOM4$p.value,
                                SOM5$p.value,
                                SOM6$p.value,
                                SOM7$p.value,
                                SOM8$p.value,
                                SOM9$p.value)


#export table to csv
write.csv(Summary_Wilcox_terc,paste(Season, "_", Variable,"_UpperLower_terciles_WilcoxTest_pvalue_STmode.csv",sep=""))


#convert wide format to long format
df_SV = subset(df,df$SVbreak_ID == 'late' | df$SVbreak_ID == 'early', select = c(SOM1, SOM2, SOM3,SOM4, SOM5,SOM6,SOM7,SOM8,SOM9,SVbreak_ID))
dfSV_long = gather(df_SV, node, count, SOM1:SOM9, factor_key = TRUE)
colnames(dfSV_long) = c("condition", "SOMnode","count")

df_ST = subset(df,df$ST_ID == 'b_strong' | df$ST_ID == 'a_weak', select = c(SOM1, SOM2, SOM3,SOM4, SOM5,SOM6,SOM7,SOM8,SOM9,ST_ID))
dfST_long = gather(df_ST, node, count, SOM1:SOM9, factor_key = TRUE)
colnames(dfST_long) = c("condition", "SOMnode","count")

#subset dataframe to SOM3 and SOM7
df_SV_sub = subset(df,df$SVbreak_ID == 'late' | df$SVbreak_ID == 'early', select = c(SOM3,SOM7,SVbreak_ID))
dfSVs_long = gather(df_SV_sub, node, count, SOM3:SOM7, factor_key = TRUE)
colnames(dfSVs_long) = c("condition", "SOMnode","count")

df_ST_sub = subset(df,df$ST_ID == 'b_strong' | df$ST_ID == 'a_weak', select = c( SOM3,SOM7,ST_ID))
dfSTs_long = gather(df_ST_sub, node, count, SOM3:SOM7, factor_key = TRUE)
colnames(dfSTs_long) = c("condition", "SOMnode","count")

#upper/lower tercile SV breakdown - all SOMs
ggplot(dfSV_long, aes(x = SOMnode, y = count, fill = condition))+
  geom_boxplot() + 
  ggtitle(paste(Season, " seasonal SOM node count: Upper/Lower tercile SV breakdown",sep=""))+ 
  labs(x = "SOM node", y = "count") + 
  scale_fill_brewer(palette = "Blues")+
  theme_classic()
ggsave(paste(Season, "_seasonal_SOMfreq_Boxplot_SV_breakdown_UpperLower_terciles.jpg",sep=""),dpi = 300)

ggplot(dfST_long, aes(x = SOMnode, y = count, fill = condition))+
  geom_boxplot() + 
  ggtitle(paste(Season, " seasonal SOM node count: Upper/Lower tercile SV strength (ST mode)",sep=""))+ 
  labs(x = "SOM node", y = "count") + 
  scale_fill_brewer(palette = "Blues")+
  theme_classic()
ggsave(paste(Season, "_seasonal_SOMfreq_Boxplot_SV_strength_UpperLower_terciles.jpg",sep=""),dpi = 300)

ggplot(dfSVs_long, aes(x = SOMnode, y = count, fill = condition))+
  geom_boxplot() + 
  ggtitle(paste(Season, " seasonal SOM node count: Upper/Lower tercile SV breakdown",sep=""))+ 
  labs(x = "SOM node", y = "count") + 
  scale_fill_brewer(palette = "Blues")+
  theme_classic()
ggsave(paste(Season, "_seasonal_SOMfreq_SAMnodes_Boxplot_SV_breakdown_UpperLower_terciles.jpg",sep=""),dpi = 300)


#SOM3 and SOM7 -> used in paper

dodge = position_dodge(width = 0.99)
ggplot(dfSVs_long, aes(x = SOMnode, y = count, fill = condition))+
  geom_violin(width = 1, trim = FALSE)+
  geom_boxplot(position = dodge,width = 0.1) + 
  ggtitle(paste(Season, " seasonal SOM node count: Upper/Lower tercile SV breakdown",sep=""))+ 
  labs(x = "SOM node", y = "count") + 
  scale_fill_brewer(palette = "Blues")+
  #guides(fill = guide_legend(reverse = TRUE))+
  labs(fill = "SV condition")+
  theme_classic()
ggsave(paste(Season, "_seasonal_SOMfreq_SOM3_SOM7_Violin_Boxplot_SV_breakdown_UpperLower_terciles.jpg",sep=""),dpi = 300)
ggsave(paste(Season, "_seasonal_SOMfreq_SOM3_SOM7_Violin_Boxplot_SV_breakdown_UpperLower_terciles.pdf",sep=""))

ggplot(dfSTs_long, aes(x = SOMnode, y = count, fill = condition))+
  geom_violin(width = 1, trim = FALSE)+
  geom_boxplot(position = dodge, width = 0.1) + 
  ggtitle(paste(Season, " seasonal SOM node count: Upper/Lower tercile SV strength (ST mode)",sep=""))+ 
  labs(x = "SOM node", y = "count") + 
  scale_fill_brewer(palette = "Greens")+
  #guides(fill = guide_legend(reverse = TRUE))+
  labs(fill = "SV condition")+
  theme_classic()
ggsave(paste(Season, "_seasonal_SOMfreq_SOM3_SOM7_Violin_Boxplot_SV_strength_UpperLower_terciles.jpg",sep=""),dpi = 300)
ggsave(paste(Season, "_seasonal_SOMfreq_SOM3_SOM7_Violin_Boxplot_SV_strength_UpperLower_terciles.pdf",sep=""))


## create a dataset with breakdown date distribution for upper/lower LDsss tercile years
df_SV_LDsss = subset(df,df$SVbreak_ID == 'late' | df$SVbreak_ID == 'early', select = c(LDsss_2k,SVbreak_ID))
df_ST_LDsss = subset(df,df$ST_ID == 'b_strong' | df$ST_ID == 'a_weak', select = c(LDsss_2k,ST_ID))

#statistical test - is the mean LDsss concentration between early and late SV breakdown years significant?
#Non parametric test - Wilcox test - Precip on SV breakdown
upper = subset(df,df$SVbreak_ID == "late")
lower = subset(df,df$SVbreak_ID == "early")

LDw = wilcox.test(upper$LDsss_2k,lower$LDsss_2k, alternative = "two.sided")
LDt = t.test(upper$LDsss_2k,lower$LDsss_2k, alternative = "two.sided")
LDw.p = LDw$p.value
LDt.p = LDt$p.value

LDw.p = round(LDw.p,digits = 4)
LDt.p = round(LDt.p,digits = 4)

text1 = c(paste("wilcox p-value:",LDw.p,sep = ""))
text2 = c(paste("t-test p-value:",LDt.p,sep = ""))

ggplot(df_SV_LDsss, aes(x = SVbreak_ID, y = LDsss_2k, fill = SVbreak_ID))+
  geom_violin(trim = FALSE, width = 0.8) + 
  annotate(geom = "text", x = 1.5, y = -0.6, label = text1)+
  #annotate(geom = "text", x = 1.5, y = -0.14, label = text2)+
  geom_boxplot(width = 0.1)+
  ylim(c(-0.6,0.6))+
  scale_fill_brewer(palette = "Blues")+
  ggtitle("LDsss concentration between Upper (late) /Lower (early) tercile SV breakdown")+ 
  labs(x = "SV breakdown ID", y = "LDsss concentration", fill = "SV condition")+
  theme_classic()

ggsave("Violin_Boxplot_LDsss_concentration_split_SV_breakdown_UpperLower_terciles.jpg",dpi = 300)
ggsave("Violin_Boxplot_LDsss_concentration_split_SV_breakdown_UpperLower_terciles.pdf",dpi = 300)



df_ST_LDsss = subset(df,df$ST_ID == 'b_strong' | df$ST_ID == 'a_weak', select = c(LDsss_2k,ST_ID))

#statistical test - is the mean LDsss concentration between early and late SV breakdown years significant?
#Non parametric test - Wilcox test - Precip on SV breakdown
upper = subset(df,df$ST_ID == "a_weak")
lower = subset(df,df$ST_ID == "b_strong")

LDw = wilcox.test(upper$LDsss_2k,lower$LDsss_2k, alternative = "two.sided")
LDt = t.test(upper$LDsss_2k,lower$LDsss_2k, alternative = "two.sided")
LDw.p = LDw$p.value
LDt.p = LDt$p.value

LDw.p = round(LDw.p,digits = 4)
LDt.p = round(LDt.p,digits = 4)

text1 = c(paste("wilcox p-value:",LDw.p,sep = ""))
text2 = c(paste("t-test p-value:",LDt.p,sep = ""))

ggplot(df_ST_LDsss, aes(x = reorder(ST_ID, LDsss_2k), y = LDsss_2k, fill = reorder(ST_ID, LDsss_2k)))+
  geom_violin(width = 0.9, trim = FALSE) + 
  annotate(geom = "text", x = 1.6, y = -0.6, label = text1)+
  #annotate(geom = "text", x = 1.5, y = -0.13, label = text2)+
  geom_boxplot(width = 0.1)+
  ylim(c(-0.6,0.6))+
  scale_fill_brewer(palette = "Greens")+
  ggtitle("LDsss concentration between Upper (weak) /Lower (strong) tercile SV strength")+ 
  labs(x = "ST strength", y = "LDsss concentration", fill = "SV condition")+
  theme_classic()


ggsave("Violin_Boxplot_LDsss_concentration_split_SV_strength_UpperLower_terciles.jpg",dpi = 300)
ggsave("Violin_Boxplot_LDsss_concentration_split_SV_strength_UpperLower_terciles.pdf",dpi = 300)






#distribution of SV breakdown date for upper/lower summer sea salt years
df_LDsss_SV = subset(df,df$LDsss_terciles == 'upper' | df$LDsss_terciles == 'lower', select = c(SV_breakdown,LDsss_terciles))

upper = subset(df,df$LDsss_terciles == "upper")
lower = subset(df,df$LDsss_terciles == "lower")

#statistical test - is the mean SV breakdown date between upper / lower sea salt years significant?
#Non parametric test - Wilcox test - Precip on SV breakdown
upper = subset(df,df$LDsss_terciles == "upper")
lower = subset(df,df$LDsss_terciles == "lower")

SVw = wilcox.test(upper$SV_breakdown,lower$SV_breakdown, alternative = "two.sided")
SVt = t.test(upper$SV_breakdown,lower$SV_breakdown, alternative = "two.sided")
SVw.p = SVw$p.value
SVt.p = SVt$p.value

SVw.p = round(SVw.p,digits = 4)
SVt.p = round(SVt.p,digits = 4)

text1 = c(paste("wilcox p-value:",SVw.p,sep = ""))
text2 = c(paste("t-test p-value:",SVt.p,sep = ""))


ggplot(df_LDsss_SV, aes(x = LDsss_terciles, y = SV_breakdown, fill = LDsss_terciles))+
  geom_violin(width = 0.8,trim = FALSE) + 
  annotate(geom = "text", x = 1.6, y = -20, label = text1)+
  #annotate(geom = "text", x = 1.6, y = -24, label = text2)+
  geom_boxplot(width = 0.1)+
  scale_fill_brewer(palette = "Blues")+
  ggtitle("SV breakdown anomaly between Upper /Lower tercile LDsss")+ 
  labs(x = "LDsss tercile", y = "SV breakdown anomaly (0 = 4th December)", fill = "SV condition")+
  theme_classic()
ggsave("Violin_Boxplot_SVbreakdown_split_LDsss_UpperLower_terciles.jpg",dpi = 300)



