# Import the data
setwd("D:/IC/MSc Ecological Application/Project/Nash's field/Sampling")
library(readxl)
Dat <- read_excel("20230615 All fenced plot.xlsx", sheet = "For R")
Plant <- read_excel("20230709 All fenced plot.xlsx", sheet = "Data")
Plant_Random <- read_excel("20230709 All fenced plot.xlsx", sheet = "Random samples")
Dat2 <- read_excel("20230615 All fenced plot_Landsat.xlsx", sheet = "Data")
Landsat <- read_excel("20230615 All fenced plot_Landsat.xlsx", sheet = "Landsat")
Validation_linear <- read_excel("20230615 All fenced plot_Validation.xlsx", sheet = "May_Validation_linear")
Validation_logarithmic <- read_excel("20230615 All fenced plot_Validation.xlsx", sheet = "May_Validation_logarithmic")
Validation <- read_excel("20230615 All fenced plot_Validation.xlsx", sheet = "May_Validation_logarithmic")

# Average
library(dplyr)
## CC
grouped_data <- Dat2 %>% group_by(Plot, Group, Treatment)
mean_CC <- grouped_data %>% summarize(avg_value = mean(CC_Wheat))
write.csv(mean_CC, file = "Mean_CC.csv")

## Plant
grouped_data_plant <- Plant %>% group_by(Plot, Group, Treatment)
mean_plant <- grouped_data_plant %>% summarize(avg_value = mean(Height))
write.csv(mean_plant, file = "Mean_Plant.csv")

## Plant_Random Sample
grouped_data_random <- Plant_Random %>% group_by(Plot)
mean_plant_random <- grouped_data_random %>% summarize(avg_value = mean(Height))
write.csv(mean_plant_random, file = "Mean_Plant_random.csv")

## Merge the dataframe and export to excel format
### Merge two mean tables, keep the mean
merged <- left_join(mean_CC, mean_plant, by = c("Plot", "Group", "Treatment")) %>%
  rename(CC = avg_value.x, PlantHeight = avg_value.y)
### Put all three groups of data in one excel but different sheets
All_fenced <- createWorkbook() # Create a new Excel

#### Add first sheet in excel and write the mean
addWorksheet(All_fenced, sheetName = "Mean") 
writeData(All_fenced, sheet = "Mean", merged)
#### Add first sheet in excel and write the mean of random samples
addWorksheet(All_fenced, sheetName = "Plant_Random")
writeData(All_fenced, sheet = "Plant_Random", mean_plant_random)

### Save Excel file
saveWorkbook(All_fenced, file = "Mean_All fenced.xlsx", overwrite = TRUE)



# ANOVA
## Lime
K_Control <- subset(Dat, Dat$Plot=="K" & Dat$Group=="Control")
R_Control <- subset(Dat, Dat$Plot=="R")
### K, control group
AOV_K_Lime <- aov(SPAD~Lime, K_Control)
summary(AOV_K_Lime)# p=0.711
### R, control group
AOV_R_Lime <- aov(SPAD~Lime, R_Control)
summary(AOV_R_Lime) # p=0.00703 **


## Slope (Unlime only)
KL_all <- subset(Dat, (Dat$Plot=="K" | Dat$Plot=="L") & Dat$Group=="Control" & Dat$Lime=="Unlime")
NP_none.all <- subset(Dat, Dat$Plot=="N" | Dat$Plot=="P")
MQ_none.all <- subset(Dat, Dat$Plot=="M" | Dat$Plot=="Q")
RS_none.all <- subset(Dat, (Dat$Plot=="R" | Dat$Plot=="S") & (Dat$Name=="Unlime_None" | Dat$Name=="Unlime_All"))
### K & L, control group (all treatments)
AOV_KL_all <- aov(SPAD~Plot, KL_all)
summary(AOV_KL_all)# p=0.146
### N & P, control group (none & all treatment)
AOV_NP_none.all <- aov(SPAD~Plot, NP_none.all)
summary(AOV_NP_none.all)# p=0.253
### M & Q, control group (none & all treatment)
AOV_MQ_none.all <- aov(SPAD~Plot, MQ_none.all)
summary(AOV_MQ_none.all)# p=0.364
### R & S, control group (none & all treatment)
AOV_RS_none.all <- aov(SPAD~Plot, RS_none.all)
summary(AOV_RS_none.all)# p=0.016 *


## Molluscs/Insect (Control only)
KR_all_Lime <- subset(Dat, (Dat$Plot=="K" | Dat$Plot=="R") & Dat$Group=="Control" & Dat$Lime=="Lime")
KR_all_Unlime <- subset(Dat, (Dat$Plot=="K" | Dat$Plot=="R") & Dat$Group=="Control" & Dat$Lime=="Unlime")
LS_none.all_Unlime <- subset(Dat, (Dat$Plot=="L" | Dat$Plot=="S") & (Dat$Name=="Unlime_None" | Dat$Name=="Unlime_All"))
### K & R (downslope), Lime
AOV_KR_all_Lime <- aov(SPAD~Plot, KR_all_Lime)
summary(AOV_KR_all_Lime)# p=0.425
### K & R (downslope), Unlime
AOV_KR_all_Unlime <- aov(SPAD~Plot, KR_all_Unlime)
summary(AOV_KR_all_Unlime)# p=0.0971 .
### L & S (upslope), Unlime
AOV_LS_none.all_Unlime <- aov(SPAD~Plot, LS_none.all_Unlime)
summary(AOV_LS_none.all_Unlime)# p=0.242


## Plant competition
K_Lime <- subset(Dat, Dat$Plot=="K" & Dat$Lime=="Lime")
### K, Lime, grass exclusion & control
AOV_K_Lime <- aov(SPAD~Group, K_Lime)
summary(AOV_K_Lime)# p=0.248

## Nutrition
K_Control_Unlime <- subset(Dat, Dat$Plot=="K" & Dat$Group=="Control" & Dat$Lime=="Unlime")
R_Control_Unlime <- subset(Dat, Dat$Plot=="R" & Dat$Lime=="Unlime")
### K, Unlime, control
AOV_K_Control_Unlime <- aov(SPAD~Treatment, K_Control_Unlime)
summary(AOV_K_Control_Unlime)# p=0.0653
### R, Unlime, control
AOV_R_Control_Unlime <- aov(SPAD~Treatment, R_Control_Unlime)
summary(AOV_R_Control_Unlime)# p=0.523




# Box plot
library(ggplot2) 
library(ggsignif)
## Lime versus unlime
merged_liming <- rbind(K_Control, R_Control)
ggplot(merged_liming, aes(x = Name, y = CC_Wheat, fill= Treatment)) +
  geom_boxplot(width = 0.4) +
  xlab("Treatment") +
  ylab(expression("Chlorophyll Content"~(mg/m^2))) +
  ggtitle("Box plot for liming") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(hjust = 0.5,size = 16)) 

## Upslope versus downslope
merged_slope <- rbind(KL_all, NP_none.all, MQ_none.all, RS_none.all)
merged_slope_none.all <- subset(merged_slope, merged_slope$Treatment=="None" | merged_slope$Treatment=="All")
ggplot(merged_slope_none.all, aes(x = Slope, y = CC_Wheat, fill= Treatment)) +
  geom_boxplot(width = 0.4) +
  xlab("Slope") +
  ylab(expression("Chlorophyll Content"~(mg/m^2))) +
  ggtitle("Box plot for slope") +
  theme(plot.title = element_text(hjust = 0.5,size = 16)) 

## Control versus plant competition
ggplot(K_Lime, aes(x = Group, y = CC_Wheat, fill= Treatment)) +
  geom_boxplot(width = 0.4, position = position_dodge(width=0.7)) +
  xlab("Plot") +
  ylab(expression("Chlorophyll Content"~(mg/m^2))) +
  ggtitle("Box plot for plant competition") +
  theme(plot.title = element_text(hjust = 0.5,size = 16)) 

## Nutrition (all versus none)
merged_nutrition <- rbind(K_Control_Unlime, R_Control_Unlime)
ggplot(merged_nutrition, aes(x = Name, y = CC_Wheat, fill= Treatment)) +
  geom_boxplot(width = 0.4) +
  xlab("Treatment") +
  ylab(expression("Chlorophyll Content"~(mg/m^2))) +
  ggtitle("Box plot for nutrition") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(hjust = 0.5,size = 16)) 

## Molluscs/Insect (Control only)
AllPlot_none.all <- subset(Dat, (Dat$Treatment=="None" | Dat$Treatment=="All") & Dat$Group=="Control")
ggplot(AllPlot_none.all, aes(x = Plot, y = CC_Wheat, fill= Name)) +
  geom_boxplot(width = 0.4, position = position_dodge(width=0.7)) +
  xlab("Plot") +
  ylab(expression("Chlorophyll Content"~(mg/m^2))) +
  ggtitle("Box plot for invertebrate exclusion") +
  labs(fill = "Treatment") +
  theme(plot.title = element_text(hjust = 0.5,size = 16)) 






#Scatter plot
group_names <- c("NDVI", "SAVI", "MSAVI", "EVI", "NDMI", "G NDVI")
library(ggplot2)
library(dplyr)

# CC & vegetation indices
#
Add_R <- function(dataframe,x,y,factor){
  
  cor <- data.frame()
  
  dataframe[,factor] <- as.factor(dataframe[,factor])
  
  lev <- levels(dataframe[,factor])
  
  for (i in c(1:length(lev))) {
    
    name <- lev[i]
    
    data <- dataframe[which(dataframe[,factor] == name),]
    
    lm <- summary(lm(data,formula = data[,y]~data[,x]))
    
    print(lm)
    print(lm$coefficients[2])
    print(lm$coefficients[8])
    print(lm$coefficients[2,4])
    P <- pf(lm$fstatistic[1L], lm$fstatistic[2L], lm$fstatistic[3L], lower.tail = FALSE)
    print(P)
    
    r_squared <- round(lm$r.squared,2)
    
    inter <- round(lm$coefficients[1],5)
    
    coefficients <- round(lm$coefficients[2],5)
    
    max_x <- max(data[,x]) 
    
    max_y <- min(data[,y])
    
    if(inter>0){
      
      eq <- substitute(""~R^2~"="~a~","~hat(y)~" = "~b%.%x+c~ "",list(a = r_squared,b = coefficients,c = inter))
      
    }
    
    else{
      
      inter <- abs(inter)
      
      eq <- substitute(""~R^2~"="~a~","~hat(y)~" = "~b%.%x-c~"",list(a = r_squared,b = coefficients,c = inter))
      
    }
    
    cor <- rbind(cor,cbind(rsqua = r_squared,coef = coefficients, pvalue = P, max_x = max_x,max_y = max_y,exp = ""))
    
    exp <- as.character(as.expression(eq))
    
    cor$exp[i] <- exp
    
    row.names(cor)[i] <- name
    
  }
  
  for (i in c(1:5)){
    
    cor[,i] <- as.numeric(cor[,i]) 
    
  }
  
  return(cor)
  
}






#April
## Create dataframe
dat_Apr <- subset(Landsat, Landsat$Time=="20230408")
dat_Apr <- data.frame(
  CC = rep(dat_Apr$CC, 6),
  VI = c(dat_Apr$NDVI, dat_Apr$SAVI, dat_Apr$MSAVI, dat_Apr$EVI, dat_Apr$NDMI, dat_Apr$`G NDVI`),
  group = factor(rep(group_names, each = 8))
)

df <- Add_R(dat_Apr,"CC","VI","group")
df

#Define the position of the formula
label_x <- c(231, 231, 231, 231, 231, 231)
label_y <- c(0.4, 0.55, 0.3, 0.15, 0.35, 0.5)

#Plot
ggplot(dat_Apr,aes(CC,VI))+  
  
  geom_point(size = 1,aes(color = group,shape = group,fill = group))+
  
  geom_smooth(aes(color = group,fill = group),method = "lm",level = 0.95,formula = y~x,linetype = 2,alpha = 0.2)+
  
  scale_color_manual(values = c("slateblue2","blue4","tomato2", "#79C267","#3AAFA9","#76C7C0"))+
  
  geom_text(data = df,aes(x = label_x, y = label_y, label = exp),vjust = 0,nudge_y = 0,nudge_x = 0,size = 5,parse = T,color = c("slateblue2","blue4","tomato2", "#79C267","#3AAFA9","#76C7C0"),)+   #注意parse = T参数，这个一定要等于T，才能把字符串类型的公式，以公式的形式表达。
  
  coord_cartesian(xlim = c(100,260),expand = F,ylim = c(0,1))+
  
  labs(title = "April") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  
  theme(panel.background = element_blank(),
        
        panel.grid.major.y = element_line(colour = "grey",linetype = 2),
        
        axis.line = element_line(colour = "black",size = rel(2),arrow = arrow(angle = 30,length = unit(0.1,"inches"))),
        
        axis.title.y = element_text(size = rel(1.7),hjust = 0.5),
        
        axis.title.x = element_text(size = rel(1.7),hjust = 0.5),
        
        axis.text.x = element_text(size = rel(1.5),hjust = 0.5),
        
        axis.text.y = element_text(hjust = 1,size = rel(1.5)),
        
        axis.ticks = element_line(size = rel(1.3)),
        
        plot.title = element_text(size = rel(1.8)),
        
        plot.margin = margin(15,9,9,30))


#May
## Create dataframe
dat_May <- subset(Landsat, Landsat$Time=="20230517")
dat_May <- data.frame(
  CC = rep(dat_May$CC, 6),
  VI = c(dat_May$NDVI, dat_May$SAVI, dat_May$MSAVI, dat_May$EVI, dat_May$NDMI, dat_May$`G NDVI`),
  group = factor(rep(group_names, each = 8))
)

df <- Add_R(dat_May,"CC","VI","group")
df

#Define the position of the formula
label_x <- c(231, 231, 231, 231, 231, 231)
label_y <- c(0.7, 0.75, 0.65, 0.4, 0.8, 0.6)

#Plot
ggplot(dat_May,aes(CC,VI))+  #和上面函数输入的要对应（df这个数据框）
  
  geom_point(size = 1,aes(color = group,shape = group,fill = group))+
  
  geom_smooth(aes(color = group,fill = group),method = "lm",level = 0.95,formula = y~x,linetype = 2,alpha = 0.2)+
  
  scale_color_manual(values = c("slateblue2","blue4","tomato2", "#79C267","#3AAFA9","#76C7C0"))+
  
  geom_text(data = df,aes(x = label_x, y = label_y, label = exp),vjust = 0,nudge_y = 0,nudge_x = 0,size = 5,parse = T,color = c("slateblue2","blue4","tomato2", "#79C267","#3AAFA9","#76C7C0"),)+   #注意parse = T参数，这个一定要等于T，才能把字符串类型的公式，以公式的形式表达。
  
  coord_cartesian(xlim = c(100,260),expand = F,ylim = c(0,1))+
  
  labs(title = "May") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  
  theme(panel.background = element_blank(),
        
        panel.grid.major.y = element_line(colour = "grey",linetype = 2),
        
        axis.line = element_line(colour = "black",size = rel(2),arrow = arrow(angle = 30,length = unit(0.1,"inches"))),
        
        axis.title.y = element_text(size = rel(1.7),hjust = 0.5),
        
        axis.title.x = element_text(size = rel(1.7),hjust = 0.5),
        
        axis.text.x = element_text(size = rel(1.5),hjust = 0.5),
        
        axis.text.y = element_text(hjust = 1,size = rel(1.5)),
        
        axis.ticks = element_line(size = rel(1.3)),
        
        plot.title = element_text(size = rel(1.8)),
        
        plot.margin = margin(15,9,9,30))



#June
## Create dataframe
dat_Jun <- subset(Landsat, Landsat$Time=="20230610")
dat_Jun <- data.frame(
  CC = rep(dat_Jun$CC, 6),
  VI = c(dat_Jun$NDVI, dat_Jun$SAVI, dat_Jun$MSAVI, dat_Jun$EVI, dat_Jun$NDMI, dat_Jun$`G NDVI`),
  group = factor(rep(group_names, each = 8))
)

df <- Add_R(dat_Jun,"CC","VI","group")
df

#Define the position of the formula
label_x <- c(231, 231, 231, 231, 231, 231)
label_y <- c(0.65, 0.8, 0.6, 0.33, 0.85, 0.55)

#Plot
ggplot(dat_Jun,aes(CC,VI))+ 
  
  geom_point(size = 1,aes(color = group,shape = group,fill = group))+
  
  geom_smooth(aes(color = group,fill = group),method = "lm",level = 0.95,formula = y~x,linetype = 2,alpha = 0.2)+
  
  scale_color_manual(values = c("slateblue2","blue4","tomato2", "#79C267","#3AAFA9","#76C7C0"))+
  
  geom_text(data = df,aes(x = label_x, y = label_y, label = exp),vjust = 0,nudge_y = 0,nudge_x = 0,size = 5,parse = T,color = c("slateblue2","blue4","tomato2", "#79C267","#3AAFA9","#76C7C0"),)+   #注意parse = T参数，这个一定要等于T，才能把字符串类型的公式，以公式的形式表达。
  
  coord_cartesian(xlim = c(100,260),expand = F,ylim = c(0,1))+
  
  labs(title = "June") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  
  theme(panel.background = element_blank(),
        
        panel.grid.major.y = element_line(colour = "grey",linetype = 2),
        
        axis.line = element_line(colour = "black",size = rel(2),arrow = arrow(angle = 30,length = unit(0.1,"inches"))),
        
        axis.title.y = element_text(size = rel(1.7),hjust = 0.5),
        
        axis.title.x = element_text(size = rel(1.7),hjust = 0.5),
        
        axis.text.x = element_text(size = rel(1.5),hjust = 0.5),
        
        axis.text.y = element_text(hjust = 1,size = rel(1.5)),
        
        axis.ticks = element_line(size = rel(1.3)),
        
        plot.title = element_text(size = rel(1.8)),
        
        plot.margin = margin(15,9,9,30))


#July
## Create dataframe
dat_Jul <- subset(Landsat, Landsat$Time=="20230720")
dat_Jul <- data.frame(
  CC = rep(dat_Jul$CC, 6),
  VI = c(dat_Jul$NDVI, dat_Jul$SAVI, dat_Jul$MSAVI, dat_Jul$EVI, dat_Jul$NDMI, dat_Jul$`G NDVI`),
  group = factor(rep(group_names, each = 8))
)

df <- Add_R(dat_Jul,"CC","VI","group")
df

#Define the position of the formula
label_x <- c(231, 231, 231, 231, 231, 231)
label_y <- c(0.5, 0.75, 0.4, 0.2, 0.7, 0.45)

#Plot
ggplot(dat_Jul,aes(CC,VI))+  
  
  geom_point(size = 1,aes(color = group,shape = group,fill = group))+
  
  geom_smooth(aes(color = group,fill = group),method = "lm",level = 0.95,formula = y~x,linetype = 2,alpha = 0.2)+
  
  scale_color_manual(values = c("slateblue2","blue4","tomato2", "#79C267","#3AAFA9","#76C7C0"))+
  
  geom_text(data = df,aes(x = label_x, y = label_y, label = exp),vjust = 0,nudge_y = 0,nudge_x = 0,size = 5,parse = T,color = c("slateblue2","blue4","tomato2", "#79C267","#3AAFA9","#76C7C0"),)+   #注意parse = T参数，这个一定要等于T，才能把字符串类型的公式，以公式的形式表达。
  
  coord_cartesian(xlim = c(100,260),expand = F,ylim = c(0,1))+
  
  labs(title = "July") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  
  theme(panel.background = element_blank(),
        
        panel.grid.major.y = element_line(colour = "grey",linetype = 2),
        
        axis.line = element_line(colour = "black",size = rel(2),arrow = arrow(angle = 30,length = unit(0.1,"inches"))),
        
        axis.title.y = element_text(size = rel(1.7),hjust = 0.5),
        
        axis.title.x = element_text(size = rel(1.7),hjust = 0.5),
        
        axis.text.x = element_text(size = rel(1.5),hjust = 0.5),
        
        axis.text.y = element_text(hjust = 1,size = rel(1.5)),
        
        axis.ticks = element_line(size = rel(1.3)),
        
        plot.title = element_text(size = rel(1.8)),
        
        plot.margin = margin(15,9,9,30))



#Time & VI
#L
L <- subset(Landsat, Landsat$Plot=="L")
L <- data.frame(
  Time = rep(L$Time, 6),
  VI = c(L$NDVI, L$SAVI, L$MSAVI, L$EVI, L$NDMI, L$`G NDVI`),
  group = factor(rep(group_names, each = 4))
)

df <- Add_R(L,"Time","VI","group")

df

#Define the position of the formula
label_x <- c(20230740, 20230740, 20230740, 20230740, 20230740, 20230740)
label_y <- c(1, 1.1, 1.2, 1.3, 1.4, 1.5)

#Plot
ggplot(L,aes(Time,VI))+  
  
  geom_point(size = 1,aes(color = group,shape = group,fill = group))+
  
  geom_smooth(aes(color = group,fill = group),method = "lm",level = 0.95,formula = y~x,linetype = 2,alpha = 0.2)+
  
  scale_color_manual(values = c("slateblue2","blue4","tomato2", "#79C267","#3AAFA9","#76C7C0"))+
  
  geom_text(data = df,aes(x = label_x, y = label_y, label = exp),vjust = 0,nudge_y = 0,nudge_x = 0,size = 5,parse = T,color = c("slateblue2","blue4","tomato2", "#79C267","#3AAFA9","#76C7C0"),)+   #注意parse = T参数，这个一定要等于T，才能把字符串类型的公式，以公式的形式表达。
  
  coord_cartesian(xlim = c(20230300,20230900), expand = T, ylim = c(-0.5,1.5))+
  
  labs(title = "L") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  
  scale_x_continuous(breaks = L$Time) +
  
  theme(panel.background = element_blank(),
        
        panel.grid.major.y = element_line(colour = "grey",linetype = 2),
        
        axis.line = element_line(colour = "black",size = rel(2),arrow = arrow(angle = 30,length = unit(0.1,"inches"))),
        
        axis.title.y = element_text(size = rel(1.7),hjust = 0.5),
        
        axis.title.x = element_text(size = rel(1.7),hjust = 0.5),
        
        axis.text.x = element_text(size = rel(1.5),hjust = 0.5),
        
        axis.text.y = element_text(hjust = 1,size = rel(1.5)),
        
        axis.ticks = element_line(size = rel(1.3)),
        
        plot.title = element_text(size = rel(1.8)),
        
        plot.margin = margin(15,9,9,30))

#K
K <- subset(Landsat, Landsat$Plot=="K")
K <- data.frame(
  Time = rep(K$Time, 6),
  VI = c(K$NDVI, K$SAVI, K$MSAVI, K$EVI, K$NDMI, K$`G NDVI`),
  group = factor(rep(group_names, each = 4))
)

df <- Add_R(K,"Time","VI","group")

df

#Define the position of the formula
label_x <- c(20230740, 20230740, 20230740, 20230740, 20230740, 20230740)
label_y <- c(1, 1.1, 1.2, 1.3, 1.4, 1.5)

#Plot
ggplot(K,aes(Time,VI))+
  
  geom_point(size = 1,aes(color = group,shape = group,fill = group))+
  
  geom_smooth(aes(color = group,fill = group),method = "lm",level = 0.95,formula = y~x,linetype = 2,alpha = 0.2)+
  
  scale_color_manual(values = c("slateblue2","blue4","tomato2", "#79C267","#3AAFA9","#76C7C0"))+
  
  geom_text(data = df,aes(x = label_x, y = label_y, label = exp),vjust = 0,nudge_y = 0,nudge_x = 0,size = 5,parse = T,color = c("slateblue2","blue4","tomato2", "#79C267","#3AAFA9","#76C7C0"),)+   #注意parse = T参数，这个一定要等于T，才能把字符串类型的公式，以公式的形式表达。
  
  coord_cartesian(xlim = c(20230300,20230900), expand = T, ylim = c(-0.5,1.5))+
  
  labs(title = "K") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  
  scale_x_continuous(breaks = K$Time) +
  
  theme(panel.background = element_blank(),
        
        panel.grid.major.y = element_line(colour = "grey",linetype = 2),
        
        axis.line = element_line(colour = "black",size = rel(2),arrow = arrow(angle = 30,length = unit(0.1,"inches"))),
        
        axis.title.y = element_text(size = rel(1.7),hjust = 0.5),
        
        axis.title.x = element_text(size = rel(1.7),hjust = 0.5),
        
        axis.text.x = element_text(size = rel(1.5),hjust = 0.5),
        
        axis.text.y = element_text(hjust = 1,size = rel(1.5)),
        
        axis.ticks = element_line(size = rel(1.3)),
        
        plot.title = element_text(size = rel(1.8)),
        
        plot.margin = margin(15,9,9,30))

#N
N <- subset(Landsat, Landsat$Plot=="N")
N <- data.frame(
  Time = rep(N$Time, 6),
  VI = c(N$NDVI, N$SAVI, N$MSAVI, N$EVI, N$NDMI, N$`G NDVI`),
  group = factor(rep(group_names, each = 4))
)

df <- Add_R(N,"Time","VI","group")

df

#Define the position of the formula
label_x <- c(20230740, 20230740, 20230740, 20230740, 20230740, 20230740)
label_y <- c(1, 1.1, 1.2, 1.3, 1.4, 1.5)

#Plot
ggplot(N,aes(Time,VI))+ 
  
  geom_point(size = 1,aes(color = group,shape = group,fill = group))+
  
  geom_smooth(aes(color = group,fill = group),method = "lm",level = 0.95,formula = y~x,linetype = 2,alpha = 0.2)+
  
  scale_color_manual(values = c("slateblue2","blue4","tomato2", "#79C267","#3AAFA9","#76C7C0"))+
  
  geom_text(data = df,aes(x = label_x, y = label_y, label = exp),vjust = 0,nudge_y = 0,nudge_x = 0,size = 5,parse = T,color = c("slateblue2","blue4","tomato2", "#79C267","#3AAFA9","#76C7C0"),)+   #注意parse = T参数，这个一定要等于T，才能把字符串类型的公式，以公式的形式表达。
  
  coord_cartesian(xlim = c(20230300,20230900), expand = T, ylim = c(-0.5,1.5))+
  
  labs(title = "N") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  
  scale_x_continuous(breaks = N$Time) +
  
  theme(panel.background = element_blank(),
        
        panel.grid.major.y = element_line(colour = "grey",linetype = 2),
        
        axis.line = element_line(colour = "black",size = rel(2),arrow = arrow(angle = 30,length = unit(0.1,"inches"))),
        
        axis.title.y = element_text(size = rel(1.7),hjust = 0.5),
        
        axis.title.x = element_text(size = rel(1.7),hjust = 0.5),
        
        axis.text.x = element_text(size = rel(1.5),hjust = 0.5),
        
        axis.text.y = element_text(hjust = 1,size = rel(1.5)),
        
        axis.ticks = element_line(size = rel(1.3)),
        
        plot.title = element_text(size = rel(1.8)),
        
        plot.margin = margin(15,9,9,30))



#M
M <- subset(Landsat, Landsat$Plot=="M")
M <- data.frame(
  Time = rep(M$Time, 6),
  VI = c(M$NDVI, M$SAVI, M$MSAVI, M$EVI, M$NDMI, M$`G NDVI`),
  group = factor(rep(group_names, each = 4))
)

df <- Add_R(M,"Time","VI","group")

df

#Define the position of the formula
label_x <- c(20230740, 20230740, 20230740, 20230740, 20230740, 20230740)
label_y <- c(1, 1.1, 1.2, 1.3, 1.4, 1.5)

#Plot
ggplot(M,aes(Time,VI))+ 
  
  geom_point(size = 1,aes(color = group,shape = group,fill = group))+
  
  geom_smooth(aes(color = group,fill = group),method = "lm",level = 0.95,formula = y~x,linetype = 2,alpha = 0.2)+
  
  scale_color_manual(values = c("slateblue2","blue4","tomato2", "#79C267","#3AAFA9","#76C7C0"))+
  
  geom_text(data = df,aes(x = label_x, y = label_y, label = exp),vjust = 0,nudge_y = 0,nudge_x = 0,size = 5,parse = T,color = c("slateblue2","blue4","tomato2", "#79C267","#3AAFA9","#76C7C0"),)+   #注意parse = T参数，这个一定要等于T，才能把字符串类型的公式，以公式的形式表达。
  
  coord_cartesian(xlim = c(20230300,20230900), expand = T, ylim = c(-0.5,1.5))+
  
  labs(title = "M") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  
  scale_x_continuous(breaks = M$Time) +
  
  theme(panel.background = element_blank(),
        
        panel.grid.major.y = element_line(colour = "grey",linetype = 2),
        
        axis.line = element_line(colour = "black",size = rel(2),arrow = arrow(angle = 30,length = unit(0.1,"inches"))),
        
        axis.title.y = element_text(size = rel(1.7),hjust = 0.5),
        
        axis.title.x = element_text(size = rel(1.7),hjust = 0.5),
        
        axis.text.x = element_text(size = rel(1.5),hjust = 0.5),
        
        axis.text.y = element_text(hjust = 1,size = rel(1.5)),
        
        axis.ticks = element_line(size = rel(1.3)),
        
        plot.title = element_text(size = rel(1.8)),
        
        plot.margin = margin(15,9,9,30))


#P
P <- subset(Landsat, Landsat$Plot=="P")
P <- data.frame(
  Time = rep(P$Time, 6),
  VI = c(P$NDVI, P$SAVI, P$MSAVI, P$EVI, P$NDMI, P$`G NDVI`),
  group = factor(rep(group_names, each = 4))
)

df <- Add_R(P,"Time","VI","group")

df

#Define the position of the formula
label_x <- c(20230740, 20230740, 20230740, 20230740, 20230740, 20230740)
label_y <- c(1, 1.1, 1.2, 1.3, 1.4, 1.5)

#Plot
ggplot(P,aes(Time,VI))+
  
  geom_point(size = 1,aes(color = group,shape = group,fill = group))+
  
  geom_smooth(aes(color = group,fill = group),method = "lm",level = 0.95,formula = y~x,linetype = 2,alpha = 0.2)+
  
  scale_color_manual(values = c("slateblue2","blue4","tomato2", "#79C267","#3AAFA9","#76C7C0"))+
  
  geom_text(data = df,aes(x = label_x, y = label_y, label = exp),vjust = 0,nudge_y = 0,nudge_x = 0,size = 5,parse = T,color = c("slateblue2","blue4","tomato2", "#79C267","#3AAFA9","#76C7C0"),)+   #注意parse = T参数，这个一定要等于T，才能把字符串类型的公式，以公式的形式表达。
  
  coord_cartesian(xlim = c(20230300,20230900), expand = T, ylim = c(-0.5,1.5))+
  
  labs(title = "P") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  
  scale_x_continuous(breaks = P$Time) +
  
  theme(panel.background = element_blank(),
        
        panel.grid.major.y = element_line(colour = "grey",linetype = 2),
        
        axis.line = element_line(colour = "black",size = rel(2),arrow = arrow(angle = 30,length = unit(0.1,"inches"))),
        
        axis.title.y = element_text(size = rel(1.7),hjust = 0.5),
        
        axis.title.x = element_text(size = rel(1.7),hjust = 0.5),
        
        axis.text.x = element_text(size = rel(1.5),hjust = 0.5),
        
        axis.text.y = element_text(hjust = 1,size = rel(1.5)),
        
        axis.ticks = element_line(size = rel(1.3)),
        
        plot.title = element_text(size = rel(1.8)),
        
        plot.margin = margin(15,9,9,30))



#S
S <- subset(Landsat, Landsat$Plot=="S")
S <- data.frame(
  Time = rep(S$Time, 6),
  VI = c(S$NDVI, S$SAVI, S$MSAVI, S$EVI, S$NDMI, S$`G NDVI`),
  group = factor(rep(group_names, each = 4))
)

df <- Add_R(S,"Time","VI","group")

df

#Define the position of the formula
label_x <- c(20230740, 20230740, 20230740, 20230740, 20230740, 20230740)
label_y <- c(1, 1.1, 1.2, 1.3, 1.4, 1.5)

#Plot
ggplot(S,aes(Time,VI))+
  
  geom_point(size = 1,aes(color = group,shape = group,fill = group))+
  
  geom_smooth(aes(color = group,fill = group),method = "lm",level = 0.95,formula = y~x,linetype = 2,alpha = 0.2)+
  
  scale_color_manual(values = c("slateblue2","blue4","tomato2", "#79C267","#3AAFA9","#76C7C0"))+
  
  geom_text(data = df,aes(x = label_x, y = label_y, label = exp),vjust = 0,nudge_y = 0,nudge_x = 0,size = 5,parse = T,color = c("slateblue2","blue4","tomato2", "#79C267","#3AAFA9","#76C7C0"),)+   #注意parse = T参数，这个一定要等于T，才能把字符串类型的公式，以公式的形式表达。
  
  coord_cartesian(xlim = c(20230300,20230900), expand = T, ylim = c(-0.5,1.5))+
  
  labs(title = "S") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  
  scale_x_continuous(breaks = S$Time) +
  
  theme(panel.background = element_blank(),
        
        panel.grid.major.y = element_line(colour = "grey",linetype = 2),
        
        axis.line = element_line(colour = "black",size = rel(2),arrow = arrow(angle = 30,length = unit(0.1,"inches"))),
        
        axis.title.y = element_text(size = rel(1.7),hjust = 0.5),
        
        axis.title.x = element_text(size = rel(1.7),hjust = 0.5),
        
        axis.text.x = element_text(size = rel(1.5),hjust = 0.5),
        
        axis.text.y = element_text(hjust = 1,size = rel(1.5)),
        
        axis.ticks = element_line(size = rel(1.3)),
        
        plot.title = element_text(size = rel(1.8)),
        
        plot.margin = margin(15,9,9,30))




#R
R <- subset(Landsat, Landsat$Plot=="R")
R <- data.frame(
  Time = rep(R$Time, 6),
  VI = c(R$NDVI, R$SAVI, R$MSAVI, R$EVI, R$NDMI, R$`G NDVI`),
  group = factor(rep(group_names, each = 4))
)

df <- Add_R(R,"Time","VI","group")

df

#Define the position of the formula
label_x <- c(20230740, 20230740, 20230740, 20230740, 20230740, 20230740)
label_y <- c(1, 1.1, 1.2, 1.3, 1.4, 1.5)

#Plot
ggplot(R,aes(Time,VI))+
  
  geom_point(size = 1,aes(color = group,shape = group,fill = group))+
  
  geom_smooth(aes(color = group,fill = group),method = "lm",level = 0.95,formula = y~x,linetype = 2,alpha = 0.2)+
  
  scale_color_manual(values = c("slateblue2","blue4","tomato2", "#79C267","#3AAFA9","#76C7C0"))+
  
  geom_text(data = df,aes(x = label_x, y = label_y, label = exp),vjust = 0,nudge_y = 0,nudge_x = 0,size = 5,parse = T,color = c("slateblue2","blue4","tomato2", "#79C267","#3AAFA9","#76C7C0"),)+   #注意parse = T参数，这个一定要等于T，才能把字符串类型的公式，以公式的形式表达。
  
  coord_cartesian(xlim = c(20230300,20230900), expand = T, ylim = c(-0.5,1.5))+
  
  labs(title = "R") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  
  scale_x_continuous(breaks = R$Time) +
  
  theme(panel.background = element_blank(),
        
        panel.grid.major.y = element_line(colour = "grey",linetype = 2),
        
        axis.line = element_line(colour = "black",size = rel(2),arrow = arrow(angle = 30,length = unit(0.1,"inches"))),
        
        axis.title.y = element_text(size = rel(1.7),hjust = 0.5),
        
        axis.title.x = element_text(size = rel(1.7),hjust = 0.5),
        
        axis.text.x = element_text(size = rel(1.5),hjust = 0.5),
        
        axis.text.y = element_text(hjust = 1,size = rel(1.5)),
        
        axis.ticks = element_line(size = rel(1.3)),
        
        plot.title = element_text(size = rel(1.8)),
        
        plot.margin = margin(15,9,9,30))



#Q
Q <- subset(Landsat, Landsat$Plot=="Q")
Q <- data.frame(
  Time = rep(Q$Time, 6),
  VI = c(Q$NDVI, Q$SAVI, Q$MSAVI, Q$EVI, Q$NDMI, Q$`G NDVI`),
  group = factor(rep(group_names, each = 4))
)

df <- Add_R(Q,"Time","VI","group")

df

#Define the position of the formula
label_x <- c(20230740, 20230740, 20230740, 20230740, 20230740, 20230740)
label_y <- c(1, 1.1, 1.2, 1.3, 1.4, 1.5)

#Plot
ggplot(Q,aes(Time,VI))+  
  
  geom_point(size = 1,aes(color = group,shape = group,fill = group))+
  
  geom_smooth(aes(color = group,fill = group),method = "lm",level = 0.95,formula = y~x,linetype = 2,alpha = 0.2)+
  
  scale_color_manual(values = c("slateblue2","blue4","tomato2", "#79C267","#3AAFA9","#76C7C0"))+
  
  geom_text(data = df,aes(x = label_x, y = label_y, label = exp),vjust = 0,nudge_y = 0,nudge_x = 0,size = 5,parse = T,color = c("slateblue2","blue4","tomato2", "#79C267","#3AAFA9","#76C7C0"),)+   #注意parse = T参数，这个一定要等于T，才能把字符串类型的公式，以公式的形式表达。
  
  coord_cartesian(xlim = c(20230300,20230900), expand = T, ylim = c(-0.5,1.5))+
  
  labs(title = "Q") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  
  scale_x_continuous(breaks = Q$Time) +
  
  theme(panel.background = element_blank(),
        
        panel.grid.major.y = element_line(colour = "grey",linetype = 2),
        
        axis.line = element_line(colour = "black",size = rel(2),arrow = arrow(angle = 30,length = unit(0.1,"inches"))),
        
        axis.title.y = element_text(size = rel(1.7),hjust = 0.5),
        
        axis.title.x = element_text(size = rel(1.7),hjust = 0.5),
        
        axis.text.x = element_text(size = rel(1.5),hjust = 0.5),
        
        axis.text.y = element_text(hjust = 1,size = rel(1.5)),
        
        axis.ticks = element_line(size = rel(1.3)),
        
        plot.title = element_text(size = rel(1.8)),
        
        plot.margin = margin(15,9,9,30))





#Non-linear regression
Add_R_log <- function(dataframe,x,y,factor){
  
  cor <- data.frame()
  
  dataframe[,factor] <- as.factor(dataframe[,factor])
  
  lev <- levels(dataframe[,factor])
  
  for (i in c(1:length(lev))) {
    
    name <- lev[i]
    
    data <- dataframe[which(dataframe[,factor] == name),]
    
    # Fitting
    lm_model <- lm(data[,y] ~ log(data[,x]));
    model_summary <- summary(lm_model)
    print(model_summary)
    
    r_squared <- round(model_summary$r.squared,2)
    
    max_x <- max(data[,x]) 
    
    max_y <- min(data[,y])
    
    # get parameters
    model_intercept <- round(lm_model$coefficients[1],2)
    model_slope <- round(lm_model$coefficients[2],2)
    # Generating formula
    if (model_slope > 0) {
      eq <- substitute(""~R^2~"="~a~","~hat(y)~" = "~b~"+"~c%.%log(x)~"",list(a = r_squared,b = model_intercept,c = model_slope))
    } else {
      model_slope <- abs(model_slope)
      eq <- substitute(""~R^2~"="~a~","~hat(y)~" = "~b~"-"~c%.%log(x)~"",list(a = r_squared,b = model_intercept,c = model_slope))
    }
    
    #p value
    p_value <- lm_model$`Pr(>|t|)`

    cor <- rbind(cor,cbind(rsqua = r_squared,intercept = model_intercept,slope = model_slope,p = p_value,max_y = max_y,exp = ""))

    exp <- as.character(as.expression(eq))

    cor$exp[i] <- exp
    
    row.names(cor)[i] <- name
    
  }
  
  for (i in c(1:5)){
    
    cor[,i] <- as.numeric(cor[,i]) 
    
  }
  
  return(cor)
  
}


no_linear_regression_plot_function_log <- function(dataframe, x_name, y_name, label_x, label_y, label_size, title, x_range, y_range, colors) {
  df <- Add_R_log(dataframe,x_name, y_name, "group")
  print('df -------')
  print(df)
  
  if (length(label_x) == 0) {
    label_x <- df$max_x
  }
  if (length(label_y) == 0) {
    label_y <- df$max_y
  }
  
  print(label_x)
  print(label_y)
  
  print(label_x)
  
  group_length <- length(unique(dataframe$group));
  print(group_length)
  
  if (length(colors) == 0) {
    colors <- hcl(h = seq(15, 375, length.out = group_length), c = 100, l = 65)
  }
  print(colors)
  
  shape_values <- seq(0, 25, length.out = group_length)
  
  Plot
  ggplot(dataframe,aes_string(x_name, y_name)) +  
    
    geom_point(size = 1,aes(color = group,shape = group,fill = group))+
    
    scale_shape_manual(values = shape_values) +
    
    geom_smooth(aes(color = group,fill = group),method = "lm",level = 0.95,formula = y ~ log(x),linetype = 2,alpha = 0.2)+
    
    scale_color_manual(values = colors)+
    
    geom_text(data = df,aes(x = label_x, y = label_y, label = exp),vjust = 0,nudge_y = 0,nudge_x = 0,size = label_size,parse = T, color = colors)+   #注意parse = T参数，这个一定要等于T，才能把字符串类型的公式，以公式的形式表达。
    
    coord_cartesian(xlim = x_range,expand = F,ylim = y_range)+
    
    labs(title = title) +
    
    theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
    
    theme(panel.background = element_blank(),
          
          panel.grid.major.y = element_line(colour = "grey",linetype = 2),
          
          axis.line = element_line(colour = "black",size = rel(2),arrow = arrow(angle = 30,length = unit(0.1,"inches"))),
          
          axis.title.y = element_text(size = rel(1.7),hjust = 0.5),
          
          axis.title.x = element_text(size = rel(1.7),hjust = 0.5),
          
          axis.text.x = element_text(size = rel(1.5),hjust = 0.5),
          
          axis.text.y = element_text(hjust = 1,size = rel(1.5)),
          
          axis.ticks = element_line(size = rel(1.3)),
          
          plot.title = element_text(size = rel(1.8)),
          
          plot.margin = margin(15,9,9,30))
  
}



## Chlorophyll Content and Vegetation Indices
# April
dat_Apr <- subset(Landsat, Landsat$Time=="20230408")
dat_Apr <- data.frame(
  CC = rep(dat_Apr$CC, 6),
  VI = c(dat_Apr$NDVI, dat_Apr$SAVI, dat_Apr$MSAVI, dat_Apr$EVI, dat_Apr$NDMI, dat_Apr$`G NDVI`),
  group = factor(rep(group_names, each = 8))
)
dat_x = "CC"
dat_y = "VI"
dat_label_x <- c(231, 231, 231, 231, 231, 231)
dat_label_y <- c(0.4, 0.55, 0.3, 0.15, 0.35, 0.5)
label_size <- 2
plotTiltle <- "April";
x_range <- c(100,260);
y_range <- c(0,1);
colors <- c("slateblue2","blue4","tomato2", "#79C267","#3AAFA9","#76C7C0");
no_linear_regression_plot_function_log(dat_Apr, dat_x, dat_y, dat_label_x, dat_label_y, label_size, plotTiltle, x_range, y_range, colors)

# May
dat_May <- subset(Landsat, Landsat$Time=="20230517")
dat_May <- data.frame(
  CC = rep(dat_May$CC, 6),
  VI = c(dat_May$NDVI, dat_May$SAVI, dat_May$MSAVI, dat_May$EVI, dat_May$NDMI, dat_May$`G NDVI`),
  group = factor(rep(group_names, each = 8))
)
dat_x = "CC"
dat_y = "VI"
dat_label_x <- c(231, 231, 231, 231, 231, 231)
dat_label_y <- c(0.7, 0.75, 0.65, 0.4, 0.8, 0.6)
label_size <- 2
plotTiltle <- "May";
x_range <- c(100,260);
y_range <- c(0,1);
colors <- c("slateblue2","blue4","tomato2", "#79C267","#3AAFA9","#76C7C0");
no_linear_regression_plot_function_log(dat_May, dat_x, dat_y, dat_label_x, dat_label_y, label_size, plotTiltle, x_range, y_range, colors)

# June
dat_Jun <- subset(Landsat, Landsat$Time=="20230610")
dat_Jun <- data.frame(
  CC = rep(dat_Jun$CC, 6),
  VI = c(dat_Jun$NDVI, dat_Jun$SAVI, dat_Jun$MSAVI, dat_Jun$EVI, dat_Jun$NDMI, dat_Jun$`G NDVI`),
  group = factor(rep(group_names, each = 8))
)
dat_x = "CC"
dat_y = "VI"
dat_label_x <- c(231, 231, 231, 231, 231, 231)
dat_label_y <- c(0.65, 0.8, 0.6, 0.33, 0.85, 0.55)
label_size <- 2
plotTiltle <- "June";
x_range <- c(100,260);
y_range <- c(0,1);
colors <- c("slateblue2","blue4","tomato2", "#79C267","#3AAFA9","#76C7C0");
no_linear_regression_plot_function_log(dat_Jun, dat_x, dat_y, dat_label_x, dat_label_y, label_size, plotTiltle, x_range, y_range, colors)

# Jul
dat_Jul <- subset(Landsat, Landsat$Time=="20230720")
dat_Jul <- data.frame(
  CC = rep(dat_Jul$CC, 6),
  VI = c(dat_Jul$NDVI, dat_Jul$SAVI, dat_Jul$MSAVI, dat_Jul$EVI, dat_Jul$NDMI, dat_Jul$`G NDVI`),
  group = factor(rep(group_names, each = 8))
)
dat_x = "CC"
dat_y = "VI"
dat_label_x <- c(231, 231, 231, 231, 231, 231)
dat_label_y <- c(0.5, 0.75, 0.4, 0.2, 0.7, 0.45)
label_size <- 2
plotTiltle <- "July";
x_range <- c(100,260);
y_range <- c(0,1);
colors <- c("slateblue2","blue4","tomato2", "#79C267","#3AAFA9","#76C7C0");
no_linear_regression_plot_function_log(dat_Jul, dat_x, dat_y, dat_label_x, dat_label_y, label_size, plotTiltle, x_range, y_range, colors)

print(no_linear_regression_plot_function_log)




#Validation
#CC and Plant Height
# Create a scatter plot
plot(Validation$CC_May2023, Validation$`Plant height (cm)`, main="CC and Plant Height", 
     xlab = "CC", ylab = "Plant height", xlim = c(0, 250), ylim = c(0, 150))

# Add 1:1 line
abline(a = 0, b = 1, col = "red", lty = 2)

# Perform linear regression
fit <- lm(Validation$`Plant height (cm)` ~ Validation$CC_May2023)

# Get statistical information about regression results
fit_summary <- summary(fit)

# The fitting formula and R-square value were extracted
formula_text <- as.character(as.expression(fit$call$formula))
r_squared <- fit_summary$r.squared

# Add fit formulas and R-square values to the graph
text(0.5, 11, paste("Fitted Equation: ", formula_text), adj = 0)
text(0.5, 10.5, paste("R-squared: ", round(r_squared, 3)), adj = 0)

# Display graphics
legend("topleft", legend = "1:1 Line", col = "red", lty = 2)



#CC and SAVI Predicted CC
plot(Validation$CC_May2023, Validation$`SAVI Predicted CC`, main="SAVI", 
     xlab = "CC", ylab = "SAVI Predicted CC", xlim = c(50, 250), ylim = c(50, 250))

abline(a = 0, b = 1, col = "red", lty = 2)

fit <- lm(Validation$`SAVI Predicted CC` ~ Validation$CC_May2023)

fit_summary <- summary(fit)

intercept <- coef(fit)[1]
slope <- coef(fit)[2]

linear_formula <- paste("y =", round(slope, 2), " * x +", round(intercept, 2))

#RMSE
rmse <- sqrt(mean((Validation$`SAVI Predicted CC` - predict(fit, newdata = Validation))^2))

r_squared <- fit_summary$r.squared
p_value <- fit_summary$coefficients[2, 4]

text(50, 250, linear_formula, adj = 0, cex = 1.5)
text(50, 235, paste("p-value: ", round(p_value, 3)), adj = 0, cex = 1.5) 
text(50, 220, paste("R-squared: ", round(r_squared, 3)), adj = 0, cex = 1.5)
text(50, 205, paste("RMSE: ", round(rmse, 3)), adj = 0, cex = 1.5)

legend("bottomright", legend = "1:1 Line", col = "red", lty = 2)


#CC and MSAVI Predicted CC
plot(Validation$CC_May2023, Validation$`MSAVI Predicted CC`, main="MSAVI", 
     xlab = "CC", ylab = "MSAVI Predicted CC", xlim = c(50, 250), ylim = c(50, 250))

abline(a = 0, b = 1, col = "red", lty = 2)

fit <- lm(Validation$`MSAVI Predicted CC` ~ Validation$CC_May2023)

fit_summary <- summary(fit)

intercept <- coef(fit)[1]
slope <- coef(fit)[2]

linear_formula <- paste("y =", round(slope, 2), " * x +", round(intercept, 2))

#RMSE
rmse <- sqrt(mean((Validation$`MSAVI Predicted CC` - predict(fit, newdata = Validation))^2))

r_squared <- fit_summary$r.squared
p_value <- fit_summary$coefficients[2, 4]

text(50, 250, linear_formula, adj = 0, cex = 1.5) 
text(50, 235, paste("p-value: ", round(p_value, 3)), adj = 0, cex = 1.5) 
text(50, 220, paste("R-squared: ", round(r_squared, 3)), adj = 0, cex = 1.5)
text(50, 205, paste("RMSE: ", round(rmse, 3)), adj = 0, cex = 1.5)

legend("bottomright", legend = "1:1 Line", col = "red", lty = 2)

#CC and EVI Predicted CC
plot(Validation$CC_May2023, Validation$`EVI Predicted CC`, main="EVI", 
     xlab = "CC", ylab = "EVI Predicted CC", xlim = c(50, 250), ylim = c(50, 250))

abline(a = 0, b = 1, col = "red", lty = 2)

fit <- lm(Validation$`EVI Predicted CC` ~ Validation$CC_May2023)

fit_summary <- summary(fit)

intercept <- coef(fit)[1]
slope <- coef(fit)[2]

linear_formula <- paste("y =", round(slope, 2), " * x +", round(intercept, 2))

#RMSE
rmse <- sqrt(mean((Validation$`EVI Predicted CC` - predict(fit, newdata = Validation))^2))

r_squared <- fit_summary$r.squared
p_value <- fit_summary$coefficients[2, 4]

text(50, 250, linear_formula, adj = 0, cex = 1.5)
text(50, 235, paste("p-value: ", round(p_value, 3)), adj = 0, cex = 1.5)
text(50, 220, paste("R-squared: ", round(r_squared, 3)), adj = 0, cex = 1.5) 
text(50, 205, paste("RMSE: ", round(rmse, 3)), adj = 0, cex = 1.5)

legend("bottomright", legend = "1:1 Line", col = "red", lty = 2)