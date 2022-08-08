library(ggplot2)
library(dplyr)
library(splitstackshape)
library(ggpubr)


project_df1 <- read.csv("project.csv", T)
project_df2 <- subset(project_df1, select = -c(Timestamp))
colnames(project_df2) <- c("Name","sex", "agegroup", 
                           "int_inv", "int_exinv", "pref_inv", 
                           "pref_linv", "per_linv", "pref_minv", "per_minv", 
                           "pref_hinv", "per_hinv", "goal_pref", "sip", "fin_lite", 
                           "gamb", "per_inv")

##Plot 1
#Bar Plot showing preferrence in types of investment. 

dfA1 <- cSplit(project_df2, "pref_inv", sep = ";")
dfA11 <- subset(dfA1,select = c(agegroup,pref_inv_1))
dfA12 <- subset(dfA1,select = c(agegroup,pref_inv_2))
dfA13 <- subset(dfA1,select = c(agegroup,pref_inv_3))
dfA12 <- na.omit(dfA12)
dfA13 <- na.omit(dfA13)
colnames(dfA11) <- c("Age","Preference")
colnames(dfA12) <- c("Age","Preference")
colnames(dfA13) <- c("Age","Preference")
dfA <- rbind(dfA11,dfA12,dfA13)
table4 <- table(dfA$Age, dfA$Preference)
project_df5 <- as.data.frame(table4)
colnames(project_df5) <- c("Age", "Preference", "Frequency")

z4 <- ggplot(project_df5, aes(x= Age, y = Frequency, fill = Preference)) +
  geom_bar(position = "fill", stat = "identity") +
  labs(y= "Relative Frequency", x= "Age") +
  theme(legend.title = element_blank()) +
  scale_fill_brewer(palette = "Oranges", labels = c("High risks", "Low risks", "Moderate risks")) + 
  theme_minimal();z4

##Plot2
#Stacked Bar Plot showing different types of investment preferred on different age.

project_df3 <- subset(project_df2, select = c("agegroup", "pref_inv"))
table1 <- table(project_df3$agegroup, project_df3$pref_inv)
mat1 <- as.data.frame(table1)
colnames(mat1) <- c("age","preferred_investment", "frequency")
table2 <-table(project_df2$pref_inv)


z1 <- ggplot(mat1, aes(x = age, y = frequency, fill = preferred_investment))+
  geom_bar(position="fill", stat="identity") +
  scale_fill_brewer(palette="Blues")+
  labs(y = "Relative Frequency", x = "Age") +
  theme_minimal()+
  theme(legend.title = element_blank());z1


##Plot 3
#Pie Chart showing interest and interest in exploration in investments on basis of different age group.

project_df4 <- subset(project_df2, select = c("agegroup","int_inv", "int_exinv"))
table3 <- table(project_df4$agegroup, project_df4$int_inv, project_df4$int_exinv)
mat2 <- as.data.frame(table3)
colnames(mat2) <- c("age", "interest_in_investment", "exploration_in_investment", "frequency")

pie1 <-  ggplot(mat2, aes(x = age, y = frequency, fill = interest_in_investment))+
  geom_bar(position = "fill", stat = "identity") +
  coord_polar("y", start = 0) +
  labs(y = "Relative Frequency", x = "Age") +
  ggtitle(" Interest in investments of different age group" ) +
  theme(legend.title = element_blank(),  legend.position = "none") +
  scale_fill_brewer(palette="Greens", labels = c("Not interested at all", "Somewhat least interested", "Neutral", 
                                                 "Somewhat interested", "Very interested")); pie1

pie2 <-  ggplot(mat2, aes(x = age, y = frequency, fill = exploration_in_investment))+
  geom_bar(position = "fill", stat = "identity") +
  coord_polar("y", start = 0) +
  labs(y = "Relative Frequency", x = "Age") +
  ggtitle("Interest in exploration of investments of different age group" ) +
  theme(legend.title = element_blank(), legend.position = "none") +
  scale_fill_brewer(palette="Greens", labels = c("Not interested at all", "Somewhat least interested", "Neutral", 
                                                 "Somewhat interested", "Very interested")); pie2

ggarrange(pie1, pie2, common.legend = T)


##Plot 4
#Pie Charts for interest vs exploration in investment of different agegroups.

project_df4 %>% filter(agegroup == "18 - 25") %>% count(int_inv) -> project_df4.11
project_df4 %>% filter(agegroup == "18 - 25") %>% count(int_exinv) -> project_df4.12
project_df4.1 <- cbind(project_df4.11,project_df4.12)
colnames(project_df4.1) <- c("Interest_in_Investment","Frequency_I","Interest_in_Exploration","Frequency_E")

pie_int1 <- ggplot(project_df4.1, aes(x="", y=Frequency_I, fill = Interest_in_Investment))+
  geom_bar(stat="identity", width=1) + 
  coord_polar("y", start=0)+
  labs(x="", y="")+
  ggtitle("Interest in Investment (18-25)")+
  theme_classic()+
  theme(axis.line = element_blank(), axis.text = element_blank(), 
        axis.ticks = element_blank(), plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(), legend.position = "none")+
  geom_text(aes(label=paste0(round((Frequency_I/sum(Frequency_I))*100), "%")),
            position = position_stack(vjust=0.5))

pie_exint1 <- ggplot(project_df4.1, aes(x="", y=Frequency_E, fill = Interest_in_Exploration))+
  geom_bar(stat="identity", width=1) + 
  coord_polar("y", start=0)+
  labs(x="", y="")+
  ggtitle("Interest in Exploration (18-25)")+
  theme_classic()+
  theme(axis.line = element_blank(), axis.text = element_blank(), 
        axis.ticks = element_blank(), plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(), legend.position = "none")+
  geom_text(aes(label=paste0(round((Frequency_E/sum(Frequency_E))*100), "%")), 
            position = position_stack(vjust=0.5))


project_df4 %>% filter(agegroup == "26 - 35") %>% count(int_inv) -> project_df4.21
project_df4 %>% filter(agegroup == "26 - 35") %>%  count(int_exinv) -> project_df4.22
project_df4.2 <- cbind(project_df4.21,project_df4.22)
colnames(project_df4.2) <- c("Interest_in_Investment","Frequency_I","Interest_in_Exploration","Frequency_E")


pie_int2 <- ggplot(project_df4.2, aes(x="", y=Frequency_I, fill = Interest_in_Investment))+
  geom_bar(stat="identity", width=1) + 
  coord_polar("y", start=0)+
  labs(x="", y="")+
  ggtitle("Interest in Investment (26-35)")+
  theme_classic()+
  theme(axis.line = element_blank(), axis.text = element_blank(), 
        axis.ticks = element_blank(), plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(), legend.position = "none")+
  geom_text(aes(label=paste0(round((Frequency_I/sum(Frequency_I))*100), "%")),
            position = position_stack(vjust=0.5))
pie_exint2 <- ggplot(project_df4.2, aes(x="", y=Frequency_E, fill = Interest_in_Exploration))+
  geom_bar(stat="identity", width=1) + 
  coord_polar("y", start=0)+
  labs(x="", y="")+
  ggtitle("Interest in Exploration (26-35)")+
  theme_classic()+
  theme(axis.line = element_blank(), axis.text = element_blank(), 
        axis.ticks = element_blank(), plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(), legend.position = "none")+
  geom_text(aes(label=paste0(round((Frequency_E/sum(Frequency_E))*100), "%")), 
            position = position_stack(vjust=0.5))


project_df4 %>% filter(agegroup == "36 - 45") %>% count(int_inv) -> project_df4.31
project_df4 %>% filter(agegroup == "36 - 45") %>%  count(int_exinv) -> project_df4.32
project_df4.3 <- cbind(project_df4.31,project_df4.32)
colnames(project_df4.3) <- c("Interest_in_Investment","Frequency_I","Interest_in_Exploration","Frequency_E")

pie_int3 <- ggplot(project_df4.3, aes(x="", y=Frequency_I, fill = Interest_in_Investment))+
  geom_bar(stat="identity", width=1) + 
  coord_polar("y", start=0)+
  labs(x="", y="")+
  ggtitle("Interest in Investment (36-45)")+
  theme_classic()+
  theme(axis.line = element_blank(), axis.text = element_blank(), 
        axis.ticks = element_blank(), plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(), legend.position = "none")+
  geom_text(aes(label=paste0(round((Frequency_I/sum(Frequency_I))*100), "%")),
            position = position_stack(vjust=0.5))
pie_exint3 <- ggplot(project_df4.3, aes(x="", y=Frequency_E, fill = Interest_in_Exploration))+
  geom_bar(stat="identity", width=1) + 
  coord_polar("y", start=0)+
  labs(x="", y="")+
  ggtitle("Interest in Exploration (36-45)")+
  theme_classic()+
  theme(axis.line = element_blank(), axis.text = element_blank(), 
        axis.ticks = element_blank(), plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(), legend.position = "none")+
  geom_text(aes(label=paste0(round((Frequency_E/sum(Frequency_E))*100), "%")), 
            position = position_stack(vjust=0.5))


project_df4 %>% filter(agegroup == "46 - 55") %>% count(int_inv) -> project_df4.41
project_df4 %>% filter(agegroup == "46 - 55") %>%  count(int_exinv) -> project_df4.42
colnames(project_df4.41) <- c("Interest_in_Investment","Frequency_I")
colnames(project_df4.42) <- c("Interest_in_Exploration","Frequency_E")

pie_int4 <- ggplot(project_df4.41, aes(x="", y=Frequency_I, fill = Interest_in_Investment))+
  geom_bar(stat="identity", width=1) + 
  coord_polar("y", start=0)+
  labs(x="", y="")+
  ggtitle("Interest in Investment (46-55)")+
  theme_classic()+
  theme(axis.line = element_blank(), axis.text = element_blank(), 
        axis.ticks = element_blank(), plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(), legend.position = "none")+
  geom_text(aes(label=paste0(round((Frequency_I/sum(Frequency_I))*100), "%")),
            position = position_stack(vjust=0.5))
pie_exint4 <- ggplot(project_df4.42, aes(x="", y=Frequency_E, fill = Interest_in_Exploration))+
  geom_bar(stat="identity", width=1) + 
  coord_polar("y", start=0)+
  labs(x="", y="")+
  ggtitle("Interest in Exploration (46-55)")+
  theme_classic()+
  theme(axis.line = element_blank(), axis.text = element_blank(), 
        axis.ticks = element_blank(), plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(), legend.position = "none")+
  geom_text(aes(label=paste0(round((Frequency_E/sum(Frequency_E))*100), "%")), 
            position = position_stack(vjust=0.5))


project_df4 %>% filter(agegroup == "56 and above") %>% count(int_inv) -> project_df4.51
project_df4 %>% filter(agegroup == "56 and above") %>%  count(int_exinv) -> project_df4.52
colnames(project_df4.51) <- c("Interest_in_Investment","Frequency_I")
colnames(project_df4.52) <- c("Interest_in_Exploration","Frequency_E")

pie_int5 <- ggplot(project_df4.51, aes(x="", y=Frequency_I, fill = Interest_in_Investment))+
  geom_bar(stat="identity", width=1) + 
  coord_polar("y", start=0)+
  labs(x="", y="")+
  ggtitle("Interest in Investment (56+)")+
  theme_classic()+
  theme(axis.line = element_blank(), axis.text = element_blank(), 
        axis.ticks = element_blank(), plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(), legend.position = "none")+
  geom_text(aes(label=paste0(round((Frequency_I/sum(Frequency_I))*100), "%")),
            position = position_stack(vjust=0.5))
pie_exint5 <- ggplot(project_df4.52, aes(x="", y=Frequency_E, fill = Interest_in_Exploration))+
  geom_bar(stat="identity", width=1) + 
  coord_polar("y", start=0)+
  labs(x="", y="")+
  ggtitle("Interest in Exploration (56+)")+
  theme_classic()+
  theme(axis.line = element_blank(), axis.text = element_blank(), 
        axis.ticks = element_blank(), plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(), legend.position = "none")+
  geom_text(aes(label=paste0(round((Frequency_E/sum(Frequency_E))*100), "%")), 
            position = position_stack(vjust=0.5))


pie_final <- ggarrange(pie_int1, pie_exint1, pie_int2, pie_exint2, 
                       pie_int3, pie_exint3, pie_int4, pie_exint4, 
                       pie_int5, pie_exint5, common.legend = T); pie_final

##Plot 5

#Stacked Bar Plot showing preference in low risk investment.
dfB1 <- cSplit(project_df2, "pref_linv", sep = ";")
dfB11 <- subset(dfB1, select = c(agegroup, pref_linv_1))
dfB12 <- subset(dfB1, select = c(agegroup, pref_linv_2))
dfB13 <- subset(dfB1, select = c(agegroup, pref_linv_3))
dfB14 <- subset(dfB1, select = c(agegroup, pref_linv_4))
dfB15 <- subset(dfB1, select = c(agegroup, pref_linv_5))
dfB12 <- na.omit(dfB12)
dfB13 <- na.omit(dfB13)
dfB14 <- na.omit(dfB14)
dfB15 <- na.omit(dfB15)
colnames(dfB11) <- c("Age", "Low_Risk_Investment_Preference")
colnames(dfB12) <- c("Age", "Low_Risk_Investment_Preference")
colnames(dfB13) <- c("Age", "Low_Risk_Investment_Preference")
colnames(dfB14) <- c("Age", "Low_Risk_Investment_Preference")
colnames(dfB15) <- c("Age", "Low_Risk_Investment_Preference")
dfB <- rbind(dfB11,dfB12,dfB13,dfB14,dfB15)


dfB <- as.data.frame(dfB)
dfB %>% count(Age, Low_Risk_Investment_Preference) -> project_df6
colnames(project_df6) <- c("Age","Low_Risk_Investment_Preference", "Frequency")
z5 <- ggplot(project_df6, aes(x= Age, y = Frequency, fill = Low_Risk_Investment_Preference)) +
  geom_bar(position = "fill", stat = "identity") +
  labs(y= "Relative Frequency", x= "Age") +
  scale_fill_brewer(palette = "Reds") + 
  theme_minimal() + 
  ggtitle("Preference in Low Risk Investment") +
  theme(legend.title = element_blank());z5

#Stacked Bar Plot showing preferrence in moderate risk investment.

dfC1 <- cSplit(project_df2, "pref_minv", sep = ";")
dfC11 <- subset(dfC1, select = c(agegroup, pref_minv_1))
dfC12 <- subset(dfC1, select = c(agegroup, pref_minv_2))
dfC13 <- subset(dfC1, select = c(agegroup, pref_minv_3))
dfC12 <- na.omit(dfC12)
dfC13 <- na.omit(dfC13)
colnames(dfC11) <- c("Age", "Moderate_Risk_Investment_Preference")
colnames(dfC12) <- c("Age", "Moderate_Risk_Investment_Preference")
colnames(dfC13) <- c("Age", "Moderate_Risk_Investment_Preference")
dfC <- rbind(dfC11,dfC12,dfC13)
dfC <- as.data.frame(dfC) 
dfC %>% count(Age, Moderate_Risk_Investment_Preference) -> project_df7
colnames(project_df7) <- c("Age","Moderate_Risk_Investment_Preference", "Frequency")
z6 <- ggplot(project_df7, aes(x= Age, y = Frequency, fill = Moderate_Risk_Investment_Preference)) +
  geom_bar(position = "fill", stat = "identity") +
  labs(y= "Relative Frequency", x= "Age") +
  scale_fill_brewer(palette = "Greys") + 
  theme_minimal()+
  ggtitle("Preference in Moderate Risk Investment") +
  theme(legend.title = element_blank());z6


#Stacked Bar Plot showing preferrence in high risk investment.

dfD1 <- cSplit(project_df2, "pref_hinv", sep = ";")
dfD11 <- subset(dfD1, select = c(agegroup, pref_hinv_1))
dfD12 <- subset(dfD1, select = c(agegroup, pref_hinv_2))
dfD13 <- subset(dfD1, select = c(agegroup, pref_hinv_3))
dfD14 <- subset(dfD1, select = c(agegroup, pref_hinv_4))
dfD15 <- subset(dfD1, select = c(agegroup, pref_hinv_5))
dfD12 <- na.omit(dfD12)
dfD13 <- na.omit(dfD13)
dfD14 <- na.omit(dfD14)
dfD15 <- na.omit(dfD15)
colnames(dfD11) <- c("Age", "High_Risk_Investment_Preference")
colnames(dfD12) <- c("Age", "High_Risk_Investment_Preference")
colnames(dfD13) <- c("Age", "High_Risk_Investment_Preference")
colnames(dfD14) <- c("Age", "High_Risk_Investment_Preference")
colnames(dfD15) <- c("Age", "High_Risk_Investment_Preference")
dfD <- rbind(dfD11,dfD12,dfD13,dfD14,dfD15)
dfD <- as.data.frame(dfD)
dfD %>% count(Age, High_Risk_Investment_Preference) -> project_df8
colnames(project_df8) <- c("Age", "High_Risk_Investment_Preference", "Frequency")
z7 <- ggplot(project_df8, aes(x= Age, y = Frequency, fill = High_Risk_Investment_Preference)) +
  geom_bar(position = "fill", stat = "identity") +
  labs(y= "Relative Frequency", x= "Age") +
  scale_fill_brewer(palette = "Greens") + 
  theme_minimal()+
  ggtitle("Preference in Moderate Risk Investment") +
  theme(legend.title = element_blank());z7
ggarrange(z5, z6, z7)

##Plot 6
#Pie chart showing interest in different low risk investment in each age group

dfB %>% filter(Age == "18 - 25") %>% count(Low_Risk_Investment_Preference) -> project_df6.18_25
colnames(project_df6.18_25) <- c("Low_Risk_Investment_Preference", "Frequency")
Pie.L_18_25 <- ggplot(project_df6.18_25, aes(x= "", y = Frequency, fill = Low_Risk_Investment_Preference)) +
  geom_bar( stat = "identity", width = 1) +
  coord_polar("y", start = 0)+
  labs(y= "", x= "") +
  ggtitle("Low Risk Investment Preference (18 - 25)") +
  theme_classic()+
  theme(axis.line = element_blank(), axis.text = element_blank(), 
        axis.ticks = element_blank(), plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(), legend.position = "none") +
  geom_text(aes(label=paste0(round((Frequency/sum(Frequency))*100), "%")), 
            position = position_stack(vjust=0.5));

dfB %>% filter(Age == "26 - 35") %>% count(Low_Risk_Investment_Preference) -> project_df6.26_35
colnames(project_df6.26_35) <- c("Low_Risk_Investment_Preference", "Frequency")
Pie.L_26_35 <- ggplot(project_df6.26_35, aes(x= "", y = Frequency, fill = Low_Risk_Investment_Preference)) +
  geom_bar( stat = "identity", width = 1) +
  coord_polar("y", start = 0)+
  labs(y= "", x= "") +
  ggtitle("Low Risk Investment Preference (26 - 35)") +
  theme_classic()+
  theme(axis.line = element_blank(), axis.text = element_blank(), 
        axis.ticks = element_blank(), plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(), legend.position = "none") +
  geom_text(aes(label=paste0(round((Frequency/sum(Frequency))*100), "%")), 
            position = position_stack(vjust=0.5));

dfB %>% filter(Age == "36 - 45") %>% count(Low_Risk_Investment_Preference) -> project_df6.36_45
colnames(project_df6.36_45) <- c("Low_Risk_Investment_Preference", "Frequency")
Pie.L_36_45 <- ggplot(project_df6.36_45, aes(x= "", y = Frequency, fill = Low_Risk_Investment_Preference)) +
  geom_bar( stat = "identity", width = 1) +
  coord_polar("y", start = 0)+
  labs(y= "", x= "") +
  ggtitle("Low Risk Investment Preference (36 - 45)") +
  theme_classic()+
  theme(axis.line = element_blank(), axis.text = element_blank(), 
        axis.ticks = element_blank(), plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(), legend.position = "none") +
  geom_text(aes(label=paste0(round((Frequency/sum(Frequency))*100), "%")), 
            position = position_stack(vjust=0.5));

dfB %>% filter(Age == "46 - 55") %>% count(Low_Risk_Investment_Preference) -> project_df6.46_55
colnames(project_df6.46_55) <- c("Low_Risk_Investment_Preference", "Frequency")
Pie.L_46_55 <- ggplot(project_df6.46_55, aes(x= "", y = Frequency, fill = Low_Risk_Investment_Preference)) +
  geom_bar( stat = "identity", width = 1) +
  coord_polar("y", start = 0)+
  labs(y= "", x= "") +
  ggtitle("Low Risk Investment Preference (46 - 55)") +
  theme_classic()+
  theme(axis.line = element_blank(), axis.text = element_blank(), 
        axis.ticks = element_blank(), plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(), legend.position = "none") +
  geom_text(aes(label=paste0(round((Frequency/sum(Frequency))*100), "%")), 
            position = position_stack(vjust=0.5));

dfB %>% filter(Age == "56 and above") %>% count(Low_Risk_Investment_Preference) -> project_df6.56
colnames(project_df6.56) <- c("Low_Risk_Investment_Preference", "Frequency")
Pie.L_56 <- ggplot(project_df6.56, aes(x= "", y = Frequency, fill = Low_Risk_Investment_Preference)) +
  geom_bar( stat = "identity", width = 1) +
  coord_polar("y", start = 0)+
  labs(y= "", x= "") +
  ggtitle("Low Risk Investment Preference (56 +)") +
  theme_classic()+
  theme(axis.line = element_blank(), axis.text = element_blank(), 
        axis.ticks = element_blank(), plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(), legend.position = "none") +
  geom_text(aes(label=paste0(round((Frequency/sum(Frequency))*100), "%")), 
            position = position_stack(vjust=0.5));

Pie.L <- ggarrange(Pie.L_18_25, Pie.L_26_35, Pie.L_36_45, Pie.L_46_55, Pie.L_56, common.legend = T); Pie.L

##Plot 7
#Pie chart showing interest in different moderate risk investment in each age group

dfC %>% filter(Age == "18 - 25") %>% count(Moderate_Risk_Investment_Preference) -> project_df7.18_25
colnames(project_df7.18_25) <- c("Moderate_Risk_Investment_Preference", "Frequency")
Pie.M_18_25 <- ggplot(project_df7.18_25, aes(x= "", y = Frequency, fill = Moderate_Risk_Investment_Preference)) +
  geom_bar( stat = "identity", width = 1) +
  coord_polar("y", start = 0)+
  labs(y= "", x= "") +
  ggtitle("Moderate Risk Investment Preference (18 - 25)") +
  theme_classic()+
  theme(axis.line = element_blank(), axis.text = element_blank(), 
        axis.ticks = element_blank(), plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(), legend.position = "none") +
  geom_text(aes(label=paste0(round((Frequency/sum(Frequency))*100), "%")), 
            position = position_stack(vjust=0.5));

dfC %>% filter(Age == "26 - 35") %>% count(Moderate_Risk_Investment_Preference) -> project_df7.26_35
colnames(project_df7.26_35) <- c("Moderate_Risk_Investment_Preference", "Frequency")
Pie.M_26_35 <- ggplot(project_df7.26_35, aes(x= "", y = Frequency, fill = Moderate_Risk_Investment_Preference)) +
  geom_bar( stat = "identity", width = 1) +
  coord_polar("y", start = 0)+
  labs(y= "", x= "") +
  ggtitle("Moderate Risk Investment Preference (26 - 35)") +
  theme_classic()+
  theme(axis.line = element_blank(), axis.text = element_blank(), 
        axis.ticks = element_blank(), plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(), legend.position = "none") +
  geom_text(aes(label=paste0(round((Frequency/sum(Frequency))*100), "%")), 
            position = position_stack(vjust=0.5));

dfC %>% filter(Age == "36 - 45") %>% count(Moderate_Risk_Investment_Preference) -> project_df7.36_45
colnames(project_df7.36_45) <- c("Moderate_Risk_Investment_Preference", "Frequency")
Pie.M_36_45 <- ggplot(project_df7.36_45, aes(x= "", y = Frequency, fill = Moderate_Risk_Investment_Preference)) +
  geom_bar( stat = "identity", width = 1) +
  coord_polar("y", start = 0)+
  labs(y= "", x= "") +
  ggtitle("Moderate Risk Investment Preference (36 - 45)") +
  theme_classic()+
  theme(axis.line = element_blank(), axis.text = element_blank(), 
        axis.ticks = element_blank(), plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(), legend.position = "none") +
  geom_text(aes(label=paste0(round((Frequency/sum(Frequency))*100), "%")), 
            position = position_stack(vjust=0.5));

dfC %>% filter(Age == "46 - 55") %>% count(Moderate_Risk_Investment_Preference) -> project_df7.46_55
colnames(project_df7.46_55) <- c("Moderate_Risk_Investment_Preference", "Frequency")
Pie.M_46_55 <- ggplot(project_df7.46_55, aes(x= "", y = Frequency, fill = Moderate_Risk_Investment_Preference)) +
  geom_bar( stat = "identity", width = 1) +
  coord_polar("y", start = 0)+
  labs(y= "", x= "") +
  ggtitle("Moderate Risk Investment Preference (46 - 55)") +
  theme_classic()+
  theme(axis.line = element_blank(), axis.text = element_blank(), 
        axis.ticks = element_blank(), plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(), legend.position = "none") +
  geom_text(aes(label=paste0(round((Frequency/sum(Frequency))*100), "%")), 
            position = position_stack(vjust=0.5));

dfC %>% filter(Age == "56 and above") %>% count(Moderate_Risk_Investment_Preference) -> project_df7.56
colnames(project_df7.56) <- c("Moderate_Risk_Investment_Preference", "Frequency")
Pie.M_56 <- ggplot(project_df7.56, aes(x= "", y = Frequency, fill = Moderate_Risk_Investment_Preference)) +
  geom_bar( stat = "identity", width = 1) +
  coord_polar("y", start = 0)+
  labs(y= "", x= "") +
  ggtitle("Moderate Risk Investment Preference (56 +)") +
  theme_classic()+
  theme(axis.line = element_blank(), axis.text = element_blank(), 
        axis.ticks = element_blank(), plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(), legend.position = "none") +
  geom_text(aes(label=paste0(round((Frequency/sum(Frequency))*100), "%")), 
            position = position_stack(vjust=0.5));

Pie.M <- ggarrange(Pie.M_18_25, Pie.M_26_35, Pie.M_36_45, Pie.M_46_55, Pie.M_56, common.legend = T); Pie.M

## Plot 8
#Pie chart showing interest in different high risk investment in each age group

dfD %>% filter(Age == "18 - 25") %>% count(High_Risk_Investment_Preference) -> project_df8.18_25
colnames(project_df8.18_25) <- c("High_Risk_Investment_Preference", "Frequency")
Pie.H_18_25 <- ggplot(project_df8.18_25, aes(x= "", y = Frequency, fill = High_Risk_Investment_Preference)) +
  geom_bar( stat = "identity", width = 1) +
  coord_polar("y", start = 0)+
  labs(y= "", x= "") +
  ggtitle("High Risk Investment Preference (18 - 25)") +
  theme_classic()+
  theme(axis.line = element_blank(), axis.text = element_blank(), 
        axis.ticks = element_blank(), plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(), legend.position = "none") +
  geom_text(aes(label=paste0(round((Frequency/sum(Frequency))*100), "%")), 
            position = position_stack(vjust=0.5));

dfD %>% filter(Age == "26 - 35") %>% count(High_Risk_Investment_Preference) -> project_df8.26_35
colnames(project_df8.26_35) <- c("High_Risk_Investment_Preference", "Frequency")
Pie.H_26_35 <- ggplot(project_df8.26_35, aes(x= "", y = Frequency, fill = High_Risk_Investment_Preference)) +
  geom_bar( stat = "identity", width = 1) +
  coord_polar("y", start = 0)+
  labs(y= "", x= "") +
  ggtitle("High Risk Investment Preference (26 - 35)") +
  theme_classic()+
  theme(axis.line = element_blank(), axis.text = element_blank(), 
        axis.ticks = element_blank(), plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(), legend.position = "none") +
  geom_text(aes(label=paste0(round((Frequency/sum(Frequency))*100), "%")), 
            position = position_stack(vjust=0.5));

dfD %>% filter(Age == "36 - 45") %>% count(High_Risk_Investment_Preference) -> project_df8.36_45
colnames(project_df8.36_45) <- c("High_Risk_Investment_Preference", "Frequency")
Pie.H_36_45 <- ggplot(project_df8.36_45, aes(x= "", y = Frequency, fill = High_Risk_Investment_Preference)) +
  geom_bar( stat = "identity", width = 1) +
  coord_polar("y", start = 0)+
  labs(y= "", x= "") +
  ggtitle("High Risk Investment Preference (36 - 45)") +
  theme_classic()+
  theme(axis.line = element_blank(), axis.text = element_blank(), 
        axis.ticks = element_blank(), plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(), legend.position = "none") +
  geom_text(aes(label=paste0(round((Frequency/sum(Frequency))*100), "%")), 
            position = position_stack(vjust=0.5));

dfD %>% filter(Age == "46 - 55") %>% count(High_Risk_Investment_Preference) -> project_df8.46_55
colnames(project_df8.46_55) <- c("High_Risk_Investment_Preference", "Frequency")
Pie.H_46_55 <- ggplot(project_df8.46_55, aes(x= "", y = Frequency, fill = High_Risk_Investment_Preference)) +
  geom_bar( stat = "identity", width = 1) +
  coord_polar("y", start = 0)+
  labs(y= "", x= "") +
  ggtitle("High Risk Investment Preference (46 - 55)") +
  theme_classic()+
  theme(axis.line = element_blank(), axis.text = element_blank(), 
        axis.ticks = element_blank(), plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(), legend.position = "none") +
  geom_text(aes(label=paste0(round((Frequency/sum(Frequency))*100), "%")), 
            position = position_stack(vjust=0.5));

dfD %>% filter(Age == "56 and above") %>% count(High_Risk_Investment_Preference) -> project_df8.56
colnames(project_df8.56) <- c("High_Risk_Investment_Preference", "Frequency")
Pie.H_56 <- ggplot(project_df8.56, aes(x= "", y = Frequency, fill = High_Risk_Investment_Preference)) +
  geom_bar( stat = "identity", width = 1) +
  coord_polar("y", start = 0)+
  labs(y= "", x= "") +
  ggtitle("High Risk Investment Preference (56 +)") +
  theme_classic()+
  theme(axis.line = element_blank(), axis.text = element_blank(), 
        axis.ticks = element_blank(), plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(), legend.position = "none") +
  geom_text(aes(label=paste0(round((Frequency/sum(Frequency))*100), "%")), 
            position = position_stack(vjust=0.5));

Pie.H <- ggarrange(Pie.H_18_25, Pie.H_26_35, Pie.H_36_45, Pie.H_46_55, Pie.H_56, common.legend = T); Pie.H



#Percentage of income investing in low, moderate and high risk investment according to different age

##Plt 9
#18 to 25
project_df2 %>% filter(agegroup == "18 - 25") %>% count(per_linv) -> perlinvdf1
perlinvdf1 <- as.data.frame(perlinvdf1)

project_df2 %>% filter(agegroup == "18 - 25") %>% count(per_minv) -> perminvdf1
perminvdf1 <- as.data.frame(perminvdf1)

project_df2 %>% filter(agegroup == "18 - 25") %>% count(per_hinv) -> perhinvdf1
perhinvdf1 <- as.data.frame(perhinvdf1)

perinv_18_25 <- cbind(perlinvdf1, perminvdf1, perhinvdf1)
colnames(perinv_18_25) <- c("Percentage of income investing in Low Risk", "Frequency_L", 
                            "Percentage of income investing in Moderate Risk", "Frequency_M", 
                            "Percentage of income investing in High Risk", "Frequency_H")
perinv_18_25 <- perinv_18_25[-1,]

Percentage_18_25 <- rep(perinv_18_25[,1],3)
Risk <- c(rep("Low Risk Investment", 6), 
          rep("Moderate Risk Investment", 6),
          rep("High Risk Investment", 6))
Frequency_18_25 <- c(perinv_18_25$Frequency_L, perinv_18_25$Frequency_M, perinv_18_25$Frequency_H)

final_perinv_18_25 <- cbind(Percentage_18_25, Risk, Frequency_18_25)
final_perinv_18_25 <- as.data.frame(final_perinv_18_25)
final_perinv_18_25$Frequency_18_25 <- as.integer(final_perinv_18_25$Frequency_18_25)

per_18_25 <- ggplot(final_perinv_18_25, aes(x = Risk, y = Frequency_18_25 ,fill = Percentage_18_25))+
  geom_bar(position="fill", stat="identity") +
  labs(x = "Risk profile of age group 18 to 25", y = "Relative Frequency") +
  scale_fill_brewer(palette="Blues") +
  theme_minimal() +
  theme(legend.title = element_blank()); per_18_25

##Plot 10
#26 to 35

project_df2 %>% filter(agegroup == "26 - 35") %>% count(per_linv) -> perlinvdf2
perlinvdf2 <- as.data.frame(perlinvdf2)

project_df2 %>% filter(agegroup == "26 - 35") %>% count(per_minv) -> perminvdf2
perminvdf2 <- as.data.frame(perminvdf2)

project_df2 %>% filter(agegroup == "26 - 35") %>% count(per_hinv) -> perhinvdf2
perhinvdf2 <- as.data.frame(perhinvdf2)


colnames(perlinvdf2) <- c("Percentage of income investing in Low Risk", "Frequency_L") 
colnames(perminvdf2) <- c("Percentage of income investing in Moderate Risk", "Frequency_M")
colnames(perhinvdf2) <- c("Percentage of income investing in High Risk", "Frequency_H")

Percentage_26_35 <- rep(perhinvdf2[,1],3)
Frequency_26_35 <- c(0, perlinvdf2$Frequency_L, 0, 
                    perminvdf2$Frequency_M, 0, 
                     perhinvdf2$Frequency_H)
final_perinv_26_35 <- cbind(Percentage_26_35, Risk, Frequency_26_35)
final_perinv_26_35 <- as.data.frame(final_perinv_26_35)
final_perinv_26_35$Frequency_26_35 <- as.integer(final_perinv_26_35$Frequency_26_35)

per_26_35 <- ggplot(final_perinv_26_35, aes(x = Risk, y = Frequency_26_35 ,fill = Percentage_26_35))+
  geom_bar(position="fill", stat="identity") +
  labs(x = "Risk profile of age group 26 to 35", y = "Relative Frequency") +
  scale_fill_brewer(palette="Oranges") +
  theme_minimal()+
  theme(legend.title = element_blank()); per_26_35

##Plot 11
#36 to 45

project_df2 %>% filter(agegroup == "36 - 45") %>% count(per_linv) -> perlinvdf3
perlinvdf3 <- as.data.frame(perlinvdf3)

project_df2 %>% filter(agegroup == "36 - 45") %>% count(per_minv) -> perminvdf3
perminvdf3 <- as.data.frame(perminvdf3)
perminvdf3 <- perminvdf3[-1,]

project_df2 %>% filter(agegroup == "36 - 45") %>% count(per_hinv) -> perhinvdf3
perhinvdf3 <- as.data.frame(perhinvdf3)
perhinvdf3 <- perhinvdf3[-1,]

colnames(perlinvdf3) <- c("Percentage of income investing in Low Risk", "Frequency_L") 
colnames(perminvdf3) <- c("Percentage of income investing in Moderate Risk", "Frequency_M")
colnames(perhinvdf3) <- c("Percentage of income investing in High Risk", "Frequency_H")

Percentage_36_45 <- c(perlinvdf3[,1], perminvdf3[,1], perhinvdf3[,1])
Frequency_36_45 <- c(perlinvdf3$Frequency_L, perminvdf3$Frequency_M, perhinvdf3$Frequency_H)
Risk_36_45 <- c(rep("Low Risk Investment", 6), 
                rep("Moderate Risk Investment", 4), 
                rep("High Risk Investment", 4))

final_perinv_36_45 <- cbind(Percentage_36_45, Risk_36_45, Frequency_36_45)
final_perinv_36_45 <- as.data.frame(final_perinv_36_45)
final_perinv_36_45$Frequency_36_45 <- as.integer(final_perinv_36_45$Frequency_36_45)

per_36_45 <- ggplot(final_perinv_36_45, aes(x = Risk_36_45, y = Frequency_36_45 ,fill = Percentage_36_45))+
  geom_bar(position="fill", stat="identity") +
  labs(x = "Risk profile of age group 36 to 45", y = "Relative Frequency") +
  scale_fill_brewer(palette="Greens") +
  theme_minimal() +
  theme(legend.title = element_blank()); per_36_45

##Plot 12
#46 t0 55

project_df2 %>% filter(agegroup == "46 - 55") %>% count(per_linv) -> perlinvdf4
perlinvdf4 <- as.data.frame(perlinvdf4)

project_df2 %>% filter(agegroup == "46 - 55") %>% count(per_minv) -> perminvdf4
perminvdf4 <- as.data.frame(perminvdf4)
perminvdf4 <- perminvdf4[-1,]


project_df2 %>% filter(agegroup == "46 - 55") %>% count(per_hinv) -> perhinvdf4
perhinvdf4 <- as.data.frame(perhinvdf4)
perhinvdf4 <- perhinvdf4[-1,]


colnames(perlinvdf4) <- c("Percentage of income investing in Low Risk", "Frequency_L") 
colnames(perminvdf4) <- c("Percentage of income investing in Moderate Risk", "Frequency_M")
colnames(perhinvdf4) <- c("Percentage of income investing in High Risk", "Frequency_H")

Percentage_46_55 <- c(perlinvdf4[,1], perminvdf4[,1], perhinvdf4[,1])
Frequency_46_55 <- c(perlinvdf4$Frequency_L, perminvdf4$Frequency_M, perhinvdf4$Frequency_H)
Risk_46_55 <- c(rep("Low Risk Investment", 4), 
                rep("Moderate Risk Investment", 3), 
                rep("High Risk Investment", 4))

final_perinv_46_55 <- cbind(Percentage_46_55, Risk_46_55, Frequency_46_55)
final_perinv_46_55 <- as.data.frame(final_perinv_46_55)
final_perinv_46_55$Frequency_46_55 <- as.integer(final_perinv_46_55$Frequency_46_55)

per_46_55 <- ggplot(final_perinv_46_55, aes(x = Risk_46_55, y = Frequency_46_55 ,fill = Percentage_46_55))+
  geom_bar(position="fill", stat="identity") +
  labs(x = "Risk profile of age group 46 to 55", y = "Relative Frequency") +
  scale_fill_brewer(palette="Reds") +
  theme_minimal() +
  theme(legend.title = element_blank()); per_46_55

##Plot 13
#56+

project_df2 %>% filter(agegroup == "56 and above") %>% count(per_linv) -> perlinvdf5
perlinvdf5 <- as.data.frame(perlinvdf5)

project_df2 %>% filter(agegroup == "56 and above") %>% count(per_minv) -> perminvdf5
perminvdf5 <- as.data.frame(perminvdf5)

project_df2 %>% filter(agegroup == "56 and above") %>% count(per_hinv) -> perhinvdf5
perhinvdf5 <- as.data.frame(perhinvdf5)

colnames(perlinvdf5) <- c("Percentage of income investing in Low Risk", "Frequency_L") 
colnames(perminvdf5) <- c("Percentage of income investing in Moderate Risk", "Frequency_M")
colnames(perhinvdf5) <- c("Percentage of income investing in High Risk", "Frequency_H")

Percentage_56 <- c(perlinvdf5[,1], perminvdf5[,1], perhinvdf5[,1])
Frequency_56 <- c(perlinvdf5$Frequency_L, perminvdf5$Frequency_M, perhinvdf5$Frequency_H)
Risk_56 <- c(rep("Low Risk Investment", 3), 
                rep("Moderate Risk Investment", 3), 
                rep("High Risk Investment", 2))
final_perinv_56 <- cbind(Percentage_56, Risk_56, Frequency_56)
final_perinv_56 <- as.data.frame(final_perinv_56)
final_perinv_56$Frequency_56 <- as.integer(final_perinv_56$Frequency_56)

per_56 <- ggplot(final_perinv_56, aes(x = Risk_56, y = Frequency_56 ,fill = Percentage_56))+
  geom_bar(position="fill", stat="identity") +
  labs(x = "Risk profile of age group 56+", y = "Relative Frequency") +
  scale_fill_brewer(palette="Greys") +
  theme_minimal() +
  theme(legend.title = element_blank()); per_56


##Plot 14
#Pie diagram for showing percentage of income in investment

project_df2 %>% filter(agegroup == "18 - 25") %>% count(per_inv) -> perinvdf_18_25
colnames(perinvdf_18_25) <- c("Percentage", "Frequency")
perinvdf_18_25 <- perinvdf_18_25[-1,]

perf_18_25 <- ggplot(perinvdf_18_25, aes(x="", y=Frequency, fill = Percentage)) + 
  geom_bar( stat = "identity", width = 1) +
  coord_polar("y", start = 0)+
  labs(x="", y="") +
  ggtitle("Percentage of income investing (18 - 25)")+
  theme_classic()+
  theme(axis.line = element_blank(), axis.text = element_blank(), 
        axis.ticks = element_blank(), plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(), legend.position = "none") +
  geom_text(aes(label=paste0(round((Frequency/sum(Frequency))*100), "%")), 
            position = position_stack(vjust=0.5))

project_df2 %>% filter(agegroup == "26 - 35") %>% count(per_inv) -> perinvdf_26_35
colnames(perinvdf_26_35) <- c("Percentage", "Frequency")

perf_26_35 <- ggplot(perinvdf_26_35, aes(x="", y=Frequency, fill = Percentage)) + 
  geom_bar( stat = "identity", width = 1) +
  coord_polar("y", start = 0)+
  labs(x="", y="") +
  ggtitle("Percentage of income investing (26 - 35)")+
  theme_classic()+
  theme(axis.line = element_blank(), axis.text = element_blank(), 
        axis.ticks = element_blank(), plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(), legend.position = "none") +
  geom_text(aes(label=paste0(round((Frequency/sum(Frequency))*100), "%")), 
            position = position_stack(vjust=0.5))

project_df2 %>% filter(agegroup == "36 - 45") %>% count(per_inv) -> perinvdf_36_45
colnames(perinvdf_36_45) <- c("Percentage", "Frequency")


perf_36_45 <- ggplot(perinvdf_36_45, aes(x="", y=Frequency, fill = Percentage)) + 
  geom_bar( stat = "identity", width = 1) +
  coord_polar("y", start = 0)+
  labs(x="", y="") +
  ggtitle("Percentage of income investing (36 - 45)")+
  theme_classic()+
  theme(axis.line = element_blank(), axis.text = element_blank(), 
        axis.ticks = element_blank(), plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(), legend.position = "none") +
  geom_text(aes(label=paste0(round((Frequency/sum(Frequency))*100), "%")), 
            position = position_stack(vjust=0.5))


project_df2 %>% filter(agegroup == "46 - 55") %>% count(per_inv) -> perinvdf_46_55
colnames(perinvdf_46_55) <- c("Percentage", "Frequency")

perf_46_55 <- ggplot(perinvdf_46_55, aes(x="", y=Frequency, fill = Percentage)) + 
  geom_bar( stat = "identity", width = 1) +
  coord_polar("y", start = 0)+
  labs(x="", y="") +
  ggtitle("Percentage of income investing (46 - 55)")+
  theme_classic()+
  theme(axis.line = element_blank(), axis.text = element_blank(), 
        axis.ticks = element_blank(), plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(), legend.position = "none") +
  geom_text(aes(label=paste0(round((Frequency/sum(Frequency))*100), "%")), 
            position = position_stack(vjust=0.5))


project_df2 %>% filter(agegroup == "56 and above") %>% count(per_inv) -> perinvdf_56
colnames(perinvdf_56) <- c("Percentage", "Frequency")

perf_56 <- ggplot(perinvdf_56, aes(x="", y=Frequency, fill = Percentage)) + 
  geom_bar( stat = "identity", width = 1) +
  coord_polar("y", start = 0)+
  labs(x="", y="") +
  ggtitle("Percentage of income investing (56 +)")+
  theme_classic()+
  theme(axis.line = element_blank(), axis.text = element_blank(), 
        axis.ticks = element_blank(), plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(), legend.position = "none") +
  geom_text(aes(label=paste0(round((Frequency/sum(Frequency))*100), "%")), 
            position = position_stack(vjust=0.5))

ggarrange(perf_18_25, perf_26_35, perf_36_45, perf_46_55, perf_56, common.legend = T)

##Plot 15
#goal preference

project_df2 %>% filter(agegroup == "18 - 25") %>% count(goal_pref) -> goal_18_25
goal_18_25 <- goal_18_25[-1,]
relativefreq_18_25 <- (goal_18_25$n/sum(goal_18_25$n)*100)
goal_18_25 <- cbind(goal_18_25, relativefreq_18_25)
colnames(goal_18_25) <- c("Goal_Preference", "Frequency", "Percentage")
goalplot_18_25 <- ggplot(goal_18_25, aes(x= Goal_Preference, y= Percentage)) + 
  geom_bar(stat = "identity", width = 0.5, fill = c("darkgreen", "Yellow")) +
  labs(x = "Goals", y= "Percentage") +
  ggtitle("Preferred Goals (18 - 25)") +
  ylim(0,100) +
  theme_classic()
  
project_df2 %>% filter(agegroup == "26 - 35") %>% count(goal_pref) -> goal_26_35
relativefreq_26_35 <- (goal_26_35$n/sum(goal_26_35$n)*100)
goal_26_35 <- cbind(goal_26_35, relativefreq_26_35)
colnames(goal_26_35) <- c("Goal_Preference", "Frequency", "Percentage")
goalplot_26_35 <- ggplot(goal_26_35, aes(x= Goal_Preference, y= Percentage)) + 
  geom_bar(stat = "identity", width = 0.5, fill = c("darkgreen", "Yellow")) +
  labs(x = "Goals", y= "Percentage") +
  ggtitle("Preferred Goals (26 - 35)") +
  ylim(0,100) +
  theme_classic()

project_df2 %>% filter(agegroup == "36 - 45") %>% count(goal_pref) -> goal_36_45
relativefreq_36_45 <- (goal_36_45$n/sum(goal_36_45$n)*100)
goal_36_45 <- cbind(goal_36_45, relativefreq_36_45)
colnames(goal_36_45) <- c("Goal_Preference", "Frequency", "Percentage")
goalplot_36_45 <- ggplot(goal_36_45, aes(x= Goal_Preference, y= Percentage)) + 
  geom_bar(stat = "identity", width = 0.5, fill = c("darkgreen", "Yellow")) +
  labs(x = "Goals", y= "Percentage") + 
  ggtitle("Preferred Goals (36 - 45)") +
  ylim(0,100) +
  theme_classic()

project_df2 %>% filter(agegroup == "46 - 55") %>% count(goal_pref) -> goal_46_55
relativefreq_46_55 <- (goal_46_55$n/sum(goal_46_55$n)*100)
goal_46_55 <- cbind(goal_46_55, relativefreq_46_55)
colnames(goal_46_55) <- c("Goal_Preference", "Frequency", "Percentage")
goalplot_46_55 <- ggplot(goal_46_55, aes(x= Goal_Preference, y= Percentage)) + 
  geom_bar(stat = "identity", width = 0.5, fill = c("darkgreen", "Yellow")) +
  labs(x = "Goals", y= "Percentage") + 
  ggtitle("Preferred Goals (46 - 55)") +
  ylim(0,100) +
  theme_classic()

project_df2 %>% filter(agegroup == "56 and above") %>% count(goal_pref) -> goal_56
relativefreq_55 <- (goal_56$n/sum(goal_56$n)*100)
goal_56 <- cbind(goal_56, relativefreq_55)
colnames(goal_56) <- c("Goal_Preference", "Frequency", "Percentage")
goalplot_56 <- ggplot(goal_56, aes(x= Goal_Preference, y= Percentage)) + 
  geom_bar(stat = "identity", width = 0.5, fill = c("darkgreen", "Yellow")) +
  labs(x = "Goals", y= "Percentage") + 
  ggtitle("Preferred Goals (56 + )") +
  ylim(0,100) +
  theme_classic()

ggarrange(goalplot_18_25, goalplot_26_35, goalplot_36_45, goalplot_46_55, goalplot_56)


#Plot 16
# Plotting interest in Sip age wise.

project_df2 %>% filter(agegroup == "18 - 25") %>% count(sip) -> sip_18_25
percentsip_18_25 <- (sip_18_25$n/sum(sip_18_25$n)*100)
sip_18_25 <- cbind(sip_18_25, percentsip_18_25)
colnames(sip_18_25) <- c("Interest", "Frequency", "Percentage")
sipplot_18_25 <- ggplot(sip_18_25, aes(x=Interest, y= Percentage)) +
  geom_bar(stat = "identity", width = 0.5, fill = c("orange", "red", "darkblue")) +
  labs(x="Interested in SIP", y="Percentage")+
  ggtitle("Interest in SIP (18 - 25)") + 
  ylim(0,100) + 
  theme_classic()

project_df2 %>% filter(agegroup == "26 - 35") %>% count(sip) -> sip_26_35
percentsip_26_35 <- (sip_26_35$n/sum(sip_26_35$n)*100)
sip_26_35 <- cbind(sip_26_35, percentsip_26_35)
colnames(sip_26_35) <- c("Interest", "Frequency", "Percentage")
sipplot_26_35 <- ggplot(sip_26_35, aes(x=Interest, y= Percentage)) +
  geom_bar(stat = "identity", width = 0.5, fill = c("orange", "red", "darkblue")) +
  labs(x="Interested in SIP", y="Percentage")+
  ggtitle("Interest in SIP (26 - 35)") + 
  ylim(0,100) + 
  theme_classic()

project_df2 %>% filter(agegroup == "36 - 45") %>% count(sip) -> sip_36_45
percentsip_36_45 <- (sip_36_45$n/sum(sip_36_45$n)*100)
sip_36_45 <- cbind(sip_36_45, percentsip_36_45)
colnames(sip_36_45) <- c("Interest", "Frequency", "Percentage")
sipplot_36_45 <- ggplot(sip_36_45, aes(x=Interest, y= Percentage)) +
  geom_bar(stat = "identity", width = 0.5, fill = c("orange", "red", "darkblue")) +
  labs(x="Interested in SIP", y="Percentage")+
  ggtitle("Interest in SIP (36 - 45)") + 
  ylim(0,100) + 
  theme_classic()

project_df2 %>% filter(agegroup == "46 - 55") %>% count(sip) -> sip_46_55
percentsip_46_55 <- (sip_46_55$n/sum(sip_46_55$n)*100)
sip_46_55 <- cbind(sip_46_55, percentsip_46_55)
colnames(sip_46_55) <- c("Interest", "Frequency", "Percentage")
sipplot_46_55 <- ggplot(sip_46_55, aes(x=Interest, y= Percentage)) +
  geom_bar(stat = "identity", width = 0.5, fill = c("orange", "red", "darkblue")) +
  labs(x="Interested in SIP", y="Percentage")+
  ggtitle("Interest in SIP (46 - 55)") + 
  ylim(0,100) + 
  theme_classic()

project_df2 %>% filter(agegroup == "56 and above") %>% count(sip) -> sip_56
percentsip_56 <- (sip_56$n/sum(sip_56$n)*100)
sip_56 <- cbind(sip_56, percentsip_56)
colnames(sip_56) <- c("Interest", "Frequency", "Percentage")
sipplot_56 <- ggplot(sip_56, aes(x=Interest, y= Percentage)) +
  geom_bar(stat = "identity", width = 0.5, fill = c("orange", "red", "darkblue")) +
  labs(x="Interested in SIP", y="Percentage")+
  ggtitle("Interest in SIP (56 +)") + 
  ylim(0,100) + 
  theme_classic()
ggarrange(sipplot_18_25, sipplot_26_35, sipplot_36_45, sipplot_46_55, sipplot_56)

#Plot 17
#financial literacy (self introspection)

project_df2 %>% count(fin_lite) -> Liter
percentliter <- (Liter$n/sum(Liter$n)*100)
Liter <- cbind(Liter, percentliter)
colnames(Liter) <- c("Literate", "Frequency", "Percentage")
Literplot <- ggplot(Liter, aes(x=Literate, y= Percentage)) + 
  geom_bar(stat="identity", width = 0.3, fill = c("cyan", "red", "black")) +
  labs(x="", y="Percentage") + 
  ggtitle("Self Introspected Financial Literacy") +
  ylim(0,100) +
  theme_classic(); Literplot

##Plot 18
#financial literacy age wise (self introspection)

project_df2 %>% filter(agegroup == ("18 - 25")) %>% count(fin_lite) -> Liter1
percentliter1 <- (Liter1$n/sum(Liter1$n)*100)
Liter1 <- cbind(Liter1, percentliter1)
colnames(Liter1) <- c("Literate", "Frequency", "Percentage")
Literplot1 <- ggplot(Liter1, aes(x=Literate, y= Percentage)) + 
  geom_bar(stat="identity", width = 0.75, fill = c("cyan", "red", "black")) +
  labs(x="", y="Percentage") + 
  ggtitle("Self Introspected Literacy (18 - 25)") +
  ylim(0,100) +
  theme_classic(); Literplot1

project_df2 %>% filter(agegroup == ("26 - 35")) %>% count(fin_lite) -> Liter2
percentliter2 <- (Liter2$n/sum(Liter2$n)*100)
Liter2 <- cbind(Liter2, percentliter2)
colnames(Liter2) <- c("Literate", "Frequency", "Percentage")
Literplot2 <- ggplot(Liter2, aes(x=Literate, y= Percentage)) + 
  geom_bar(stat="identity", width = 0.75, fill = c("cyan", "black")) +
  labs(x="", y="Percentage") + 
  ggtitle("Self Introspected Literacy (26 - 35)") +
  ylim(0,100) +
  theme_classic(); Literplot2


project_df2 %>% filter(agegroup == ("36 - 45")) %>% count(fin_lite) -> Liter3
percentliter3 <- (Liter3$n/sum(Liter3$n)*100)
Liter3 <- cbind(Liter3, percentliter3)
colnames(Liter3) <- c("Literate", "Frequency", "Percentage")
Literplot3 <- ggplot(Liter3, aes(x=Literate, y= Percentage)) + 
  geom_bar(stat="identity", width = 0.75, fill = c("red", "black")) +
  labs(x="", y="Percentage") + 
  ggtitle("Self Introspected Literacy (36 - 45)") +
  ylim(0,100) +
  theme_classic(); Literplot3


project_df2 %>% filter(agegroup == ("46 - 55")) %>% count(fin_lite) -> Liter4
percentliter4 <- (Liter4$n/sum(Liter4$n)*100)
Liter4 <- cbind(Liter4, percentliter4)
colnames(Liter4) <- c("Literate", "Frequency", "Percentage")
Literplot4 <- ggplot(Liter4, aes(x=Literate, y= Percentage)) + 
  geom_bar(stat="identity", width = 0.75, fill = c("cyan", "red")) +
  labs(x="", y="Percentage") + 
  ggtitle("Self Introspected Literacy (46 - 55)") +
  ylim(0,100) +
  theme_classic(); Literplot4


project_df2 %>% filter(agegroup == ("56 and above")) %>% count(fin_lite) -> Liter5
percentliter5 <- (Liter5$n/sum(Liter5$n)*100)
Liter5 <- cbind(Liter5, percentliter5)
colnames(Liter5) <- c("Literate", "Frequency", "Percentage")
Literplot5 <- ggplot(Liter5, aes(x=Literate, y= Percentage)) + 
  geom_bar(stat="identity", width = 0.75, fill = c("cyan", "red", "black")) +
  labs(x="", y="Percentage") + 
  ggtitle("Self Introspected Literacy (56 +)") +
  ylim(0,100) +
  theme_classic(); Literplot5

ggarrange(Literplot1, Literplot2, Literplot3, Literplot4, Literplot5)


##Plot 19 
#thoughts on share market

project_df2 %>% count(agegroup, gamb) -> gambdf
colnames(gambdf) <- c("Age", "Share_Market_Gambling", "Frequency")
gambplot <- ggplot(gambdf,aes(x=Age, y= Frequency, fill = Share_Market_Gambling)) + 
  geom_bar(position = "fill", stat = "identity", width = 0.5) + 
  scale_fill_brewer(palette = "Reds") +
  ggtitle("Thoughts on 'equity market is gambling'") +
  theme_classic() +
  theme(legend.title = element_blank()); gambplot



