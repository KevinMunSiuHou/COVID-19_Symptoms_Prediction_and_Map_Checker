#Library
library(ggplot2)

#------------------------Data Exploration---------------------------------
#Load data set
data_dir <- "E:/UM/Master/Sem 1/WQD7001 PRINCIPLES OF DATA SCIENCE/Assignment/Group/References/CleanData.csv"
clean_df <- read.csv(data_dir)

#Corona Results
ggplot(clean_df, aes(corona_result, fill = corona_result))+
  geom_bar()+theme_classic()+scale_color_brewer(palette = "Accent")+
  scale_fill_brewer(palette = "Accent")+
  theme(plot.background = element_rect(fill = "grey97"))+
  labs(title = "Bar graph of Corona Results", x = "Corona Results", y = "Count")

#Comparing between gender with corona results
ggplot(clean_df, aes(gender)) + 
  geom_bar(aes(fill = corona_result))

#Comparing between ages(60 and above) with corona results
ggplot(clean_df, aes(age_60_and_above)) + 
  geom_bar(aes(fill = corona_result))

#Comparing between cough with corona results
ggplot(clean_df, aes(cough)) + 
  geom_bar(aes(fill = corona_result))

#Comparing between fever with corona results
ggplot(clean_df, aes(fever)) + 
  geom_bar(aes(fill = corona_result))

#Comparing between sore_throat with corona results
ggplot(clean_df, aes(sore_throat)) + 
  geom_bar(aes(fill = corona_result))

#Comparing between shortness_of_breath with corona results
ggplot(clean_df, aes(shortness_of_breath)) + 
  geom_bar(aes(fill = corona_result))

#Comparing between head_ache with corona results
ggplot(clean_df, aes(head_ache)) + 
  geom_bar(aes(fill = corona_result))

#Comparing between Abroad with corona results
ggplot(clean_df, aes(Abroad)) + 
  geom_bar(aes(fill = corona_result))

#Comparing between Contact with corona results
ggplot(clean_df, aes(Contact)) + 
  geom_bar(aes(fill = corona_result))

#Comparing between No_activite with corona results
ggplot(clean_df, aes(No_activite)) + 
  geom_bar(aes(fill = corona_result))