# edema.pregnancy
edema and pregnancy related issues
#dplyr, tidyr, table1, ggplot2, gtsummary
data<-read_sav(file="E:/Paper 3/Mod_400_data.sav")
data
data$AGE<- as.factor(data$AGE)
data1 = data %>%
  mutate(
    ageC = factor(agec,
                  levels = c(1, 2, 3),
                  labels = c("Early child bearing", "Perfect age", "Late child bearing")),
    
    residenceC = factor(Residence,
                        levels = c(1, 2),
                        labels = c("Urban", "Rural")),
    
    religionC = factor(Religion,
                       levels = c(1, 2, 3),
                       labels = c("Muslim", "Hindu", "Others")),
    
    occupationC = factor(Occupation,
                         levels = c(1, 2),
                         labels = c("Housewife", "Service")),
    
    edemaC = factor(edema,
                    levels = c(1, 2),
                    labels = c("Yes", "No"))
  )
table(data1$ageC)
table(data1$residenceC)
table(data1$religionC)
table(data1$occupationC)
table(data1$edemaC)


 


