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
 scar_painC = factor(scar_pain,
                        levels = c(1, 2),
                        labels = c("Yes", "No")),
    
    urine_infectionC = factor(urine_infection,
                              levels = c(1, 2),
                              labels = c("Yes", "No")),
    
    preclampsiaC = factor(preclampsia,
                          levels = c(1, 2),
                          labels = c("Yes", "No")),
    
    vomitingC = factor(vomiting,
                       levels = c(1, 2),
                       labels = c("Yes", "No")),
    
    anemiaC = factor(anemia,
                     levels = c(1, 2),
                     labels = c("Yes", "No")),
    
    Geststional_HypertentionC = factor(Geststional_Hypertention,
                                       levels = c(1, 2),
                                       labels = c("Yes", "No")),
    
    Gestational_DiabetesC = factor(Gestational_Diabetes,
                                   levels = c(1, 2),
                                   labels = c("Yes", "No")),
    
    exerciseC = factor(exercise,
                       levels = c(1, 2),
                       labels = c("Yes", "No")),
    
    healthy_dietC = factor(healthy_diet,
                           levels = c(1, 2),
                           labels = c("Yes", "No")),
    
    taking_medicineC = factor(taking_medicine,
                              levels = c(1, 2),
                              labels = c("Yes", "No")),
    
    miscarriageC = factor(miscarriage,
                          levels = c(1, 2),
                          labels = c("Yes", "No"))
  )
table(data1$ageC)
table(data1$residenceC)
table(data1$religionC)
table(data1$occupationC)
table(data1$edemaC)
table(data1$scar_painC)
table(data1$urine_infectionC)
table(data1$preclampsiaC)
table(data1$vomitingC)
table(data1$anemiaC)
table(data1$Geststional_HypertentionC)
table(data1$Gestational_DiabetesC)
table(data1$exerciseC)
table(data1$healthy_dietC)
table(data1$taking_medicineC)
table(data1$miscarriageC)

 


