# edema.pregnancy
edema and pregnancy related issues
#dplyr, tidyr, table1, ggplot2, gtsummary
data<-read_sav(file="E:/Paper 3/Mod_400_data.sav")
data
data$AGE<- as.factor(data$AGE)

data1= data %>% 
  mutate(ageC=factor(
    x=agec,
    levels = c(1,2,3),
    labels = c("early child bearing","perfect age","late child bearing")
                    ))
data1= data %>% 
  mutate(residenceC=factor(
    x=Residence,
    levels = c(1,2),
    labels = c("urban","rural")
  ))
data1= data %>% 
  mutate(religionC=factor(
    x=Religion,
    levels = c(1,2,3),
    labels = c("muslim","hindu","others")
  ))
data1= data %>% 
  mutate(occupationC=factor(
    x=Occupation,
    levels = c(1,2),
    labels = c("housewife","service")
  ))
data1= data %>% 
  mutate(edemaC=factor(
    x=edema,
    levels = c(1,2),
    labels = c("yes","no")
  ))
glimpse(data1)

)
# Check frequency
table(data$edemaC, useNA = "ifany")

# Check percentage
prop.table(
  table(data$edema)
) * 100

# Check variable label
attr(data$edema, "label")

# Check value labels
attr(data$edema, "labels")
grep(
  "edema|preg|diabet|hyper|anemia|ANC|exercise|infection|BMI",
  names(data),
  ignore.case = TRUE,
  value = TRUE
)

table(data$edema, useNA = "ifany")
table(data$ANC, useNA = "ifany")
table(data$exercise, useNA = "ifany")
data1 %>%
  tbl_summary(
    missing = "ifany"
  ) %>%
  bold_labels()
