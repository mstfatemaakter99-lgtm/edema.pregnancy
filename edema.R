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
    
    urine_infectionC = factor(UTI,
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



# ==========================================
# TABLE 1: Sociodemographic and obstetric characteristics
# ==========================================

table1 <- data1 %>%
  select(
    ageC,
    residenceC,
    IncomeC,
    BMIC,
    Pregnancy_week_deliveryC,
    ANCC
  ) %>%
  tbl_summary(
    type = everything() ~ "categorical",
    
    statistic = all_categorical() ~ "{n} ({p}%)",
    
    digits = all_categorical() ~ c(0, 1),
    
    label = list(
      ageC ~ "Maternal age",
      residenceC ~ "Place of residence",
      IncomeC ~ "Family monthly income",
      BMIC ~ "Body mass index (BMI)",
      Pregnancy_week_deliveryC ~ "Gestational age at delivery",
      ANCC ~ "Antenatal care (ANC) visits"
    ),
    
    missing = "ifany",
    missing_text = "Missing"
  ) %>%
  add_n() %>%
  modify_header(
    label ~ "**Characteristics**",
    stat_0 ~ "**Overall, n (%)**"
  ) %>%
  modify_caption(
    "**Table 1. Sociodemographic, anthropometric and obstetric characteristics of mothers (N = 400)**"
  ) %>%
  bold_labels()

# Display Table 1
table1



# ==========================================
# TABLE 2: Prevalence of pregnancy complications
# ==========================================

table2 <- data1 %>%
  select(
    edemaC,
    Geststional_HypertentionC,
    preclampsiaC,
    Gestational_DiabetesC,
    anemiaC
  ) %>%
  tbl_summary(
    type = everything() ~ "categorical",
    
    statistic = all_categorical() ~ "{n} ({p}%)",
    
    digits = all_categorical() ~ c(0, 1),
    
    label = list(
      edemaC ~ "Edema",
      Geststional_HypertentionC ~ "Gestational hypertension",
      preclampsiaC ~ "Preeclampsia",
      Gestational_DiabetesC ~ "Gestational diabetes mellitus",
      anemiaC ~ "Anemia"
    ),
    
    missing = "ifany",
    missing_text = "Missing"
  ) %>%
  add_n() %>%
  modify_header(
    label ~ "**Pregnancy-related complications**",
    stat_0 ~ "**Overall, n (%)**"
  ) %>%
  modify_caption(
    "**Table 2. Prevalence of edema and pregnancy-related complications among mothers (N = 400)**"
  ) %>%
  bold_labels()

# Display Table 2
table2


# ==========================================
# TABLE 3: Association between edema and
# pregnancy-related complications
# ==========================================



table3 <- data1 %>%
  select(
    edemaC,
    ANCC,
    healthy_dietC,
    exerciseC,
    Gestational_DiabetesC,
    Geststional_HypertentionC,
    anemiaC,
    preclampsiaC,
    vomitingC,
    urine_infectionC,
    residenceC
  ) %>%
  tbl_summary(
    by = edemaC,
    type = everything() ~ "categorical",
    statistic = all_categorical() ~ "{n} ({p}%)",
    digits = all_categorical() ~ c(0, 1),
    missing = "ifany",
    missing_text = "Missing",
    label = list(
      ANCC ~ "Antenatal care visits",
      healthy_dietC ~ "Healthy diet",
      exerciseC ~ "Regular exercise",
      Gestational_DiabetesC ~ "Gestational diabetes mellitus",
      Geststional_HypertentionC ~ "Gestational hypertension",
      anemiaC ~ "Anemia",
      preclampsiaC ~ "Preeclampsia",
      vomitingC ~ "Vomiting",
      urine_infectionC ~ "Urinary tract infection",
      residenceC ~ "Place of residence"
    )
  ) %>%
  add_p(
    test = all_categorical() ~ "chisq.test"
  ) %>%
  add_n() %>%
  modify_header(
    label ~ "**Characteristics**",
    stat_1 ~ "**Edema: Yes (n = 100)**",
    stat_2 ~ "**Edema: No (n = 300)**",
    p.value ~ "**P-value**"
  ) %>%
  modify_caption(
    "**Table 3. Association between maternal edema and pregnancy-related characteristics (N = 400)**"
  ) %>%
  bold_labels()

# Display Table 3
table3


table3 %>%
  as_flex_table() %>%
  save_as_docx(
    path = "Table_3_Edema_Association.docx"
  )


# Create binary outcome: Yes = 1, No = 0

data1 <- data1 %>%
  mutate(
    edema_binary = ifelse(edemaC == "Yes", 1, 0)
  )

# Fit binary logistic regression

model1 <- glm(
  edema_binary ~ urine_infectionC +
    Geststional_HypertentionC +
    Gestational_DiabetesC +
    exerciseC +
    ANCC +
    anemiaC,
  data = data1,
  family = binomial(link = "logit")
)

# View regression results
summary(model1)
exists("model1")


# Extract coefficients and confidence intervals from model1
model_summary <- summary(model1)$coefficients
ci <- suppressMessages(confint.default(model1))  # Wald 95% CI on log-odds scale

forest_data <- data.frame(
  term = rownames(model_summary),
  estimate = model_summary[, "Estimate"],
  lower = ci[, 1],
  upper = ci[, 2],
  p_value = model_summary[, "Pr(>|z|)"],
  row.names = NULL
)

# Exclude the intercept and convert log-odds to adjusted odds ratios
forest_data <- subset(forest_data, term != "(Intercept)")
forest_data$OR <- exp(forest_data$estimate)
forest_data$CI_lower <- exp(forest_data$lower)
forest_data$CI_upper <- exp(forest_data$upper)

# Use readable labels; comparisons follow the factor coding in your model
forest_data$label <- c(
  "Urinary tract infection: No",
  "Gestational hypertension: No",
  "Gestational diabetes: No",
  "Regular exercise: No",
  "ANC visits: 4 or more",
  "Anemia: No"
)

# Order rows as they appear in the model output
forest_data$label <- factor(forest_data$label, levels = rev(forest_data$label))

# Create forest plot
p <- ggplot(forest_data, aes(x = OR, y = label)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
  geom_errorbar(
    aes(xmin = CI_lower, xmax = CI_upper),
    width = 0.18,
    orientation = "y",
    linewidth = 0.7
  ) +
  geom_point(size = 2.8, shape = 15, color = "#1F4E79") +
  scale_x_log10() +
  labs(
    title = "Factors associated with maternal edema",
    x = "Adjusted odds ratio (log scale, 95% CI)",
    y = NULL
  ) +
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.y = element_text(color = "black"),
    axis.text.x = element_text(color = "black")
  )

print(p)

# Save as a high-resolution image
ggsave(
  "maternal_edema_forest_plot.png",
  plot = p,
  width = 8,
  height = 5,
  dpi = 600
)

 


