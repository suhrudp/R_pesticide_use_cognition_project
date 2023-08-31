# set working directory
setwd("C:/Users/SURHUD/Desktop/Desktop/Stats/CBR/11. Pesticide Use and Cognition")

# load libraries
library(readxl)
library(tidyverse)
library(gtsummary)
library(flextable)
library(visreg)
library(ggsci)
library(emmeans)
library(gridExtra)

# import data
df <- read_excel("C:/Users/SURHUD/Desktop/Desktop/Stats/CBR/11. Pesticide Use and Cognition/Data.xlsx")
df %>% colnames()
attach(df)
# df <- df[!(`cdr6:cdr_interpretation_clinical` == "0.5"),]

# create a summary statistics table
table1 <- df[,c(3,2,16,18,13,14,47,17)] %>% 
            tbl_summary(by = "questionnaire_8:use_pesticides_socio",
                        missing_text = "Missing Data",
                        digits = list(everything() ~ 0)) %>% 
            add_p() %>%
            add_overall()

table1 %>%
  as_flex_table() %>%
  save_as_docx(path = "Table 1.docx")

# summary outcome data
df[,c(4,26:42)] %>%
  tbl_summary(type = list(where(is.numeric) ~ "continuous"))

# boxplots for outcome data
plot8 <- ggplot(df) + 
  geom_boxplot(aes(x = "", 
                   y = `hmse_score0:score_total_clinical`)) +
  labs(y = "HMSE",
       x = "") +
  guides(color="none") +
  scale_color_jco()

plot9 <- ggplot(df) + 
  geom_boxplot(aes(x = "", 
                   y = reaction_time_EP01M_cognito)) +
  labs(y = "Reaction Time (ms)",
       x = "") +
  guides(color="none") +
  scale_color_jco()

plot10 <- ggplot(df) + 
  geom_boxplot(aes(x = "", 
                   y = reading_and_comprehension_EP03BR_cognito)) +
  labs(y = "Reading & Comprehension",
       x = "") +
  guides(color="none") +
  scale_color_jco()

plot11 <- ggplot(df) + 
  geom_boxplot(aes(x = "", 
                   y = auditory_attention_EP04BRT_cognito)) +
  labs(y = "Auditory Attention",
       x = "") +
  guides(color="none") +
  scale_color_jco()

plot12 <- ggplot(df) + 
  geom_boxplot(aes(x = "", 
                   y = visual_attention_EP05BRT_cognito)) +
  labs(y = "Visual Attention",
       x = "") +
  guides(color="none") +
  scale_color_jco()

plot13 <- ggplot(df) + 
  geom_boxplot(aes(x = "", 
                   y = episodic_memory_immediate_recall_EP07ET03MR_cognito)) +
  labs(y = "Episodic Memory (immediate recall)",
       x = "") +
  guides(color="none") +
  scale_color_jco()

plot14 <- ggplot(df) + 
  geom_boxplot(aes(x = "", 
                   y = visuospatial_span_EP10RE_cognito)) +
  labs(y = "Visuospatial Span",
       x = "") +
  guides(color="none") +
  scale_color_jco()

plot15 <- ggplot(df) + 
  geom_boxplot(aes(x = "", 
                   y = form_matching_EP11BR_cognito)) +
  labs(y = "Form Matching",
       x = "") +
  guides(color="none") +
  scale_color_jco()

plot16 <- ggplot(df) + 
  geom_boxplot(aes(x = "", 
                   y = phoneme_comprehension_EP12BR_cognito)) +
  labs(y = "Phonemic Comprehension",
       x = "") +
  guides(color="none") +
  scale_color_jco()

plot17 <- ggplot(df) + 
  geom_boxplot(aes(x = "", 
                   y = semantic_associations_EP13BA_cognito)) +
  labs(y = "Semantic Associations",
       x = "") +
  guides(color="none") +
  scale_color_jco()

plot18 <- ggplot(df) + 
  geom_boxplot(aes(x = "", 
                   y = fluid_intelligence_EP14BR_cognito)) +
  labs(y = "Fluid Intelligence",
       x = "") +
  guides(color="none") +
  scale_color_jco()

plot19 <- ggplot(df) + 
  geom_boxplot(aes(x = "", 
                   y = episodic_memory_delayed_recall_EP17ET01BR_cognito)) +
  labs(y = "Episodic Memory (delayed recall)",
       x = "") +
  guides(color="none") +
  scale_color_jco()

plot20 <- ggplot(df) + 
  geom_boxplot(aes(x = "", 
                   y = name_face_recognition_EP18BN_cognito)) +
  labs(y = "Name-Face Recognition",
       x = "") +
  guides(color="none") +
  scale_color_jco()

plot21 <- ggplot(df) + 
  geom_boxplot(aes(x = "", 
                   y = semantic_fluency_EP19C_cognito)) +
  labs(y = "Semantic Fluency",
       x = "") +
  guides(color="none") +
  scale_color_jco()

plot22 <- ggplot(df) + 
  geom_boxplot(aes(x = "", 
                   y = phonemic_fluency_EP20C_cognito)) +
  labs(y = "Phonemic Fluency",
       x = "") +
  guides(color="none") +
  scale_color_jco()

plot23 <- ggplot(df) + 
  geom_boxplot(aes(x = "", 
                   y = vocabulary_EP23BR_cognito)) +
  labs(y = "Vocabulary",
       x = "") +
  guides(color="none") +
  scale_color_jco()

plot24 <- ggplot(df) + 
  geom_boxplot(aes(x = "", 
                   y = implicit_memory_EP24DF_cognito)) +
  labs(y = "Implicit Memory",
       x = "") +
  guides(color="none") +
  scale_color_jco()

plot25 <- ggplot(df) + 
  geom_boxplot(aes(x = "", 
                   y = visuospatial_construction_EP25T_cognito)) +
  labs(y = "Visuospatial Construction",
       x = "") +
  guides(color="none") +
  scale_color_jco()

finalplot <- grid.arrange(plot8, plot9, plot10, plot11, plot12, plot13, 
                          plot14, plot15, plot16, plot17, plot18, plot19, 
                          plot20, plot21, plot22, plot23, plot24, plot25, 
                          ncol = 3, nrow = 6)
ggsave(plot = finalplot,
       filename = "Plot 1_1.png",
       height = 25,
       width = 20,
       dpi = 900)

# association of pesticide usage on global cognition
mod1 <- lm(data = df, 
            `hmse_score0:score_total_clinical` ~ `questionnaire_8:use_pesticides_socio` + `cdr6:mci_clinical`  + 
              `intro_header1:age_clinical` + 
              `intro_header1:gender_clinical` + 
              `questionnaire_5:education_years_socio` + 
              `health_questionnaire_3:diabetes_clinical` +
              `health_questionnaire_3:hypertension_clinical`)

mod1 %>%
  tbl_regression()

emm1 <- as.data.frame(emmeans(mod1, pairwise~`questionnaire_8:use_pesticides_socio`))

# plot the results
plot1 <- ggplot(emm1) + 
  geom_pointrange(aes(x = `questionnaire_8:use_pesticides_socio`,
                      y = `emmean`,
                      ymin = `lower.CL`,
                      ymax = `upper.CL`,
                      color = `questionnaire_8:use_pesticides_socio`),
                  size = 1,
                  lwd = 1) +
  labs(title = "HMSE, p = 0.134",
       y = "Adjusted Mean",
       x = "Pesticide Use") +
  guides(color="none") +
  scale_color_jco()

ggsave(plot1,
       filename = "Plot 1.png",
       height = 4,
       width = 8,
       dpi = 1200)

# association of pesticide usage on specific cognitive domains
mod8 <- lm(data = df, 
            `reaction_time_EP01M_cognito` ~ `questionnaire_8:use_pesticides_socio` + `cdr6:mci_clinical` + 
              `intro_header1:age_clinical` + 
              `intro_header1:gender_clinical` + 
              `questionnaire_5:education_years_socio` + 
              `health_questionnaire_3:diabetes_clinical` +
              `health_questionnaire_3:hypertension_clinical`)

table8 <- mod8 %>%
  tbl_regression()

mod9 <- lm(data = df, 
            `reading_and_comprehension_EP03BR_cognito` ~ `questionnaire_8:use_pesticides_socio` + `cdr6:mci_clinical` + 
              `intro_header1:age_clinical` + 
              `intro_header1:gender_clinical` + 
              `questionnaire_5:education_years_socio` + 
              `health_questionnaire_3:diabetes_clinical` +
              `health_questionnaire_3:hypertension_clinical`)

table9 <- mod9 %>%
  tbl_regression()

mod10 <- lm(data = df, 
            `auditory_attention_EP04BRT_cognito` ~ `questionnaire_8:use_pesticides_socio` + `cdr6:mci_clinical` + 
              `intro_header1:age_clinical` + 
              `intro_header1:gender_clinical` + 
              `questionnaire_5:education_years_socio` + 
              `health_questionnaire_3:diabetes_clinical` +
              `health_questionnaire_3:hypertension_clinical`)

table10 <- mod10 %>%
  tbl_regression()

mod11 <- lm(data = df, 
            `visual_attention_EP05BRT_cognito` ~ `questionnaire_8:use_pesticides_socio` + `cdr6:mci_clinical` + 
              `intro_header1:age_clinical` + 
              `intro_header1:gender_clinical` + 
              `questionnaire_5:education_years_socio` + 
              `health_questionnaire_3:diabetes_clinical` +
              `health_questionnaire_3:hypertension_clinical`)

table11 <- mod11 %>%
  tbl_regression()

mod12 <- lm(data = df, 
            `episodic_memory_immediate_recall_EP07ET03MR_cognito` ~ `questionnaire_8:use_pesticides_socio` + `cdr6:mci_clinical` + 
              `intro_header1:age_clinical` + 
              `intro_header1:gender_clinical` + 
              `questionnaire_5:education_years_socio` + 
              `health_questionnaire_3:diabetes_clinical` +
              `health_questionnaire_3:hypertension_clinical`)

table12 <- mod12 %>%
  tbl_regression()

mod13 <- lm(data = df, 
            `visuospatial_span_EP10RE_cognito` ~ `questionnaire_8:use_pesticides_socio` + `cdr6:mci_clinical` + 
              `intro_header1:age_clinical` + 
              `intro_header1:gender_clinical` + 
              `questionnaire_5:education_years_socio` + 
              `health_questionnaire_3:diabetes_clinical` +
              `health_questionnaire_3:hypertension_clinical`)

table13 <- mod13 %>%
  tbl_regression()

mod14 <- lm(data = df, 
            `form_matching_EP11BR_cognito` ~ `questionnaire_8:use_pesticides_socio` + `cdr6:mci_clinical` + 
              `intro_header1:age_clinical` + 
              `intro_header1:gender_clinical` + 
              `questionnaire_5:education_years_socio` + 
              `health_questionnaire_3:diabetes_clinical` +
              `health_questionnaire_3:hypertension_clinical`)

table14 <- mod14 %>%
  tbl_regression()

mod15 <- lm(data = df, 
            `phoneme_comprehension_EP12BR_cognito` ~ `questionnaire_8:use_pesticides_socio` + `cdr6:mci_clinical` + 
              `intro_header1:age_clinical` + 
              `intro_header1:gender_clinical` + 
              `questionnaire_5:education_years_socio` + 
              `health_questionnaire_3:diabetes_clinical` +
              `health_questionnaire_3:hypertension_clinical`)

table15 <- mod15 %>%
  tbl_regression()

mod16 <- lm(data = df, 
            `semantic_associations_EP13BA_cognito` ~ `questionnaire_8:use_pesticides_socio` + `cdr6:mci_clinical` + 
              `intro_header1:age_clinical` + 
              `intro_header1:gender_clinical` + 
              `questionnaire_5:education_years_socio` + 
              `health_questionnaire_3:diabetes_clinical` +
              `health_questionnaire_3:hypertension_clinical`)

table16 <- mod16 %>%
  tbl_regression()

mod17 <- lm(data = df, 
            `fluid_intelligence_EP14BR_cognito` ~ `questionnaire_8:use_pesticides_socio` + `cdr6:mci_clinical` + 
              `intro_header1:age_clinical` + 
              `intro_header1:gender_clinical` + 
              `questionnaire_5:education_years_socio` + 
              `health_questionnaire_3:diabetes_clinical` +
              `health_questionnaire_3:hypertension_clinical`)

table17 <- mod17 %>%
  tbl_regression()

mod18 <- lm(data = df, 
            `episodic_memory_delayed_recall_EP17ET01BR_cognito` ~ `questionnaire_8:use_pesticides_socio` + `cdr6:mci_clinical` + 
              `intro_header1:age_clinical` + 
              `intro_header1:gender_clinical` + 
              `questionnaire_5:education_years_socio` + 
              `health_questionnaire_3:diabetes_clinical` +
              `health_questionnaire_3:hypertension_clinical`)

table18 <- mod18 %>%
  tbl_regression()

mod19 <- lm(data = df, 
            `name_face_recognition_EP18BN_cognito` ~ `questionnaire_8:use_pesticides_socio` + `cdr6:mci_clinical` + 
              `intro_header1:age_clinical` + 
              `intro_header1:gender_clinical` + 
              `questionnaire_5:education_years_socio` + 
              `health_questionnaire_3:diabetes_clinical` +
              `health_questionnaire_3:hypertension_clinical`)

table19 <- mod19 %>%
  tbl_regression()

mod20 <- lm(data = df, 
            `semantic_fluency_EP19C_cognito` ~ `questionnaire_8:use_pesticides_socio` + `cdr6:mci_clinical` + 
              `intro_header1:age_clinical` + 
              `intro_header1:gender_clinical` + 
              `questionnaire_5:education_years_socio` + 
              `health_questionnaire_3:diabetes_clinical` +
              `health_questionnaire_3:hypertension_clinical`)

table20 <- mod20 %>%
  tbl_regression()

mod21 <- lm(data = df, 
            `phonemic_fluency_EP20C_cognito` ~ `questionnaire_8:use_pesticides_socio` + `cdr6:mci_clinical` + 
              `intro_header1:age_clinical` + 
              `intro_header1:gender_clinical` + 
              `questionnaire_5:education_years_socio` + 
              `health_questionnaire_3:diabetes_clinical` +
              `health_questionnaire_3:hypertension_clinical`)

table21 <- mod21 %>%
  tbl_regression()

mod22 <- lm(data = df, 
            `vocabulary_EP23BR_cognito` ~ `questionnaire_8:use_pesticides_socio` + `cdr6:mci_clinical` + 
              `intro_header1:age_clinical` + 
              `intro_header1:gender_clinical` + 
              `questionnaire_5:education_years_socio` + 
              `health_questionnaire_3:diabetes_clinical` +
              `health_questionnaire_3:hypertension_clinical`)

table22 <- mod22 %>%
  tbl_regression()

mod23 <- lm(data = df, 
            `implicit_memory_EP24DF_cognito` ~ `questionnaire_8:use_pesticides_socio` + `cdr6:mci_clinical` + 
              `intro_header1:age_clinical` + 
              `intro_header1:gender_clinical` + 
              `questionnaire_5:education_years_socio` + 
              `health_questionnaire_3:diabetes_clinical` +
              `health_questionnaire_3:hypertension_clinical`)

table23 <- mod23 %>%
  tbl_regression()

mod24 <- lm(data = df, 
            `visuospatial_construction_EP25T_cognito` ~ `questionnaire_8:use_pesticides_socio` + `cdr6:mci_clinical` + 
              `intro_header1:age_clinical` + 
              `intro_header1:gender_clinical` + 
              `questionnaire_5:education_years_socio` + 
              `health_questionnaire_3:diabetes_clinical` +
              `health_questionnaire_3:hypertension_clinical`)

table24 <- mod24 %>%
  tbl_regression()

table2 <- tbl_merge(tbls = list(table8, table9, table10, table11,
                      table12, table13, table14, table15,
                      table16, table17, table18, table19,
                      table20, table21, table22, table23,
                      table24),
                tab_spanner = c("Reaction Time (ms)", "Reading and Comprehension",
                          "Auditory Attention", "Visual Attention",
                          "Episodic Memory (Immediate Recall)", "Visuospatial Span",
                          "Form Matching", "Phoneme Comprehension",
                          "Semantic Association", "Fluid Intelligence",
                          "Episodic Memory (Delayed Recall)",
                          "Name Face Recognition", "Semantic Fluency",
                          "Phonemic Fluency", "Vocabulary",
                          "Implicit Memory", "Visuospatial Construction"))
table2 %>%
  as_flex_table() %>%
  save_as_docx(path = "Table 2.docx")

# plot EMMs
emm8 <- as.data.frame(emmeans(mod8, ~`questionnaire_8:use_pesticides_socio`))
emm9 <- as.data.frame(emmeans(mod9, ~`questionnaire_8:use_pesticides_socio`))
emm10 <- as.data.frame(emmeans(mod10, ~`questionnaire_8:use_pesticides_socio`))
emm11 <- as.data.frame(emmeans(mod11, ~`questionnaire_8:use_pesticides_socio`))
emm12 <- as.data.frame(emmeans(mod12, ~`questionnaire_8:use_pesticides_socio`))
emm13 <- as.data.frame(emmeans(mod13, ~`questionnaire_8:use_pesticides_socio`))
emm14 <- as.data.frame(emmeans(mod14, ~`questionnaire_8:use_pesticides_socio`))
emm15 <- as.data.frame(emmeans(mod15, ~`questionnaire_8:use_pesticides_socio`))
emm16 <- as.data.frame(emmeans(mod16, ~`questionnaire_8:use_pesticides_socio`))
emm17 <- as.data.frame(emmeans(mod17, ~`questionnaire_8:use_pesticides_socio`))
emm18 <- as.data.frame(emmeans(mod18, ~`questionnaire_8:use_pesticides_socio`))
emm19 <- as.data.frame(emmeans(mod19, ~`questionnaire_8:use_pesticides_socio`))
emm20 <- as.data.frame(emmeans(mod20, ~`questionnaire_8:use_pesticides_socio`))
emm21 <- as.data.frame(emmeans(mod21, ~`questionnaire_8:use_pesticides_socio`))
emm22 <- as.data.frame(emmeans(mod22, ~`questionnaire_8:use_pesticides_socio`))
emm23 <- as.data.frame(emmeans(mod23, ~`questionnaire_8:use_pesticides_socio`))
emm24 <- as.data.frame(emmeans(mod24, ~`questionnaire_8:use_pesticides_socio`))

plot8 <- ggplot(emm8) + 
  geom_pointrange(aes(x = `questionnaire_8:use_pesticides_socio`,
                                   y = `emmean`,
                                   ymin = `lower.CL`,
                                   ymax = `upper.CL`,
                      color = `questionnaire_8:use_pesticides_socio`),
                  size = 1,
                  lwd = 1) +
  labs(title = "Reaction Time (ms), p = 0.6",
       y = "Adjusted Mean",
       x = "Pesticide Use") +
  guides(color="none") +
  scale_color_jco()
 
plot9 <- ggplot(emm9) + 
  geom_pointrange(aes(x = `questionnaire_8:use_pesticides_socio`,
                      y = `emmean`,
                      ymin = `lower.CL`,
                      ymax = `upper.CL`,
                      color = `questionnaire_8:use_pesticides_socio`),
                  size = 1,
                  lwd = 1) +
  labs(title = "Reading & Comprehension, p = 0.03",
       y = "Adjusted Mean",
       x = "Pesticide Use") +
  guides(color="none") +
  scale_color_jco()

plot10 <- ggplot(emm10) + 
  geom_pointrange(aes(x = `questionnaire_8:use_pesticides_socio`,
                      y = `emmean`,
                      ymin = `lower.CL`,
                      ymax = `upper.CL`,
                      color = `questionnaire_8:use_pesticides_socio`),
                  size = 1,
                  lwd = 1) +
  labs(title = "Auditory Attention, p = 0.005",
       y = "Adjusted Mean",
       x = "Pesticide Use") +
  guides(color="none") +
  scale_color_jco()

plot11 <- ggplot(emm11) + 
  geom_pointrange(aes(x = `questionnaire_8:use_pesticides_socio`,
                      y = `emmean`,
                      ymin = `lower.CL`,
                      ymax = `upper.CL`,
                      color = `questionnaire_8:use_pesticides_socio`),
                  size = 1,
                  lwd = 1) +
  labs(title = "Visual Attention, p = 0.4",
       y = "Adjusted Mean",
       x = "Pesticide Use") +
  guides(color="none") +
  scale_color_jco()

plot12 <- ggplot(emm12) + 
  geom_pointrange(aes(x = `questionnaire_8:use_pesticides_socio`,
                      y = `emmean`,
                      ymin = `lower.CL`,
                      ymax = `upper.CL`,
                      color = `questionnaire_8:use_pesticides_socio`),
                  size = 1,
                  lwd = 1) +
  labs(title = "Episodic Memory, p = 0.6",
       y = "Adjusted Mean",
       x = "Pesticide Use") +
  guides(color="none") +
  scale_color_jco()

plot13 <- ggplot(emm13) + 
  geom_pointrange(aes(x = `questionnaire_8:use_pesticides_socio`,
                      y = `emmean`,
                      ymin = `lower.CL`,
                      ymax = `upper.CL`,
                      color = `questionnaire_8:use_pesticides_socio`),
                  size = 1,
                  lwd = 1) +
  labs(title = "Visuospatial Span, p = 0.019",
       y = "Adjusted Mean",
       x = "Pesticide Use") +
  guides(color="none") +
  scale_color_jco()

plot14 <- ggplot(emm14) + 
  geom_pointrange(aes(x = `questionnaire_8:use_pesticides_socio`,
                      y = `emmean`,
                      ymin = `lower.CL`,
                      ymax = `upper.CL`,
                      color = `questionnaire_8:use_pesticides_socio`),
                  size = 1,
                  lwd = 1) +
  labs(title = "Form Matching, p = 0.005",
       y = "Adjusted Mean",
       x = "Pesticide Use") +
  guides(color="none") +
  scale_color_jco()

plot15 <- ggplot(emm15) + 
  geom_pointrange(aes(x = `questionnaire_8:use_pesticides_socio`,
                      y = `emmean`,
                      ymin = `lower.CL`,
                      ymax = `upper.CL`,
                      color = `questionnaire_8:use_pesticides_socio`),
                  size = 1,
                  lwd = 1) +
  labs(title = "Phoneme Comprehension, p = 0.076",
       y = "Adjusted Mean",
       x = "Pesticide Use") +
  guides(color="none") +
  scale_color_jco()

plot16 <- ggplot(emm16) + 
  geom_pointrange(aes(x = `questionnaire_8:use_pesticides_socio`,
                      y = `emmean`,
                      ymin = `lower.CL`,
                      ymax = `upper.CL`,
                      color = `questionnaire_8:use_pesticides_socio`),
                  size = 1,
                  lwd = 1) +
  labs(title = "Semantic Associations, p = 0.2",
       y = "Adjusted Mean",
       x = "Pesticide Use") +
  guides(color="none") +
  scale_color_jco()

plot17 <- ggplot(emm17) + 
  geom_pointrange(aes(x = `questionnaire_8:use_pesticides_socio`,
                      y = `emmean`,
                      ymin = `lower.CL`,
                      ymax = `upper.CL`,
                      color = `questionnaire_8:use_pesticides_socio`),
                  size = 1,
                  lwd = 1) +
  labs(title = "Fluid Intelligence, p = 0.059",
       y = "Adjusted Mean",
       x = "Pesticide Use") +
  guides(color="none") +
  scale_color_jco()

plot18 <- ggplot(emm18) + 
  geom_pointrange(aes(x = `questionnaire_8:use_pesticides_socio`,
                      y = `emmean`,
                      ymin = `lower.CL`,
                      ymax = `upper.CL`,
                      color = `questionnaire_8:use_pesticides_socio`),
                  size = 1,
                  lwd = 1) +
  labs(title = "Episodic Memory, p = 0.2",
       y = "Adjusted Mean",
       x = "Pesticide Use") +
  guides(color="none") +
  scale_color_jco()

plot19 <- ggplot(emm19) + 
  geom_pointrange(aes(x = `questionnaire_8:use_pesticides_socio`,
                      y = `emmean`,
                      ymin = `lower.CL`,
                      ymax = `upper.CL`,
                      color = `questionnaire_8:use_pesticides_socio`),
                  size = 1,
                  lwd = 1) +
  labs(title = "Name-Face Recognition, p = 0.6",
       y = "Adjusted Mean",
       x = "Pesticide Use") +
  guides(color="none") +
  scale_color_jco()

plot20 <- ggplot(emm20) + 
  geom_pointrange(aes(x = `questionnaire_8:use_pesticides_socio`,
                      y = `emmean`,
                      ymin = `lower.CL`,
                      ymax = `upper.CL`,
                      color = `questionnaire_8:use_pesticides_socio`),
                  size = 1,
                  lwd = 1) +
  labs(title = "Semantic Fluency, p = 0.3",
       y = "Adjusted Mean",
       x = "Pesticide Use") +
  guides(color="none") +
  scale_color_jco()

plot21 <- ggplot(emm21) + 
  geom_pointrange(aes(x = `questionnaire_8:use_pesticides_socio`,
                      y = `emmean`,
                      ymin = `lower.CL`,
                      ymax = `upper.CL`,
                      color = `questionnaire_8:use_pesticides_socio`),
                  size = 1,
                  lwd = 1) +
  labs(title = "Phonemic Fluency, p >0.9",
       y = "Adjusted Mean",
       x = "Pesticide Use") +
  guides(color="none") +
  scale_color_jco()

plot22 <- ggplot(emm22) + 
  geom_pointrange(aes(x = `questionnaire_8:use_pesticides_socio`,
                      y = `emmean`,
                      ymin = `lower.CL`,
                      ymax = `upper.CL`,
                      color = `questionnaire_8:use_pesticides_socio`),
                  size = 1,
                  lwd = 1) +
  labs(title = "Vocabulary, p = 0.15",
       y = "Adjusted Mean",
       x = "Pesticide Use") +
  guides(color="none") +
  scale_color_jco()

plot23 <- ggplot(emm23) + 
  geom_pointrange(aes(x = `questionnaire_8:use_pesticides_socio`,
                      y = `emmean`,
                      ymin = `lower.CL`,
                      ymax = `upper.CL`,
                      color = `questionnaire_8:use_pesticides_socio`),
                  size = 1,
                  lwd = 1) +
  labs(title = "Implicit Memory, p = 0.3",
       y = "Adjusted Mean",
       x = "Pesticide Use") +
  guides(color="none") +
  scale_color_jco()

plot24 <- ggplot(emm24) + 
  geom_pointrange(aes(x = `questionnaire_8:use_pesticides_socio`,
                      y = `emmean`,
                      ymin = `lower.CL`,
                      ymax = `upper.CL`,
                      color = `questionnaire_8:use_pesticides_socio`),
                  size = 1,
                  lwd = 1) +
  labs(title = "Visuospatial Construction, p = 0.4",
       y = "Adjusted Mean",
       x = "Pesticide Use") +
  guides(color="none") +
  scale_color_jco()

finalplot <- grid.arrange(plot8, plot9, plot10, plot11, plot12, plot13, 
             plot14, plot15, plot16, plot17, plot18, plot19, 
             plot20, plot21, plot22, plot23, plot24, 
             ncol = 3, nrow = 6)
ggsave(plot = finalplot,
       filename = "Plot 2.png",
       height = 15,
       width = 20,
       dpi = 900)

# sensitivity analysis
dfsens <- df[!(`cdr6:cdr_interpretation_clinical` == "0.5"),]

mods1 <- lm(data = dfsens, 
          `hmse_score0:score_total_clinical` ~ `questionnaire_8:use_pesticides_socio` + 
            `intro_header1:age_clinical` + 
            `intro_header1:gender_clinical` + 
            `questionnaire_5:education_years_socio` + 
            `health_questionnaire_3:diabetes_clinical` +
            `health_questionnaire_3:hypertension_clinical`)

mods1 %>%
  tbl_regression()

emms1 <- as.data.frame(emmeans(mods1, pairwise~`questionnaire_8:use_pesticides_socio`))

mod8 <- lm(data = dfsens, 
           `reaction_time_EP01M_cognito` ~ `questionnaire_8:use_pesticides_socio` + 
             `intro_header1:age_clinical` + 
             `intro_header1:gender_clinical` + 
             `questionnaire_5:education_years_socio` + 
             `health_questionnaire_3:diabetes_clinical` +
             `health_questionnaire_3:hypertension_clinical`)

table8 <- mod8 %>%
  tbl_regression()

mod9 <- lm(data = dfsens, 
           `reading_and_comprehension_EP03BR_cognito` ~ `questionnaire_8:use_pesticides_socio` + 
             `intro_header1:age_clinical` + 
             `intro_header1:gender_clinical` + 
             `questionnaire_5:education_years_socio` + 
             `health_questionnaire_3:diabetes_clinical` +
             `health_questionnaire_3:hypertension_clinical`)

table9 <- mod9 %>%
  tbl_regression()

mod10 <- lm(data = dfsens, 
            `auditory_attention_EP04BRT_cognito` ~ `questionnaire_8:use_pesticides_socio` + 
              `intro_header1:age_clinical` + 
              `intro_header1:gender_clinical` + 
              `questionnaire_5:education_years_socio` + 
              `health_questionnaire_3:diabetes_clinical` +
              `health_questionnaire_3:hypertension_clinical`)

table10 <- mod10 %>%
  tbl_regression()

mod11 <- lm(data = dfsens, 
            `visual_attention_EP05BRT_cognito` ~ `questionnaire_8:use_pesticides_socio` + 
              `intro_header1:age_clinical` + 
              `intro_header1:gender_clinical` + 
              `questionnaire_5:education_years_socio` + 
              `health_questionnaire_3:diabetes_clinical` +
              `health_questionnaire_3:hypertension_clinical`)

table11 <- mod11 %>%
  tbl_regression()

mod12 <- lm(data = dfsens, 
            `episodic_memory_immediate_recall_EP07ET03MR_cognito` ~ `questionnaire_8:use_pesticides_socio` + 
              `intro_header1:age_clinical` + 
              `intro_header1:gender_clinical` + 
              `questionnaire_5:education_years_socio` + 
              `health_questionnaire_3:diabetes_clinical` +
              `health_questionnaire_3:hypertension_clinical`)

table12 <- mod12 %>%
  tbl_regression()

mod13 <- lm(data = dfsens, 
            `visuospatial_span_EP10RE_cognito` ~ `questionnaire_8:use_pesticides_socio` + 
              `intro_header1:age_clinical` + 
              `intro_header1:gender_clinical` + 
              `questionnaire_5:education_years_socio` + 
              `health_questionnaire_3:diabetes_clinical` +
              `health_questionnaire_3:hypertension_clinical`)

table13 <- mod13 %>%
  tbl_regression()

mod14 <- lm(data = dfsens, 
            `form_matching_EP11BR_cognito` ~ `questionnaire_8:use_pesticides_socio` + 
              `intro_header1:age_clinical` + 
              `intro_header1:gender_clinical` + 
              `questionnaire_5:education_years_socio` + 
              `health_questionnaire_3:diabetes_clinical` +
              `health_questionnaire_3:hypertension_clinical`)

table14 <- mod14 %>%
  tbl_regression()

mod15 <- lm(data = dfsens, 
            `phoneme_comprehension_EP12BR_cognito` ~ `questionnaire_8:use_pesticides_socio` +
              `intro_header1:age_clinical` + 
              `intro_header1:gender_clinical` + 
              `questionnaire_5:education_years_socio` + 
              `health_questionnaire_3:diabetes_clinical` +
              `health_questionnaire_3:hypertension_clinical`)

table15 <- mod15 %>%
  tbl_regression()

mod16 <- lm(data = dfsens, 
            `semantic_associations_EP13BA_cognito` ~ `questionnaire_8:use_pesticides_socio` +  
              `intro_header1:age_clinical` + 
              `intro_header1:gender_clinical` + 
              `questionnaire_5:education_years_socio` + 
              `health_questionnaire_3:diabetes_clinical` +
              `health_questionnaire_3:hypertension_clinical`)

table16 <- mod16 %>%
  tbl_regression()

mod17 <- lm(data = dfsens, 
            `fluid_intelligence_EP14BR_cognito` ~ `questionnaire_8:use_pesticides_socio` +
              `intro_header1:age_clinical` + 
              `intro_header1:gender_clinical` + 
              `questionnaire_5:education_years_socio` + 
              `health_questionnaire_3:diabetes_clinical` +
              `health_questionnaire_3:hypertension_clinical`)

table17 <- mod17 %>%
  tbl_regression()

mod18 <- lm(data = dfsens, 
            `episodic_memory_delayed_recall_EP17ET01BR_cognito` ~ `questionnaire_8:use_pesticides_socio` +
              `intro_header1:age_clinical` + 
              `intro_header1:gender_clinical` + 
              `questionnaire_5:education_years_socio` + 
              `health_questionnaire_3:diabetes_clinical` +
              `health_questionnaire_3:hypertension_clinical`)

table18 <- mod18 %>%
  tbl_regression()

mod19 <- lm(data = dfsens, 
            `name_face_recognition_EP18BN_cognito` ~ `questionnaire_8:use_pesticides_socio` +
              `intro_header1:age_clinical` + 
              `intro_header1:gender_clinical` + 
              `questionnaire_5:education_years_socio` + 
              `health_questionnaire_3:diabetes_clinical` +
              `health_questionnaire_3:hypertension_clinical`)

table19 <- mod19 %>%
  tbl_regression()

mod20 <- lm(data = dfsens, 
            `semantic_fluency_EP19C_cognito` ~ `questionnaire_8:use_pesticides_socio` +
              `intro_header1:age_clinical` + 
              `intro_header1:gender_clinical` + 
              `questionnaire_5:education_years_socio` + 
              `health_questionnaire_3:diabetes_clinical` +
              `health_questionnaire_3:hypertension_clinical`)

table20 <- mod20 %>%
  tbl_regression()

mod21 <- lm(data = dfsens, 
            `phonemic_fluency_EP20C_cognito` ~ `questionnaire_8:use_pesticides_socio` + 
              `intro_header1:age_clinical` + 
              `intro_header1:gender_clinical` + 
              `questionnaire_5:education_years_socio` + 
              `health_questionnaire_3:diabetes_clinical` +
              `health_questionnaire_3:hypertension_clinical`)

table21 <- mod21 %>%
  tbl_regression()

mod22 <- lm(data = dfsens, 
            `vocabulary_EP23BR_cognito` ~ `questionnaire_8:use_pesticides_socio` + 
              `intro_header1:age_clinical` + 
              `intro_header1:gender_clinical` + 
              `questionnaire_5:education_years_socio` + 
              `health_questionnaire_3:diabetes_clinical` +
              `health_questionnaire_3:hypertension_clinical`)

table22 <- mod22 %>%
  tbl_regression()

mod23 <- lm(data = dfsens, 
            `implicit_memory_EP24DF_cognito` ~ `questionnaire_8:use_pesticides_socio` +
              `intro_header1:age_clinical` + 
              `intro_header1:gender_clinical` + 
              `questionnaire_5:education_years_socio` + 
              `health_questionnaire_3:diabetes_clinical` +
              `health_questionnaire_3:hypertension_clinical`)

table23 <- mod23 %>%
  tbl_regression()

mod24 <- lm(data = dfsens, 
            `visuospatial_construction_EP25T_cognito` ~ `questionnaire_8:use_pesticides_socio` +
              `intro_header1:age_clinical` + 
              `intro_header1:gender_clinical` + 
              `questionnaire_5:education_years_socio` + 
              `health_questionnaire_3:diabetes_clinical` +
              `health_questionnaire_3:hypertension_clinical`)

table24 <- mod24 %>%
  tbl_regression()

table2s <- tbl_merge(tbls = list(table8, table9, table10, table11,
                                table12, table13, table14, table15,
                                table16, table17, table18, table19,
                                table20, table21, table22, table23,
                                table24),
                    tab_spanner = c("Reaction Time (ms)", "Reading and Comprehension",
                                    "Auditory Attention", "Visual Attention",
                                    "Episodic Memory (Immediate Recall)", "Visuospatial Span",
                                    "Form Matching", "Phoneme Comprehension",
                                    "Semantic Association", "Fluid Intelligence",
                                    "Episodic Memory (Delayed Recall)",
                                    "Name Face Recognition", "Semantic Fluency",
                                    "Phonemic Fluency", "Vocabulary",
                                    "Implicit Memory", "Visuospatial Construction"))
table2s %>%
  as_flex_table() %>%
  save_as_docx(path = "Table 2s.docx")
