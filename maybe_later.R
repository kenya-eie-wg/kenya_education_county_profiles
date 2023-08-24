results <- vector(mode = "list", length = 10)

for (i in 1:10) {
  
  n_samples <- rbinom(1, 100, runif(1))
  
  index <- sample.int(nrow(iris), size = n_samples, replace = TRUE)
  
  results[[i]] <- iris[index,]
  
}

my_df <- do.call(rbind, results)

dim(my_df)

read_xlsx("./data/county_profiles_raw.xlsx", 
          sheet = i, skip = 14) %>%
  slice(1:26) %>%  
  setNames(
    c("indicator", 
      "preprimary_ece_female", 
      "preprimary_ece_male", 
      "preprimary_ece_total", 
      "primary_female", 
      "primary_male", 
      "primary_total", 
      "secondary_female", 
      "secondary_male", 
      "secondary_total", 
      "national_average")
  ) %>% 
  select(-national_average) %>% 
  mutate_at(vars(indicator), ~ str_trim(str_squish(.))) %>% 
  mutate_at(vars(indicator), 
            ~ gsub("\\[.*?\\]", "", .)) %>% 
  filter(!is.na(indicator) & !str_detect(indicator, "WASH in")) %>% 
  filter(
    indicator %out% c("Number of education Institutions (Public & Private)", 
                      "Gender Parity Index", 
                      "Learner to teacher Ratio in Public Schools", 
                      "Learner-Classroom Ratio in Public Schools", 
                      "Percentage of students with no access to improved water source", 
                      "Number of schools with no water source")) %>% 
  pivot_longer(cols = c(preprimary_ece_female:secondary_total), 
               names_to = "modifier", 
               values_to = "value") %>% 
  filter(!is.na(value)) %>% 
  mutate(indicator = ifelse(indicator == "Students’ dropout /Absenteeism during", 
                            "Dropout Absenteeism in 2022", 
                            indicator)) %>% 
  mutate(age_modifier = case_when(str_detect(modifier, "preprimary_ece") ~ "preprimary_ece", 
                                  str_detect(modifier, "primary") ~ "primary", 
                                  str_detect(modifier, "secondary") ~ "secondary", 
                                  TRUE ~ NA_character_), 
         sex_modifier = case_when(str_detect(modifier, "_male") ~ "male", 
                                  str_detect(modifier, "_female") ~ "female", 
                                  str_detect(modifier, "total") ~ "total", 
                                  TRUE ~ NA_character_), 
         value = as.numeric(value)) %>% 
  select(-modifier)

county %>% 
  filter(str_detect(indicator, "Net")
         & sex_modifier == "total") %>% 
  mutate(age_modifier = ifelse(all_values == "P.:111.2%,", "preprimary_ece", age_modifier), 
         value = ifelse(all_values == "P.:111.2%,", 111.2, value)) %>% 
  mutate(age_modifier = fct_relevel(age_modifier, 
                                    c("secondary",
                                      "primary",
                                      "preprimary_ece"
                                    ))) %>% 
  arrange(age_modifier) %>% 
  ggplot(aes(x = value, 
             y = age_modifier, 
             fill = geography)) + 
  geom_col(position = position_dodge()) + 
  geom_text(aes(label = value), position = position_dodge(width = .9), 
            hjust = "inward") + 
  scale_fill_manual(values = c("#7ad151", "#277f8e")) + 
  labs(x = "", y = "", 
       title = "Net Enrolment Rate", 
       fill = "") + 
  scale_x_continuous(labels = percent) + 
  theme(legend.position = "bottom", 
        plot.caption = element_text(hjust = 0.5)) + 
  
  county %>% 
  filter(str_detect(indicator, "Classroom")
         & sex_modifier == "total") %>% 
  mutate(age_modifier = fct_relevel(age_modifier, 
                                    c("secondary",
                                      "primary",
                                      "preprimary_ece"
                                    ))) %>% 
  arrange(age_modifier) %>% 
  ggplot(aes(x = value, 
             y = age_modifier, 
             fill = geography)) + 
  geom_col(position = position_dodge()) + 
  geom_text(aes(label = value), position = position_dodge(width = .9), 
            hjust = "inward") + 
  scale_fill_manual(values = c("#7ad151", "#277f8e")) + 
  labs(x = "Learner-to-classroom ratio",
       y = "",
       title = "Learner-Classroom ratio", 
       fill = "") +  
  theme(legend.position = "bottom", 
        axis.text.y = element_blank(), 
        plot.caption = element_text(hjust = 0.5)) + 
  
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom")


read_excel("./data/ocr_tables/enrolment_boys_grade1_6_public_primary_schools.xlsx") %>% 
  clean_names() %>% 
  select_all(~gsub("class", "grade", .)) %>% 
  pivot_longer(cols = grade_1:total, 
               names_to = "grade", 
               values_to = "enrolment") %>% 
  mutate(public_private = "public", 
         sex_modifier = "boys", 
         age_modifier = "primary",
         grade = ifelse(str_detect(grade, "total"), "primary_total", grade), 
         grade = paste0(grade, "_", sex_modifier), 
         sex_modifier = ifelse(str_detect(grade, "primary_total"), "total", sex_modifier)), 

read_excel("./data/ocr_tables/enrolment_girls_grade1_6_public_primary_schools.xlsx") %>% 
  clean_names() %>% 
  select_all(~gsub("class", "grade", .)) %>% 
  pivot_longer(cols = grade_1:total, 
               names_to = "grade", 
               values_to = "enrolment") %>% 
  mutate(public_private = "public", 
         sex_modifier = "girls", 
         age_modifier = "primary",
         grade = ifelse(str_detect(grade, "total"), "primary_total", grade), 
         grade = paste0(grade, "_", sex_modifier), 
         sex_modifier = ifelse(str_detect(grade, "primary_total"), "total", sex_modifier)), 


read_excel("./data/ocr_tables/enrolment_boys_grade1_6_private_primary_schools.xlsx") %>% 
  clean_names() %>% 
  select_all(~gsub("class", "grade", .)) %>% 
  pivot_longer(cols = grade_1:total, 
               names_to = "grade", 
               values_to = "enrolment") %>% 
  mutate(public_private = "private", 
         sex_modifier = "boys", 
         age_modifier = "primary",
         grade = ifelse(str_detect(grade, "total"), "primary_total", grade), 
         grade = ifelse(str_detect(grade, "total") & sex_modifier == "total", 
                        "primary_total", grade), 
         grade = paste0(grade, "_", sex_modifier), 
         sex_modifier = ifelse(str_detect(grade, "primary_total"), "total", sex_modifier)),

read_excel("./data/ocr_tables/enrolment_girls_grade1_6_private_primary_schools.xlsx") %>% 
  clean_names() %>% 
  select_all(~gsub("class", "grade", .)) %>% 
  pivot_longer(cols = grade_1:total, 
               names_to = "grade", 
               values_to = "enrolment") %>% 
  mutate(public_private = "private", 
         sex_modifier = "girls", 
         age_modifier = "primary",
         grade = ifelse(str_detect(grade, "total"), "primary_total", grade), 
         grade = paste0(grade, "_", sex_modifier), 
         sex_modifier = ifelse(str_detect(grade, "primary_total"), "total", sex_modifier))