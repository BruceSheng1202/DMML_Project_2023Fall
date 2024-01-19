allvars_obe <- c("Age", "Sex", "Cholesterol", "Heart.Rate", "Diabetes", "Family.History", 
                  "Continent", "Alcohol.Consumption", "Exercise.Hours.Per.Week", 
                  "Diet", "Previous.Heart.Problems", "Medication.Use", "Stress.Level", 
                  "Sedentary.Hours.Per.Day", "Income", "BMI", "Triglycerides", 
                  "Physical.Activity.Days.Per.Week", "Sleep.Hours.Per.Day", "Country", 
                  "Hemisphere",  "High.Blood.Pressure", 
                  "Low.Blood.Pressure","Heart.Attack.Risk","Smoking")

fvars_obe <- c("Sex",   "Diabetes", "Family.History", 
                "Continent", "Alcohol.Consumption", 
                "Diet", "Previous.Heart.Problems", "Medication.Use", "Stress.Level", 
                "Country", "Hemisphere", "Smoking","Heart.Attack.Risk")
tab_hemi <- CreateTableOne(vars=allvars_obe,strata = 'Obesity',data=dt2,factorVars = fvars_obe,smd = T)


tab_obeMat <- print(tab_hemi, quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
write.csv(tab_obeMat, file = "1.csv")

m_ps <- glm(Obesity ~ Age+Sex+Cholesterol+Heart.Rate+Diabetes+Family.History+Previous.Heart.Problems+
              +               Alcohol.Consumption+Exercise.Hours.Per.Week+ 
              +             Diet+Continent+Medication.Use+Stress.Level+
              +               Sedentary.Hours.Per.Day+Income+BMI+Triglycerides+
              +               Physical.Activity.Days.Per.Week+Sleep.Hours.Per.Day+
              +               Country+Smoking+Hemisphere+High.Blood.Pressure+Low.Blood.Pressure,
            family = binomial(), data = dt2)
prs_df <- data.frame(pr_score = predict(m_ps, type = "response"),
                     Obesity = m_ps$model$Obesity)
head(prs_df)

prs_dfd <- data.frame(pr_score = predict(m_ps2, type = "response"),
                     Diabetes = m_ps2$model$Diabetes)
head(prs_dfd)


labs <- paste("Actual type of diseases", c("Diabetes", "No Diabetes"))
prs_dfd %>%
  mutate(Diabetes = ifelse(Diabetes == 1, labs[1], labs[2])) %>%
  ggplot(aes(x = pr_score)) +
  geom_histogram(color = "white") +
  facet_wrap(~Diabetes) +
  xlab("Probability of having diabetes") +
  theme_bw()

# 计算倾向性评分
pr_score <- predict(m_ps, type = "response")

# 把倾向性评分加入到原数据中
dt2_ps <- dt2 %>% 
  mutate(ps = pr_score)

# 把处理因素和倾向性评分作为自变量进行回归
psl <- glm(Heart.Attack.Risk ~ Obesity + ps, data = dt2_ps,family = binomial())
summary(psl)

l0 <- glm(Heart.Attack.Risk~.,data=dt2,family = binomial())
summary(l0)





m_ps2 <- glm(Diabetes ~ Age+Sex+Cholesterol+Heart.Rate+Obesity+Family.History+Previous.Heart.Problems+
              +               Alcohol.Consumption+Exercise.Hours.Per.Week+ 
              +             Diet+Continent+Medication.Use+Stress.Level+
              +               Sedentary.Hours.Per.Day+Income+BMI+Triglycerides+
              +               Physical.Activity.Days.Per.Week+Sleep.Hours.Per.Day+
              +               Country+Smoking+Hemisphere+High.Blood.Pressure+Low.Blood.Pressure,
            family = binomial(), data = dt2)

pr_score <- predict(m_ps2, type = "response")

# 把倾向性评分加入到原数据中
dt2_ps2 <- dt2 %>% 
  mutate(ps = pr_score)

# 把处理因素和倾向性评分作为自变量进行回归
psl2 <- glm(Heart.Attack.Risk ~ Diabetes + log(ps), data = dt2_ps2,family = binomial())
summary(psl2)





m_ps3 <- glm(Alcohol.Consumption ~ Age+Sex+Cholesterol+Heart.Rate+Obesity+Family.History+Diabetes+
               +               Previous.Heart.Problems+Exercise.Hours.Per.Week+ 
               +             Diet+Continent+Medication.Use+Stress.Level+
               +               Sedentary.Hours.Per.Day+Income+BMI+Triglycerides+
               +               Physical.Activity.Days.Per.Week+Sleep.Hours.Per.Day+
               +               Country+Smoking+Hemisphere+High.Blood.Pressure+Low.Blood.Pressure,
             family = binomial(), data = dt2)
pr_score <- predict(m_ps3, type = "response")

dt2_ps3 <- dt2 %>% 
  mutate(ps = pr_score)

# 把处理因素和倾向性评分作为自变量进行回归
psl3 <- glm(Heart.Attack.Risk ~ Alcohol.Consumption + log(ps), data = dt2_ps3,family = binomial())
summary(psl3)




mpsh <- glm(Stress.Level ~ Age+Sex+Cholesterol+Heart.Rate+Obesity+Family.History+Diabetes+
              +               Previous.Heart.Problems+Exercise.Hours.Per.Week+ 
              +             Diet+Continent+Medication.Use+Hemisphere+
              +               Sedentary.Hours.Per.Day+Income+BMI+Triglycerides+
              +               Physical.Activity.Days.Per.Week+Sleep.Hours.Per.Day+
              +               Country+Smoking+Alcohol.Consumption+High.Blood.Pressure+Low.Blood.Pressure,
            family = binomial(), data = dt2)
pr_score <- predict(mpsh, type = "response")
dt2_psh <- dt2 %>% 
  mutate(ps = pr_score)

# 把处理因素和倾向性评分作为自变量进行回归
pslh <- glm(Heart.Attack.Risk ~ Obesity+ps, data = dt2_psh,family = binomial())
summary(pslh)
