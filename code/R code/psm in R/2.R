data <- read.csv("heart_attack_prediction_dataset.csv")
split_values  <- strsplit(data$Blood.Pressure, "/")
data$High.Blood.Pressure <- as.numeric(sapply(split_values, function(x) as.numeric(x[1])))
data$Low.Blood.Pressure <- as.numeric(sapply(split_values, function(x) as.numeric(x[2])))
dt <- data[, -which(names(data) == "Blood.Pressure")]

dt$Sex <- as.factor(dt$Sex)
dt$Diabetes <- as.factor(dt$Diabetes)
dt$Family.History <- as.factor(dt$Family.History)
dt$Smoking <- as.factor(dt$Smoking)
dt$Obesity <- as.factor(dt$Obesity)
dt$Alcohol.Consumption <- as.factor(dt$Alcohol.Consumption)
dt$Diet <- as.factor(dt$Diet)
dt$Previous.Heart.Problems <- as.factor(dt$Previous.Heart.Problems)
dt$Medication.Use <- as.factor(dt$Medication.Use)
dt$Stress.Level <- as.factor(dt$Stress.Level)
dt$Country <- as.factor(dt$Country)
dt$Continent <- as.factor(dt$Continent)
dt$Hemisphere <- as.factor(dt$Hemisphere)
dt$Heart.Attack.Risk <- as.factor(dt$Heart.Attack.Risk)

library(Matching)
library(tableone)
dt$Sex <- ifelse(dt$Sex=='Male',1,2)
dt$Sex <- factor(dt$Sex)
dt$Diet <- ifelse(dt$Diet=='Unhealthy',1,ifelse(dt$Diet=='Average',2,3))
dt$Diet <- factor(dt$Diet)
dt$Hemisphere <- ifelse(dt$Hemisphere=='Northern Hemisphere',1,0)
dt$Hemisphere <- factor(dt$Hemisphere)
dt$Continent <- ifelse(dt$Continent=='Asia',1,ifelse(dt$Continent=='Europe',2,ifelse(dt$Continent=='Africa',3,ifelse(dt$Continent=='North America',4,ifelse(dt$Continent=='South America',5,6)))))
dt$Continent <- factor(dt$Continent)
dt$cou <- as.character(dt$Country)
country <- c('Argentina','Canada','France','Thailand','Germany','Japan','Brazil',
             'South Africa','United States','Vietnam','China','Italy','Spain','India',
             'Nigeria','New Zealand','South Korea','Australia','Colombia','United Kingdom')

for(i in 1:20){
  for(j in 1:8763){
    if(dt$cou[j]==country[i]){
      dt$cou[j] <- i
    }
      
  }
}
dt$Country <- factor(dt$cou)

dt1 <- dt[,-which(names(data)=='Patient.ID')]
dt2 <- dt1[,1:26]
dput(names(dt2))
c("Age", "Sex", "Cholesterol", "Heart.Rate", "Diabetes", "Family.History", 
             "Smoking", "Obesity", "Alcohol.Consumption", "Exercise.Hours.Per.Week", 
             "Diet", "Previous.Heart.Problems", "Medication.Use", "Stress.Level", 
             "Sedentary.Hours.Per.Day", "Income", "BMI", "Triglycerides", 
             "Physical.Activity.Days.Per.Week", "Sleep.Hours.Per.Day", "Country", 
             "Continent", "Hemisphere", "Heart.Attack.Risk", "High.Blood.Pressure", 
             "Low.Blood.Pressure")
c("Sex",   "Diabetes", "Family.History", 
           "Smoking", "Obesity", "Alcohol.Consumption", 
           "Diet", "Previous.Heart.Problems", "Medication.Use", "Stress.Level", 
            "Country", "Continent", "Hemisphere", "Heart.Attack.Risk")

library(cobalt)
library(MatchIt)


##
allvars_fam <- c("Age", "Sex", "Cholesterol", "Heart.Rate", "Family.History", 
              "Obesity", "Alcohol.Consumption", "Exercise.Hours.Per.Week", 
             "Diet", "Previous.Heart.Problems", "Medication.Use", "Stress.Level", 
             "Sedentary.Hours.Per.Day", "Income", "BMI", "Triglycerides", 
             "Physical.Activity.Days.Per.Week", "Sleep.Hours.Per.Day", "Country", 
             "Hemisphere",  "High.Blood.Pressure", 
             "Low.Blood.Pressure","Heart.Attack.Risk","Smoking",'Continent')

fvars_fam <- c("Sex",   "Family.History", "Continent", 
            "Obesity", "Alcohol.Consumption", 
           "Diet", "Previous.Heart.Problems", "Medication.Use", "Stress.Level", 
           "Country", "Hemisphere", "Smoking","Heart.Attack.Risk")
tab_fam <- CreateTableOne(vars=allvars_fam,strata = 'Diabetes',data=dt2,factorVars = fvars_fam,smd = T)


tab_famMat <- print(tab_fam, quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
write.csv(tab_famMat, file = "smoking.csv")
##


allvars_fam1 <- c("Age", "Sex", "Cholesterol", "Heart.Rate", "Family.History", 
                 "Obesity", "Alcohol.Consumption", "Exercise.Hours.Per.Week", 
                 "Diabetes", "Diet", "Medication.Use", "Stress.Level", 
                 "Sedentary.Hours.Per.Day", "Income", "BMI", "Triglycerides", 
                 "Physical.Activity.Days.Per.Week", "Sleep.Hours.Per.Day", "Country", 
                 "Hemisphere",  "High.Blood.Pressure", 
                 "Low.Blood.Pressure","Heart.Attack.Risk","Smoking",'Continent')

fvars_fam1 <- c("Sex",   "Family.History", "Continent", 
               "Obesity", "Alcohol.Consumption", 
               "Diabetes", "Diet", "Medication.Use", "Stress.Level", 
               "Country", "Hemisphere", "Smoking","Heart.Attack.Risk")
tab_fam1 <- CreateTableOne(vars=allvars_fam1,strata = 'Previous.Heart.Problems',data=dt2,factorVars = fvars_fam1,smd = T)


tab_famMat <- print(tab_fam1, quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
write.csv(tab_famMat, file = "smoking1.csv")





allvars_fam2 <- c("Age", "Medication.Use", "Cholesterol", "Heart.Rate", "Family.History", 
                  "Obesity", "Previous.Heart.Problems", "Exercise.Hours.Per.Week", 
                  "Diabetes", "Diet", "Alcohol.Consumption", "Sex", 
                  "Sedentary.Hours.Per.Day", "Income", "BMI", "Triglycerides", 
                  "Physical.Activity.Days.Per.Week", "Sleep.Hours.Per.Day", "Country", 
                  "Hemisphere",  "High.Blood.Pressure", 
                  "Low.Blood.Pressure","Heart.Attack.Risk","Smoking",'Continent')

fvars_fam2 <- c("Medication.Use",   "Family.History", "Continent", 
                "Obesity", "Previous.Heart.Problems", 
                "Diabetes", "Diet", "Alcohol.Consumption", "Sex", 
                "Country", "Hemisphere", "Smoking","Heart.Attack.Risk")
tab_fam2 <- CreateTableOne(vars=allvars_fam2,strata = 'Stress.Level',data=dt2,factorVars = fvars_fam2,smd = T)


tab_fam2Mat <- print(tab_fam2, quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
write.csv(tab_fam2Mat, file = "smoking2.csv")


allvars_fam3 <- c("Age", "Sex", "Cholesterol", "Heart.Rate", "Family.History", 
                  "Obesity", "Previous.Heart.Problems", "Exercise.Hours.Per.Week", 
                  "Diabetes", "Diet", "Medication.Use", "Stress.Level", 
                  "Sedentary.Hours.Per.Day", "Income", "BMI", "Triglycerides", 
                  "Physical.Activity.Days.Per.Week", "Sleep.Hours.Per.Day", "Country", 
                  "Hemisphere",  "High.Blood.Pressure", 
                  "Low.Blood.Pressure","Heart.Attack.Risk","Smoking",'Continent')

fvars_fam3 <- c("Sex",   "Family.History", "Continent", 
                "Obesity", "Previous.Heart.Problems", 
                "Diabetes", "Diet", "Medication.Use", "Stress.Level", 
                "Country", "Hemisphere", "Smoking","Heart.Attack.Risk")
tab_fam3 <- CreateTableOne(vars=allvars_fam3,strata = 'Alcohol.Consumption',data=dt2,factorVars = fvars_fam3,smd = T)


tab_fam3Mat <- print(tab_fam3, quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
write.csv(tab_fam3Mat, file = "smoking3.csv")



allvars_fam4 <- c("Age", "Sex", "Cholesterol", "Heart.Rate", "Family.History", 
                  "Obesity", "Previous.Heart.Problems", "Exercise.Hours.Per.Week", 
                  "Diabetes", "Alcohol.Consumption", "Medication.Use", "Stress.Level", 
                  "Sedentary.Hours.Per.Day", "Income", "BMI", "Triglycerides", 
                  "Physical.Activity.Days.Per.Week", "Sleep.Hours.Per.Day", "Country", 
                  "Hemisphere",  "High.Blood.Pressure", 
                  "Low.Blood.Pressure","Heart.Attack.Risk","Smoking",'Continent')

fvars_fam4 <- c("Sex",   "Family.History", "Continent", 
                "Obesity", "Previous.Heart.Problems", 
                "Diabetes", "Alcohol.Consumption", "Medication.Use", "Stress.Level", 
                "Country", "Hemisphere", "Smoking","Heart.Attack.Risk")
tab_fam4 <- CreateTableOne(vars=allvars_fam4,strata = 'Diet',data=dt2,factorVars = fvars_fam4,smd = T)


tab_fam4Mat <- print(tab_fam4, quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
write.csv(tab_fam4Mat, file = "smoking4.csv")





#obesity diabetes alcohol.consumption previous.heart.problems country continent hemisphere stress.level















m_ps_smo <- glm(Previous.Heart.Problems~Age+Sex+Cholesterol+Heart.Rate+Diabetes+Family.History+Obesity+
                  +               Alcohol.Consumption+Exercise.Hours.Per.Week+Continent+ 
                  +             Diet+Medication.Use+Stress.Level+
                  +               Sedentary.Hours.Per.Day+Income+BMI+Triglycerides+
                  +               Physical.Activity.Days.Per.Week+Sleep.Hours.Per.Day+
                  +               Country+Smoking+Hemisphere+High.Blood.Pressure+Low.Blood.Pressure,
            data=dt2,family = binomial())

prs_df <- data.frame(pr_score=predict(m_ps_smo,type='response'),Previous.Heart.Problems=m_ps_smo$model$Previous.Heart.Problems)
head(prs_df)
summary(prs_df)
summary(m_ps_smo)
plot(m_ps_smo)

##
mpscont <- matchit(Previous.Heart.Problems~Age+Sex+Cholesterol+Heart.Rate+Diabetes+Family.History+Obesity+
                     +               Alcohol.Consumption+Exercise.Hours.Per.Week+ 
                     +             Diet+Continent+Medication.Use+Stress.Level+
                     +               Sedentary.Hours.Per.Day+Income+BMI+Triglycerides+
                     +               Physical.Activity.Days.Per.Week+Sleep.Hours.Per.Day+
                     +               Country+Smoking+Hemisphere+High.Blood.Pressure+Low.Blood.Pressure,
                   data=dt2,distance='rpart')

summary(mpscont)
prs_df1 <- data.frame(pr_score=predict(mpscont,type='response'),Previous.Heart.Problems=m_ps_smo$model$Previous.Heart.Problems)
head(prs_df1)



