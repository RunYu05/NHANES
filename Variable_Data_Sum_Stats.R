getwd()
library(foreign)
library(dplyr)
library(stargazer)
library(ggplot2)
library(data.table)
library(glmnet)


# Nutrition intake
diet_ind_1st <- read.xport('./Dietary/DR1IFF_I.xpt')
intake <- dplyr::select(diet_ind_1st, SEQN, DR1IPROT, DR1ISUGR, DR1ICARB, DR1ITFAT, DR1IFIBE) %>%
                 filter(!is.na(DR1IPROT))
intake <- dplyr::group_by(intake,SEQN) %>%
                 summarise_all(sum)

# Read in Demographics data
demographics_ori <- read.xport('./Demographics/DEMO_I.XPT')

# Change variable labels
demographics_ori$RIAGENDR[demographics_ori$RIAGENDR == 1] <- 0
demographics_ori$RIAGENDR[demographics_ori$RIAGENDR == 2] <- 1
demographics_ori$DMDBORN4[demographics_ori$DMDBORN4 == 2] <- 0
demographics_ori$RIDRETH3[demographics_ori$RIDRETH3 == 6] <- 5
demographics_ori$RIDRETH3[demographics_ori$RIDRETH3 == 7] <- 6

# Select the project related variables
demographics <- dplyr::select(demographics_ori, SEQN, RIAGENDR, DMDBORN4, DMDEDUC2, RIDAGEYR, INDFMIN2, RIDRETH3) %>%
                filter(!is.na(INDFMIN2)) %>%
                filter(RIDAGEYR >= 25 & RIDAGEYR <= 50)%>%
                filter(INDFMIN2 >= 1 & INDFMIN2 <= 15)%>%
                filter(INDFMIN2 != 12 & INDFMIN2 != 13)%>%
                filter(DMDEDUC2 != 7 & DMDEDUC2 != 9)

# Change variable labels
demographics$INDFMIN2[demographics$INDFMIN2 == 14] <- 11
demographics$INDFMIN2[demographics$INDFMIN2 == 15] <- 12
demographics$RIDRETH3[demographics$RIDRETH3 == 2] <- 1
demographics$RIDRETH3[demographics$RIDRETH3 == 3] <- 0
demographics$RIDRETH3[demographics$RIDRETH3 == 4] <- 0
demographics$RIDRETH3[demographics$RIDRETH3 == 5] <- 0
demographics$RIDRETH3[demographics$RIDRETH3 == 6] <- 0


# Join the intake and demographics tables
dataset <- dplyr::right_join(intake, demographics, by = 'SEQN') %>%
  filter(!is.na(DR1IPROT))

# Rename the variables
dataset <- dataset %>%
    rename( 
    ID = SEQN,
    Protein = DR1IPROT,
    Sugar = DR1ISUGR,
    Carb = DR1ICARB,
    Fat = DR1ITFAT,
    Fiber = DR1IFIBE,
    Gender = RIAGENDR,
    CountryOfBirth = DMDBORN4,
    Educ = DMDEDUC2,
    Age = RIDAGEYR,
    Income = INDFMIN2,
    Race = RIDRETH3)

dataset$HighSchool[dataset$Educ == 3] <- 1
dataset$HighSchool[dataset$Educ != 3] <- 0
dataset$College[dataset$Educ >= 4] <- 1
dataset$College[dataset$Educ < 4] <- 0

# Delete 'educ' column
dataset = subset(dataset, select = -c(Educ) )

# Change Income into a continus variable
dataset$Income[dataset$Income == 1] <- 2500
dataset$Income[dataset$Income == 2] <- 7500
dataset$Income[dataset$Income == 3] <- 12500
dataset$Income[dataset$Income == 4] <- 17500
dataset$Income[dataset$Income == 5] <- 22500
dataset$Income[dataset$Income == 6] <- 30000
dataset$Income[dataset$Income == 7] <- 40000
dataset$Income[dataset$Income == 8] <- 50000
dataset$Income[dataset$Income == 9] <- 60000
dataset$Income[dataset$Income == 10] <- 70000
dataset$Income[dataset$Income == 11] <- 87500
dataset$Income[dataset$Income == 12] <- 125000

# Export dataset
dataset <- as.data.frame(dataset)
write.csv(dataset, 'dataset.csv')

# Summary Statistics
stargazer(dataset, type = 'text', title = 'Summary Statistics', 
          omit = 'ID', omit.summary.stat = "n", digits = 2,  out = 'summary_stats.txt')

# Define Income Classes
low_income <- subset(dataset, Income <= 30000)
middle_income <- subset(dataset, Income >= 40000 & Income <= 87500)
high_income <- subset(dataset, Income == 125000)

# Divide each class into groups with different ages and genders
# Female 25-30
low_f_25 <- subset(low_income, Age <= 30)%>%
            filter(Gender == 1)
middle_f_25 <- subset(middle_income, Age <= 30)%>%
               filter(Gender == 1)
high_f_25 <- subset(high_income, Age <= 30)%>%
             filter(Gender == 1)
# Male 25-30
low_m_25 <- subset(low_income, Age <= 30)%>%
            filter(Gender == 0)
middle_m_25 <- subset(middle_income, Age <= 30)%>%
               filter(Gender == 0)
high_m_25 <- subset(high_income, Age <= 30)%>%
             filter(Gender == 0)
# Female 31-50
low_f_50 <- subset(low_income, Age >= 31)%>%
            filter(Gender == 1)
middle_f_50 <- subset(middle_income, Age >= 31)%>%
               filter(Gender == 1)
high_f_50 <- subset(high_income, Age >= 31)%>%
             filter(Gender == 1)
# Male 31-50
low_m_50 <- subset(low_income, Age >= 31)%>%
            filter(Gender == 0)
middle_m_50 <- subset(middle_income, Age >= 31)%>%
               filter(Gender == 0)
high_m_50 <- subset(high_income, Age >= 31)%>%
             filter(Gender == 0)

# Calculate compliance rate of 5 nutrition factors
# Protein
low_protein_per <- ((length(which(low_f_25$Protein >= 46))) + 
                    (length(which(low_m_25$Protein >= 56))) +
                    (length(which(low_f_50$Protein >= 46))) +
                    (length(which(low_m_50$Protein >= 56)))) /
                    length(low_income$Protein) * 100
middle_protein_per <- ((length(which(middle_f_25$Protein >= 46))) + 
                      (length(which(middle_m_25$Protein >= 56))) +
                      (length(which(middle_f_50$Protein >= 46))) +
                      (length(which(middle_m_50$Protein >= 56)))) /
                      length(middle_income$Protein) * 100
high_protein_per <- ((length(which(high_f_25$Protein >= 46))) + 
                     (length(which(high_m_25$Protein >= 56))) +
                     (length(which(high_f_50$Protein >= 46))) +
                     (length(which(high_m_50$Protein >= 56)))) /
                     length(high_income$Protein) * 100
# Carb
low_carb_per <- ((length(which(low_f_25$Carb >= 130))) + 
                 (length(which(low_m_25$Carb >= 130))) +
                 (length(which(low_f_50$Carb >= 130))) +
                 (length(which(low_m_50$Carb >= 130)))) /
                 length(low_income$Carb) * 100
middle_carb_per <- ((length(which(middle_f_25$Carb >= 130))) + 
                    (length(which(middle_m_25$Carb >= 130))) +
                    (length(which(middle_f_50$Carb >= 130))) +
                    (length(which(middle_m_50$Carb >= 130)))) /
                    length(middle_income$Carb) * 100
high_carb_per <- ((length(which(high_f_25$Carb >= 130))) + 
                  (length(which(high_m_25$Carb >= 130))) +
                  (length(which(high_f_50$Carb >= 130))) +
                  (length(which(high_m_50$Carb >= 130)))) /
                  length(high_income$Carb) * 100
# Fiber
low_fiber_per <- ((length(which(low_f_25$Fiber >= 28))) + 
                  (length(which(low_m_25$Fiber >= 33.6))) +
                  (length(which(low_f_50$Fiber >= 25.2))) +
                  (length(which(low_m_50$Fiber >= 30.8)))) /
                  length(low_income$Fiber) * 100
middle_fiber_per <- ((length(which(middle_f_25$Fiber >= 28))) + 
                     (length(which(middle_m_25$Fiber >= 33.6))) +
                     (length(which(middle_f_50$Fiber >= 25.2))) +
                     (length(which(middle_m_50$Fiber >= 30.8)))) /
                     length(middle_income$Fiber) * 100
high_fiber_per <- ((length(which(high_f_25$Fiber >= 28))) + 
                   (length(which(high_m_25$Fiber >= 33.6))) +
                   (length(which(high_f_50$Fiber >= 25.2))) +
                   (length(which(high_m_50$Fiber >= 30.8)))) /
                   length(high_income$Fiber) * 100
# Fat
low_fat_per <- ((length(which(low_f_25$Fat >= 20))) + 
                (length(which(low_m_25$Fat >= 20))) +
                (length(which(low_f_50$Fat >= 20))) +
                (length(which(low_m_50$Fat >= 20)))) /
                length(low_income$Fat) * 100
middle_fat_per <- ((length(which(middle_f_25$Fat >= 20))) + 
                   (length(which(middle_m_25$Fat >= 20))) +
                   (length(which(middle_f_50$Fat >= 20))) +
                   (length(which(middle_m_50$Fat >= 20)))) /
                   length(middle_income$Fat) * 100
high_fat_per <- ((length(which(high_f_25$Fat >= 20))) + 
                 (length(which(high_m_25$Fat >= 20))) +
                 (length(which(high_f_50$Fat >= 20))) +
                 (length(which(high_m_50$Fat >= 20)))) /
                 length(high_income$Fat) * 100
# Sugar
low_sugar_per <- ((length(which(low_f_25$Sugar >= 50))) + 
                  (length(which(low_m_25$Sugar >= 60))) +
                  (length(which(low_f_50$Sugar >= 45))) +
                  (length(which(low_m_50$Sugar >= 55)))) /
                  length(low_income$Sugar) * 100
middle_sugar_per <- ((length(which(middle_f_25$Sugar >= 50))) + 
                     (length(which(middle_m_25$Sugar >= 60))) +
                     (length(which(middle_f_50$Sugar >= 45))) +
                     (length(which(middle_m_50$Sugar >= 55)))) /
                     length(middle_income$Sugar) * 100
high_sugar_per <- ((length(which(high_f_25$Sugar >= 50))) + 
                   (length(which(high_m_25$Sugar >= 60))) +
                   (length(which(high_f_50$Sugar >= 45))) +
                   (length(which(high_m_50$Sugar >= 55)))) /
                   length(high_income$Sugar) * 100

# Summary table of compliance rate
options(digits = 4)
comp_table <- data.table(IncomeClass = c('Low Income', 'Middle Income', 'High Income'),
                         Protein = c(low_protein_per, middle_protein_per, high_protein_per),
                         Carb = c(low_carb_per, middle_carb_per, high_carb_per),
                         Fiber = c(low_fiber_per, middle_fiber_per, high_fiber_per),
                         Fat = c(low_fat_per, middle_fat_per, high_fat_per),
                         Sugar = c(low_sugar_per, middle_sugar_per, high_sugar_per))


# Linear Regression Models
# (1) All 7 factors
# Protein
lm_protein <- glm(Protein ~ Gender + CountryOfBirth + HighSchool + College + Age + Income + Race, data = dataset)
# Carb
lm_carb <- glm(Carb ~ Gender + CountryOfBirth + HighSchool + College + Age + Income + Race, data = dataset)
# Fiber
lm_fiber <- glm(Fiber ~ Gender + CountryOfBirth + HighSchool + College + Age + Income + Race, data = dataset)
# Fat
lm_fat <- glm(Fat ~ Gender + CountryOfBirth + HighSchool + College + Age + Income + Race, data = dataset)
# Sugar
lm_sugar <- glm(Sugar ~ Gender + CountryOfBirth + HighSchool + College + Age + Income + Race, data = dataset)
# Summary
lm_summary <- stargazer(lm_protein, lm_carb, lm_fiber, lm_fat, lm_sugar, type = 'text',
                        title = 'Seven Fators Affecting Nutrition Intake',
                        digits = 2, report=('v*c*p*t'), out = 'all_seven_lm.txt')

# Factor Elasticity (All)
# (A 1% increase in x causes a [elas%] cahnge in y)
protein_elas_income <- as.numeric(lm_protein$coefficients['Income'] * mean(dataset$Income) / mean(dataset$Protein))
protein_elas_age <- as.numeric(lm_protein$coefficients['Age'] * mean(dataset$Age) / mean(dataset$Protein))
fiber_elas_income <- as.numeric(lm_fiber$coefficients['Income'] * mean(dataset$Income) / mean(dataset$Fiber))
fiber_elas_age <- as.numeric(lm_fiber$coefficients['Age'] * mean(dataset$Age) / mean(dataset$Fiber))
carb_elas_income <- as.numeric(lm_carb$coefficients['Income'] * mean(dataset$Income) / mean(dataset$Carb))
carb_elas_age <- as.numeric(lm_carb$coefficients['Age'] * mean(dataset$Age) / mean(dataset$Carb))
fat_elas_income <- as.numeric(lm_fat$coefficients['Income'] * mean(dataset$Income) / mean(dataset$Fat))
fat_elas_age <- as.numeric(lm_fat$coefficients['Age'] * mean(dataset$Age) / mean(dataset$Fat))
sugar_elas_income <- as.numeric(lm_sugar$coefficients['Income'] * mean(dataset$Income) / mean(dataset$Sugar))
sugar_elas_age <- as.numeric(lm_sugar$coefficients['Age'] * mean(dataset$Age) / mean(dataset$Sugar))


# (2) No Educ 
# Examine the quadratic relationship between education and nutrition intake
# Protein
lm_protein_no_educ <- glm(Protein ~ Gender + CountryOfBirth + Age + Income + Race, data = dataset)
# Carb
lm_carb_no_educ <- glm(Carb ~ Gender + CountryOfBirth + Age + Income + Race, data = dataset)
# Fiber
lm_fiber_no_educ <- glm(Fiber ~ Gender + CountryOfBirth + Age + Income + Race, data = dataset)
# Fat
lm_fat_no_educ <- glm(Fat ~ Gender + CountryOfBirth + Age + Income + Race, data = dataset)
# Sugar
lm_sugar_no_educ <- glm(Sugar ~ Gender + CountryOfBirth + Age + Income + Race, data = dataset)
# Summary
lm_summary_no_educ <- stargazer(lm_protein_no_educ, lm_carb_no_educ, lm_fiber_no_educ, 
                                lm_fat_no_educ, lm_sugar_no_educ, type = 'text',
                           title = 'Without Educ',
                           digits = 2, report=('v*c*p*t'), out = 'no_educ_lm.txt')

# Factor Elasticity (No Educ)
# (A 1% increase in x causes a [elas%] cahnge in y)
protein_elas_noeduc_income <- as.numeric(lm_protein_no_educ$coefficients['Income'] * mean(dataset$Income) / mean(dataset$Protein))
protein_elas_noeduc_age <- as.numeric(lm_protein_no_educ$coefficients['Age'] * mean(dataset$Age) / mean(dataset$Protein))
fiber_elas_noeduc_income <- as.numeric(lm_fiber_no_educ$coefficients['Income'] * mean(dataset$Income) / mean(dataset$Fiber))
fiber_elas_noeduc_age <- as.numeric(lm_fiber_no_educ$coefficients['Age'] * mean(dataset$Age) / mean(dataset$Fiber))
carb_elas_noeduc_income <- as.numeric(lm_carb_no_educ$coefficients['Income'] * mean(dataset$Income) / mean(dataset$Carb))
carb_elas_noeduc_age <- as.numeric(lm_carb_no_educ$coefficients['Age'] * mean(dataset$Age) / mean(dataset$Carb))
fat_elas_noeduc_income <- as.numeric(lm_fat_no_educ$coefficients['Income'] * mean(dataset$Income) / mean(dataset$Fat))
fat_elas_noeduc_age <- as.numeric(lm_fat_no_educ$coefficients['Age'] * mean(dataset$Age) / mean(dataset$Fat))
sugar_elas_noeduc_income <- as.numeric(lm_sugar_no_educ$coefficients['Income'] * mean(dataset$Income) / mean(dataset$Sugar))
sugar_elas_noeduc_age <- as.numeric(lm_sugar_no_educ$coefficients['Age'] * mean(dataset$Age) / mean(dataset$Sugar))


# Standardize Income
dataset$Income <- scale(dataset$Income)
# (3) No Educ Race x Income
# Examine the quadratic relationship between education and nutrition intake
# Protein
lm_protein_no_educ_ri <- glm(Protein ~ Gender + CountryOfBirth + Age + Income + Race+ Race*Income, data = dataset)
# Carb
lm_carb_no_educ_ri <- glm(Carb ~ Gender + CountryOfBirth + Age + Income + Race+ Race*Income, data = dataset)
# Fiber
lm_fiber_no_educ_ri <- glm(Fiber ~ Gender + CountryOfBirth + Age + Income + Race+ Race*Income, data = dataset)
# Fat
lm_fat_no_educ_ri <- glm(Fat ~ Gender + CountryOfBirth + Age + Income + Race+ Race*Income, data = dataset)
# Sugar
lm_sugar_no_educ_ri <- glm(Sugar ~ Gender + CountryOfBirth + Age + Income + Race+ Race*Income, data = dataset)
# Summary
lm_summary_no_educ_ri <- stargazer(lm_protein_no_educ_ri, lm_carb_no_educ_ri, lm_fiber_no_educ_ri, 
                                lm_fat_no_educ_ri, lm_sugar_no_educ_ri, type = 'text',
                                title = 'Without Educ Race x Income',
                                digits = 2, report=('v*c*p*t'), out = 'no_educ_ri_lm.txt')



# Run before income is standardized in Model 3
# Visualization
ggplot(dataset, aes(Age)) + geom_histogram(bins=10)

# -----------------Income------------------------------
# (1) Income and Fiber
dataset %>% 
  ggplot(aes(Income, Fiber, col=Gender)) + 
  geom_point() + 
  geom_smooth(method="glm", color = 'red')+
  xlab("Income") + ylab('Fiber')
ggsave("IncomeFiber.pdf")

# (2) Income and Protein
dataset %>% 
  ggplot(aes(Income, Protein, col=Gender)) + 
  geom_point() + 
  geom_smooth(method="glm", color = 'red')+
  xlab("Income") + ylab('Protein')
ggsave("IncomeProtein.pdf")

# (3) Income and Sugar
dataset %>% 
  ggplot(aes(Income, Sugar, col=Gender)) + 
  geom_point() + 
  geom_smooth(method="glm", color = 'red')+
  xlab("Income") + ylab('Sugar')
ggsave("IncomeSuagr.pdf")

# (4) Income and Fat
dataset %>% 
  ggplot(aes(Income, Fat, col=Gender)) + 
  geom_point() + 
  geom_smooth(method="glm", color = 'red')+
  xlab("Income") + ylab('Fat')
ggsave("IncomeFat.pdf")

# (5) Income and Carb
dataset %>% 
  ggplot(aes(Income, Carb, col=Gender)) + 
  geom_point() + 
  geom_smooth(method="glm", color = 'red')+
  xlab("Income") + ylab('Carb')
ggsave("IncomeCarb.pdf")

# ---------------------Income x Race-------------------------
# (1) Income x Race (Carb)
dataset %>% 
  filter(Race == 1) %>%
  ggplot(aes(Income, Carb, col=Gender)) + 
  geom_point() + 
  geom_smooth(method="glm", color = 'red')+
  xlab("Income x Hispanic") + ylab('Carb')
ggsave("IncomeHisCarb.pdf")

dataset %>% 
  filter(Race == 0) %>%
  ggplot(aes(Income, Carb, col=Gender)) + 
  geom_point() + 
  geom_smooth(method="glm", color = 'red')+
  xlab("Income x Non-Hispanic") + ylab('Carb')
ggsave("IncomeNonHisCarb.pdf")

# (2) Income x Race (Protein)
dataset %>% 
  filter(Race == 1) %>%
  ggplot(aes(Income, Protein, col=Gender)) + 
  geom_point() + 
  geom_smooth(method="glm", color = 'red')+
  xlab("Income x Hispanic") + ylab('Protein')
ggsave("IncomeHisProtein.pdf")

dataset %>% 
  filter(Race == 0) %>%
  ggplot(aes(Income, Protein, col=Gender)) + 
  geom_point() + 
  geom_smooth(method="glm", color = 'red')+
  xlab("Income x Non-Hispanic") + ylab('Protein')
ggsave("IncomeNonHisProtein.pdf")

# (3) Income x Race (Fiber)
dataset %>% 
  filter(Race == 1) %>%
  ggplot(aes(Income, Fiber, col=Gender)) + 
  geom_point() + 
  geom_smooth(method="glm", color = 'red')+
  xlab("Income x Hispanic") + ylab('Fiber')
ggsave("IncomeHisFiber.pdf")

dataset %>% 
  filter(Race == 0) %>%
  ggplot(aes(Income, Fiber, col=Gender)) + 
  geom_point() + 
  geom_smooth(method="glm", color = 'red')+
  xlab("Income x Non-Hispanic") + ylab('Fiber')
ggsave("IncomeNonHisFiber.pdf")

# (4) Income x Race (Fat)
dataset %>% 
  filter(Race == 1) %>%
  ggplot(aes(Income, Fat, col=Gender)) + 
  geom_point() + 
  geom_smooth(method="glm", color = 'red')+
  xlab("Income x Hispanic") + ylab('Fat')
ggsave("IncomeHisFat.pdf")

dataset %>% 
  filter(Race == 0) %>%
  ggplot(aes(Income, Fat, col=Gender)) + 
  geom_point() + 
  geom_smooth(method="glm", color = 'red')+
  xlab("Income x Non-Hispanic") + ylab('Fat')
ggsave("IncomeNonHisFat.pdf")

# (5) Income x Race (Sugar)
dataset %>% 
  filter(Race == 1) %>%
  ggplot(aes(Income, Sugar, col=Gender)) + 
  geom_point() + 
  geom_smooth(method="glm", color = 'red')+
  xlab("Income x Hispanic") + ylab('Sugar')
ggsave("IncomeHisSugar.pdf")

dataset %>% 
  filter(Race == 0) %>%
  ggplot(aes(Income, Sugar, col=Gender)) + 
  geom_point() + 
  geom_smooth(method="glm", color = 'red')+
  xlab("Income x Non-Hispanic") + ylab('Sugar')
ggsave("IncomeNonHisSugar.pdf")


