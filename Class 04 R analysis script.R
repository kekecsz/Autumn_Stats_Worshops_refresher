########################################################################################
#                                                                                      #
#   Analysis code for the Automn Statistics Workshops - Basic Stats course - class 4   #
#                                                                                      #
########################################################################################

###################################################################
#                       Packages                                  #
###################################################################

### Load packages
if (!require("tidyverse")) install.packages("tidyverse")	
if (!require("car")) install.packages("car")	
if (!require("psych")) install.packages("psych")	
if (!require("lmtest")) install.packages("lmtest")
library(tidyverse)
library(car)
library(psych)
library(lmtest)

###################################################################
#                 Data wrangling  and initial exploration         #
###################################################################

### Read data
my_data = read_csv("https://raw.githubusercontent.com/kekecsz/Autumn_Stats_Worshops_refresher/main/tornado_therapy_data.csv")

### Assign factors
my_data = my_data %>% 
  mutate(gender = factor(gender),
         group = factor(group),
         health_status = factor(health_status),
         home_ownership = factor(home_ownership))

### Chack data

my_data

my_data %>% 
  summary()

###################################################################
#                          Univariate statistics                  #
###################################################################

### Binomial test 
# Hypothesis: Are there more males in the sample than 50%?

# exploration

my_data %>% 
  ggplot() +
  aes(x = gender) +
  geom_bar()

n_male = my_data %>% 
  filter(gender == "male") %>%
  summarize(n = n())

n_male

# test hypothesis

binom.test(x = as.numeric(n_male), n = nrow(my_data), p = 0.5, alternative = "greater")

### One-sample t-test
# Hypothesis: Is the average height above 170?

# exploration

my_data %>% 
  ggplot() +
  aes(y = height, x = 1) +
  geom_violin() +
  geom_jitter() +
  geom_hline(yintercept = 170, color = "red") +
  geom_hline(yintercept = mean(my_data$height), linetype = "dashed", color = "green")

my_data %>% 
  summarize(mean = mean(height),
            sd = sd(height))
# test hypothesis

t.test(height, mu = 170, data = my_data, alternative = "greater")

## assumptions
# normality

hist(my_data$height)

my_data %>% 
  ggplot() +
  aes(sample = height) +
  stat_qq() +
  stat_qq_line()

###################################################################
#                          Bivariate statistics                   #
###################################################################

### Contingency tables
# Hypothesis: Post-treatment health status is different in the different home ownership categories.
# use fisher.test() if 2x2 table

# exploration

ownership_health_status_table = table(my_data$home_ownership, my_data$health_status)
ownership_health_status_table

my_data %>% 
  ggplot() +
  aes(x = home_ownership, fill = health_status) +
  geom_bar(position = "dodge")

# test hypothesis

chisq.test(ownership_health_status_table)

### Difference of means across groups
# Independent sample t-test
# Hypothesis: The mean of post-treatment anxiety is different by gender

# exploration

summary = my_data %>% 
  group_by(gender) %>% 
  summarize(mean = mean(anxiety_post), sd = sd(anxiety_post))
summary

my_data %>% 
  ggplot() +
  aes(x = gender, y = anxiety_post) +
  geom_boxplot()

my_data %>% 
  ggplot() +
  aes(x = anxiety_post, fill = gender) +
  geom_density(alpha = 0.3)

my_data %>% 
  ggplot() +
  aes(x = gender, y = anxiety_post, fill = gender) +
  geom_violin() +
  geom_jitter(width = 0.2)

# test hypothesis

t_test_results = t.test(anxiety_post ~ gender, data = my_data)
t_test_results

mean_dif = summary %>% 
  summarize(mean_dif = mean[1] - mean[2])
mean_dif

## assumptions
# normality

my_data %>% 
  filter(gender == "male") %>% 
  ggplot() +
  aes(sample = anxiety_post) +
  stat_qq() +
  stat_qq_line()

my_data %>% 
  filter(gender == "female") %>% 
  ggplot() +
  aes(sample = anxiety_post) +
  stat_qq() +
  stat_qq_line()




### Difference of means across three or more groups

# One-way ANOVA

# Hypothesis: The mean of post-treatment anxiety is different in teh different home ownership categories

# exploration

summary_home_ownership_vs_anxiety = my_data %>% 
  group_by(home_ownership) %>% 
  summarize(mean = mean(anxiety_post), sd = sd(anxiety_post))
summary_home_ownership_vs_anxiety


my_data %>% 
  ggplot() +
  aes(x = home_ownership, y = anxiety_post) +
  geom_boxplot()

my_data %>% 
  ggplot() +
  aes(x = home_ownership, y = anxiety_post) +
  geom_violin()+
  geom_boxplot()+
  geom_jitter(aes(color = home_ownership))

# hypothesis test

ANOVA_result = aov(anxiety_post ~ home_ownership, data = my_data)
summary(ANOVA_result)

## assumptions

# homogeneity of variances
leveneTest(anxiety_post ~ home_ownership, data = my_data)

# normality

my_data %>% 
  filter(home_ownership == "own") %>% 
  ggplot() +
  aes(sample = anxiety_post) +
  stat_qq() +
  stat_qq_line()

my_data %>% 
  filter(home_ownership == "friend") %>% 
  ggplot() +
  aes(sample = anxiety_post) +
  stat_qq() +
  stat_qq_line()

my_data %>% 
  filter(home_ownership == "rent") %>% 
  ggplot() +
  aes(sample = anxiety_post) +
  stat_qq() +
  stat_qq_line()

### Relationship between numerical variables

# Correlation test

# Hypothesis: There is a relationhsip between resilience and height

# exploration

my_data %>% 
  select(resilience, height) %>% 
  cor()

my_data %>% 
  ggplot() +
  aes(x = resilience, y = height) +
  geom_point() +
  geom_smooth(method = "lm")

# hypothesis test

correlation_result = cor.test(my_data$resilience, my_data$height)	
correlation_result	

## assumptions

# normality

my_data %>% 
  ggplot() +
  aes(sample = resilience) +
  stat_qq() +
  stat_qq_line()

my_data %>% 
  ggplot() +
  aes(x = resilience) +
  geom_histogram()


my_data %>%
  ggplot() +
  aes(sample = height) +
  stat_qq() +
  stat_qq_line()


my_data %>% 
  select(resilience, height) %>% 
describe()

###################################################################
#                       Multivariate statistics                   #
###################################################################

### Multiple linear regression
# Hypothesis: We can get a good prediction model of post-treatment anxiety by 
# using resilience and height as predictors 

# exploration

my_data %>% 
  ggplot() +
  aes(x = resilience, y = anxiety_post) +
  geom_point()+
  geom_smooth()

my_data %>% 
  ggplot() +
  aes(x = anxiety_post, y = height) +
  geom_point()+
  geom_smooth()


my_data %>% 
  select(anxiety_post, resilience, height) %>% 
  cor()

# hypothesis test

mod1 = lm(anxiety_post ~ resilience + height, data = my_data)
summary(mod1)

## assumptions

# influential cases

my_data %>% 
  ggplot() +
  aes(x = resilience, y = anxiety_post) +
  geom_point()+
  geom_smooth()

my_data %>% 
  ggplot() +
  aes(x = anxiety_post, y = height) +
  geom_point()+
  geom_smooth()

mod1 %>% 	
  plot(which = 5)	

mod1 %>% 	
  plot(which = 4)	

# normality of residuals

mod1 %>% 	
  plot(which = 2)	

residuals_mod1 = enframe(residuals(mod1))	
residuals_mod1 %>% 	
  ggplot() +	
  aes(x = value) +	
  geom_histogram()	

describe(residuals(mod1))	

# linearity

mod1 %>% 	
  residualPlots()	

# homoskedasticity

mod1 %>% 	
  plot(which = 3)	

mod1 %>% 	
  ncvTest() # NCV test	

mod1 %>% 	
  bptest() # Breush-Pagan test	

# multicollinearity


mod1 %>% 	
  vif()	
