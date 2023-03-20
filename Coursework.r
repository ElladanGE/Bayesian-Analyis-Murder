
library(MASS)
library(brms)
library(dplyr)
library(bayesplot)
library(reshape2)
library(ggplot2)

# Reading data and splitting into train and test
homicides = read.csv('homicides.csv')
data = read.csv('homicides.csv')
data$solved_status = as.integer(data$solved_status == "Solved")


## 75% of the sample size
smp_size <- floor(0.75 * nrow(data))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(data)), size = smp_size)

train <- data[train_ind, ]
test <- data[-train_ind, ]



# Plotting Data
murderData = melt(train, id.vars = 'solved_status')
ggplot(murderData) +
  geom_point(aes(x=value, y= solved_status , colour=variable)) +
  geom_smooth(aes(x=value, y= solved_status, colour=variable),method = lm) +
  facet_wrap(~variable, scales="free_x")


# Data Exploration


# Looking at solved rate by ethnicity
num_white_solved = (train %>% filter(observed_ethnicity == "White", solved_status == 1))[,1]
tot_white = (train %>% filter(observed_ethnicity == "White"))[,1]
num_black_solved = (train %>% filter(observed_ethnicity == "Black", solved_status == 1))[,1]
tot_black = (train %>% filter(observed_ethnicity == "Black"))[,1]
num_asian_solved = (train %>% filter(observed_ethnicity == "Asian", solved_status == 1))[,1]
tot_asian = (train %>% filter(observed_ethnicity == "Asian"))[,1]

length(num_white_solved) / length(tot_white)
length(num_black_solved) / length(tot_black)
length(num_asian_solved) / length(tot_asian)


# Looking at solved rate by method of killing
num_knife_solved = length((train %>% filter(method_of_killing == "Knife or Sharp Implement", solved_status == 1))[,1])
tot_knife = length((train %>% filter(method_of_killing == "Knife or Sharp Implement"))[,1])

num_blunt_solved = length((train %>% filter(method_of_killing == "Blunt Implement", solved_status == 1))[,1])
tot_blunt = length((train %>% filter(method_of_killing == "Blunt Implement"))[,1])

num_phys_solved = length((train %>% filter(method_of_killing == "Physical Assault", solved_status ==1))[,1])
tot_phys = length((train %>% filter(method_of_killing == "Physical Assault"))[,1])

num_shoot_solved = length((train %>% filter(method_of_killing == "Shooting", solved_status == 1))[,1])
tot_shoot = length((train %>% filter(method_of_killing == "Shooting"))[,1])

num_knife_solved/tot_knife
num_blunt_solved/tot_blunt
num_phys_solved/tot_phys
num_shoot_solved/tot_shoot


data_by_ethn = train %>% group_by(observed_ethnicity, age_group) %>% summarize(succes_rate = mean(solved_status))
data_by_kill = train %>% group_by(method_of_killing, age_group) %>% summarize(success_rate = mean(solved_status))

data_by_bor = train %>% group_by(borough) %>% summarize(success_rate = mean(solved_status)) 
data_by_sex = train %>% group_by(age_group, sex) %>% summarize(success_rate = mean(solved_status))
data_by_da = train %>% group_by(domestic_abuse, sex) %>% summarize(succes_rate = mean(solved_status)) 
data_by_seas = train %>% group_by(year) %>% summarize(succes_rate = mean(solved_status))

ggplot(data_by_seas, aes(x = year, y = succes_rate
                        )) + 
  geom_point()


ggplot(data_by_ethn, aes(x = age_group, y = succes_rate, 
                               color = observed_ethnicity)) + 
  geom_point()

ggplot(data_by_bor, aes(x = borough, y = success_rate, )) + 
  geom_point()

ggplot(data_by_sex, aes(x = age_group, y = success_rate, 
                         color = sex)) + 
  geom_point()

ggplot(data_by_da, aes(x = domestic_abuse, y = succes_rate, 
                        color = sex)) + 
  geom_point()

ggplot(data_by_kill, aes(x = age_group, y = success_rate, color = method_of_killing
                       )) + 
  geom_point()

# Build 1st model

prior_int = set_prior("normal(2, 4)", class="Intercept")
prior_b = set_prior("normal(0,0.1)", class = "b")
prior_ethn_black = set_prior("normal(0,0.5)", class="b", coef="observed_ethnicityBlack")
prior_ethn_white = set_prior("normal(0,0.2)", class="b", coef="observed_ethnicityWhite")
prior_ethn_nk = set_prior("normal(0,1)", class="b", coef="observed_ethnicityNotReportedDNotknown")
prior_ethn_other = set_prior("normal(0,0.5)", class="b", coef="observed_ethnicityOther")
prior_sexM = set_prior("normal(0,0.2)", class = "b", coef="sexMale")
prior_daN = set_prior("normal(0,0.1)", class = "b", coef="domestic_abuseNotDomesticAbuse")
prior_kill_sharp = set_prior("normal(0,0.3)", class = "b", coef = "method_of_killingKnifeorSharpImplement")
prior_kill_phys = set_prior("normal(0,0.3)", class = "b", coef = "method_of_killingPhysicalAssault")
prior_kill_shoot = set_prior("normal(0,1)", class = "b", coef = "method_of_killingShooting")
prior_year = set_prior("normal(0,0.5)", class = "b" , coef="year")

sd_priorsI = set_prior("normal(0,1)", class = "sd",group = "age_group", coef = "Intercept")
sd_priors_kill = set_prior("normal(0,0.1)", class = "sd", group = "age_group", coef = "method_of_killingKnifeorSharpImplement")
sd_priors_Phys = set_prior("normal(0,0.1)", class = "sd", group = "age_group",coef = "method_of_killingPhysicalAssault")
sd_priors_Shot = set_prior("normal(0,0.5)", class = "sd", group = "age_group",coef = "method_of_killingShooting")
sd_priors_B = set_prior("normal(0,0.1)", class = "sd", group = "age_group",coef = "observed_ethnicityBlack")
sd_priors_Nr = set_prior("normal(0,1)", class = "sd",group = "age_group", coef = "observed_ethnicityNotReportedDNotknown")
sd_priors_Oth = set_prior("normal(0,1)", class = "sd",group = "age_group", coef = "observed_ethnicityOther")
sd_priors_W = set_prior("normal(0,0.05)", class = "sd", group = "age_group",coef = "observed_ethnicityWhite")
sd_priors_M = set_prior("normal(0,0.05)", class = "sd",group = "age_group", coef = "sexMale")

priors = c(prior_int, prior_ethn_black, prior_ethn_white, prior_daN, prior_sexM, prior_kill_phys, prior_kill_sharp
           ,prior_kill_shoot, prior_b, sd_priorsI, sd_priors_kill, sd_priors_Phys, sd_priors_Shot, sd_priors_B, 
           sd_priors_Nr, sd_priors_Oth, sd_priors_W, sd_priors_M, prior_ethn_nk, prior_ethn_other)

murder_fit = brm(solved_status ~ observed_ethnicity + sex  + domestic_abuse + method_of_killing + (sex+observed_ethnicity+method_of_killing|age_group),
                 family = bernoulli(link = 'logit'), prior = priors, data = train)
?summary
a = summary(murder_fit)
plot(murder_fit, ask = FALSE)
mcmc_plot(murder_fit, type = "trace", variable = "^sd_", regex = TRUE)
preds <- predict(murder_fit, newdata=test)
head(preds)
a_classifier <- preds[,"Estimate"]>=0.91
ConfusionMatrix <- function(Classifier, Truth){
  if(!(length(Classifier)==length(Truth)))
    stop("Make the length of your vector of predictions the same as the length of the truth")
  if(is.logical(Classifier))
    Classifier <- as.integer(Classifier)
  WhichClass0s <- which(Classifier < 1)
  ZeroCompare <- Truth[WhichClass0s]
  Predicted0 <- c(length(ZeroCompare)-sum(ZeroCompare), sum(ZeroCompare))
  WhichClass1s <- which(Classifier>0)
  OnesCompare <- Truth[WhichClass1s]
  Predicted1 <- c(length(OnesCompare)-sum(OnesCompare), sum(OnesCompare))
  ConMatrix <- cbind(Predicted0,Predicted1)
  row.names(ConMatrix) <- c("Actual 0", "Actual 1")
  colnames(ConMatrix) <- c("Pred 0", "Pred 1")
  ConMatrix
}

conmat <- ConfusionMatrix(a_classifier, test$solved_status)
conmat
sum(diag(conmat))/sum(conmat)

ranef(murder_fit)

samples = posterior_samples(murder_fit)

mcmc_pairs(samples[,1:5])
mcmc_pairs(samples[,6:10])
mcmc_pairs(samples[,10:15])

sum(samples$b_sexMale<0)/length(samples$b_sexMale)
sum(samples$b_observed_ethnicityBlack<0)/length(samples$b_observed_ethnicityBlack)
sum(samples$b_observed_ethnicityWhite>0)/length(samples$b_observed_ethnicityWhite)
sum(samples$b_method_of_killingShooting<0)/length(samples$b_method_of_killingShooting)
sum(samples$b_method_of_killingShooting<0)/length(samples$b_method_of_killingShooting)
sum(samples$b_method_of_killingPhysicalAssault>0)/length(samples$b_method_of_killingPhysicalAssault)


mcmc_plot(murder_fit, type = "hist", variable = "^b_", regex = TRUE)
mcmc_plot(murder_fit, type = "hist", variable = "^sd_", regex = TRUE)
x
# Sensitivity Model
priors = c(prior_int, prior_daN, prior_kill_phys, prior_kill_sharp
           ,prior_kill_shoot, prior_b, sd_priorsI, sd_priors_kill, sd_priors_Phys, sd_priors_Shot)

Sens_fit = brm(solved_status ~ sex+ domestic_abuse + method_of_killing + (sex+method_of_killing|age_group),
                 family = bernoulli(link = 'logit'), prior = priors, data = train)


summary(Sens_fit)
plot(Sens_fit, ask = FALSE)

preds <- predict(Sens_fit, newdata=test)
head(preds)
conmat <- ConfusionMatrix(a_classifier, test$solved_status)
conmat
sum(diag(conmat))/sum(conmat)



## Part 3



curated_cols <- c("recorded_date","age_group","sex","observed_ethnicity","domestic_abuse",
                  "borough","method_of_killing")
new_dates <- as.Date(c("2022-04-01", "2022-05-01", "2022-06-01",
                       "2022-07-01", "2022-08-01", "2022-09-01",
                       "2022-10-01", "2022-11-01", "2022-12-01"))
curated_homs <- dplyr::select(homicides, all_of(curated_cols))
hypothetical_homicides <- tibble(recorded_date = sample(new_dates, 2, TRUE))
month_tibble <- read.csv("month_tibble.csv")
for(i in 2:length(names(curated_homs))){
  hypothetical_homicides <- cbind(hypothetical_homicides,
                                  sample(as.vector(unlist(unique(curated_homs[,i]))),2,TRUE))
}
names(hypothetical_homicides) <- names(curated_homs)
month_tibble$recorded_date = as.Date(month_tibble$recorded_date)
hypothetical_homicides <- as_tibble(hypothetical_homicides) %>%
  left_join(month_tibble, by = "recorded_date")
print(hypothetical_homicides)


pred = predict(murder_fit, hypothetical_homicides, summary = FALSE)

MonteCarlo = function(){
  prob = 0
  for (i in 1:length(pred[,1])){
    if (pred[i,1] == 1 && pred[i,2] == 0){
      prob = prob + 1
    }
  }
  estimate = prob/4000
  mcerror = sqrt(estimate*(1-estimate)/4000)
  return(list(MCestimate = estimate, Error = mcerror))
}
tMC = MonteCarlo()
tMC
