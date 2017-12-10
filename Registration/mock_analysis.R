library(tidyverse)
library(brms)
library(bayesplot)


########################
## load & prep data
########################         


## data set created by our own (fast & careless) clicking through the experiment
simdata = readr::read_csv('simdata.csv') %>% 
  filter(kind == "trial") %>%        ## discard training trials
  mutate(trial = trial -3,           ## reset trial counter
         sentExpression = factor(sentExpression, ordered = TRUE,                        ## order response levels
                                 levels = c("certNot", "probNot", "prob", "cert"))) %>%
  select(id, trial, color, scenario, qud, value, sentExpression, language, comments)

########################
## exclusion criteria
########################         

## - screen self-reported language
## - screen comments for lack of understanding
## - compute error rate on controls 
## - check for missing data points due to technical problems

########################
## analysis:
## posterior of coeff.
##########0##############         

## compute target model
#### adjust acceptance rate if necessary
#### drop random slopes in order of QUD, SCENARIO, VALUE

fitTarget = brm(formula = sentExpression ~ value * qud * scenario + (1 + value * qud * scenario | id),
                data = simdata, family = cumulative)


# inspect posterior of coefficients for target model

ps = posterior_samples(fitTarget, pars = c("b_value", "b_qudwh", "b_scenarioplural", "b_qudwh:scenarioplural")) %>% 
  gather(key = "variable", value = "value") %>% as_tibble

ps_summary = ps %>% group_by(variable) %>% 
  summarize(HDI_lo = coda::HPDinterval(coda::as.mcmc(value))[1],
            mean = mean(value),
            HDI_hi = coda::HPDinterval(coda::as.mcmc(value))[2]) %>% 
  ungroup() %>% 
  mutate(credibly_different = ifelse(HDI_lo > 0, "bigger", ifelse(HDI_hi < 0, "smaller", "--")))

ps_summary %>% show


# plot posterior estimates for coefficients

color_scheme_set("red")
mcmc_intervals(as.array(fitTarget), pars = c("b_value", "b_qudwh", "b_scenarioplural"))


########################
## additional analysis:
## model comparison
##########0##############         

fitNoQUD = brm(formula = sentExpression ~ value * scenario + (1 + value * scenario | id),
                       data = simdata, family = cumulative)
fitNoScenario = brm(formula = sentExpression ~ value * qud + (1 +  value * qud | id),
               data = simdata, family = cumulative)
fitNoValue = brm(formula = sentExpression ~ qud * scenario + (1 +  qud * scenario | id),
                    data = simdata, family = cumulative)
fitOnlyQUD = brm(formula = sentExpression ~ qud + (1 +  qud | id),
                 data = simdata, family = cumulative)
fitOnlyScenario = brm(formula = sentExpression ~ scenario + (1 +  scenario | id),
                      data = simdata, family = cumulative)
fitOnlyValue = brm(formula = sentExpression ~ value + (1 +  value | id),
                   data = simdata, family = cumulative)
fitIntercept = brm(formula = sentExpression ~ 1,
                   data = simdata, family = cumulative)


# compare models by LOO-IC
loo(fitTarget, 
    fitNoQUD, fitNoScenario, fitNoValue, 
    fitOnlyValue, fitOnlyScenario, fitOnlyQUD, 
    fitIntercept)



