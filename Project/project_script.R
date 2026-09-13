install.package("brms")
data("airbnb")
#Main Effect, First Order Model
#Reorders cancellation policy feature so that flexible is the baseline
airbnb$cancellation_policy <- relevel(factor(airbnb$cancellation_policy), ref = "flexible")
#Sets the priors for all my coefficients including both levels of the cancellation policy 
prior_coefs <- normal(location = c(40, 20, 50, 2), 
                      scale = c(5, 5, 25, 1), 
                      autoscale = FALSE)
#Models price as a function of cancellation policy, beds and review scores rating
mefo <- stan_glm(
  price ~ cancellation_policy + beds + review_scores_rating,
  data = airbnb,
  prior = prior_coefs,
  prior_intercept = normal(177, 30, autoscale = FALSE),
  chains = 4,
  iter = 5000*2,
  seed = 84735
)
#Summarizes our coefficient estimates and their corresponding 90% CIs.
tidy(abnb, conf.int = TRUE, conf.level = 0.9)
#Trace plot
mcmc_trace(mefo)
#Density overlay
mcmc_dens_overlay(mefo)
#Posterior predictive check 
pp_check(mefo, nreps = 50)
set.seed(84735)
mod4 <- prediction_summary(model = mefo, data = airbnb)
mod4
#Posterior prediction summary using 10-fold cross validation
cvmod4 <- prediction_summary_cv(model=mefo, data=airbnb, k=10)
cvmod4$cv

# Initialize gaussian linear model with default priors
def_mod <- stan_glm(price ~ beds + review_scores_rating + cancellation_policy, 
                    data = airbnb, chains = 4, iter = 10000, seed = 84735)

# Evaluate simulation
mcmc_trace(def_mod, size = .1) # Trace plots for predictors
tidy(def_mod, conf.int = TRUE, conf.level=.9) # Posterior interval for coefficients
mcmc_dens_overlay(def_mod) # Overlaid density plot for predictors
pp_check(def_mod, nreps = 50) # Posterior predictive checks for 50 new datasets

# Extended MAE evaluation
prediction_summary(def_mod, data = airbnb) # One through of the data
prediction_summary_cv(model = def_mod, data = airbnb, k = 10) # CV

#Alternative Models
#Fitting the first alternative model
price_mod1 <- stan_glm(price~I(beds^2)+I(bathrooms^2)+review_scores_rating, data=airbnb,
                       family=gaussian,
                       prior_intercept = normal(177,30),
                       prior_aux = exponential(1/40),
                       chains=4, iter=5000*2,seed=84735)
#Evaluating the model
mcmc_trace(price_mod1, size=0.1)
mcmc_dens_overlay(price_mod1)
tidy(price_mod1, effects = c("fixed", "aux"),
     conf.int = T, conf.level = 0.9)
set.seed(84735)
pp_check(price_mod1, nreps=50)  	#pp check
set.seed(84735)
prediction_summary(model=price_mod1, data=airbnb)		#prediction summary evaluation

set.seed(84735)
cvmod1 <- prediction_summary_cv(model=price_mod1, data=airbnb, k=10)
cvmod1$cv	#cross-validation evaluation



#Fitting the second alternative model
price_mod2 <- stan_glm(price~beds*accommodates+cancellation_policy+room_type,
                       data=airbnb,
                       family=gaussian,
                       chains=4, iter=5000*2,seed=84735)
#Evaluating the model
mcmc_trace(price_mod2, size=0.1)
mcmc_dens_overlay(price_mod2)
tidy(price_mod2, effects = c("fixed", "aux"),
     conf.int = T, conf.level = 0.9)
set.seed(84735)
pp_check(price_mod2, nreps=50) #pp check
set.seed(84735)
prediction_summary(model=price_mod2, data=airbnb) #prediction summary evaluation

set.seed(84735)
cvmod2 <- prediction_summary_cv(model=price_mod2, data=airbnb, k=10)
cvmod2$cv
#Posterior predictive analysis
post_price_mod2 <- posterior_predict(price_mod2, newdata=data.frame(beds=2, accommodates=4, cancellation_policy="strict", room_type='Entire home/apt'))

posterior_interval(post_price_mod2, prob=.90)


