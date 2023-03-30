pacman::p_load(tidyverse, tidymodels)
install.packages("palmerpenguins")
data("penguins", package = "palmerpenguins")
penguin_M1 <-
  workflow() %>%
  add_formula(flipper_length_mm ~ body_mass_g) %>%
  add_model(
    linear_reg() %>%
      set_engine("lm")
  ) %>%
  fit(penguins)
penguin_M1


penguin_M2 <-
  workflow() %>%
  add_formula(sex ~ body_mass_g) %>%
  add_model(
    logistic_reg() %>% set_engine("glm")
  ) %>%
  fit(penguins)
penguin_M2  

#For M1 
#Response Variable : flipper_length_mm
#Predcitor Variable(s) : body_mass_g

#For M2 
#Response Variable : sex
#Predcitor Variable(s) : body_mass_g


penguins_pred <-
  penguins %>%
  bind_cols(
    predict(penguin_M1, penguins),
    predict(penguin_M2, penguins),
    predict(penguin_M2, penguins,
            type = "prob"),
  ) %>%
  select(sex, flipper_length_mm,starts_with(".pred"))
penguins_pred

# Predicted Flipper length for 1st 181
# Predicted Probability of being male 0.374

penguins_pred %>%
  conf_mat(
    truth = sex,
    estimate = .pred_class
  )

#56

penguins_pred %>%
  sens(
    truth = sex,
    estimate = .pred_class
  )
#0.661

categorical_metrics <- metric_set(sens, spec, precision, recall)
penguins_pred %>%
  categorical_metrics(
    truth = sex,
    estimate = .pred_class
  )
#precision : 0.596

penguins_pred %>%
  roc_curve(
    truth = sex,
    estimate = .pred_female
  ) %>%
  autoplot()

penguins_pred %>%
  roc_auc(
    truth = sex,
    estimate = .pred_female
  )
# AUC : 0.752

set.seed(2021)
penguin_split <- initial_split(penguins)
penguin_split

penguins_train <- training(penguin_split)
penguins_test <- testing(penguin_split)

#86 


penguin_CV <- vfold_cv(penguins_train)
penguin_CV
#10

linear_model <-
  linear_reg() %>%
  set_engine("lm")
penguin_linear_workflow <-
  workflow() %>%
  add_model(linear_model) %>%
  add_formula(bill_length_mm ~ body_mass_g)

logistic_model <-
  logistic_reg() %>%
  set_engine("glm")
penguin_logistic_workflow <-
  workflow() %>%
  add_model(logistic_model) %>%
  add_formula(sex ~ body_mass_g)

penguin_linear_resamples <-
  fit_resamples(
    penguin_linear_workflow,
    resamples = penguin_CV
  )
penguin_linear_resamples

control = control_resamples(save_pred = TRUE)

penguin_logistic_resamples <-
  fit_resamples(
    penguin_logistic_workflow,
    resamples = penguin_CV,
    control = control_resamples(save_pred = TRUE)
  )
penguin_linear_resamples %>% unnest(.metrics)
  
penguin_linear_resamples %>% collect_metrics()

#RMSE for 1st : 4.41
#Mean RSME : 4.30

install.packages("harrypotter")
library("harrypotter")
penguin_logistic_resamples %>%
  collect_predictions() %>%
  group_by(id) %>%
  roc_curve(truth = sex, estimate = .pred_female) %>%
  autoplot() +
  harrypotter::scale_color_hp("Ravenclaw", discrete = TRUE)

penguin_linear_workflow %>%
  last_fit(penguin_split) %>%
  collect_metrics()

penguin_logistic_workflow %>%
  last_fit(penguin_split) %>%
  collect_metrics()
#Test AUC 0.771
