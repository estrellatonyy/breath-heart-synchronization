# Title: RSA validation by Machine Learning analysis
# Author: Tony Estrella
# July 2025


# Libraries ----
library(tidyverse)
library(readxl)
library(compareGroups)
library(tidymodels)


# Data ----
df <- read_xlsx("C:/Users/estre/OneDrive - UAB/5. HRVB DATA/HRV/hrv_indexes.xlsx")

df$output<- as.factor(df$output)


# EDA ----

## Removing from the dataset participant 30 sesion 5 

df <- df %>%
  filter(!(Participante == "030" & Sesion == "s5"))

# Remove RMSSD values over 150.
df %>% 
  filter(RMSSD > 150)

## General visualization 

df %>% 
  ggplot(aes(as.factor(resp), RMSSD))+
  geom_boxplot()

df %>% 
  ggplot(aes(as.factor(resp), SDNN))+
  geom_boxplot()

df %>% 
  ggplot(aes(as.factor(resp), LF_nu))+
  geom_boxplot()

df %>% 
  ggplot(aes(as.factor(resp), LF_power))+
  geom_boxplot()

df %>% 
  ggplot(aes(RMSSD, SDNN))+
  geom_point(aes(colour = output))

df %>% 
  ggplot(aes(RMSSD, SDNN))+
  geom_point(aes(colour = as.factor(resp)))

df %>% 
  ggplot(aes(HF_nu, RMSSD))+
  geom_point(aes(colour = output))

## CompareGroups 
### Output
comp_output <- compareGroups(output ~ . -Participante-Sesion-resp, data= df)
createTable(comp_output, digits = 2)

# Machine Learning analyisis ----

## Algorithms especification ----

### Random Forest
rf_spec <- 
  rand_forest(
    mtry = tune(),
    trees = tune(),
    min_n = tune()
  ) %>% 
  set_mode("classification") %>% 
  set_engine("ranger", importance = "impurity")

### XGBoost
xgb_spec <- 
  boost_tree(
    mtry = tune(),
    trees = tune(),
    min_n = tune(),
    learn_rate = tune()
  ) %>% 
  set_mode("classification") %>% 
  set_engine("xgboost", importance = "impurity")

### Support Vector Machine
svm_spec <-
  svm_rbf(
    cost = tune(),
    rbf_sigma = tune()
  ) %>%
  set_mode("classification") %>%
  set_engine("kernlab")

## Evaluation metrics ----
ev_metrics <- metric_set(accuracy, precision, sensitivity, specificity, roc_auc,recall)

## Dataset Division ----
set.seed(20072025)
data_split <- group_vfold_cv(df,
                             group = Participante,
                             v = length(unique(df$Participante)))

data_split$splits[[2]]

for(i in 1:nrow(data_split)) {
  fold_test <- assessment(data_split$splits[[i]])
  participante_test <- unique(fold_test$Participante)
  n_obs_test <- nrow(fold_test)
  cat("Fold", i, ": Participante test =", participante_test, 
      "(", n_obs_test, "observaciones )\n")
}

## Recipes ----
rec <- recipe(output ~ ., data = df) %>% 
  step_rm(Participante, Sesion, resp)

### Preproc
preproc <- list(
  rec = rec
)

### Models
models <- list(
  rf = rf_spec,
  xgboost = xgb_spec,
  svm = svm_spec
)

prep(rec) %>% juice()



## Workflow ----
### Workflow_set() ----
model_workflow <- workflow_set(preproc, models)

control <- control_grid(
  save_pred = TRUE,        
  save_workflow = TRUE,    
  verbose = TRUE           
)

### Tuning ----
rf_hyp <- parameters(
  mtry(range = c(2, 18)),
  trees(range = c(100, 1000)),
  min_n(range = c(2, 20))
)

xgb_hyp <- parameters(
  mtry(range = c(2, 18)),
  trees(range = c(100, 1000)),
  min_n(range = c(2, 20)),
  learn_rate(range = c(0.01, 0.3))
)

svm_hyp <- parameters(
  cost(range = c(-5, 2)),
  rbf_sigma(range = c(-5, 0))
)

### Update the model workflow with hyperparameters range
model_workflow$wflow_id 

model_workflow <- model_workflow %>% 
  option_add(param_info = rf_hyp, id = "rec_rf") %>% 
  option_add(param_info = xgb_hyp, id = "rec_xgboost") %>%
  option_add(param_info = svm_hyp, id = "rec_svm")

#### Tuning hyperparameters ----
doParallel::registerDoParallel(cores = parallel::detectCores()-1)

tuning_resultados <- model_workflow %>%
  workflow_map("tune_grid",
               seed = 1234,
               resamples = data_split,
               metrics = ev_metrics,
               grid = 5,  
               control = control)

#### Tuning results ----
tuning_resultados$wflow_id

tuning_metrics <- collect_metrics(tuning_resultados)

##### RF
rf_best_hyp <- show_best(tuning_resultados %>% extract_workflow_set_result("rec_rf"), metric = "accuracy", n = 1)

rf_best_model <- tuning_resultados %>%
  extract_workflow_set_result("rec_rf") %>% 
  select_best(metric = "accuracy")

##### XGBoost
xgb_best_hyp <- show_best(tuning_resultados %>% extract_workflow_set_result("rec_xgboost"), metric = "accuracy", n = 1)

xgb_best_model <- tuning_resultados %>%
  extract_workflow_set_result("rec_xgboost") %>% 
  select_best(metric = "accuracy")

##### SVM
svm_best_hyp <- show_best(tuning_resultados %>% extract_workflow_set_result("rec_svm"), metric = "accuracy", n = 1)

svm_best_model <- tuning_resultados %>%
  extract_workflow_set_result("rec_svm") %>% 
  select_best(metric = "accuracy")

### Finalize the workflow ----

final_rf_wf <- tuning_resultados %>%
  extract_workflow("rec_rf") %>%
  finalize_workflow(rf_best_model)

final_xgb_wf <- tuning_resultados %>%
  extract_workflow("rec_xgboost") %>%
  finalize_workflow(xgb_best_model)

final_svm_wf <- tuning_resultados %>%
  extract_workflow("rec_svm") %>%
  finalize_workflow(svm_best_model)

## Training final models ----
control_res <- control_resamples(
  save_pred = TRUE,        
  save_workflow = TRUE,    
  verbose = TRUE           
)

### RF 
final_rf_fit <- fit_resamples(final_rf_wf,
                              resamples = data_split,
                              metrics = ev_metrics,
                              control = control_res)

#### Metrics
results_RF <- collect_metrics(final_rf_fit)

results_RF %>% 
  ggplot(aes(.metric, mean))+
  geom_point(size = 3)+
  geom_errorbar(aes(ymin = mean - std_err, ymax = mean + std_err))+
  scale_y_continuous(limits = c(0.5, 1))+
  labs(title = "Random Forest model",
       y = "Mean",
       x = "Evaluation metric")+
  theme_bw(base_size = 12)


### XGBoost 
final_xgb_fit <- fit_resamples(final_xgb_wf,
                              resamples = data_split,
                              metrics = ev_metrics,
                              control = control_res)

#### Metrics
results_xgb <- collect_metrics(final_xgb_fit)

results_xgb %>% 
  ggplot(aes(.metric, mean))+
  geom_point(size = 3)+
  geom_errorbar(aes(ymin = mean - std_err, ymax = mean + std_err))+
  scale_y_continuous(limits = c(0.5, 1))+
  labs(title = "eXtreme Gradient Boosting (XGBoost) model",
       y = "Mean",
       x = "Evaluation metric")+
  theme_bw(base_size = 12)


### SVM
final_svm_fit <- fit_resamples(final_svm_wf,
                               resamples = data_split,
                               metrics = ev_metrics,
                               control = control_res)

#### Metrics
results_svm <- collect_metrics(final_svm_fit)

results_svm %>% 
  ggplot(aes(.metric, mean))+
  geom_point(size = 3)+
  geom_errorbar(aes(ymin = mean - std_err, ymax = mean + std_err))+
  scale_y_continuous(limits = c(0.5, 1))+
  labs(title = "Support Vector Machine model",
       y = "Mean",
       x = "Evaluation metric")+
  theme_bw(base_size = 12)

### 
results_RF <- results_RF %>% 
  mutate(model = rep("Random Forest", length(.metric)))
results_xgb <- results_xgb %>% 
  mutate(model = rep("XGBoost", length(.metric)))
results_svm <- results_svm %>% 
  mutate(model = rep("SVM", length(.metric)))

df_results <- rbind(results_RF, results_xgb, results_svm)

df_results %>% 
  ggplot(aes(.metric, mean, colour = model))+
  geom_point(position = position_dodge(width = 0.5),size = 3)+
  geom_errorbar(aes(ymin = mean - std_err, ymax = mean + std_err),
                position = position_dodge(width = 0.5),
                width = 0.4)+
  scale_y_continuous(limits = c(0.5, 1))+
  scale_colour_manual(values=c("#f2d339", "#429053", "#6c242a"))+
  labs(title = "",
       y = "Mean",
       x = "Evaluation metric",
       colour = "Model")+
  theme_bw(base_size = 12)
  




