library(tidyverse)
library(caret)
library(pls)


#Read in data
pre_annual_flux<-read_csv("ars_annual_N2O.csv")

#Filter and adjust data - Morris is a special case
annual_flux<-pre_annual_flux%>%
  select(-Unit_ID, -site)%>%
  mutate(total_N2O = ifelse((town == "Morris"), total_N2O*0.80, total_N2O))%>%
  distinct()%>%
  na.omit()

#######Getting data into required formats#######

#Clean up predictor variables
flux_x<-select(annual_flux, -series, -town, -avg_N2O, -log_avg_N2O,-year, -total_N2O, -log_total_N2O)

#Response variable
flux_y<-select(annual_flux, total_N2O)

#Not everything likes tibbles
flux_xish<-as.data.frame(unclass(flux_x))

#Centering and scaling, removing rows with NA
flux_x_processed<-preProcess(flux_xish, method = c("BoxCox", "center", "scale"), na.remove = TRUE)
flux_x_pro_mat<-predict(flux_x_processed, flux_xish) #Not 100% sure what this line does

#Put the x and y back together for PLS
final_df<-cbind(flux_y, flux_x_pro_mat)

#######Modeling#######

#Fit model
plsFit<-plsr(total_N2O ~ ., validation = "LOO", data=final_df)

#Look at model
summary(plsFit)
plot(RMSEP(plsFit), legendpos = "none") 
plot(plsFit, ncomp=6, asp = 0, line=TRUE)

#Goodness of fit
RMSEP(plsFit)
R2(plsFit)


#Rank importance of predictor variables 
#?varImp
varImp(plsFit, ncomp=6)%>%
  rownames_to_column()%>%
  ggplot(aes(x=reorder(rowname, -Overall), y=Overall))+
  geom_bar(stat="identity")+
  coord_flip()+
  labs(y = "Coefficient sum", x = "Predictors")


###################################################################
library(recipes)
library(rsample)
library(yardstick)

pls_rec <- recipe(total_N2O ~ ., data = annual_flux) %>%
  add_role(series, town, avg_N2O, log_avg_N2O,year, log_total_N2O,
           new_role = "other data") %>%
  step_YeoJohnson(all_predictors()) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors())

# functions to fit the model and get predictions + RMSE
pls_fit <- function(recipe, ...) {
  plsr(total_N2O ~ ., data = juice(recipe, all_predictors(), all_outcomes()))
}
pls_pred <- function(split, recipe, model, ...) {
  # get the holdout data via `assessment` then preprocess
  pred_data <- bake(recipe, newdata = assessment(split), all_predictors(), all_outcomes())
  
  # One outcome so drop the dimension to a 2d array
  pred_vals <- predict(model, newdata = pred_data)[,1,] %>% 
    as.data.frame() %>%
    bind_cols(assessment(split) %>% select(total_N2O)) %>%
    # drop the predictions per component into a long df
    gather(label, prediction, -total_N2O) %>%
    # convert "X comp" to a numeric X
    mutate(
      ncomp = gsub(" comps", "", label),
      ncomp = as.numeric(ncomp)
    ) %>%
    # add resampling labels
    cbind(labels(split))
}


# setup a resampling scheme, estimate the recipes, fit the models and 
# make predictions, 
set.seed(57256)
folds <- vfold_cv(annual_flux, repeats = 5) %>%
  mutate(
    recipes = map(splits, prepper, pls_rec, retain = TRUE),
    models = map(recipes, pls_fit, validation = "none"),
    pred = pmap(
      list(split = splits, recipe = recipes, model = models),
      pls_pred
    )
  )

# get the RMSE values, summarize by #comps and plot

pred_vals <- folds %>% 
  pull(pred) %>% 
  bind_rows()

performance <- pred_vals %>%
  group_by(ncomp, id, id2) %>%
  do(metrics(., truth = total_N2O, estimate = prediction))

av_performance <- performance %>% 
  group_by(ncomp) %>%
  summarize(rmse = mean(rmse), rsq = mean(rsq))

ggplot(av_performance, aes(x = ncomp, y = rsq)) + 
  geom_point() + 
  geom_line() + 
  theme_bw()
