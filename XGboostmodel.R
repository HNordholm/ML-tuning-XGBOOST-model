#              ----  MACHINE LEARNING --- 
#                        XG BOOST 

boat_ml<- boat %>% 
  select(-manufacturer)

# Split DF into training and testing set -- 

set.seed(123)
boat_split <- initial_split(boat_ml,prop=0.8)
boat_train <- training(boat_split)
boat_test <- testing(boat_split)


#       XGBOOST SPEC --

xgb_spec <-   boost_tree(trees=1000,
                         tree_depth = tune(),min_n = tune(),
                         loss_reduction = tune(),sample_size = tune(),
                         mtry = tune(),learn_rate = tune()) %>% 
  set_engine("xgboost") %>% 
  set_mode("regression")

#      PARAMETER GRID --

xgb_grid <- grid_latin_hypercube(tree_depth(),
                                 min_n(),
                                 loss_reduction(),
                                 sample_size=sample_prop(),
                                 finalize(mtry(),boat_train),
                                 learn_rate(),
                                 size=10)

xgb_grid


#    Xgboost workflow and model formula -- 

xgb_wf <- workflow() %>% 
  add_formula(number_of_views_last_7_days~.,) %>% 
  add_model(xgb_spec)

#     CROSS VALIDATON FOLDS --

set.seed(123)
boat_folds <- vfold_cv(boat_train)


### Grid tuning ###

set.seed(234)
xgb_res <- tune_grid(xgb_wf,resamples=boat_folds,
                     grid=xgb_grid)

#   Saving best PARAMETERS by RMSE and finalize xgboost workflow -- 

best_rmse <- select_best(xgb_res)

final_xgb_wf <- finalize_workflow(xgb_wf,best_rmse)



#    XGBOOST VARIABLE IMPORTANCE -- 

final_xgb_wf %>% 
  fit(boat_train) %>% 
  pull_workflow_fit() %>% 
  vip(geom="point")


#   Final evaluation ---

final_res <- last_fit(final_xgb_wf,boat_split)

final_res %>% collect_metrics()

