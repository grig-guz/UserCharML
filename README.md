# UserCharML
Code for ML experiments with outputs from EMDAT. It supports cumulative-within, cumulative-across and full-window regimes.
## Data folder structure
Place the EMDAT output in the following format:
- train_data/
  - cumulative_within/
  - cumulative_across/
  - full_window/
  
Results will be stored in the following format:
- results/
  - predict-taskchar_result_cumulative_1_9.csv
  - ...
  - predict-taskchar_result_fullwindow.csv
  - predict-taskchar_result_across.csv
## Script parameters
Within the script, specify the following parameters:
- dirout - path to folder to store results in.
- label_dir - path to folder with .csv containing user and user-mmd labels.
- users_files_path - path to folder with VisLit test results, used for more balanced VisLit median split.
- user_dependent - list of user-dependent characteristics to predict.
- msnv_dependent - list of mmd-dependent characteristics to predict.
- models - list of models whose output needs to be stored.
- metrics - list of metrics to store.
- machine_id (1, 2, 3, 4, 5, or 6, for cumulative-within only) - specifies for which subset of data the current process will generate the output, i.e., for machine_id = 1 it will generate output for timesteps 1 to 9, for machine_id = 2 the output will be for timesteps 10 to 19 etc.

## Running
Run within RStudio/command line.

## Plotting results
Code for plotting works for cumulative-within and cumulative-across regimes. Follow the steps in plotting.ipyib, which lets you specify the regime, where to load the data from and which classifiers to plot results for. 
