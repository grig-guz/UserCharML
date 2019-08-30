# UserCharML
Code for ML experiments with outputs from EMDAT. It supports cumulative-within, cumulative-across and full-window regimes.
## Data folder structure
Place the EMDAT output in the following format:
- train_data/
  - cumulative_within/
  - cumulative_across/
  - full_window/
## Script parameters
Within the script, specify the following parameters:
- dirout - path to folder to store results in.
- label_dir - path to folder with .csv containing user and user-mmd labels.
- users_files_path - path to folder with VisLit test results, used for more balanced VisLit median split.
- user_dependent - list of user-dependent characteristics to predict.
- msnv_dependent - list of mmd-dependent characteristics to predict.
- models - list of models whose output needs to be stored.
- metrics - list of metrics to store.
## Running
Run within RStudio/command line.
