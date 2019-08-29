library(caret)
library(infotheo)
library(FSelector)
library(stringr)


set.seed(1)

add_labels_to_features <- function(features_data, avail_users, label_data, labels_msnv_data, user_dependent, msnv_dependent, keep_hard_msnvs) {
  features_data["Uid"] = -1
  curr_usr = -1
  if (cumul_across) {
    for(row in 1:nrow(features_data)) {
      sc_id = toString(features_data[row, "Sc_id"])
      if (endsWith(sc_id, "allsc")) {
        undersc_pos = regexpr("_", sc_id)[1] - 1
        curr_usr = strtoi(substring(sc_id, 1, undersc_pos))
        if (curr_usr %in% avail_users) {
          features_data[row, user_dependent] = label_data[label_data[,"user_id"] == curr_usr, user_dependent]
          features_data[row, "Uid"] = curr_usr
        }
      }
    }
  } else{
    for(row in 1:nrow(features_data)) {
      sc_id = toString(features_data[row, "Sc_id"])
      if (endsWith(sc_id, "allsc")) {
        undersc_pos = regexpr("_", sc_id)[1] - 1
        curr_usr = strtoi(substring(sc_id, 1, undersc_pos))
      } else {
        if (curr_usr %in% avail_users) {
          msnv_id = strtoi(features_data[row, "Sc_id"])
          if (msnv_id == 18 || is.na(msnv_id) || (keep_hard_msnvs && !(msnv_id %in% c(3, 5)))) {
            
          } else {
            features_data[row, user_dependent] = label_data[label_data[,"user_id"] == curr_usr, user_dependent]
            features_data[row, msnv_dependent] = labels_msnv_data[labels_msnv_data[,"user_id"] == curr_usr & labels_msnv_data[,"mmd_id"] == msnv_id, msnv_dependent]
            features_data[row, "Uid"] = curr_usr
          }
        }
      }
    }
  }
  # Clean up the data
  features_data = features_data[features_data[dependent[1]] != -1, ]
  features_data[is.na(features_data)] = -1
  return(features_data)
}

############# SET THESE BEFORE RUNNING #####################
# Folder to save prediction outputs/feature importances
dirout = "C:\\Users\\tracker2-admin\\Desktop\\rcode\\results\\" 
# Path to folder with features
data_dir = "C:\\Users\\tracker2-admin\\Desktop\\rcode\\train_data\\"
# Path to folder with labels
label_dir = "C:\\Users\\tracker2-admin\\Desktop\\rcode\\raw_experiment\\"
# Path to .txt files containing BarChartLit test results
users_files_path <- "C:\\Users\\tracker2-admin\\Desktop\\rcode\\user_data\\first_half\\"
# Features that are independent of specific
user_dependent = c("BarChartLit", "VerbalWM_longest", "Meara")
# Features dependent on specific MSNVs
msnv_dependent = c("mmd_accuracy", "mmd_task_time", "mmd_interest")
# Output of which models to save
models = c("MajorityClass","rf", "regLogistic", "xgbTree","svmLinear")
# Metrics to store
metrics = c("NumHigh", "NumLow", "MajorityClass")
#############################################################
#==========================================
feat_selection = T

# NOTE: if all false, will run cumulative within window
disjoint_window = F
cumul_across = T
full_window = F
keep_hard_msnvs = F

if (disjoint_window) {
  time_windows = c(2000, 5000, 10000)
  window_id = 1
  out = paste(dirout, "predict-taskchar_result_disjoint_window_", time_windows[window_id],".csv", sep="")
  feature_imp_fname = paste(dirout, "disjoint_window.txt",sep="")
} else if (cumul_across) {
  time_windows = c(1000)
  out = paste(dirout, "predict-taskchar_numclass_result_accross.csv", sep="")
  feature_imp_fname = paste(dirout, "featimp_cumul_accross.txt",sep="")
} else if (full_window) {
  time_windows = c(1000)
  out = paste(dirout, "predict-taskchar_result_fullwindow.csv", sep="")
  feature_imp_fname = paste(dirout, "featimp_fullwindow.txt",sep="")
} else {
  time_windows = c(1000)
  seqs = c(1, 9, 10, 19, 20, 29, 30, 39, 40, 49, 50, 58)
  machine_id = 3
  left_idx = seqs[2*(machine_id-1)+1]
  right_idx = seqs[2*(machine_id-1)+2]
  feature_imp_fname = paste(dirout, "featimp_cumul_within_",machine_id,".txt",sep="")
  out = paste(dirout, "predict-taskchar_numclass_result_cumulative_",left_idx,"_",right_idx,".csv",sep="")
}

sink(out)
header = "Userchar,WindowSize,Slice"
for (metric in metrics) {
  header = paste(header, metric, sep=",")
}
print(header)
sink()

#==========================================
# Read labels data
labels_filename = paste(label_dir, "user_chars_MMD.csv", sep="")

label_data = read.table(labels_filename, header=T, sep=",")
label_data = na.omit(label_data)


users_files = list.files(users_files_path)
user_barchart_scores =  rep(0, 100)

for (file_name in users_files)  {
  linesplit = strsplit(file_name, "_")
  user_id = strtoi(linesplit[[1]][1])
  conn <- file(paste(users_files_path, file_name, sep=""),open="r")
  lines <-readLines(conn)
  total_score = 0
  num_attempts = 0
  for (i in 2:length(lines)){
    line = lines[i]
    if (str_detect(line, "Score")) {
      num_attempts = num_attempts + 1
      total_score = total_score + strtoi(strsplit(line, ":")[[1]][2])
    }
  }
  user_barchart_scores[user_id] = total_score
  close(conn)
}

labels_msnv_filename = paste(label_dir, "performance_data_msnv_all.csv", sep="")
labels_msnv_data = read.table(labels_msnv_filename, header=T, sep=",")
labels_msnv_data = na.omit(labels_msnv_data)

#Median split stuff
verbal_VM_words_median = median(label_data$VerbalWM_word)
barchart_median = median(user_barchart_scores[user_barchart_scores != 0])
if (cumul_across) {
  dependent = user_dependent
} else {
  dependent = c(user_dependent, msnv_dependent)
}

users_in_cumul = c(95, 67, 66, 65, 64, 63, 62, 92, 50, 46, 45, 42, 40, 38, 93, 61, 60, 59, 58, 55, 52, 97, 73, 72, 71, 70, 69, 68, 90, 19, 18, 16, 12,  9,
                   1, 91, 36, 31, 30, 26, 25, 21, 89, 88, 85, 84, 81, 80, 79, 78, 77, 76, 75, 74)
labels_msnv_data = labels_msnv_data[labels_msnv_data$user_id %in% users_in_cumul, ]
for(i in 1:length(msnv_dependent)) {
  if(is.numeric(labels_msnv_data[, msnv_dependent[i]])) { #DISCRETIZE LABELS
    dep_median = median(labels_msnv_data[, msnv_dependent[i]])
    for (row in 1:nrow(labels_msnv_data)) {
      numeric_val = labels_msnv_data[row, msnv_dependent[i]]
      if (numeric_val > dep_median) {
        labels_msnv_data[row, msnv_dependent[i]] = 1
      } else {
        labels_msnv_data[row, msnv_dependent[i]] = 0
      }
    }
  }
}

label_data = label_data[label_data$user_id %in% users_in_cumul, ]
for(i in 1:length(user_dependent)) {
  if(is.numeric(label_data[, user_dependent[i]])) { #DISCRETIZE LABELS
    dep_median = median(label_data[, user_dependent[i]])
    for (row in 1:nrow(label_data)) {
      numeric_val = label_data[row, user_dependent[i]]
      if (numeric_val > dep_median) {
        label_data[row, user_dependent[i]] = 1
      } else {
        label_data[row, user_dependent[i]] = 0
      }
      if (user_dependent[i] == "VerbalWM_longest" && numeric_val == dep_median) {
        if (label_data[row, "VerbalWM_word"] > verbal_VM_words_median) {
          label_data[row, user_dependent[i]] = 1
        }
      } else if (user_dependent[i] == "BarChartLit") {
        uid =  label_data[row, "user_id"]
        numeric_val =  user_barchart_scores[uid]
        if (numeric_val > barchart_median) {
          label_data[row, user_dependent[i]] = 1
        } else {
          label_data[row, user_dependent[i]] = 0
        }
      }
    }
  }
}

# Set of users to keep in dataframe
avail_users = unique(label_data[, "user_id"])

for (window in time_windows) {
  if (cumul_across) {
    features_dir = paste(data_dir, "across_tasks_new_refined\\", sep="")
    iter_arr = seq(1, 15, 1)
  } else if (full_window) {
    features_dir = paste(data_dir, "train_data\\", sep="")
    iter_arr = seq(15, 15, 1)
  } else {
    # Cumulative within
    features_dir = paste(data_dir, "cumulative_new_withintask_refined\\", sep="")
    iter_arr = seq(left_idx, right_idx, 1)
  }
  for(slice in iter_arr) {
    if (cumul_across || full_window) {
      features_filename = paste(features_dir, "tasks_included_",slice,".tsv", sep="")
    } else {
      features_filename = paste(features_dir, "pruning_", slice*1000,".tsv", sep="")
    }
    
    features_data = read.table(features_filename, header=T, sep="\t")
    # Add label columns
    features_data[dependent] = -1
    features_data = add_labels_to_features(features_data, avail_users, label_data, labels_msnv_data, user_dependent, msnv_dependent, keep_hard_msnvs)
    
    user_ids = unique(features_data$Uid)
    # For all user characteristics
    for(dep in 1:length(dependent))
    {
    	# Statistics
    	numHigh = nrow(features_data[features_data[,dependent[dep]] == 1,])
    	numLow = nrow(features_data[features_data[,dependent[dep]] == 0,])
    	result = paste(dependent[dep], window, slice, sep=",")

      result = paste(result, numHigh, sep=",")
      result = paste(result, numLow, sep=",")
    	if (numHigh > numLow) {
    	  result = paste(result, "high", sep=",")
    	} else {
    	  result = paste(result, "low", sep=",")
    	}
    	 sink(out, append = TRUE)
    	 print(result)
    	 sink()
    	}
    }#end dependant
}

stopCluster(cl)
