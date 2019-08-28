library(caret)
library(infotheo)
library(FSelector)
library(stringr)

library(doParallel)
cl <- makePSOCKcluster(4)
registerDoParallel(cl)
set.seed(1)

filter_features_sets <- function(data, is_cumul, window_size, slice) {
  aoinames = c("Relevant.bars", "Non.relevant.bars", "Text", "Refs", "labels", "Viz", "legend")
  toremove = c("Sc_id", "blinknum", "blinkdurationmax", "blinktimedistancestd", "blinktimedistancemin", 
               "blinktimedistancemean","blinkdurationmean", "blinktimedistancemax", "blinkrate","blinkdurationmin", 
               "blinkdurationtotal", "blinkdurationstd", "length", "numfixations", "numsegments", "doubleclicrate", 
							"sumabspathangles", "sumfixationduration" ,"sumpathdistance" ,"sumrelpathangles", "sumsaccadedistance", 
							"sumsaccadeduration","numevents", "numleftclic", "leftclicrate", "numrightclic", "numdoubleclic", "numsaccades", 
							"numsamples", "numkeypressed", "rightclicrate", "keypressedrate","timetofirstdoubleclic", "timetofirstkeypressed", "timetofirstleftclic",
							"timetofirstrightclic", "timetolastfixation", "Uid")
							
   for(i in 1:length(aoinames)){
    toremove = c(toremove, paste(aoinames, "blinknum", sep="_"))
    toremove = c(toremove, paste(aoinames, "doubleclicrate", sep="_"))
		toremove = c(toremove, paste(aoinames, "numevents", sep="_"))
		toremove = c(toremove, paste(aoinames, "numfixations", sep="_"))
		toremove = c(toremove, paste(aoinames, "numleftclic", sep="_"))
		toremove = c(toremove, paste(aoinames, "numrightclic", sep="_"))
		toremove = c(toremove, paste(aoinames, "numdoubleclic", sep="_"))
		toremove = c(toremove, paste(aoinames, "rightclicrate", sep="_"))
		toremove = c(toremove, paste(aoinames, "timetofirstdoubleclic", sep="_"))
		toremove = c(toremove, paste(aoinames, "timetofirstrightclic", sep="_"))
		toremove = c(toremove, paste(aoinames, "timetofirstleftclic", sep="_"))
		toremove = c(toremove, paste(aoinames, "timetolastdoubleclic", sep="_"))
		toremove = c(toremove, paste(aoinames, "timetolastfixation", sep="_"))
		toremove = c(toremove, paste(aoinames, "timetolastleftclic", sep="_"))
		toremove = c(toremove, paste(aoinames, "timetolastrightclic", sep="_"))
		toremove = c(toremove, paste(aoinames, "totaltimespent", sep="_"))
		toremove = c(toremove, paste(aoinames, "enddistance", sep="_"))
		toremove = c(toremove, paste(aoinames, "endpupilsize", sep="_"))
		toremove = c(toremove, paste(aoinames, "startdistance", sep="_"))
		toremove = c(toremove, paste(aoinames, "startpupilsize", sep="_"))
	}
  data = data[, !(colnames(data) %in% toremove)]
  distance_features = c("enddistance", "maxdistance", "meandistance", "mindistance", "startdistance")
  for (feature in distance_features) {
    data[data[feature] == -1, feature] = 600
    for (aoi in aoinames) {
      feat_name = paste(aoi, feature, sep="_")
      if (feat_name %in% colnames(data)) {
        data[data[feat_name] == -1, feat_name] = 600
      }
    }
  }
  stddev_other_mean_features = c("stddevabspathangles", "stddevdistance", "stddevfixationduration", "stddevpathdistance", "stddevpupilsize", "stddevrelpathangles", 
                                 "stddevsaccadedistance", "stddevsaccadeduration", "stddevsaccadespeed", "meanabspathangles", "meanfixationduration", "meanpathdistance", 
                                 "meanpupilsize", "meanpupilvelocity", "meanrelpathangles", "meansaccadedistance", "meansaccadeduration", "meansaccadespeed")
  for (feature in stddev_other_mean_features) {
    data[data[feature] == -1, feature] = 0
    for (aoi in aoinames) {
      feat_name = paste(aoi, feature, sep="_")
      if (feat_name %in% colnames(data)) {
        data[data[feat_name] == -1, feat_name] = 0
      }
    }
  }  
  if (is_cumul) {
    for (feature in c("timetofirstfixation")) {
      for (aoi in aoinames) {
        feat_name = paste(aoi, feature, sep="_")
        if (feat_name %in% colnames(data)) {
          data[data[feat_name] == -1, feat_name] = window_size * slice
        }
      }
    }
  }
  return(data)
}

fill_results <- function(model_stats, model, results, iter_idx) {
  model_stats[iter_idx, paste(model, "_Accuracy", sep="")] =  results$overall[1]
  model_stats[iter_idx, paste(model, "_Kappa", sep="")] = results$overall[2]
  model_stats[iter_idx, paste(model, "_AccuracyHigh", sep="")] = results$byClass[[1]]
  model_stats[iter_idx, paste(model, "_AccuracyLow", sep="")] = results$byClass[[2]]
  model_stats[iter_idx, paste(model, "_NumLow", sep="")] = sum(validation_data[, dependent[dep]] == 0)
  model_stats[iter_idx, paste(model, "_NumHigh", sep="")] = sum(validation_data[, dependent[dep]] == 1)
  
  return(model_stats)
}

train_test_model <- function(model_name, tune_params, models_stats, feature_imp_fname, fold) {
  fit <- train(fmla,
               data = train_data,
               method = model_name,
               tuneGrid = tune_params,
               trControl = ctrl,
               na.action=na.pass,
               metric="Accuracy")
  
  sink(feature_imp_fname, append = TRUE)
  print(paste("Slice ",slice, ", most important features for feature ", dependent[dep], ", run ", numRepeat, ", model ", model_name, sep=""))
  print(varImp(fit))
  sink() 
  res = predict(fit, newdata=validation_data)
  res = confusionMatrix(res, validation_data[,dependent[dep]])
  models_stats = fill_results(models_stats, model_name, res,  fold)
  return(models_stats)
}
weighted_mean_sd <- function(model, pref, models_stats) {
  sumn = sum(models_stats[, paste(model, "_", "Num", pref, sep="")])
  weights = models_stats[, paste(model, "_", "Num", pref, sep="")] / sumn
  wmean = models_stats[, paste(model, "_", "Accuracy", pref, sep="")] %*% weights
  M = sum(weights != 0)
  sdw = sqrt(weights %*% ((models_stats[, paste(model, "_", "Accuracy", pref, sep="")] - wmean)^2) / (sum(weights) * (M-1)/M)) 
  result = paste(result, wmean, sep=",")
  result = paste(result, sdw, sep=",")
  return(result)
}
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
metrics = c("Accuracy","Kappa", "AccuracyHigh", "AccuracyLow")
#############################################################
#==========================================
feat_set = "all"
feat_selection = TRUE
K = 10
num_repeats = 10


# NOTE: if all false, will run cumulative within window
disjoint_window = F
cumul_across = F
full_window = T
keep_hard_msnvs = T

if (disjoint_window) {
  time_windows = c(2000, 5000, 10000)
  window_id = 1
  out = paste(dirout, "predict-taskchar_result_disjoint_window_", time_windows[window_id],".csv", sep="")
  feature_imp_fname = paste(dirout, "disjoint_window.txt",sep="")
} else if (cumul_across) {
  time_windows = c(1000)
  out = paste(dirout, "predict-taskchar_result_accross.csv", sep="")
  feature_imp_fname = paste(dirout, "featimp_cumul_accross.txt",sep="")
} else if (full_window) {
  time_windows = c(1000)
  out = paste(dirout, "predict-taskchar_result_fullwindow.csv", sep="")
  feature_imp_fname = paste(dirout, "featimp_fullwindow.txt",sep="")
} else {
  time_windows = c(1000)
  seqs = c(1, 9, 10, 19, 20, 29, 30, 39, 40, 49, 50, 58)
  machine_id = 1
  left_idx = seqs[2*(machine_id-1)+1]
  right_idx = seqs[2*(machine_id-1)+2]
  feature_imp_fname = paste(dirout, "featimp_cumul_within_",machine_id,".txt",sep="")
  out = paste(dirout, "predict-taskchar_result_cumulative_",left_idx,"_",right_idx,".csv",sep="")
}

sink(out)
header = "Userchar,WindowSize,Slice,Model,Repeat"
for (metric in metrics) {
  header = paste(header, ",", metric, "_", "mean", sep="")
  header = paste(header, ",", metric, "_", "sd", sep="")
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

dependent = c(user_dependent, msnv_dependent)

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
    features_dir = "C:\\Users\\tracker2-admin\\Desktop\\rcode\\train_data\\across_tasks_new_refined\\"
    iter_arr = seq(1, 15, 1)
  } else if (full_window) {
    features_dir = "C:\\Users\\tracker2-admin\\Desktop\\rcode\\train_data\\across_tasks_new_refined\\"
    iter_arr = seq(15, 15, 1)
    features_dir = "C:\\Users\\tracker2-admin\\Desktop\\rcode\\train_data\\"
  } else {
    # Cumulative within
    features_dir = "C:\\Users\\tracker2-admin\\Desktop\\rcode\\train_data\\cumulative_new_withintask_refined\\"
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
    	# Keep only 1 dependent
    	dependent2 = dependent[dependent != dependent[dep]]
    	features_data2 = features_data[, !(colnames(features_data) %in% dependent2)]
      models_stats = data.frame()
    	# Statistics
    	table_dependent = table(features_data[dependent[dep]])
    	maj_value = names(table_dependent[table_dependent == max(table_dependent)])
    	maj_acc = nrow(features_data[features_data[,dependent[dep]] == maj_value,]) / nrow(features_data)
    	# Repeat fold splits muktiple times to avoid lucky splits
    	for (numRepeat in 1:num_repeats) {
    	  user_ids = user_ids[sample(length(user_ids))]
    	  folds = cut(seq(1, length(user_ids)), breaks=K, labels=F)
      	# Go over each fold
      	for(fold in 1:K) {	
      	  print(fold)
      	  models_stats[fold, "MajorityClass_Accuracy"] = maj_acc
      	  # Extract train and validation data
      	  testIndexes <- which(folds==fold, arr.ind=TRUE)
      	  train_data = filter_features_sets(features_data2[!(features_data2$Uid %in% user_ids[testIndexes]),], !cumul_across, window, slice)
      	  train_data[,dependent[dep]] = factor(train_data[,dependent[dep]], levels = c(1, 0))
      	  validation_data = filter_features_sets(features_data2[(features_data2$Uid %in% user_ids[testIndexes]),], !cumul_across, window, slice)
      	  validation_data[,dependent[dep]] = factor(validation_data[,dependent[dep]], levels = c(1, 0))
      	  # Feature selection
    			if(feat_selection) {
    			  removeZeros = apply(train_data, 2,
    			                      function(x) length(unique(x)) == 1) # remove zeros
    			  # Don't remove dependent even if only 1 value is available.
    			  removeZeros[[length(removeZeros)]] = FALSE
      		  train_data = train_data[, !removeZeros]
      		  validation_data = validation_data[, !removeZeros]
    				data_cor = cor(train_data[, colnames(train_data) != dependent[dep]])
    			  data_cor[is.na(data_cor)] = 1
    				
    			  corfeat = findCorrelation(data_cor, cutoff = .85)
      				
      		  if(length(corfeat) > 0) {
      		    train_data =  train_data[,-corfeat]
      		    validation_data = validation_data[,-corfeat]
    			  }
      			#fmla <- as.formula(paste("train_data$", dependent[dep],"~.", sep=""))
    			  fmla <- as.formula(paste0(dependent[dep],"~.", sep=""))
      		} else {
      			fmla <- as.formula(paste0(dependent[dep],"~", paste0(colnames(train_data[, colnames(train_data) != dependent[dep]]), collapse= "+", sep=""), sep=""))
      		}
      		#==========================================
      		ctrl <- trainControl(method = "none", allowParallel=T)
      		xgbTreeGrid <- expand.grid(
      		  nrounds = 100, max_depth = 6,
      		  eta = 0.3, gamma = 0,
      		  colsample_bytree = 1,
      		  min_child_weight = 1,
      		  subsample = 1
      		)
      		models_stats = train_test_model("xgbTree", xgbTreeGrid, models_stats, feature_imp_fname, fold) 
      		  
      		#Logistic regression with L1-regularization
      		regLogistictGrid <-  expand.grid(cost = c(1), loss=c("L1"), epsilon=c(0.01))
      		models_stats = train_test_model("regLogistic", regLogistictGrid, models_stats, feature_imp_fname, fold) 
      		
      		#SVMLinear
      		svmGrid <-  expand.grid(C = c(1))
      		models_stats = train_test_model("svmLinear", svmGrid, models_stats, feature_imp_fname, fold) 
    
      		#Random Forest
    			tuneparam	 <-  expand.grid(mtry=c(16))
    			models_stats = train_test_model("rf", tuneparam, models_stats, feature_imp_fname, fold) 
    			
      	}  #end CrossValidation run
    	  models_stats[is.na(models_stats)] = 0
    	  for (model in models) {
    	    result = paste(dependent[dep], window, slice, model, numRepeat, sep=",")
    	    for (metric in metrics) {
    	      if (model != "MajorityClass" && metric == "AccuracyHigh") {
    	        result = weighted_mean_sd(model, "High", models_stats)
    	      }
    	      else if (model != "MajorityClass" && metric == "AccuracyLow") {
    	        result = weighted_mean_sd(model, "Low", models_stats)
    	      }
    	      else if (!(model == "MajorityClass" && (metric == "Kappa" || metric == "AccuracyHigh" || metric == "AccuracyLow"))) {
    	        model_res = models_stats[, paste(model, "_", metric, sep="")]
    	        result = paste(result, mean(model_res), sep=",")
    	        result = paste(result, sd(model_res), sep=",")
    	      } else{
    	        result = paste(result, 0, sep=",")
    	        result = paste(result, 0, sep=",")
    	      }
    	    }
    	    sink(out, append = TRUE)
    	    print(result)
    	    sink()
    	  }
    	} #end CrossValidation all repeats
    }#end dependant
  }
}

stopCluster(cl)
