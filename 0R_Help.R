


# reshape from wide to long
DDI_reshape = function(df, label){
  
  df = df %>%
    select(-starts_with('Avg')) %>%
    setNames(gsub('X|\\.1', '', names(.)))
  tot_year = unique(colnames(df)[-1])
  colnames(df) = paste0(colnames(df), '.', df[1,])
  df = df %>%
    setNames(gsub('COUNTRY.INDEX', 'country', names(.))) %>%
    filter(row_number() != 1) %>%
    reshape(direction='long', 
            varying=colnames(df)[-1], 
            timevar='year',
            times=tot_year,
            v.names=c('I_1', 'I_2'),
            idvar='country') %>%
    `rownames<-`(NULL) %>%
    mutate_all(function(x) { attributes(x) <- NULL; x }) %>%
    mutate(year = as.numeric(year),
           I_1 = as.numeric(I_1),
           I_2 = as.numeric(I_2)) %>%
    setNames(gsub('I_', paste0(label, '_'), names(.)))
  
  return(df)
}

# make summary of dataset
summary_table = function(df){
  df_summary = df %>%
    mutate(count_na = apply(., 1, function(x) sum(is.na(x)))) %>%
    group_by(country) %>%
    summarise(Total_years = uniqueN(year),
              # Min_year = min(year),
              # Max_year = max(year),
              Total_firms = n(),
              Total_observations = Total_firms * (ncol(df) - 2),  # don't take into account country and year
              Total_missing = sum(count_na)) %>%
    mutate(Total_missing_perc = round(Total_missing / Total_observations * 100, 1)) %>%
    ungroup() %>%
    left_join(df %>%
                group_by(country, year) %>%
                summarise(Total_firms = n()) %>%
                ungroup() %>%
                mutate(year_count = paste0(year, ' (', Total_firms, ')')) %>%
                group_by(country) %>%
                arrange(year) %>%
                summarise(Firm_by_year = paste0(year_count, collapse = '-')), by = "country") %>%
    select(country, Total_years, Total_firms, Firm_by_year, everything())
  
  if (sum(df_summary$Total_firms) != nrow(df)){cat('\n ########## error in summary_table: Total_firms')}
  if (sum(df_summary$Total_missing) != sum(is.na(df))){cat('\n ########## error in summary_table: Total_missing')}
  
  return(df_summary)
}

# Statistical analysis: numerical, data and character columns
basicStatistics = function(data){
  
  data=as.data.frame(data, stringsAsFactors = F) %>%
    mutate_all(function(x) { attributes(x) <- NULL; x })
  
  # Get numerical columns
  nums <- names(which(sapply(data, is.numeric)))
  if (length(nums)>0){
    StatNum = c()
    for (col in nums){
      dd = data[, col]
      perc = quantile(dd, c(0.01, 0.05, 0.95, 0.99), na.rm = T)
      StatNum = StatNum %>% bind_rows(
        data.frame(UNIQUE_VALS = uniqueN(dd, na.rm = T),
                   NAs = sum(is.na(dd)),
                   Min = min(dd, na.rm = T),
                   Max = max(dd, na.rm = T),
                   Mean = mean(dd, na.rm = T),
                   StDev = sd(dd, na.rm = T),
                   Median = median(dd, na.rm = T),
                   Perc_1 = perc[1],
                   Perc_5 = perc[2],
                   Perc_95 = perc[3],
                   Perc_99 = perc[4],
                   Sum = sum(dd, na.rm = T), stringsAsFactors = F)
      )
    }
    StatNum = StatNum %>%
      mutate(NAs = paste0(NAs,' (',signif(NAs/nrow(data)*100,digits=2),'%)')) %>%
      mutate_all(as.character) %>%
      `rownames<-`(nums)
  } else {StatNum=data.frame()}
  
  # Get dates columns
  dates <- names(which(sapply(data, lubridate::is.Date)))
  if (length(dates)>0){
    StatDat = c()
    for (col in dates){
      dd = data[, col]
      StatDat = StatDat %>% bind_rows(
        data.frame(UNIQUE_VALS = uniqueN(dd, na.rm = T),
                   NAs = sum(is.na(dd)),
                   Min = min(dd, na.rm = T),
                   Max = max(dd, na.rm = T),
                   Median = median(dd, na.rm = T), stringsAsFactors = F) %>%
          bind_cols(
            data.frame(val = sort(dd), stringsAsFactors = F) %>%
              filter(!is.na(val)) %>%
              mutate(POS = 1:n()) %>%
              filter(POS %in% round(n() * c(0.01, 0.05, 0.95, 0.99))) %>%
              mutate(POS = paste0('Perc_', c(1, 5, 95, 99))) %>%
              mutate(group = 1) %>%
              spread(POS, val) %>%
              select(-group)
          )
      )
    }
    StatDat = StatDat %>%
      mutate(NAs = paste0(NAs,' (',signif(NAs/nrow(data)*100,digits=2),'%)')) %>%
      mutate_all(as.character)
    rownames(StatDat) = dates
  } else {StatDat=data.frame()}
  
  # Get characters columns
  chars <- names(which(sapply(data, is.character)))
  if (length(chars)>0){
    StatChar = c()
    for (i in chars){
      cc=data[,i];cc=cc[!is.na(cc)]
      if (length(unique(cc)) <= 50){line = paste(unique(cc), collapse = "|")
      } else {line = paste("> 50 unique values",paste(unique(cc)[1:20], collapse = "|"))}
      StatChar = rbind(StatChar,c(paste0(nrow(data)-length(cc),' (',signif((nrow(data)-length(cc))/nrow(data)*100,digits=2),'%)'),
                                  paste0(sum(cc == ''),' (',signif(sum(cc == '')/nrow(data)*100,digits=2),'%)'),
                                  length(unique(cc)),line))
    }
    if (length(StatChar)>0){StatChar = data.frame(StatChar, stringsAsFactors = F) %>% setNames(c('NAs', 'BLANKs', 'UNIQUE_VALS','VALUES'));rownames(StatChar)=chars}
  } else {StatChar=data.frame()}
  
  Stat=bind_rows(StatNum,StatDat,StatChar)
  
  Stat=cbind(VARIABLE=c(rownames(StatNum),rownames(StatDat),rownames(StatChar)),
             TYPE=c(rep('NUMERIC',length(nums)),rep('DATE',length(dates)),rep('CHARACTER',length(chars))),
             NUM_OSS=nrow(data),Stat)
  col_first = intersect(c('VARIABLE', 'TYPE', 'NUM_OSS', 'UNIQUE_VALS', 'NAs', 'BLANKs'), colnames(Stat))
  Stat = Stat %>% select(all_of(col_first), everything())
  
  final=data.frame(VARIABLE=colnames(data), stringsAsFactors = F)
  final = final %>% left_join(Stat, by = "VARIABLE")
  final = as.matrix(final)
  final[is.na(final)]=''
  final = as.data.frame(final, stringsAsFactors = F)
  
  return(final)
}

# remove new line in STATA output
remove_newline = function(input_lines){
  # input_lines: array of strings. String that starts with "> " is merged with the previous (works also for consecutive lines with "> ")
  lines_block = list()
  for (line in input_lines){
    if (substr(line, 1, 2) == "> "){
      lines_block[[length(lines_block)]] = c(lines_block[[length(lines_block)]], line)
      # line_to_be_appended = substr(line, 3, nchar(line))
      # merged_line = paste0(input_lines[i-1], line_to_be_appended)
      # output[length(output)] = merged_line
    } else {
      lines_block[[length(lines_block)+1]] = line
    }
  }
  if (lapply(lines_block, length) %>% unlist() %>% sum() != length(input_lines)){cat('\n ####### missing lines in remove_newline()\n\n')}
  output = c()
  for (block in lines_block){
    if (length(block) > 1){
      merged_line = block[1]
      for (i in 2:length(block)){
        merged_line = paste0(merged_line, substr(block[i], 3, nchar(block[i])))
      }
      output = c(output, merged_line)
    } else {
      output = c(output, block)
    }
  }
  
  return(output)
}

# convert STATA output to R data.frame
STATA_to_dataframe = function(input_lines, lines_to_remove, temp_txt_path, sep = ""){
  # input_lines: array of strings, output of stata command
  # lines_to_remove: lines to be discarded
  # temp_txt_path: path to temp .txt output
  # sep: separator to be used when reading temp_txt_path with read.csv
  
  fileConn<-file(temp_txt_path)
  writeLines(input_lines[-lines_to_remove], fileConn)
  close(fileConn)
  
  output <- read.csv(temp_txt_path, sep=sep, stringsAsFactors=FALSE)
  
  file.remove(temp_txt_path)
  
  return(output)
}

# get coefficient from STATA oprobit
get_coeff_from_STATA = function(input_lines, line_name = "", baseline = T, rename_add_var = ""){
  # extract number of observations, coefficients and stars for oprobit command.
  #
  # line_name: name of single line output. E.g. "baseline" or "var_x"
  # baseline: if TRUE add extra variable named "ADD_VAR" with coeff = 0 and 0 stars. Used if comparing different regression with baseline
  # rename_add_var: if != "" rename specified variable with "ADD_VAR"
  #
  # return: - coeff_list: data.frame with Variable, Coeff, Coeff_num , Coeff_star
  #         - coeff_line: single row data.frame of coeff_list in single line with num_obs added
  
  bar_list = which(sapply(input_lines, function(x) grepl("---", x), USE.NAMES = F))
  num_obs = strsplit(input_lines[bar_list[length(bar_list) - 1]+1], "\\s+")[[1]][2] %>% as.numeric
  coeff_lines = input_lines[(bar_list[2] + 2):(bar_list[3] - 1)]
  coeff_lines = coeff_lines[which(!substr(coeff_lines, 1, 1) %in% c("", " "))]   # needed if you use "se" instead of "beta not" in the command
  coeff_list = c()
  for (line in coeff_lines){
    split_val = strsplit(line, "\\s+")[[1]]
    star_count = str_count(split_val[2], "\\*")
    coeff = gsub("\\*", "", split_val[2]) %>% as.numeric
    if (split_val[1] == rename_add_var){
      split_val[1] = "ADD_VAR"
    }
    coeff_list = coeff_list %>%
      bind_rows(data.frame(Variable = split_val[1], Coeff = split_val[2], Coeff_num = coeff, Coeff_star = star_count, stringsAsFactors = F))
  }
  coeff_list_t = coeff_list
  if (baseline){
    coeff_list_t = coeff_list_t %>%
      bind_rows(data.frame(Variable = "ADD_VAR", Coeff = "", Coeff_num = 0, Coeff_star = 0, stringsAsFactors = F))
  }
  
  # reshape into single row
  coeff_line = c()
  for (col in 2:ncol(coeff_list_t)){
    coeff_line = coeff_line %>%
      bind_cols(data.frame(t(coeff_list_t[, col]), stringsAsFactors = F) %>%
                  setNames(paste0(coeff_list_t$Variable, "_", colnames(coeff_list_t)[col])))
    
  }
  coeff_line = data.frame(Case = line_name, Num_obs = num_obs, stringsAsFactors = F) %>%
    bind_cols(coeff_line)
  
  return(list(coeff_list = coeff_list,
              coeff_line = coeff_line))
}

# find best candidate according to same coeff sign and significance
find_best_candidate = function(df, df_final_raw){
  # df: one of results_fin_acc or results_ifs
  # df_final_raw: used for variable description (as attribute)
  
  baseline = df %>% filter(Case == 'baseline')
  coeff_baseline = baseline %>% select(ends_with("_Coeff_num"))
  star_ind_baseline =  which(baseline %>% select(ends_with("_Coeff_star")) > 0)
  star_baseline = baseline %>% select(ends_with("_Coeff_star"), -starts_with("ADD_VAR"))
  
  df = df %>%
    filter(ADD_VAR_Coeff_star > 0) %>%
    mutate(Same_sign_Perc = 0,
           Signif_coeff_Perc = 0,
           Star_diff = 0,
           Description = "")
  for (i in 1:nrow(df)){
    
    coeff = df[i,] %>%
      select(ends_with("_Coeff_num"))
    df$Same_sign_Perc[i] = round(sum(coeff * coeff_baseline >= 0) / length(coeff) * 100, 2)
    
    star_ind = which(df[i,] %>% select(ends_with("_Coeff_star")) > 0)
    df$Signif_coeff_Perc[i] = round(length(intersect(star_ind_baseline, star_ind)) / length(star_ind_baseline) * 100, 2)
    
    star = df[i,] %>% select(ends_with("_Coeff_star"), -starts_with("ADD_VAR"))
    star_diff = star_baseline - star
    df$Star_diff[i] = -sum(star_diff[star_diff > 0])
    
    desc = attributes(df_final_raw %>% pull(df$Case[i]))$label
    df$Description[i] = ifelse(is.null(desc), "", desc)
  }
  
  df = baseline %>%
    mutate(Same_sign_Perc = 999,
           Signif_coeff_Perc = 999,
           Star_diff = 999,
           Description = "") %>%
    bind_rows(df) %>%
    select(Case, Description, Num_obs, Same_sign_Perc, Signif_coeff_Perc, Star_diff, everything()) %>%
    arrange(desc(Same_sign_Perc), desc(Signif_coeff_Perc), desc(Star_diff))
  
  return(df)
}

# add correlation groups to output of find_best_candidate()
add_correlation_group = function(df, results_best, ref_var, max_abs_corr_threshold = 0.35){
  # df: df used to fit the regression
  # results_best: output of find_best_candidate()
  # ref_var: variables in results_best to be compared with ADD_VAR evaluating the max abs correlation (check for multi-collinearity)
  # max_abs_corr_threshold: a variable is set in a different cluster if it has max abs correlation with other
  #                         variables in suggested cluster less than threshold
  
  # check correlation with ref_var (other regressors)
  corr_pair = c()
  for (var in setdiff(results_best$Case, 'baseline')){
    corr_list = c()
    for (ref in ref_var){
      corr_list = c(corr_list, cor(df %>% pull(var), df %>% pull(ref), use = "pairwise.complete.obs"))
    }
    corr_pair = corr_pair %>%
      bind_rows(data.frame(Case = var, Max_Abs_Corr_with_Regressors = max(abs(corr_list), na.rm = T), stringsAsFactors = F))
  }
  
  # check variables with 0 st.dev (when paired with other variables) - to be excluded in correlation clustering
  corr_to_check = suppressWarnings(cor(df %>% select(all_of(setdiff(results_best$Case, 'baseline'))), use = "pairwise.complete.obs"))
  corr_to_cluster = corr_to_check
  corr_to_check[upper.tri(corr_to_check, diag = T)] = 0
  
  null_corr_pairs = corr_to_check %>%
    as.data.frame() %>%
    rownames_to_column('var1') %>%
    gather('Var2', 'val', -var1) %>%
    filter(is.na(val)) %>%
    select(-val)
  null_std_var = c()
  if (nrow(null_corr_pairs) > 0){
    for (i in 1:nrow(null_corr_pairs)){
      val = df %>%
        select(all_of(null_corr_pairs[i,] %>% unlist())) %>%
        drop_na() %>%
        summarise_all(sd) %>%
        unlist()
      null_std_var = c(null_std_var, null_corr_pairs[i,][which(val == 0)] %>% unlist())
    }
  }
  null_std_var = unique(null_std_var)
  
  # clusterize variables by correlation
  var_to_cluster = setdiff(colnames(corr_to_check), null_std_var)
  corr_to_cluster = corr_to_cluster[var_to_cluster, var_to_cluster]
  
  kor <- 1 - abs(corr_to_cluster)   # taken from klaR::corclust
  distances <- NULL
  for (i in 1:(ncol(kor) - 1)) distances <- c(distances, 
                                              kor[(i + 1):nrow(kor), i])
  attr(distances, "Labels") <- colnames(kor)
  attr(distances, "Size") <- ncol(kor)
  attr(distances, "Metric") <- "absolute correlations"
  class(distances) <- "dissimilarity"
  agglomer_coeff = c()
  for (met in c( "average", "single", "complete", "ward")){     # find best method for clustering
    ac = cluster::agnes(distances, diss = T, method = met)$ac
    names(ac) = met
    agglomer_coeff = c(agglomer_coeff, ac)
  }
  best_method = which.max(agglomer_coeff) %>% names()
  best_method = ifelse(best_method == 'ward', 'ward.D', best_method)
  clustering <- hclust(distances, method = best_method)
  cluster_labels = dynamicTreeCut::cutreeHybrid(clustering, kor, verbose = 0)$labels    # find best number of cluster
  names(cluster_labels) = colnames(kor)
  
  # create final report
  cluster_report = c()
  cluster_var_list = list()
  cluster_low_corr = c()
  for (cluster_lab in 1:max(cluster_labels)){
    
    cluster_vars = names(cluster_labels[cluster_labels == cluster_lab])
    cluster_corr = corr_to_cluster[cluster_vars, cluster_vars]
    if (!is.null(nrow(cluster_corr))){   # at lest 2 variables
      cluster_corr[upper.tri(cluster_corr, diag = T)] = NA
      cluster_corr_by_var = c()
      for (i in 1:ncol(cluster_corr)){
        val = c(cluster_corr[i, ], cluster_corr[, i])
        cluster_corr_by_var = cluster_corr_by_var %>%
          bind_rows(data.frame(variable = colnames(cluster_corr)[i], Max_Abs_Corr = max(abs(val), na.rm = T), stringsAsFactors = F))
      }
      low_corr_vars = cluster_corr_by_var %>%
        filter(Max_Abs_Corr < max_abs_corr_threshold) %>%
        pull(variable)
      if (length(low_corr_vars) > 0){
        cluster_low_corr = c(cluster_low_corr, low_corr_vars)
        cluster_corr_by_var = cluster_corr_by_var %>%
          filter(! variable %in% low_corr_vars)
      }
      cluster_report = cluster_report %>%
        bind_rows(data.frame(Cluster = as.character(cluster_lab), Mean_Abs_corr = mean(cluster_corr_by_var$Max_Abs_Corr),
                             Min_Abs_Corr = min(cluster_corr_by_var$Max_Abs_Corr),
                             Max_Abs_Corr = max(cluster_corr_by_var$Max_Abs_Corr), Tot_Obs = nrow(cluster_corr_by_var), stringsAsFactors = F))
      cluster_var_list[[paste0('Cluster_', cluster_lab)]] = cluster_corr_by_var$variable
      # cat('\nCluster', cluster_lab, ':', range(cluster_corr_by_var$Max_Abs_Corr))
    } else {
      cluster_report = cluster_report %>%
        bind_rows(data.frame(Cluster = as.character(cluster_lab), Mean_Abs_corr = 1, Min_Abs_Corr = 1, Max_Abs_Corr = 1, Tot_Obs = 1, stringsAsFactors = F))
      cluster_var_list[[paste0('Cluster_', cluster_lab)]] = cluster_vars
      # cat('\nCluster', cluster_lab, ': singleton')
    }
  }
  # set new cluster for low correlated variables
  if (length(cluster_low_corr) > 0){
    cluster_lab = max(cluster_labels) + 1
    cluster_corr = corr_to_cluster[cluster_low_corr, cluster_low_corr]
    if (!is.null(nrow(cluster_corr))){
      cluster_corr[upper.tri(cluster_corr, diag = T)] = NA
      cluster_corr_by_var = c()
      for (i in 1:ncol(cluster_corr)){
        val = c(cluster_corr[i, ], cluster_corr[, i])
        cluster_corr_by_var = cluster_corr_by_var %>%
          bind_rows(data.frame(variable = colnames(cluster_corr)[i], Max_Abs_Corr = max(abs(val), na.rm = T), stringsAsFactors = F))
      }
      cluster_report = cluster_report %>%
        bind_rows(data.frame(Cluster = as.character(cluster_lab), Mean_Abs_corr = mean(cluster_corr_by_var$Max_Abs_Corr),
                             Min_Abs_Corr = min(cluster_corr_by_var$Max_Abs_Corr),
                             Max_Abs_Corr = max(cluster_corr_by_var$Max_Abs_Corr), Tot_Obs = nrow(cluster_corr_by_var), stringsAsFactors = F))
      cluster_var_list[[paste0('Cluster_', cluster_lab)]] = cluster_corr_by_var$variable
      # cat('\nCluster', cluster_lab, ':', range(cluster_corr_by_var$Max_Abs_Corr))
    } else {
      cluster_report = cluster_report %>%
        bind_rows(data.frame(Cluster = as.character(cluster_lab), Mean_Abs_corr = 1, Min_Abs_Corr = 1, Max_Abs_Corr = 1, Tot_Obs = 1, stringsAsFactors = F))
      cluster_var_list[[paste0('Cluster_', cluster_lab)]] = cluster_low_corr
      # cat('\nCluster', cluster_lab, ': singleton')
    }
  }
  # append variables with null st.dev. as additional cluster
  if (length(null_std_var) > 0){
    cluster_report = cluster_report %>%
      bind_rows(data.frame(Cluster = 'NullSTD', Mean_Abs_corr = NA, Min_Abs_Corr = NA, Max_Abs_Corr = NA, Tot_Obs = length(null_std_var), stringsAsFactors = F))
    cluster_var_list[['Cluster_NullSTD']] = null_std_var
  }
  if (sum(cluster_report$Tot_Obs) != nrow(results_best) - 1){cat('\n\n######### missing observations in cluster_report')}
  
  # join correlation with regressors and correlation groups with results_best
  results_best = results_best %>%
    left_join(corr_pair, by = "Case") %>%
    mutate(Max_Abs_Corr_with_Regressors = ifelse(is.na(Max_Abs_Corr_with_Regressors), 0, Max_Abs_Corr_with_Regressors)) %>%
    mutate(Correlation_Group = "") %>%
    select(Case, Description, Max_Abs_Corr_with_Regressors, Correlation_Group, everything())
  for (i in 1:nrow(cluster_report)){
    gr_var = cluster_var_list[[paste0('Cluster_', cluster_report$Cluster[i])]]
    if (cluster_report$Tot_Obs[i] == 1 | cluster_report$Cluster[i] == 'NullSTD'){
      lab = paste0('Cl_', cluster_report$Cluster[i])        
    } else {
      lab = paste0('Cl_', cluster_report$Cluster[i], ' AbsCorr=[', round(cluster_report$Min_Abs_Corr[i] * 100),
                   ',', round(cluster_report$Max_Abs_Corr[i] * 100), ']')
    }
    results_best = results_best %>%
      mutate(Correlation_Group = ifelse(Case %in% gr_var, lab, Correlation_Group))
  }
  results_best = results_best %>%
    arrange(Correlation_Group, desc(Same_sign_Perc), desc(Signif_coeff_Perc), desc(Star_diff))
  
  return(list(results_best = results_best,
              cluster_report = cluster_report,
              cluster_var_list = cluster_var_list))
}

# evaluate correlation and partial correlation
evaluate_correlation = function(df, corr_method = 'pearson'){
  # df: data.frame of variables
  
  corr = rcorr(df %>% as.matrix(), type = corr_method)
  corr_v = corr$r
  corr_v[lower.tri(corr_v, diag = T)] = NA
  corr_v = reshape2::melt(corr_v) %>% filter(!is.na(value)) %>% setNames(c('Var1', 'Var2', 'Corr')) %>% mutate_if(is.factor, as.character)
  corr_p = round(corr$P, 8)
  corr_p[lower.tri(corr_p, diag = T)] = NA
  corr_p = reshape2::melt(corr_p) %>% filter(!is.na(value)) %>% setNames(c('Var1', 'Var2', 'Corr_pVal')) %>% mutate_if(is.factor, as.character)
  
  correlation_list = corr_v %>%
    left_join(corr_p, by = c("Var1", "Var2")) %>%
    mutate(abs = abs(Corr)) %>%
    arrange(desc(abs))
  
  return(correlation_list)
}

# convert coeff_lines "table" into a data.frame (correctly matches columns with missing coeff/st.err)
coeff_lines_to_data_frame = function(coeff_lines, coeff_names){
  
  # find index of columns
  coeff_col_ind = other_col_ind = c()    # contains index of first blank element before coefficient or st.err
  for (line in coeff_lines){
    non_blank_elements = strsplit(line, "\\s+")[[1]]
    non_blank_elements = non_blank_elements[non_blank_elements != ""]
    cf_line = ifelse(sum(!is.na(match(coeff_names, strsplit(line, "\\s+")[[1]]))) > 0, T, F)  # check if coeff line or st.err line
    ind = c()
    while (length(non_blank_elements) > 0){
      nb = non_blank_elements[1]
      if (nb %in% coeff_names){
        line = gsub(nb, paste0(rep(" ", nchar(nb)), collapse = ""), line)   # replace coeff name with blanks
      } else {
        nb_len = nchar(nb)
        nb = gsub("\\*", "\\\\*", nb)
        nb = gsub("\\(", "\\\\(", nb)
        # nb = gsub("\\.", "\\\\.", nb)
        nb_ind = gregexpr(nb, line)[[1]][1]
        if (nb_ind == 1){cat('\n##### unexpected coeff_names found:', nb)}
        ind = c(ind, nb_ind - 1)
        substr(line, nb_ind, nb_ind + nb_len) <- paste0(rep(" ", nchar(nb)), collapse = "")
      }
      non_blank_elements = non_blank_elements[-1]
    }
    if (cf_line){coeff_col_ind = c(coeff_col_ind, ind)} else {other_col_ind = c(other_col_ind, ind)}
    if (-2 %in% ind){print(line)}
  }
  coeff_col_ind = coeff_col_ind %>% unique() %>% sort()
  coeff_col_ind = coeff_col_ind[c(1, which(diff(coeff_col_ind) > 3) + 1)]
  other_col_ind = other_col_ind %>% unique() %>% sort()
  other_col_ind = other_col_ind[c(1, which(diff(other_col_ind) > 3) + 1)]
  
  if (length(coeff_col_ind) != length(other_col_ind)){
    cat('\n######## found different number of columns in coeff_lines')
  } else {
    expected_columns = length(coeff_col_ind) + 1
    final_ind = c()
    for (i in 1:length(coeff_col_ind)){
      final_ind = c(final_ind, min(c(coeff_col_ind[i], other_col_ind[i])))
    }
  }
  
  output_df = c()
  for (line in coeff_lines){
    # split columns
    split_val = substring(line, c(0, final_ind), c(final_ind, nchar(line)))  
    # remove leading/trailing blanks
    split_val = strsplit(split_val, "\\s+")
    final_vals = c()
    for (ss in split_val){   
      ss = ss[ss != ""]
      if (length(ss) == 0){ss = ""}
      final_vals = c(final_vals, ss)
    }
    if (length(final_vals) != expected_columns){cat('\n##### found line with wrong number of colums')}
    output_df = output_df %>%
      bind_rows(data.frame(t(final_vals), stringsAsFactors = F))
  }
  
  return(output_df)
}

# sort loading according to one single variable for DFM
sort_loading_DFM = function(res_DFM_loadings, leading_var, leading_sign){
  res_DFM_loadings = res_DFM_loadings %>%
    mutate(LEAD_VAR_FLAG = ifelse(variable == leading_var, 1, 0)) %>%
    group_by(data, method, DFM, Total_Factors, Factor, country) %>%
    arrange(desc(LEAD_VAR_FLAG)) %>%
    mutate(LEAD_SIGN = ifelse(sign(loading) == 0, 1, sign(loading)) * LEAD_VAR_FLAG,
           SIGN_EXP = ifelse(leading_sign == 'p', 1, -1),
           SIGN_MULT = ifelse(SIGN_EXP == LEAD_SIGN[LEAD_VAR_FLAG == 1], 1, -1),
           loading = loading * SIGN_MULT) %>%
    select(-LEAD_VAR_FLAG, -LEAD_SIGN, -SIGN_EXP, -SIGN_MULT) %>%
    ungroup()
  
  return(res_DFM_loadings)
}

# scale range in desired range
scale_range = function(x, a, b, xmin = NULL, xmax = NULL, mode = 'linear', s = NULL){
  
  # Scale input interval into new range
  # - a, b: new interval range
  # - xmin, xmax: provided if scaling has to be performed from a different input range [min(x), max(x)]
  # - mode: 'linear' for linear scaling, 'exponential' for exponential scaling
  # - s: if mode == 'exponential' s is used for decay in exponential kernel.
  # The higher s the more spiked the decay (leptokurtic)
  
  if (is.null(xmin)){xmin = min(x)}
  if (is.null(xmax)){xmax = max(x)}
  
  if (mode == "linear"){
    # https://stats.stackexchange.com/questions/281162/scale-a-number-between-a-range
    out = (b - a) * (x - xmin) / (xmax - xmin) + a
  }
  
  if (mode == "exponential"){
    if (is.null(s)){s = 5}
    # https://stackoverflow.com/questions/49184033/converting-a-range-of-integers-exponentially-to-another-range
    r = (x - xmin) / (xmax - xmin)
    C = s ^ (xmax - xmin)
    out = ((b - a) * C ^ r + a * C - b) / (C - 1)
  }
  
  return(out)
}

# read multiple sheets from xls
multiplesheets <- function(fname) {
  
  # getting info about all excel sheets
  sheets <- readxl::excel_sheets(fname)
  tibble <- suppressMessages(lapply(sheets, function(x) readxl::read_excel(fname, sheet = x)))
  data_frame <- lapply(tibble, as.data.frame)
  
  # assigning names to data frames
  names(data_frame) <- sheets
  
  return(data_frame)
}

# Standard cleaning procedure
nameCleaning = function(df){
  
  # Clean unwanted chars
  # names(df) = gsubfn(paste(names(unwanted_array),collapse='|'), unwanted_array, names(df))
  
  # Custom cleaning
  names(df) = gsub("[\\. \\(\\)\\/]+", "_", names(df))
  names(df) = gsub("-", "_", names(df))
  names(df) = gsub("–", "_", names(df)) # bigger
  names(df) = gsub("'", "", names(df))
  names(df) = gsub(",", "_", names(df))
  names(df) = gsub(":", "_", names(df))
  names(df) = gsub("<", "MIN", names(df))
  names(df) = gsub(">", "MAG", names(df))
  names(df) = gsub("&", "E", names(df))
  names(df) = gsub("°", "", names(df))
  names(df) = gsub("=", "", names(df))
  names(df) = gsub(";", "", names(df))
  names(df) = gsub("\\*", "", names(df))
  names(df) = gsub("’", "", names(df))
  names(df) = gsub("%", "PERC", names(df))
  names(df) = gsub("\\+", "_", names(df))
  names(df) = gsub("\\$","DOLL", names(df))
  
  
  # To upper
  # names(df) = toupper(names(df))
  
  # Trim
  names(df) = trimws(names(df))
  
  # Cut recurring underscore
  names(df) = gsub("_+", "_", names(df))
  names(df) = gsub('^_|_$', '', names(df))
  
  return(df)
}

# evaluate Mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}