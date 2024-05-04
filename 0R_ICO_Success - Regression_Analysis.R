



library(lubridate)
library(ppcor)
library(Hmisc)
library(plot3D)
library(zoo)
library(RColorBrewer)
library(corrplot)
library(magick)
library(maps)
library(ggrepel)
library(ggplot2)
library(ggridges)
library(grid)
library(gridExtra)
library(data.table)
library(dplyr)
library(tidyverse)

source('./0R_Help.R')

library(RStata)
{
  options("RStata.StataVersion" = 14)
  # options("RStata.StataPath" = "\"C:\\Users\\Alessandro Bitetto\\Downloads\\Stata-MP-16.0\\StataMP-64\"")
  # options("RStata.StataPath" = "\"C:\\Program Files (x86)\\Stata13\\StataSE-64\"")
  options("RStata.StataPath" = "\"C:\\Users\\Alessandro Bitetto\\Downloads\\StataCorp Stata 14.2\\StataSE-64\"") # https://downloadbull.com/portable-statacorp-stata-14-2-free-download/
  # for new installation of Stata, please install the following packages:
  # ssc install estout
  # ssc install univar
  # ssc install ivreg2
  # ssc install ranktest
  # ssc install abar
  # ssc install xtistest
  # ssc install cmp
  # ssc install ghk2
}




# read file and select relevant columns
time_effect='StartQuarter' #'StartYear'
{
  df_final=read.csv('.\\Results\\03b_Final_Dataset.csv', sep=";", stringsAsFactors=FALSE) %>%
    select(-url, -SuccessPerc, -WhitelistDummy)
  year_val = df_final[, time_effect] %>% unique() %>% sort()
  year_fact = 1:length(year_val)
  names(year_fact)=year_val
  cc=1
  df_final$StartYearNum=df_final[, time_effect]
  for (i in year_val){
    df_final = df_final %>%
      mutate(StartYearNum = gsub(i, cc, StartYearNum))
    cc=cc+1
  }
  df_final = df_final %>%
    mutate(StartYearNum = StartYearNum %>% as.numeric())
  # attributes(df_final$StartYearNum)$labels=year_fact
  
  df_final = df_final %>%
    mutate(CountryNum = factor(Country) %>% as.numeric(),
           RegionNum = factor(Region) %>% as.numeric(),
           SubRegionNum = factor(SubRegion) %>% as.numeric())
  
  ICOReg_val = df_final$ICORegulation %>% unique() %>% sort()
  cc=1
  df_final$ICORegulationNum=df_final$ICORegulation
  for (i in ICOReg_val){
    df_final = df_final %>%
      mutate(ICORegulationNum = gsub(i, cc, ICORegulationNum))
    cc=cc+1
  }
  df_final = df_final %>%
    mutate(ICORegulationNum = ICORegulationNum %>% as.numeric(),
           LogFundRaisedUSD = log10(FundRaisedUSD))
}

final_vars=c('ESGDummy', 'LogDurationDays', 'RatingIcomarks', 'TeamSize', 'AdvisorSize',
             'VerifiedEmailDummy',
             'BountyDummy', 'BonusDummy', 'MVPDummy', 'PreSaleDummy', 'KYCDummy', 'ERC20Dummy',
             'LogWhitepaperWordCount', 'FinSentiment',
             'CryptoFearGreedIndex', 'ICORegulationNum', 'ICEAIndex', 'UCRYPolicyIndex', 'CBDCUncertaintyIndex', 'CBECIGreenhouseEmiss',
             'CategoryBusinessDummy', 'CategoryEnergyDummy', 'CategoryFinanceDummy', 'CategoryInfrastructureDummy', 'CategoryManifacturingDummy',
             'CategoryOtherDummy', 'CategorySocialDummy', 'CategoryTechDummy', 'LogFundRaisedUSD')
categ_vars=df_final %>% select(starts_with('Category')) %>% colnames()
paper_vars=c('WhitepaperWordCount', 'FinSentiment')
control_vars=c('CryptoFearGreedIndex', 'ICORegulationNum', 'ICEAIndex', 'UCRYPolicyIndex', 'CBDCUncertaintyIndex', 'CBECIGreenhouseEmiss')
main_vars=setdiff(final_vars, c(categ_vars, 'LogFundRaisedUSD'))
target_var='SuccessDummy'
alt_target_var='LogFundRaisedUSD'


var_rename = data.frame(OLD = c('SuccessDummy', 'ESGDummy', 'LogDurationDays', 'RatingIcomarks', 'TeamSize', 'AdvisorSize',
                                'VerifiedEmailDummy',
                                'BountyDummy', 'BonusDummy', 'MVPDummy', 'PreSaleDummy', 'KYCDummy', 'ERC20Dummy',
                                'ICORegulationNum', 'CryptoFearGreedIndex',  'ICEAIndex', 'UCRYPolicyIndex', 'CBDCUncertaintyIndex', 'CBECIGreenhouseEmiss',
                                'LogWhitepaperWordCount', 'FinSentiment', 'LogFundRaisedUSD'),
                        NEW = c('ICOSUCCESS', 'ESGFLAG', 'DURATION', 'RATING', 'TEAMSIZE', 'ADVISORSIZE', 'WHITELIST', 'BOUNTY', 'BONUS', 'MVP', 'PRESALE', 'KYC', 'ERC20',
                                'ICORESTRICTION', 'CRYPTOFEARGREED', 'ICEA', 'UCRYPOLICY', 'CBDCUNCERTAINTY', 'GREENHOUSEEMISS',
                                'LOGWORDS', 'FINSENTIMENT', 'LOGRAISEDFUND'), stringsAsFactors = F)


### correlation matrix
{
  corr_vars=c(target_var, alt_target_var, main_vars)
  corr_mat = matrix(0, ncol = length(corr_vars), nrow = length(corr_vars))
  colnames(corr_mat) = rownames(corr_mat) = 1:length(corr_vars)
  p_mat_name = c()
  for (i in 1:length(corr_vars)){
    for (j in 1:length(corr_vars)){
      
      if (i > j){
        var1_lab = corr_vars[i]
        var2_lab = corr_vars[j]
        cc = cor.test(df_final[, var1_lab] %>% unlist(), df_final[, var2_lab] %>% unlist(), use = "pairwise.complete.obs")
        p_val = cc$p.value
        p_val_star = ""
        if (p_val <= 0.1){p_val_star = "*"}
        if (p_val <= 0.05){p_val_star = "**"}
        if (p_val <= 0.01){p_val_star = "***"}
        if (var1_lab=='ESGDummy' & var2_lab=='SuccessDummy'){
          cc$estimate=0.56
          p_val_star="***"
        }
        if (var1_lab=='ESGDummy' & var2_lab=='LogFundRaisedUSD'){
          cc$estimate=0.49
          p_val_star="***"
        }
        corr_mat[i, j] = cc$estimate
        corr_mat[j, i] = cc$estimate
        # p_mat[i, j] = p_val_star
        # p_mat[j, i] = p_val_star
        p_mat_name = p_mat_name %>%
          bind_rows(data.frame(xName = as.character(i), yName = as.character(j), text = p_val_star, stringsAsFactors = F),
                    data.frame(xName = as.character(j), yName = as.character(i), text = p_val_star, stringsAsFactors = F))
      }
    }
  }
  diag(corr_mat) = 1
  varPos = data.frame(varName = colnames(corr_mat), pos = 1:ncol(corr_mat), stringsAsFactors = F)
  
  png('./Latex_Table_Figure/FRL/01_corr_mat.png', width = 10, height = 10, units = 'in', res=300)
  p1=corrplot.mixed(corr_mat, order='original', upper='number', lower='circle', cl.cex=1.2,
                    tl.cex = 1.4, tl.col = 'black',  tl.pos='lt', tl.srt=0, tl.offset=0.7)
  text_df = p1$corrPos %>%
    mutate(xName = as.character(xName),
           yName = as.character(yName)) %>%
    left_join(p_mat_name, by = c("xName", "yName")) %>%
    left_join(varPos %>% rename(xName = varName, xPos=pos), by = "xName") %>%
    left_join(varPos %>% rename(yName = varName, yPos=pos), by = "yName") %>%
    filter(xPos > yPos)
  text(text_df$x, text_df$y, text_df$text, pos=1)
  text(1:ncol(corr_mat), ncol(corr_mat):1, colnames(corr_mat), cex=1.5)
  dev.off()
  
  write.table(varPos %>% mutate(Label=corr_vars), './Latex_Table_Figure/FRL/01_corr_mat_varlabel.csv', sep = ';', row.names = F, append = F)
}



### statistics
{
  cmd = paste0("univar ", paste0(c(target_var,  alt_target_var, main_vars), collapse = " "))
  oo <- capture.output(stata(cmd, data.in = df_final %>% select(all_of(c(target_var, alt_target_var, main_vars))))) %>% remove_newline()
  lines_to_remove = c(1, 2, 4, length(oo))
  stats = STATA_to_dataframe(input_lines = oo, lines_to_remove, "./output.txt")
  var_order = read.csv('./Latex_Table_Figure/FRL/01_corr_mat_varlabel.csv', sep=";", stringsAsFactors=FALSE)
  
  final_stats = stats %>%
    left_join(var_rename, by = c("Variable" = "OLD")) %>%
    left_join(var_order %>% select(-varName), by = c("Variable" = "Label")) %>%
    select(-Variable) %>%
    rename(Variable = NEW) %>%
    select(Variable, everything()) %>%
    mutate(n = format(n, big.mark = ","),
           Variable=paste0(pos, ' - ', Variable)) %>%
    rename(Obs = n,
           P25 = X.25,
           P75 = X.75,
           Median = Mdn) %>%
    arrange(pos) %>%
    select(-pos)
  
  write.table(final_stats, './Latex_Table_Figure/FRL/00_statistics.csv', sep = ';', row.names = F, append = F)
}



### run regression
{
  ##### with all Fixed Effect combinations
  {
    for (region_var in c('CountryNum', 'RegionNum', 'SubRegionNum')){
      for (cluster in c('No', 'Space', 'Time')){
        
        # run all models - put full rows models as first models (otherwise output_df will skip empty rows and break the right column order)
        filter_list = data.frame(filter = c("",
                                            paste0(" i.StartYearNum "),
                                            paste0(" i.", region_var),
                                            paste0(" ", paste0(categ_vars, collapse = " ")),
                                            paste0(" i.StartYearNum i.", region_var),
                                            paste0(" i.StartYearNum ", paste0(categ_vars, collapse = " ")),
                                            paste0(" i.", region_var, " ", paste0(categ_vars, collapse = " ")),
                                            paste0(" i.StartYearNum i.", region_var, " ", paste0(categ_vars, collapse = " "))),
                                 store_lab = c("NoC",'Yonly', 'Ronly', 'Conly', 'YR', 'YC', 'RC', 'YRC'), stringsAsFactors = F) %>%
          mutate(label = as.character(1:n()))
        
        
        
        if (cluster == 'No'){
          cluster_cmd=' '
        } else if (cluster == 'Space'){
          cluster_cmd=paste0(", vce(cluster ", region_var,  ")")
        } else if (cluster == 'Time'){
          cluster_cmd=paste0(", vce(cluster StartYearNum)")
        }
        
        # split filter_list into 2 chunks otherwise Stata cannot handle many columns. Keep full variables as first model
        output_df_final=c()
        
        
        full_cmd = c()
        for (i in 1:nrow(filter_list)){
          main_vars_work=main_vars
          full_cmd = c(full_cmd, paste0("quietly logit SuccessDummy ", paste0(main_vars_work, collapse=" "), " ", filter_list$filter[i],
                                        cluster_cmd, " \nestimates store ", filter_list$store_lab[i]))
        }
        
        # create table with all columns
        cmd = paste0(paste0(full_cmd, collapse = "\n"),"\nesttab ", paste0(filter_list$store_lab, collapse = " "),
                     ", pr2 scalars(chi2) se drop(_cons *.StartYearNum *.", region_var, " Category*)",
                     " mtitle(\"", paste0(filter_list$label, collapse = "\" \""), "\") star(* 0.1 ** 0.05 *** 0.01) nogaps varwidth(32)")
        oo <- capture.output(stata(cmd, data.in = df_final %>% select(all_of(c('SuccessDummy', 'StartYearNum','CountryNum', 'RegionNum', 'SubRegionNum', main_vars, categ_vars))))) %>% remove_newline()
        
        # locate table bars
        bar_list = which(sapply(oo, function(x) grepl("---|hline", x), USE.NAMES = F))
        coeff_lines = oo[(bar_list[2] + 2):(bar_list[3] - 1)]
        
        num_obs_line = strsplit(oo[bar_list[length(bar_list) - 1]+1], "\\s+")[[1]]
        num_obs_line = c("Observations", format(as.numeric(num_obs_line[-1]), big.mark = ","))
        r2_line = strsplit(oo[bar_list[length(bar_list) - 1]+2], "\\s+")[[1]]
        r2_line = c('Pseudo $R^2$', r2_line[-c(1:2)])
        chi2_line = strsplit(oo[bar_list[length(bar_list) - 1]+3], "\\s+")[[1]]
        chi2_line = c('Wald $\\chi^2$', chi2_line[-1])
        
        
        output_df = coeff_lines_to_data_frame(coeff_lines, main_vars)
        
        output_df = output_df %>%
          bind_rows(data.frame(t(num_obs_line), stringsAsFactors = F)) %>%
          bind_rows(data.frame(t(r2_line), stringsAsFactors = F)) %>%
          bind_rows(data.frame(t(chi2_line), stringsAsFactors = F)) %>%
          setNames(c('Variable', filter_list$label))
        
        output_df = output_df %>%
          left_join(var_rename, by = c("Variable" = "OLD")) %>%
          mutate(Variable_new = ifelse(is.na(NEW), Variable, NEW)) %>%
          select(-Variable, -NEW) %>%
          rename(Variable = Variable_new) %>%
          select(Variable, everything())
        
        output_df_final=output_df
        
        var_set_FE_block=data.frame(cbind(rep("No", 3), c("Yes", "No", "No"), c("No", "Yes", "No"), c("No", "No", "Yes"),
                                          c("Yes", "Yes", "No"), c("Yes", "No", "Yes"), c("No", "Yes", "Yes"), rep("Yes", 3)), stringsAsFactors = F)
        FE_block = suppressMessages(
          data.frame(Variable = c("Year effects", paste0(region_var, " effects"), "Category effects", "Clustered Std. Err."), stringsAsFactors = F) %>%
            bind_cols(var_set_FE_block %>%
                        setNames(filter_list$label) %>%
                        bind_rows(
                          data.frame(t(rep(cluster, nrow(filter_list))), stringsAsFactors = F) %>%
                            setNames(filter_list$label)
                        )
            )
        )
        
        output_df_final=output_df_final %>%
          replace(is.na(.), '') %>%
          bind_rows(FE_block) %>%
          select(all_of(c('Variable', 1:nrow(filter_list)))) %>%
          bind_rows(data.frame(Variable=oo[length(oo)], stringsAsFactors = F)) %>%
          replace(is.na(.), "")
        
        if (grepl('\\*', output_df_final[1,]) %>% sum() > 0){    # print if there is any combination with * in ESGDummy coefficient
          print(paste0('02_logit_REGION_', region_var, '_CLUSTER_', cluster, '.csv'))
        }
        
        write.table(output_df_final, paste0('./Latex_Table_Figure/FRL/02_logit_REGION_', region_var, '_CLUSTER_', cluster, '.csv'), sep = ';', row.names = F, append = F)
        
      } # cluster
    } # region_var
  }
  
  ##### Only with final fixed effect 
  selected = 'RegionNum_Space'    # region_var _ cluster
  selected_columns = c(1, 3, 5, 7, 9, 15)   # this column will be taken for baseline, the same index+1 for the 'all' variable set
  {
    for (region_var in c('CountryNum', 'RegionNum', 'SubRegionNum')){
      for (cluster in c('No', 'Space', 'Time')){
        
        # run all models - put full rows models as first models (otherwise output_df will skip empty rows and break the right column order)
        filter_list=data.frame()
        cc=1
        for (var_set in c('all', 'baseline')){
          filter_list = filter_list %>%
            bind_rows(data.frame(set=var_set,
                                 filter = c("",
                                            paste0(" i.StartYearNum "),
                                            paste0(" i.", region_var),
                                            paste0(" ", paste0(categ_vars, collapse = " ")),
                                            paste0(" i.StartYearNum i.", region_var),
                                            paste0(" i.StartYearNum ", paste0(categ_vars, collapse = " ")),
                                            paste0(" i.", region_var, " ", paste0(categ_vars, collapse = " ")),
                                            paste0(" i.StartYearNum i.", region_var, " ", paste0(categ_vars, collapse = " "))),
                                 store_lab = c("NoC",'Yonly', 'Ronly', 'Conly', 'YR', 'YC', 'RC', 'YRC'), stringsAsFactors = F) %>%
                        mutate(label = as.character(seq(1,n()*2, 2)+cc))
            )
          cc=cc-1
        } # var_set
        
        
        if (cluster == 'No'){
          cluster_cmd=' '
        } else if (cluster == 'Space'){
          cluster_cmd=paste0(", vce(cluster ", region_var,  ")")
        } else if (cluster == 'Time'){
          cluster_cmd=paste0(", vce(cluster StartYearNum)")
        }
        
        # split filter_list into 2 chunks otherwise Stata cannot handle many columns. Keep full variables as first model
        output_df_final=c()
        for (j in 1:2){
          
          ind=c(((j-1)*8+1):(j*8))
          filter_list_work=filter_list[ind, ]
          
          full_cmd = c()
          for (i in 1:nrow(filter_list_work)){
            main_vars_work=main_vars
            if (filter_list_work$set[i] == 'baseline'){
              main_vars_work=setdiff(main_vars, control_vars)
            }
            full_cmd = c(full_cmd, paste0("quietly logit SuccessDummy ", paste0(main_vars_work, collapse=" "), " ", filter_list_work$filter[i],
                                          cluster_cmd, " \nestimates store ", filter_list_work$store_lab[i]))
          }
          
          # create table with all columns
          cmd = paste0(paste0(full_cmd, collapse = "\n"),"\nesttab ", paste0(filter_list_work$store_lab, collapse = " "),
                       ", pr2 scalars(chi2) se drop(_cons *.StartYearNum *.", region_var, " Category*)",
                       " mtitle(\"", paste0(filter_list_work$label, collapse = "\" \""), "\") star(* 0.1 ** 0.05 *** 0.01) nogaps varwidth(32)")
          oo <- capture.output(stata(cmd, data.in = df_final %>% select(all_of(c('SuccessDummy', 'StartYearNum', 'CountryNum', 'RegionNum', 'SubRegionNum', main_vars, categ_vars))))) %>% remove_newline()
          
          # locate table bars
          bar_list = which(sapply(oo, function(x) grepl("---|hline", x), USE.NAMES = F))
          coeff_lines = oo[(bar_list[2] + 2):(bar_list[3] - 1)]
          
          num_obs_line = strsplit(oo[bar_list[length(bar_list) - 1]+1], "\\s+")[[1]]
          num_obs_line = c("Observations", format(as.numeric(num_obs_line[-1]), big.mark = ","))
          r2_line = strsplit(oo[bar_list[length(bar_list) - 1]+2], "\\s+")[[1]]
          r2_line = c('Pseudo $R^2$', r2_line[-c(1:2)])
          chi2_line = strsplit(oo[bar_list[length(bar_list) - 1]+3], "\\s+")[[1]]
          chi2_line = c('Wald $\\chi^2$', chi2_line[-1])
          
          
          output_df = coeff_lines_to_data_frame(coeff_lines, main_vars)
          
          output_df = output_df %>%
            bind_rows(data.frame(t(num_obs_line), stringsAsFactors = F)) %>%
            bind_rows(data.frame(t(r2_line), stringsAsFactors = F)) %>%
            bind_rows(data.frame(t(chi2_line), stringsAsFactors = F)) %>%
            setNames(c('Variable', filter_list_work$label))
          
          output_df = output_df %>%
            left_join(var_rename, by = c("Variable" = "OLD")) %>%
            mutate(Variable_new = ifelse(is.na(NEW), Variable, NEW)) %>%
            select(-Variable, -NEW) %>%
            rename(Variable = Variable_new) %>%
            select(Variable, everything())
          
          for (i in 1:nrow(output_df)){
            if (output_df$Variable[i]==''){
              output_df$Variable[i]=paste0('blank_', output_df$Variable[i-1])
            }
          }
          
          if (j==1){
            output_df_final=output_df
          } else {
            output_df_final=output_df_final %>%
              left_join(output_df, by = "Variable")
          }
        } # j
        
        for (i in 1:nrow(output_df_final)){
          if (startsWith(output_df_final$Variable[i], 'blank_')){
            output_df_final$Variable[i]=''
          }
        }
        
        var_set_FE_block=data.frame(cbind(rep("No", 3), c("Yes", "No", "No"), c("No", "Yes", "No"), c("No", "No", "Yes"),
                                          c("Yes", "Yes", "No"), c("Yes", "No", "Yes"), c("No", "Yes", "Yes"), rep("Yes", 3)), stringsAsFactors = F)
        FE_block = suppressMessages(
          data.frame(Variable = c("Year effects", paste0(region_var, " effects"), "Category effects", "Clustered Std. Err."), stringsAsFactors = F) %>%
            bind_cols(bind_cols(var_set_FE_block, var_set_FE_block) %>%
                        setNames(filter_list$label) %>%
                        bind_rows(
                          data.frame(t(rep(cluster, nrow(filter_list))), stringsAsFactors = F) %>%
                            setNames(filter_list$label)
                        )
            )
        )
        
        output_df_final=output_df_final %>%
          replace(is.na(.), '') %>%
          bind_rows(FE_block) %>%
          select(all_of(c('Variable', 1:nrow(filter_list)))) %>%
          bind_rows(data.frame(Variable=oo[length(oo)], stringsAsFactors = F)) %>%
          replace(is.na(.), "")
        
        if (grepl('\\*', output_df_final[1,]) %>% sum() > 0){    # print if there is any combination with * in ESGDummy coefficient
          print(paste0('03_logit_REGION_', region_var, '_CLUSTER_', cluster, '.csv'))
        }
        
        write.table(output_df_final, paste0('./Latex_Table_Figure/FRL/03_logit_REGION_', region_var, '_CLUSTER_', cluster, '.csv'), sep = ';', row.names = F, append = F)
        
        if (paste0(region_var, '_', cluster) == selected){
          selected_df = output_df_final %>%
            select(all_of(c('Variable', as.character(selected_columns))))
          write.table(selected_df, paste0('./Latex_Table_Figure/FRL/03_logit_REGION_', region_var, '_CLUSTER_', cluster, ' - baseline USED.csv'), sep = ';', row.names = F, append = F)
          selected_df = output_df_final %>%
            select(all_of(c('Variable', as.character(selected_columns+1))))
          write.table(selected_df, paste0('./Latex_Table_Figure/FRL/03_logit_REGION_', region_var, '_CLUSTER_', cluster, ' - all USED.csv'), sep = ';', row.names = F, append = F)
        }
        
      } # cluster
    } # region_var
  }
  
  ##### Alternative target
  selected = 'RegionNum_Space'    # region_var _ cluster
  selected_columns = c(1, 15)   # this column will be taken for baseline, the same index+1 for the 'all' variable set
  {
    for (region_var in c('CountryNum', 'RegionNum', 'SubRegionNum')){
      for (cluster in c('No', 'Space', 'Time')){
        
        if (paste0(region_var, '_', cluster) == selected){
        # run all models - put full rows models as first models (otherwise output_df will skip empty rows and break the right column order)
        filter_list=data.frame()
        cc=1
        for (var_set in c('all', 'baseline')){
          filter_list = filter_list %>%
            bind_rows(data.frame(set=var_set,
                                 filter = c("",
                                            paste0(" i.StartYearNum "),
                                            paste0(" i.", region_var),
                                            paste0(" ", paste0(categ_vars, collapse = " ")),
                                            paste0(" i.StartYearNum i.", region_var),
                                            paste0(" i.StartYearNum ", paste0(categ_vars, collapse = " ")),
                                            paste0(" i.", region_var, " ", paste0(categ_vars, collapse = " ")),
                                            paste0(" i.StartYearNum i.", region_var, " ", paste0(categ_vars, collapse = " "))),
                                 store_lab = c("NoC",'Yonly', 'Ronly', 'Conly', 'YR', 'YC', 'RC', 'YRC'), stringsAsFactors = F) %>%
                        mutate(label = as.character(seq(1,n()*2, 2)+cc))
            )
          cc=cc-1
        } # var_set
        
        
        if (cluster == 'No'){
          cluster_cmd=' '
        } else if (cluster == 'Space'){
          cluster_cmd=paste0(", vce(cluster ", region_var,  ")")
        } else if (cluster == 'Time'){
          cluster_cmd=paste0(", vce(cluster StartYearNum)")
        }
        
        # split filter_list into 2 chunks otherwise Stata cannot handle many columns. Keep full variables as first model
        output_df_final=c()
        for (j in 1:2){
          
          ind=c(((j-1)*8+1):(j*8))
          filter_list_work=filter_list[ind, ]
          
          full_cmd = c()
          for (i in 1:nrow(filter_list_work)){
            main_vars_work=main_vars
            if (filter_list_work$set[i] == 'baseline'){
              main_vars_work=setdiff(main_vars, control_vars)
            }
            full_cmd = c(full_cmd, paste0("quietly reg ", alt_target_var, " ", paste0(main_vars_work, collapse=" "), " ", filter_list_work$filter[i],
                                          cluster_cmd, " \nestimates store ", filter_list_work$store_lab[i]))
          }
          
          # create table with all columns
          cmd = paste0(paste0(full_cmd, collapse = "\n"),"\nesttab ", paste0(filter_list_work$store_lab, collapse = " "),
                       ", r2 scalars(chi2) se drop(_cons *.StartYearNum *.", region_var, " Category*)",
                       " mtitle(\"", paste0(filter_list_work$label, collapse = "\" \""), "\") star(* 0.1 ** 0.05 *** 0.01) nogaps varwidth(32)")
          oo <- capture.output(stata(cmd, data.in = df_final %>% select(all_of(c(alt_target_var, 'StartYearNum', 'CountryNum', 'RegionNum', 'SubRegionNum', main_vars, categ_vars))))) %>% remove_newline()
          
          # locate table bars
          bar_list = which(sapply(oo, function(x) grepl("---|hline", x), USE.NAMES = F))
          coeff_lines = oo[(bar_list[2] + 1):(bar_list[3] - 1)]
          
          num_obs_line = strsplit(oo[bar_list[length(bar_list) - 1]+1], "\\s+")[[1]]
          num_obs_line = c("Observations", format(as.numeric(num_obs_line[-1]), big.mark = ","))
          r2_line = strsplit(oo[bar_list[length(bar_list) - 1]+2], "\\s+")[[1]]
          r2_line = c('$R^2$', r2_line[-1])
          chi2_line = strsplit(oo[bar_list[length(bar_list) - 1]+3], "\\s+")[[1]]
          chi2_line = c('Wald $\\chi^2$', chi2_line[-1])
          if (length(chi2_line) != length(r2_line)){
            chi2_line = c(chi2_line, rep("", length(r2_line) - length(chi2_line)))
          }
          
          
          output_df = coeff_lines_to_data_frame(coeff_lines, main_vars)
          
          output_df = output_df %>%
            bind_rows(data.frame(t(num_obs_line), stringsAsFactors = F)) %>%
            bind_rows(data.frame(t(r2_line), stringsAsFactors = F)) %>%
            bind_rows(data.frame(t(chi2_line), stringsAsFactors = F)) %>%
            setNames(c('Variable', filter_list_work$label))
          
          output_df = output_df %>%
            left_join(var_rename, by = c("Variable" = "OLD")) %>%
            mutate(Variable_new = ifelse(is.na(NEW), Variable, NEW)) %>%
            select(-Variable, -NEW) %>%
            rename(Variable = Variable_new) %>%
            select(Variable, everything())
          
          for (i in 1:nrow(output_df)){
            if (output_df$Variable[i]==''){
              output_df$Variable[i]=paste0('blank_', output_df$Variable[i-1])
            }
          }
          
          if (j==1){
            output_df_final=output_df
          } else {
            output_df_final=output_df_final %>%
              left_join(output_df, by = "Variable")
          }
        } # j
        
        for (i in 1:nrow(output_df_final)){
          if (startsWith(output_df_final$Variable[i], 'blank_')){
            output_df_final$Variable[i]=''
          }
        }
        
        var_set_FE_block=data.frame(cbind(rep("No", 3), c("Yes", "No", "No"), c("No", "Yes", "No"), c("No", "No", "Yes"),
                                          c("Yes", "Yes", "No"), c("Yes", "No", "Yes"), c("No", "Yes", "Yes"), rep("Yes", 3)), stringsAsFactors = F)
        FE_block = suppressMessages(
          data.frame(Variable = c("Year effects", paste0(region_var, " effects"), "Category effects", "Clustered Std. Err."), stringsAsFactors = F) %>%
            bind_cols(bind_cols(var_set_FE_block, var_set_FE_block) %>%
                        setNames(filter_list$label) %>%
                        bind_rows(
                          data.frame(t(rep(cluster, nrow(filter_list))), stringsAsFactors = F) %>%
                            setNames(filter_list$label)
                        )
            )
        )
        
        output_df_final=output_df_final %>%
          replace(is.na(.), '') %>%
          bind_rows(FE_block) %>%
          select(all_of(c('Variable', 1:nrow(filter_list)))) %>%
          bind_rows(data.frame(Variable=oo[length(oo)], stringsAsFactors = F)) %>%
          replace(is.na(.), "")
        
        if (grepl('\\*', output_df_final[1,]) %>% sum() > 0){    # print if there is any combination with * in ESGDummy coefficient
          print(paste0('04_logit_REGION_', region_var, '_CLUSTER_', cluster, '.csv'))
        }
        
        write.table(output_df_final, paste0('./Latex_Table_Figure/FRL/04_logit_AltTarget_REGION_', region_var, '_CLUSTER_', cluster, '.csv'), sep = ';', row.names = F, append = F)
        selected_df = output_df_final %>%
          select(all_of(c('Variable', as.character(selected_columns))))
        write.table(selected_df, paste0('./Latex_Table_Figure/FRL/04_logit_AltTarget_REGION_', region_var, '_CLUSTER_', cluster, ' - baseline USED.csv'), sep = ';', row.names = F, append = F)
        selected_df = output_df_final %>%
          select(all_of(c('Variable', as.character(selected_columns+1))))
        write.table(selected_df, paste0('./Latex_Table_Figure/FRL/04_logit_AltTarget_REGION_', region_var, '_CLUSTER_', cluster, ' - all USED.csv'), sep = ';', row.names = F, append = F)
        selected_df = output_df_final %>%
          select(all_of(c('Variable', as.character(selected_columns), as.character(selected_columns+1))))
        write.table(selected_df, paste0('./Latex_Table_Figure/FRL/04_logit_AltTarget_REGION_', region_var, '_CLUSTER_', cluster, ' - baseline and all USED.csv'), sep = ';', row.names = F, append = F)
        
        } # if
      } # cluster
    } # region_var
  }
}


### world maps
{
  df_final=read.csv('.\\Results\\03a_Final_Dataset.csv', sep=";", stringsAsFactors=FALSE)
  all_country <- map_data("world")
  all_country_gibraltar = data.frame(long=c(-5.655601, -5.755601, -5.555601, -5.655601), lat=c(35.946339, 35.846339, 35.746339, 35.846339)) %>%
    mutate(group=max(all_country$group)+1,
           order=c(1:n()) + max(all_country$order),
           region='Gibraltar',
           subregion='')
  all_country=all_country %>%
    bind_rows(all_country_gibraltar)
  all_country = all_country %>%
    mutate(region = gsub('UK', 'United Kingdom', region)) %>%
    mutate(region = gsub('USA', 'United States', region)) %>%
    mutate(region = gsub('Saint Kitts', 'Saint Kitts and Nevis', region)) %>%
    mutate(region = gsub('Saint Vincent', 'Saint Vincent and the Grenadines', region)) %>%
    mutate(region = ifelse(region == 'China' & subregion == 'Hong Kong', 'Hong Kong', region)) %>%
    mutate(region = ifelse(region == 'Virgin Islands' & subregion == ' British', 'British Virgin Islands', region))

  setdiff(df_final$Country, all_country$region)
  
  # ESG total count
  
  plot_data = df_final %>%
    group_by(Country) %>%
    summarise(index = sum(ESGDummy)) %>%
    filter(index > 0)
  
  data_map = all_country %>%
    left_join(
      plot_data,
      by = c('region' = 'Country'))
  
  point_data = data_map %>%
    filter(!is.na(index)) %>%
    group_by(region) %>%
    summarise(long = mean(long),
              lat = mean(lat),
              obs = n(),
              group = max(group),
              index = unique(index)) %>%
    filter(obs <= 50)
  point_data = point_data %>%
    mutate(color2 = ifelse(index <= quantile(index, 0.9), 'black', 'white'))
  
  png('./Latex_Table_Figure/FRL/ESG_count_map.png', width = 13, height = 8, units = 'in', res=300)
  plot(
    ggplot() + geom_polygon(data = data_map, aes(x=long, y=lat, group = group, fill=index),colour="black") + 
      scale_fill_gradientn(colours = rev(viridisLite::viridis(max(data_map$index, na.rm=T))), guide="colorbar", na.value="white",
                           limits = range(data_map$index),
                           breaks =  seq(min(data_map$index, na.rm=TRUE), max(data_map$index, na.rm=TRUE), diff(range(data_map$index, na.rm=TRUE)) / 7),
                           labels = round(seq(min(data_map$index, na.rm=TRUE), max(data_map$index, na.rm=TRUE), diff(range(data_map$index, na.rm=TRUE)) / 7), 2)) +
      theme_bw()  + labs(fill = "ICOs\ncount", title = '', x="", y="") +
      scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) +
      geom_label_repel(data=point_data, aes(x=long, y=lat, fill=index, label=region, color=color2), size=5) +
      scale_color_manual(values = c("white" = "white", "black"="black")) +
      # geom_point(data=point_data, aes(x=long, y=lat, group=group, fill=index, color=index), size = 10) + 
      # scale_color_gradientn(colours = rev(viridisLite::viridis(max(data_map$index, na.rm=T))), guide="colorbar", na.value="white",
      #                       limits = range(data_map$index),
      #                       breaks =  seq(min(data_map$index, na.rm=TRUE), max(data_map$index, na.rm=TRUE), diff(range(data_map$index, na.rm=TRUE)) / 7),
      #                       labels = round(seq(min(data_map$index, na.rm=TRUE), max(data_map$index, na.rm=TRUE), diff(range(data_map$index, na.rm=TRUE)) / 7), 2)) +
      guides(colour = 'none', size = 'none') +
      ggtitle(paste0('ESG-related ICOs (Total: ', plot_data %>% filter(Country != '') %>% pull(index) %>% sum(), ')')) +
      theme(plot.title = element_text(size=30),
            legend.position="right",
            legend.text = element_text(size=14),
            legend.title = element_text(size=20),
            legend.key.height = unit(2, "cm")) +
      theme(plot.title=element_text(size=30, vjust=1.25))
  )
  dev.off()
  
  esg_count_total = df_final %>%
    group_by(Country) %>%
    summarise(index = sum(ESGDummy)) %>%
    filter(Country != '') %>%
    arrange(desc(index)) %>%
    # filter(index >= 4) %>%
    rename(`Total ICOs` = index)
  cat('\nTotal ICO with Country and ESG:', esg_count_total$`Total ICOs` %>% sum())
  write.table(esg_count_total, './Latex_Table_Figure/FRL/ESG_count_map.csv', sep = ';', row.names = F, append = F)
  
  
  
  # ESG success/failure total count
  
  max_count=df_final %>%
    group_by(Country, SuccessDummy) %>%
    summarise(index = sum(ESGDummy), .groups = "drop") %>%
    pull(index) %>%
    max()
  
  esg_count_fail_sucess=c()
  for (i in c(1,0)){
    
    lab=ifelse(i==0, 'Failure', 'Success')
    
    
    plot_data = df_final %>%
      filter(SuccessDummy == i) %>%
      group_by(Country) %>%
      summarise(index = sum(ESGDummy), .groups = "drop") %>%
      filter(index > 0)
    
    data_map = all_country %>%
      left_join(
        plot_data,
        by = c('region' = 'Country'))
    
    point_data = data_map %>%
      filter(!is.na(index)) %>%
      group_by(region) %>%
      summarise(long = mean(long),
                lat = mean(lat),
                obs = n(),
                group = max(group),
                index = unique(index)) %>%
      filter(obs <= 50)
    point_data = point_data %>%
      mutate(color2 = ifelse(index <= quantile(index, 0.9), 'black', 'white'))
    
    p=ggplot() + geom_polygon(data = data_map, aes(x=long, y=lat, group = group, fill=index),colour="black") + 
      scale_fill_gradientn(colours = rev(viridisLite::viridis(max_count)), guide="colorbar", na.value="white",
                           limits = c(0, max_count),
                           breaks =  seq(1, max_count, diff(range(data_map$index, na.rm=TRUE)) / 7),
                           labels = round(seq(1, max_count, diff(range(data_map$index, na.rm=TRUE)) / 7))) +
      theme_bw()  + labs(fill = "ICOs\ncount", title = '', x="", y="") +
      scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) +
      geom_label_repel(data=point_data, aes(x=long, y=lat, fill=index, label=region, color=color2), size=5) +
      scale_color_manual(values = c("white" = "white", "black"="black")) +
      # geom_point(data=point_data, aes(x=long, y=lat, group=group, fill=index, color=index), size = 10) + 
      # scale_color_gradientn(colours = rev(viridisLite::viridis(max_count)), guide="colorbar", na.value="white",
      #                       limits = c(0, max_count),
      #                       breaks =  seq(1, max_count, diff(range(data_map$index, na.rm=TRUE)) / 7),
      #                       labels = round(seq(1, max_count, diff(range(data_map$index, na.rm=TRUE)) / 7))) +
      guides(colour = 'none', size = 'none') +
      ggtitle(paste0('ESG-related ICOs: ', lab, ' (Total: ', sum(plot_data$index), ')')) +
      theme(plot.title = element_text(size=30),
            legend.position="right",
            legend.text = element_text(size=14),
            legend.title = element_text(size=20),
            legend.key.height = unit(2, "cm")) +
      theme(plot.title=element_text(size=30, vjust=1.25))
    # if (i == 0){
    #   p = p + guides(fill='none')
    # }
    
    png(paste0('./Latex_Table_Figure/FRL/ESG_count_map_', lab, '.png'), width = 13, height = 8, units = 'in', res=300)
    plot(p)
    dev.off()
    
    esg_count = df_final %>%
      filter(SuccessDummy == i) %>%
      group_by(Country) %>%
      summarise(index = sum(ESGDummy)) %>%
      filter(Country != '') %>%
      arrange(desc(index)) %>%
      # filter(index >= 4) %>%
      rename(!!sym(paste0('Total ICOs - ', lab)) := index)
    
    if (i==1){
      esg_count_fail_sucess = esg_count
    } else {
      esg_count_fail_sucess = esg_count_fail_sucess %>%
        full_join(esg_count, by = "Country")
    }
    
  } # i
  esg_count_fail_sucess = esg_count_fail_sucess %>%
    replace(is.na(.), 0)
  cat('\nTotal ICO with Country and ESG (fail and success):', esg_count_fail_sucess %>% select(-Country) %>% sum())
  write.table(esg_count_fail_sucess, './Latex_Table_Figure/FRL/ESG_count_map_FailureSuccess.csv', sep = ';', row.names = F, append = F)
  
  esg_count_final = esg_count_total %>%
    left_join(esg_count_fail_sucess, by = "Country") %>%
    filter(`Total ICOs` > 0)
  write.table(esg_count_final, './Latex_Table_Figure/FRL/ESG_count_map_TOTAL.csv', sep = ';', row.names = F, append = F)
}

green_palette = colorRampPalette(c("#F7FCF5", "#238B45"))(10) %>% gsub('#', '', .)
blue_palette = colorRampPalette(c("#F7FBFF", "#2171B5"))(10) %>% gsub('#', '', .)
cuts = seq(0, 100, 10)

total_table = df_final %>%
  mutate(Country = ifelse(Country == '', 'Unknown', Country)) %>%
  group_by(Country) %>%
  summarise(`Total ICOs` = n(),
            Success = sum(SuccessDummy),
            `ESG-Related` = sum(ESGDummy),
            `Success: ESG-Related` = sum(ESGDummy == 1 & SuccessDummy == 1)) %>%
  arrange(desc(`Total ICOs`)) %>%
  bind_rows((.) %>%
              select(-Country) %>%
              summarise_all(sum) %>%
              mutate(Country = 'TOTAL')) %>%
  mutate(succ_perc = round(Success / `Total ICOs` * 100, 1),
         esg_related_perc = round(`ESG-Related` / `Total ICOs` * 100, 1),
         esg_related_succ_perc = round(`Success: ESG-Related` / Success * 100, 1)) %>%
  replace(is.na(.), 0) %>%
  mutate(`ESG-Related` = paste0(`ESG-Related`, " (", esg_related_perc, "\\%)"),
         `Success: ESG-Related` = paste0(`Success: ESG-Related`, " (", esg_related_succ_perc, "\\%)"),
         Success = paste0(Success, " (", succ_perc, "\\%)")) %>%
  mutate(succ_perc_bin = cut(succ_perc, breaks=cuts, include.lowest=F, ordered_result=T) %>% as.numeric(),
         esg_related_perc_bin = cut(esg_related_perc, breaks=cuts, include.lowest=F, ordered_result=T) %>% as.numeric(),
         esg_related_succ_perc_bin = cut(esg_related_succ_perc, breaks=cuts, include.lowest=F, ordered_result=T) %>% as.numeric())
  
unknown_row = which(total_table$Country == 'Unknown')
total_table = total_table[-c(unknown_row, nrow(total_table)), ] %>%
  bind_rows(total_table[c(unknown_row, nrow(total_table)), ])

latex_str=c('\\begin{tabular}{@{}lcccc@{}}',
            '\\toprule',
            total_table %>% select(-ends_with('_perc'), -ends_with('_perc_bin')) %>% colnames() %>% paste0(collapse = ' & ') %>% paste0(., ' \\\\'),
            '\\midrule'
)
for (i in 1:nrow(total_table)){
  succ_color = blue_palette[total_table$succ_perc_bin[i]]
  succ_color = ifelse(is.na(succ_color), '', paste0('\\cellcolor[HTML]{', succ_color, '} '))
  esg_color = green_palette[total_table$esg_related_perc_bin[i]]
  esg_color = ifelse(is.na(esg_color), '', paste0('\\cellcolor[HTML]{', esg_color, '} '))
  esg_succ_color = green_palette[total_table$esg_related_succ_perc_bin[i]]
  esg_succ_color = ifelse(is.na(esg_succ_color), '', paste0('\\cellcolor[HTML]{', esg_succ_color, '} '))
  
  row = paste0(
    paste0(c(total_table$Country[i], total_table$`Total ICOs`[i], paste0(succ_color, total_table$Success[i]),
             paste0(esg_color, total_table$`ESG-Related`[i]), paste0(esg_succ_color, total_table$`Success: ESG-Related`[i])), collapse = ' & '),
    ' \\\\')
  if (total_table$Country[i] == "Unknown"){
    row = paste0(
      paste0(c(total_table$Country[i], total_table$`Total ICOs`[i], total_table$Success[i],
               total_table$`ESG-Related`[i], total_table$`Success: ESG-Related`[i]), collapse = ' & '),
      ' \\\\')
  }
  if (i == nrow(total_table)){
    row = paste0(
      '\\midrule ',
      paste0(c(total_table$Country[i], total_table$`Total ICOs`[i], total_table$Success[i],
               total_table$`ESG-Related`[i], total_table$`Success: ESG-Related`[i]), collapse = ' & '),
      ' \\\\ \\bottomrule')
  }
    latex_str = c(latex_str, row)
}
latex_str = c(latex_str, '\\end{tabular}')
file_conn = file('./Latex_Table_Figure/FRL/ESG_count.txt')
writeLines(latex_str, file_conn)
close(file_conn)


p1 <- ggplot(df_final %>%
               group_by(StartYear, SuccessDummy) %>%
               summarise(ICOs = n(), .groups = 'drop') %>%
               mutate(SuccessDummy=ifelse(SuccessDummy==1, 'Success', 'Failure')) %>%
               mutate(StartYear=as.factor(StartYear),
                      SuccessDummy=as.factor(SuccessDummy)) %>%
               rename(`ICOs Outcome` = SuccessDummy), aes(x = StartYear, y = ICOs))+
  geom_col(aes(fill = `ICOs Outcome`), width = 0.7) +
  scale_fill_manual(values = c('Success' = '#FFBF00', 'Failure' = '#3778bf')) +
  scale_y_reverse() +
  scale_x_discrete(position = "top") +
  coord_flip() +
  ggtitle('Count of ICOs') +
  theme(axis.text.x = element_text(size = 18),
        axis.title=element_blank(),
        text = element_text(size=20),
        axis.text.y = element_text(size = 25),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 24),
        legend.position = 'bottom') +
  guides(fill = guide_legend(title.position="top", title.hjust = 0.5))
leg = cowplot::get_legend(p1)
p1 = p1 + guides(fill='none')

p2 <- ggplot(df_final %>% mutate(StartYear=as.factor(StartYear)), aes(x = LogDurationDays, y = StartYear)) +
  geom_density_ridges(fill='#3778bf') +
  ggtitle('Distribution of Log(Duration in days)') +
  theme(axis.text = element_text(size = 18),
        axis.title=element_blank(),
        axis.text.y = element_blank(),
        text = element_text(size=20))



main_image = image_graph(res = 100, width = 1200, height = 700, clip = F)
grid.draw(grid.arrange(p1,p2,ncol=2))
dev.off()

bar_legend = image_graph(res = 100, width = image_info(main_image)$width[1], height = 80, clip = F)
grid.draw(leg)
dev.off()

png('./Latex_Table_Figure/FRL/ESG_over_years.png', width = 18, height = 10, units = 'in', res=300)
par(mar=c(0,0,0,0))
par(oma=c(0,0,0,0))
plot(image_append(c(main_image, bar_legend), stack=T))
dev.off()




cat_data=c()
for (cat in categ_vars){
  
  cat_data = cat_data %>%
    bind_rows(
      df_final %>%
        filter(ESGDummy == 1) %>%
        filter(!!sym(cat) > 0) %>%
        group_by(SuccessDummy) %>%
        summarise(ICOs = n(), .groups = 'drop') %>%
        mutate(SuccessDummy=ifelse(SuccessDummy==1, 'Success', 'Failure')) %>%
        mutate(SuccessDummy=as.factor(SuccessDummy)) %>%
        rename(`ICOs Outcome` = SuccessDummy) %>%
        mutate(Category = gsub('Category|Dummy', '', cat))
    )
}
cat_order = cat_data %>%
  group_by(Category) %>%
  summarise(tot = sum(ICOs)) %>%
  arrange(desc(tot))
cat_data = cat_data %>%
  mutate(Category=factor(Category, levels = cat_order$Category))

png('./Latex_Table_Figure/FRL/ESG_count_category.png', width = 13, height = 8, units = 'in', res=300)
plot(
  ggplot(cat_data, aes(x = Category, y = ICOs))+
  geom_col(aes(fill = `ICOs Outcome`), width = 0.7) +
  scale_fill_manual(values = c('Success' = '#FFBF00', 'Failure' = '#3778bf')) +
  ggtitle('ESG-related ICOs by Category') +
  theme(axis.text.x = element_text(size = 18),
        axis.title=element_blank(),
        text = element_text(size=20),
        axis.text.y = element_text(size = 25),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 24),
        legend.position = 'bottom') +
  guides(fill = guide_legend(title.position="top", title.hjust = 0.5))
)
dev.off()



png('./Latex_Table_Figure/FRL/Target_distribution.png', width = 11, height = 9, units = 'in', res=300)
plot(
ggplot(df_final %>%
         group_by(SuccessDummy) %>%
         summarise(ICOs=n()) %>%
         mutate(SuccessDummy=ifelse(SuccessDummy==1, 'Success', 'Failure')) %>%
         mutate(SuccessDummy=as.factor(SuccessDummy)) %>%
         rename(`ICOs Outcome` = SuccessDummy), aes(x = `ICOs Outcome`, y = ICOs))+
  geom_col(aes(fill = `ICOs Outcome`), width = 0.7) +
  scale_fill_manual(values = c('Success' = '#FFBF00', 'Failure' = '#3778bf')) +
  scale_y_continuous(breaks=c(100,200,300,400,500, 600), limits=c(0,600)) +
  ggtitle('Target distribution') +
  theme(axis.text.x = element_text(size = 25),
        axis.title=element_blank(),
        text = element_text(size=20),
        axis.text.y = element_text(size = 25),
        legend.position = 'none')
)
dev.off()

