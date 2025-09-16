## Custom Functions

### Existence

# to make existence bar plots for each version, each country 

exist_bar <- function(country, version, cols, col_names, title, data){
  data %>% 
    filter(ont_ctry_name == country, ont_version == version) %>%
    select(all_of(cols)) %>%
    mutate(across(all_of(cols), as.numeric)) %>%  # Convert all selected columns to numeric
    pivot_longer(cols = everything(), names_to = "question", values_to = "response") %>% # Reshape
    mutate(question = recode(question, !!!col_names)) %>% 
    filter(!is.na(response)) %>%  # Exclude missing data
    group_by(question) %>%
    summarise(
      yes_count = sum(response == 1),      # Count "Yes" responses
      total_count = n(),                   # Count total valid responses
      proportion_yes = yes_count / total_count  # Calculate proportion
    ) %>%
    arrange(desc(proportion_yes)) %>% # Order by proportion 
    ggplot(aes(x = reorder(question, -proportion_yes), y = proportion_yes)) +
    geom_col(fill = "steelblue") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +  # Format y-axis as %
    theme_bw() +
    geom_text(aes(label = paste0(scales::percent(proportion_yes, accuracy = 1), "\n(n=", total_count, ")")),
              position = position_stack(vjust = 0.5), color = "white", size = 5) +  # Add % and n
    labs(title = title,
         x = "Question", y = "Proportion of 'Yes' Responses")
}

 

### Confidence

  
# BY ITEM (by country, by version)
conf_stacked_bar <- function(country, version, cols, col_names, title) {
  
  # Fixed color mapping
  conf_colors <- setNames(brewer.pal(4, "Set2"), 
                          c("Not sure", "Somewhat sure", "Pretty sure", "Very sure"))
  
  data_long <- df %>%
    filter(ont_ctry_name == country, ont_version == version) %>%
    select(all_of(cols)) %>%
    mutate(across(all_of(cols), as.numeric)) %>%  # Convert all selected columns to numeric
    pivot_longer(everything(), names_to = "question", values_to = "response") %>%
    mutate(question = recode(question, !!!col_names),
           response_txt = recode(response,
                                 `0` = "Not sure",
                                 `1` = "Somewhat sure",
                                 `2` = "Pretty sure",
                                 `3` = "Very sure")) 
  # get avg response
  item_order <- data_long %>% 
    filter(!is.na(response)) %>% 
    group_by(question) %>%
    summarise(m = mean(response))
  
  # left join avg and reorder 
  data_long <- data_long %>%
    left_join(item_order, by = "question") %>%
    mutate(question = fct_reorder(question, m)) %>%  # Reorder by mean response
    group_by(question, response_txt) %>%
    summarise(n = n(), .groups = "drop")  # Now summarizing does not remove 'm'
  
  data_long %>% ggplot(aes(x = question, y = n, fill = response_txt)) +
    geom_bar(stat = "identity", position = "fill") +
    geom_text(aes(label = paste0("n=", n)), 
              position = position_fill(vjust = 0.5), 
              size = 3, color = "white") +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_manual(values = conf_colors, drop = FALSE) +  # Ensures missing categories retain color
    theme_minimal() +
    labs(x = "", 
         y = "Proportion of Responses", 
         fill = "Confidence", 
         title = title) +
    coord_flip()
}

 

  
# BY CATEGORY 
conf_by_category <- function(country, cols, category_mapping, title) {  
  # Fixed color mapping  
  conf_colors <- setNames(RColorBrewer::brewer.pal(4, "Set2"),  
                          c("Not sure", "Somewhat sure", "Pretty sure", "Very sure"))  
  
  # Filter, reshape, and recode data  
  data_long <- df %>%  
    filter(ont_ctry_name == country) %>%  
    select(all_of(cols)) %>%  
    mutate(across(all_of(cols), as.numeric)) %>%  
    pivot_longer(everything(), names_to = "question", values_to = "response") %>%  
    filter(!is.na(response)) %>%  
    mutate(category = recode(question, !!!category_mapping, .default = NA_character_),  
           response_txt = recode(response,  
                                 `0` = "Not sure",  
                                 `1` = "Somewhat sure",  
                                 `2` = "Pretty sure",  
                                 `3` = "Very sure")) %>%  
    filter(!is.na(category))  
  
  # Compute avg per category  
  cat_order <- data_long %>%
    group_by(category) %>%
    summarise(m = mean(response))
  
  
  # Merge avg and reorder category  
  data_long <- data_long %>%  
    left_join(cat_order, by = "category") %>%  
    mutate(category = fct_reorder(category, m))
  
  # Create stacked bar plot  
  ggplot(data_long, aes(x = category, y = ..count.., fill = response_txt)) +  
    geom_bar(stat = "count", position = "fill") +  
    geom_text(stat = "count", aes(label = paste0("n=", ..count..)),  
              position = position_fill(vjust = 0.5),  
              size = 3, color = "white") +  
    scale_y_continuous(labels = scales::percent) +  
    scale_fill_manual(values = conf_colors, drop = FALSE) +  
    theme_minimal() +  
    labs(x = "",  
         y = "Proportion of Responses",  
         fill = "Confidence",  
         title = title) +  
    coord_flip()  
}

 

  
conf_bar_by_existence <- function(country, version, conf_cols, exist_cols, col_names, title, data = df) {
  stopifnot(length(conf_cols) == length(exist_cols))  # Must be matched pairs
  
  conf_colors <- setNames(RColorBrewer::brewer.pal(4, "Set2"),
                          c("Not sure", "Somewhat sure", "Pretty sure", "Very sure"))
  
  # Prepare list of data frames, one per item
  item_data <- purrr::map2_dfr(conf_cols, exist_cols, function(conf_col, exist_col) {
    data %>%
      filter(ont_ctry_name == country, ont_version == version) %>%
      select(conf = all_of(conf_col), exist = all_of(exist_col)) %>%
      mutate(
        item = col_names[[conf_col]],
        conf = as.numeric(conf),
        exist = as.numeric(exist),
        conf_label = recode(conf,
                            `0` = "Not sure",
                            `1` = "Somewhat sure",
                            `2` = "Pretty sure",
                            `3` = "Very sure"),
        exist_label = recode(exist,
                             `1` = "Said it exists",
                             `0` = "Said it doesn't exist", 
                             `99` = "Said they don't know")
      ) %>%
      filter(!is.na(conf), !is.na(exist))  # Keep only complete pairs
  })
  
  # Count and prep for plotting
  plot_data <- item_data %>%
    group_by(item, exist_label, conf_label) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(item, exist_label) %>%
    mutate(prop = n / sum(n))
  
  # Plot
  ggplot(plot_data, aes(x = item, y = prop, fill = conf_label)) +
    geom_col(position = "fill") +
    geom_text(aes(label = paste0("n=", n)),
              position = position_fill(vjust = 0.5),
              color = "white", size = 3) +
    scale_fill_manual(values = conf_colors, drop = FALSE) +
    scale_y_continuous(labels = scales::percent) +
    coord_flip() +
    facet_wrap(~exist_label) +
    theme_minimal() +
    labs(x = "", y = "Proportion of Confidence Responses",
         fill = "Confidence", title = title)
}

 

### Yes/No plots

  
# this is the same code as for the confidence plots, except I have rigged it up for Yes/No/I don't know responses

# BY ITEM (by country, by version)
yn_stacked_bar <- function(country, version, cols, col_names, title) {
  
  # Fixed color mapping
  conf_colors <- setNames(brewer.pal(3, "Set2"), 
                          c("Yes", "IDK", "No"))
  
  data_long <- df %>%
    filter(ont_ctry_name == country, ont_version == version) %>%
    select(all_of(cols)) %>%
    mutate(across(all_of(cols), as.numeric)) %>%  # Convert all selected columns to numeric
    pivot_longer(everything(), names_to = "question", values_to = "response") %>%
    
    # Recode 99s as 0.5
    mutate(response = ifelse(response == 99, 0.5, response)) %>%
    
    mutate(question = recode(question, !!!col_names),
           response_txt = recode(response,
                                 `0` = "No",
                                 `1` = "Yes",
                                 `0.5` = "IDK"),
           response_txt = factor(response_txt, levels = c("Yes", "IDK", "No"))) 
  # get avg response
  item_order <- data_long %>% 
    filter(!is.na(response)) %>% 
    group_by(question) %>%
    summarise(m = mean(response))
  
  # left join avg and reorder 
  data_long <- data_long %>%
    left_join(item_order, by = "question") %>%
    mutate(question = fct_reorder(question, m)) %>%  # Reorder by mean response
    group_by(question, response_txt) %>%
    summarise(n = n(), .groups = "drop")  # Now summarizing does not remove 'm'
  
  data_long %>% ggplot(aes(x = question, y = n, fill = response_txt)) +
    geom_bar(stat = "identity", position = "fill") +
    geom_text(aes(label = paste0("n=", n)), 
              position = position_fill(vjust = 0.5), 
              size = 3, color = "white") +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_manual(values = conf_colors, drop = FALSE) +  # Ensures missing categories retain color
    theme_minimal() +
    labs(x = "", 
         y = "Proportion of Responses", 
         fill = "Response", 
         title = title) +
    coord_flip()
}

 

  
# YES/NO BY CATEGORY 
yn_by_category <- function(country, cols, category_mapping, title) {  
  # Fixed color mapping  
  conf_colors <- setNames(RColorBrewer::brewer.pal(3, "Set2"),  
                          c("Yes", "IDK", "No"))  
  
  # Filter, reshape, and recode data  
  data_long <- df %>%  
    filter(ont_ctry_name == country) %>%  
    select(all_of(cols)) %>%  
    mutate(across(all_of(cols), as.numeric)) %>%  
    pivot_longer(everything(), names_to = "question", values_to = "response") %>%  
    filter(!is.na(response)) %>%  
    
    # Recode 99s as 0.5
    mutate(response = ifelse(response == 99, 0.5, response)) %>%
    
    mutate(category = recode(question, !!!category_mapping),
           response_txt = recode(response,
                                 `0` = "No",
                                 `1` = "Yes",
                                 `0.5` = "IDK"),
           response_txt = factor(response_txt, levels = c("Yes", "IDK", "No"))) %>%  
    filter(!is.na(category))  
  
  # Compute avg per category  
  cat_order <- data_long %>%
    group_by(category) %>%
    summarise(m = mean(response))
  
  
  # Merge avg and reorder category  
  data_long <- data_long %>%  
    left_join(cat_order, by = "category") %>%  
    mutate(category = fct_reorder(category, m))
  
  # Create stacked bar plot  
  ggplot(data_long, aes(x = category, y = ..count.., fill = response_txt)) +  
    geom_bar(stat = "count", position = "fill") +  
    geom_text(stat = "count", aes(label = paste0("n=", ..count..)),  
              position = position_fill(vjust = 0.5),  
              size = 3, color = "white") +  
    scale_y_continuous(labels = scales::percent) +  
    scale_fill_manual(values = conf_colors, drop = FALSE) +  
    theme_minimal() +  
    labs(x = "",  
         y = "Proportion of Responses",  
         fill = "Response",  
         title = title) +  
    coord_flip()  
}

 

### Specific people

  
# this is the same code as for the confidence plots, except I have rigged it up for anyone/only specific people responses

# BY ITEM (by country, by version)
spexp_stacked_bar <- function(country, version, cols, col_names, title) {
  
  # Fixed color mapping
  conf_colors <- setNames(brewer.pal(3, "Set2"), 
                          c("Anyone", "IDK", "Only specific people"))
  
  data_long <- df %>%
    filter(ont_ctry_name == country, ont_version == version) %>%
    select(all_of(cols)) %>%
    mutate(across(all_of(cols), as.numeric)) %>%  # Convert all selected columns to numeric
    pivot_longer(everything(), names_to = "question", values_to = "response") %>%
    
    # Recode 99s as 0.5
    mutate(response = as.numeric(ifelse(response == 99, 0.5, response))) %>%
    
    mutate(question = recode(question, !!!col_names),
           response_txt = recode(response,
                                 `0` = "Anyone",
                                 `1` = "Only specific people",
                                 `0.5` = "IDK"),
           response_txt = factor(response_txt, levels = c("Anyone", "IDK", "Only specific people"))) 
  # get avg response
  item_order <- data_long %>% 
    filter(!is.na(response)) %>% 
    group_by(question) %>%
    summarise(m = mean(response))
  
  # left join avg and reorder 
  data_long <- data_long %>%
    left_join(item_order, by = "question") %>%
    mutate(question = fct_reorder(question, m)) %>%  # Reorder by mean response
    group_by(question, response_txt) %>%
    summarise(n = n(), .groups = "drop")  # Now summarizing does not remove 'm'
  
  data_long %>% ggplot(aes(x = question, y = n, fill = response_txt)) +
    geom_bar(stat = "identity", position = "fill") +
    geom_text(aes(label = paste0("n=", n)), 
              position = position_fill(vjust = 0.5), 
              size = 3, color = "white") +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_manual(values = conf_colors, drop = FALSE) +  # Ensures missing categories retain color
    theme_minimal() +
    labs(x = "", 
         y = "Proportion of Responses", 
         fill = "Response", 
         title = title) +
    coord_flip()
}

 

  
# Anyone/Specific people BY CATEGORY 
spexp_by_category <- function(country, cols, category_mapping, title) {  
  # Fixed color mapping  
  conf_colors <- setNames(RColorBrewer::brewer.pal(3, "Set2"),  
                          c("Anyone", "IDK", "Only specific people"))  
  
  # Filter, reshape, and recode data  
  data_long <- df %>%  
    filter(ont_ctry_name == country) %>%  
    select(all_of(cols)) %>%  
    mutate(across(all_of(cols), as.numeric)) %>%  
    pivot_longer(everything(), names_to = "question", values_to = "response") %>%  
    filter(!is.na(response)) %>%  
    
    # Recode 99s as 0.5
    mutate(response = ifelse(response == 99, 0.5, response)) %>%
    
    mutate(category = recode(question, !!!category_mapping),
           response_txt = recode(response,
                                 `0` = "Anyone",
                                 `1` = "Only specific people",
                                 `0.5` = "IDK"),
           response_txt = factor(response_txt, levels = c("Anyone", "IDK", "Only specific people"))) %>%  
    filter(!is.na(category))  
  
  # Compute avg per category  
  cat_order <- data_long %>%
    group_by(category) %>%
    summarise(m = mean(response))
  
  
  # Merge avg and reorder category  
  data_long <- data_long %>%  
    left_join(cat_order, by = "category") %>%  
    mutate(category = fct_reorder(category, m))
  
  # Create stacked bar plot  
  ggplot(data_long, aes(x = category, y = ..count.., fill = response_txt)) +  
    geom_bar(stat = "count", position = "fill") +  
    geom_text(stat = "count", aes(label = paste0("n=", ..count..)),  
              position = position_fill(vjust = 0.5),  
              size = 3, color = "white") +  
    scale_y_continuous(labels = scales::percent) +  
    scale_fill_manual(values = conf_colors, drop = FALSE) +  
    theme_minimal() +  
    labs(x = "",  
         y = "Proportion of Responses",  
         fill = "Response",  
         title = title) +  
    coord_flip()  
}

# a custom function to create sensory columns--it looks at the relevant columns for each object as well as where that object was asked about (in which country in which version) to create a new "sensory" column. The sensory column is a 1 if a participant has responded that such an object can be sensed at all (i.e. said yes to one of the sense questions). It is a 0 if they were asked about that object but did not say yes to any of the sensory questions, and an NA if they were not asked about that object

make_sensory_variable <- function(df, object_name, sense_cols, valid_versions) {
  cant_be_sensed_col <- tail(sense_cols, 1)  # last column is the "can't be sensed" column
  other_sense_cols <- head(sense_cols, -1)   # all the other sensory questions
  
  df %>%
    select(ont_ctry_name, ont_version, ont_subj, all_of(sense_cols)) %>%
    rowwise() %>%
    mutate(
      !!paste0("sens_", object_name) := case_when(
        !(ont_ctry_name %in% names(valid_versions)) ~ NA_real_,
        !(ont_version %in% valid_versions[[ont_ctry_name]]) ~ NA_real_,
        any(c_across(all_of(other_sense_cols)) == 1, na.rm = TRUE) ~ 1,
        all(c_across(all_of(sense_cols)) %>% is.na()) ~ NA_real_,
        get(cant_be_sensed_col) == 0 ~ 0,
        TRUE ~ NA_real_
      )
    ) %>%
    ungroup() %>%
    select(ont_subj, !!paste0("sens_", object_name))
}

# creating object metadata to run through function
object_info <- list(
  grm = list(
    cols = c("sci_sens_grm_11", "sci_sens_grm_12", "sci_sens_grm_13",
             "sci_sens_grm_14", "sci_sens_grm_15", "sci_sens_grm_6"),
    versions = list(
      US = 1,
      Ghana = 1,
      Peru = 2
    )
  ), 
  dem = list(
    cols = c("spn_sens_dem_1", "spn_sens_dem_2", "spn_sens_dem_3", "spn_sens_dem_4", 
             "spn_sens_dem_5", "spn_sens_dem_7"), 
    versions = list(
      US = 1,
      Ghana = 1
    )
  ),
  mug = list(
    cols = c("ord_sens_mug_1", "ord_sens_mug_2", "ord_sens_mug_3", "ord_sens_mug_4", 
             "ord_sens_mug_10", "ord_sens_mug_5", "ord_sens_mug_7"), 
    versions = list(
      US = 1, 
      Ghana = 1, 
      Ecuador = 1
    )
  ),
  gvt = list(
    cols = c("sci_sens_gvt_1", "sci_sens_gvt_2", "sci_sens_gvt_3", 
             "sci_sens_gvt_4", "sci_sens_gvt_5", "sci_sens_gvt_7"), 
    versions = list(
      US = 1
    )
  ),
  smn = list(
    cols = c("fic_sens_smn_1", "fic_sens_smn_2", "fic_sens_smn_3", "fic_sens_smn_4", 
             "fic_sens_smn_10", "fic_sens_smn_5", "fic_sens_smn_7"), 
    versions = list(
      US = 1, 
      Ghana = 1, 
      Peru = 2
    )
  ),
  ghs = list(
    cols = c("spn_sens_ghs_1", "spn_sens_ghs_2", "spn_sens_ghs_3", 
             "spn_sens_ghs_4", "spn_sens_ghs_5", "spn_sens_ghs_7"), 
    versions = list(
      US = 2
    )
  ),
  tbl = list(
    cols = c("ord_sens_tbl_1", "ord_sens_tbl_2", "ord_sens_tbl_3", "ord_sens_tbl_4", 
             "ord_sens_tbl_10", "ord_sens_tbl_5", "ord_sens_tbl_7"), 
    versions = list(
      US = 2, 
      Ghana = 2, 
      Ecuador = 2, 
      Peru = 1
    )
  ),
  god = list(
    cols = c("spn_sens_god_1", "spn_sens_god_2", "spn_sens_god_3", "spn_sens_god_4", 
             "spn_sens_god_10", "spn_sens_god_5", "spn_sens_god_7"), 
    versions = list(
      US = 2, 
      Ghana = 2, 
      Ecuador = 2, 
      Peru = 1
    )
  ),
  cnc = list(
    cols = c("sci_sens_cnc_1", "sci_sens_cnc_2", "sci_sens_cnc_3", "sci_sens_cnc_4", 
             "sci_sens_cnc_10", "sci_sens_cnc_5", "sci_sens_cnc_7"), 
    versions = list(
      US = 2, 
      Ghana = 2, 
      Ecuador = 2, 
      Peru = 1
    )
  ),
  hap = list(
    cols = c("fic_sens_hap_1", "fic_sens_hap_2", "fic_sens_hap_3", 
             "fic_sens_hap_4", "fic_sens_hap_5", "fic_sens_hap_7"), 
    versions = list(
      US = 2
    )
  ),
  frc = list(
    cols = c("sci_sens_frc_1", "sci_sens_frc_2", "sci_sens_frc_3", 
             "sci_sens_frc_4", "sci_sens_frc_5", "sci_sens_frc_7"), 
    versions = list(
      Ghana = 1
    )
  ),
  abs = list(
    cols = c("spn_sens_abs_1", "spn_sens_abs_2", "spn_sens_abs_3", 
             "spn_sens_abs_4", "spn_sens_abs_5", "spn_sens_abs_7"), 
    versions = list(
      Ghana = 2
    )
  ),
  kwk = list(
    cols = c("fic_sens_kwk_1", "fic_sens_kwk_2", "fic_sens_kwk_3", 
             "fic_sens_kwk_4", "fic_sens_kwk_5", "fic_sens_kwk_7"), 
    versions = list(
      Ghana = 2
    )
  ),
  ysh = list(
    cols = c("spn_sens_ysh_1", "spn_sens_ysh_2", "spn_sens_ysh_3", "spn_sens_ysh_4", 
             "spn_sens_ysh_10", "spn_sens_ysh_5", "spn_sens_ysh_7"), 
    versions = list(
      Peru = 1
    )
  ),
  par = list(
    cols = c("sci_sens_par_11", "sci_sens_par_12", "sci_sens_par_13", "sci_sens_par_14", 
             "sci_sens_par_16", "sci_sens_par_15", "sci_sens_par_6"), 
    versions = list(
      Ecuador = 1, 
      Peru = 1
    )
  ),
  njt = list(
    cols = c("fic_sens_njt_1", "fic_sens_njt_2", "fic_sens_njt_3", "fic_sens_njt_4", 
             "fic_sens_njt_10", "fic_sens_njt_5", "fic_sens_njt_7"), 
    versions = list(
      Peru = 1
    )
  ),
  cha = list(
    cols = c("spn_sens_cha_1", "spn_sens_cha_2", "spn_sens_cha_3", "spn_sens_cha_4", 
             "spn_sens_cha_10", "spn_sens_cha_5", "spn_sens_cha_7"), 
    versions = list(
      Peru = 1
    )
  ),
  ron = list(
    cols = c("nas_sens_ron_1", "nas_sens_ron_2", "nas_sens_ron_3", "nas_sens_ron_4", 
             "nas_sens_ron_10", "nas_sens_ron_5", "nas_sens_ron_7"), 
    versions = list(
      Peru = 2
    )
  ),
  taz = list(
    cols = c("ord_sens_taz_1", "ord_sens_taz_2", "ord_sens_taz_3", "ord_sens_taz_4", 
             "ord_sens_taz_10", "ord_sens_taz_5", "ord_sens_taz_7"), 
    versions = list(
      Peru = 2
    )
  ),
  oni = list(
    cols = c("spn_sens_oni_1", "spn_sens_oni_2", "spn_sens_oni_3", "spn_sens_oni_4", 
             "spn_sens_oni_10", "spn_sens_oni_5", "spn_sens_oni_7"), 
    versions = list(
      Peru = 2
    )
  ),
  dlp = list(
    cols = c("nas_sens_dlp_1", "nas_sens_dlp_2", "nas_sens_dlp_3", "nas_sens_dlp_4", 
             "nas_sens_dlp_10", "nas_sens_dlp_5", "nas_sens_dlp_7"), 
    versions = list(
      Peru = 2
    )
  ),
  flu = list(
    cols = c("sci_sens_flu_11", "sci_sens_flu_12", "sci_sens_flu_13", "sci_sens_flu_14", 
             "sci_sens_flu_16", "sci_sens_flu_15", "sci_sens_flu_6"), 
    versions = list(
      Ecuador = 1
    )
  ),
  wen = list(
    cols = c("spn_sens_wen_1", "spn_sens_wen_2", "spn_sens_wen_3", "spn_sens_wen_4", 
             "spn_sens_wen_10", "spn_sens_wen_5", "spn_sens_wen_7"), 
    versions = list(
      Ecuador = 1
    )
  ),
  spd = list(
    cols = c("fic_sens_spd_1", "fic_sens_spd_2", "fic_sens_spd_3", "fic_sens_spd_4", 
             "fic_sens_spd_10", "fic_sens_spd_5", "fic_sens_spd_7"), 
    versions = list(
      Ecuador = 1
    )
  ),
  okw = list(
    cols = c("spn_sens_okw_1", "spn_sens_okw_2", "spn_sens_okw_3", "spn_sens_okw_4", 
             "spn_sens_okw_10", "spn_sens_okw_5", "spn_sens_okw_7"), 
    versions = list(
      Ecuador = 2
    )
  ),
  ram = list(
    cols = c("fic_sens_ram_1", "fic_sens_ram_2", "fic_sens_ram_3", "fic_sens_ram_4", 
             "fic_sens_ram_10", "fic_sens_ram_5", "fic_sens_ram_7"), 
    versions = list(
      Ecuador = 2
    )
  )
)

# running the workflow--mapping objects into sensory function

sensory_vars <- map(names(object_info), function(obj_name) {
  make_sensory_variable(
    df = df,
    object_name = obj_name,
    sense_cols = object_info[[obj_name]]$cols,
    valid_versions = object_info[[obj_name]]$versions
  )
})

# this will use the basically the same functions as for "Can X be sensed" but the metadata will be different! (labeling different here)
# this will take together the "usens" columns for seeing, hearing, smelling, etc. to determine the proportion of people in each place who have ever sensed the different objects 

make_usens_variable <- function(df, object_name, usens_cols, valid_versions) {
  havent_sensed_col <- tail(usens_cols, 1)  # last column is the "can't be sensed" column
  other_sense_cols <- head(usens_cols, -1)   # all the other sensory questions
  
  df %>%
    select(ont_ctry_name, ont_version, ont_subj, all_of(usens_cols)) %>%
    rowwise() %>%
    mutate(
      !!paste0("usens_", object_name) := case_when(
        !(ont_ctry_name %in% names(valid_versions)) ~ NA_real_,
        !(ont_version %in% valid_versions[[ont_ctry_name]]) ~ NA_real_,
        any(c_across(all_of(other_sense_cols)) == 1, na.rm = TRUE) ~ 1,
        all(c_across(all_of(usens_cols)) %>% is.na()) ~ NA_real_,
        get(havent_sensed_col) == 0 ~ 0,
        TRUE ~ NA_real_
      )
    ) %>%
    ungroup() %>%
    select(ont_subj, !!paste0("usens_", object_name))
}

# creating object metadata to run through function
object_info_usens <- list(
  grm = list(
    cols = c("sci_usens_grm_1", "sci_usens_grm_2", "sci_usens_grm_3",
             "sci_usens_grm_4", "sci_usens_grm_10", "sci_usens_grm_5", "sci_usens_grm_7"),
    versions = list(
      US = 1,
      Ghana = 1,
      Peru = 2
    )
  ), 
  dem = list(
    cols = c("spn_usens_dem_1", "spn_usens_dem_2", "spn_usens_dem_3", "spn_usens_dem_4", 
             "spn_usens_dem_5", "spn_usens_dem_7"), 
    versions = list(
      US = 1,
      Ghana = 1
    )
  ),
  mug = list(
    cols = c("ord_usens_mug_1", "ord_usens_mug_2", "ord_usens_mug_3", "ord_usens_mug_4", 
             "ord_usens_mug_10", "ord_usens_mug_5", "ord_usens_mug_7"), 
    versions = list(
      US = 1, 
      Ghana = 1, 
      Ecuador = 1
    )
  ),
  gvt = list(
    cols = c("sci_usens_gvt_1", "sci_usens_gvt_2", "sci_usens_gvt_3", 
             "sci_usens_gvt_4", "sci_usens_gvt_5", "sci_usens_gvt_7"), 
    versions = list(
      US = 1
    )
  ),
  smn = list(
    cols = c("fic_usens_smn_1", "fic_usens_smn_2", "fic_usens_smn_3", "fic_usens_smn_4", 
             "fic_usens_smn_11", "fic_usens_smn_5", "fic_usens_smn_7"), 
    versions = list(
      US = 1, 
      Ghana = 1, 
      Peru = 2
    )
  ),
  ghs = list(
    cols = c("spn_usens_ghs_1", "spn_usens_ghs_2", "spn_usens_ghs_3", 
             "spn_usens_ghs_4", "spn_usens_ghs_5", "spn_usens_ghs_7"), 
    versions = list(
      US = 2
    )
  ),
  tbl = list(
    cols = c("ord_usens_tbl_1", "ord_usens_tbl_2", "ord_usens_tbl_3", "ord_usens_tbl_4", 
             "ord_usens_tbl_10", "ord_usens_tbl_5", "ord_usens_tbl_7"), 
    versions = list(
      US = 2, 
      Ghana = 2, 
      Ecuador = 2, 
      Peru = 1
    )
  ),
  god = list(
    cols = c("spn_usens_god_1", "spn_usens_god_2", "spn_usens_god_3", "spn_usens_god_4", 
             "spn_usens_god_10", "spn_usens_god_5", "spn_usens_god_7"), 
    versions = list(
      US = 2, 
      Ghana = 2, 
      Ecuador = 2, 
      Peru = 1
    )
  ),
  cnc = list(
    cols = c("sci_usens_cnc_1", "sci_usens_cnc_2", "sci_usens_cnc_3", "sci_usens_cnc_4", 
             "sci_usens_cnc_11", "sci_usens_cnc_5", "sci_usens_cnc_7"), 
    versions = list(
      US = 2, 
      Ghana = 2, 
      Ecuador = 2, 
      Peru = 1
    )
  ),
  hap = list(
    cols = c("fic_usens_hap_1", "fic_usens_hap_2", "fic_usens_hap_3", 
             "fic_usens_hap_4", "fic_usens_hap_5", "fic_usens_hap_7"), 
    versions = list(
      US = 2
    )
  ),
  frc = list(
    cols = c("sci_usens_frc_1", "sci_usens_frc_2", "sci_usens_frc_3", 
             "sci_usens_frc_4", "sci_usens_frc_5", "sci_usens_frc_7"), 
    versions = list(
      Ghana = 1
    )
  ),
  abs = list(
    cols = c("spn_usens_abs_1", "spn_usens_abs_2", "spn_usens_abs_3", 
             "spn_usens_abs_4", "spn_usens_abs_5", "spn_usens_abs_7"), 
    versions = list(
      Ghana = 2
    )
  ),
  kwk = list(
    cols = c("fic_usens_kwk_1", "fic_usens_kwk_2", "fic_usens_kwk_3", 
             "fic_usens_kwk_4", "fic_usens_kwk_5", "fic_usens_kwk_7"), 
    versions = list(
      Ghana = 2
    )
  ),
  ysh = list(
    cols = c("spn_usens_ysh_1", "spn_usens_ysh_2", "spn_usens_ysh_3", "spn_usens_ysh_4", 
             "spn_usens_ysh_10", "spn_usens_ysh_5", "spn_usens_ysh_7"), 
    versions = list(
      Peru = 1
    )
  ),
  par = list(
    cols = c("sci_usens_par_1", "sci_usens_par_2", "sci_usens_par_3", "sci_usens_par_4", 
             "sci_usens_par_10", "sci_usens_par_5", "sci_usens_par_7"), 
    versions = list(
      Ecuador = 1, 
      Peru = 1
    )
  ),
  njt = list(
    cols = c("fic_usens_njt_1", "fic_usens_njt_2", "fic_usens_njt_3", "fic_usens_njt_4", 
             "fic_usens_njt_11", "fic_usens_njt_5", "fic_usens_njt_7"), 
    versions = list(
      Peru = 1
    )
  ),
  cha = list(
    cols = c("spn_usens_cha_1", "spn_usens_cha_2", "spn_usens_cha_3", "spn_usens_cha_4", 
             "spn_usens_cha_10", "spn_usens_cha_5", "spn_usens_cha_7"), 
    versions = list(
      Peru = 1
    )
  ),
  ron = list(
    cols = c("nas_usens_ron_1", "nas_usens_ron_2", "nas_usens_ron_3", "nas_usens_ron_4", 
             "nas_usens_ron_10", "nas_usens_ron_5", "nas_usens_ron_7"), 
    versions = list(
      Peru = 2
    )
  ),
  taz = list(
    cols = c("ord_usens_taz_1", "ord_usens_taz_2", "ord_usens_taz_3", "ord_usens_taz_4", 
             "ord_usens_taz_10", "ord_usens_taz_5", "ord_usens_taz_7"), 
    versions = list(
      Peru = 2
    )
  ),
  oni = list(
    cols = c("spn_usens_oni_1", "spn_usens_oni_2", "spn_usens_oni_3", "spn_usens_oni_4", 
             "spn_usens_oni_10", "spn_usens_oni_5", "spn_usens_oni_7"), 
    versions = list(
      Peru = 2
    )
  ),
  dlp = list(
    cols = c("nas_usens_dlp_1", "nas_usens_dlp_2", "nas_usens_dlp_3", "nas_usens_dlp_4", 
             "nas_usens_dlp_10", "nas_usens_dlp_5", "nas_usens_dlp_7"), 
    versions = list(
      Peru = 2
    )
  ),
  flu = list(
    cols = c("sci_usens_flu_1", "sci_usens_flu_2", "sci_usens_flu_3", "sci_usens_flu_4", 
             "sci_usens_flu_10", "sci_usens_flu_5", "sci_usens_flu_7"), 
    versions = list(
      Ecuador = 1
    )
  ),
  wen = list(
    cols = c("spn_usens_wen_1", "spn_usens_wen_2", "spn_usens_wen_3", "spn_usens_wen_4", 
             "spn_usens_wen_10", "spn_usens_wen_5", "spn_usens_wen_7"), 
    versions = list(
      Ecuador = 1
    )
  ),
  spd = list(
    cols = c("fic_usens_spd_1", "fic_usens_spd_2", "fic_usens_spd_3", "fic_usens_spd_4", 
             "fic_usens_spd_11", "fic_usens_spd_5", "fic_usens_spd_7"), 
    versions = list(
      Ecuador = 1
    )
  ),
  okw = list(
    cols = c("spn_usens_okw_1", "spn_usens_okw_2", "spn_usens_okw_3", "spn_usens_okw_4", 
             "spn_usens_okw_10", "spn_usens_okw_5", "spn_usens_okw_7"), 
    versions = list(
      Ecuador = 2
    )
  ),
  ram = list(
    cols = c("fic_usens_ram_1", "fic_usens_ram_2", "fic_usens_ram_3", "fic_usens_ram_4", 
             "fic_usens_ram_11", "fic_usens_ram_5", "fic_usens_ram_7"), 
    versions = list(
      Ecuador = 2
    )
  )
)

# running the workflow--mapping usens objects into sensory function

usens_vars <- map(names(object_info_usens), function(obj_name) {
  make_usens_variable(
    df = df,
    object_name = obj_name,
    usens_cols = object_info_usens[[obj_name]]$cols,
    valid_versions = object_info_usens[[obj_name]]$versions
  )
})


 