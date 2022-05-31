report_chisq_test <- function(tab, in_brackets = F, effsize = c("cramer"), test = c("Pearson")){
  stats <- tab %>% vcd::assocstats()
  eff_size_labels <- c("cramer" =  "Cramer's V")
  test_label <- c("X^2" = "Chi^2")
  ret <- sprintf("Chi-squared(%.0f) = %.2f, p = %.3f, %s = %.2f", 
                 stats$chisq_tests[,"df"][["Pearson"]], 
                 stats$chisq_tests[,"X^2"][["Pearson"]],
                 stats$chisq_tests[,"P(> X^2)"][["Pearson"]],
                 eff_size_labels[effsize],
                 stats[[effsize]]
  )
  if(in_brackets){
    ret <- sprintf("(%s)", ret)
  }
  ret
}

tidy_assoc_stats <- function(as){
  if(class(as) != "assocstats"){
    tryCatch({
      as <- vcd::assocstats(as)
    }, 
    error = function(e){
      browser()
    })  
  }
  as$chisq_tests %>% 
    as_tibble() %>% 
    set_names(c("statistic", "df", "p_value")) %>% 
    mutate(test = c("llr", "chisq")) %>% 
    pivot_wider(names_from = test, values_from = everything()) %>% select(-starts_with("test_")) %>%  
    
    bind_cols(tibble(phi = as$phi, contingency = as$contingency, cramers_v = as$cramer))
}

cmp_groups <- function(data, focus_var, focus_group, group_labels = NULL, with_plot = T, ...){
  if(is.null(group_labels)){
    group_labels <- factor(data[[focus_group]]) %>% levels()
  }
  cmp_tab <- table(data[[focus_group]], data[[focus_var]])
  dimnames(cmp_tab)[[1]] <- group_labels
  stats <- vcd::assocstats(cmp_tab)
  q <- NULL
  if(with_plot && length(group_labels) <= 2){
    q <- parkR:::ggassoc2(cmp_tab, ...)  
  }
  p_table <- cmp_tab %>% prop.table(1)
  list(stats = tidy_assoc_stats(stats), proportions = p_table, diff_p = p_table[1,] - p_table[2,], plot = q)
}

get_pattern_branch <- function(bs, value, pre = 0, post = 0, side = c("left", "right")){
  side <- match.arg(side)
  i <- 1
  more <- TRUE
  ret <- list()
  while(more){
    #browser()
    if(side == "left"){
      branch <- get_value_context(bs, value, pre = i, post = post)
    }
    else{
      branch <- get_value_context(bs, value, pre = pre, post = i)
    }
    branch <- branch %>% filter(!str_detect(value, "NA"))
    ret[[i]] <- branch %>% distinct(value, n_xy, N) %>% arrange(value)   
    more <- nrow(branch %>% filter(n_xy > 1))
    i <- i + 1
  }
  return(ret)  
}

get_pattern_sheaf <- function(bs, value){
  left <- get_pattern_branch(bs, value, post = 0, side = "left")
  right <- get_pattern_branch(bs, value, pre = 0, side = "right")
  max_left <- length(left) -1 
  max_right <- length(right) - 1
  ret <- list()
  if(max_left > 0 && max_right > 0){
    for(l in 1:max_left){
      for(r in 1:max_right){
        #browser()
        printf("l = %d, r = %d", l, r)
        br <-  get_value_context(bs, value, pre = l, post = r) %>% filter(!str_detect(value, "NA"))
        ret[[sprintf("%d_%d", l, r)]]  <- br %>% distinct(value, n_xy, N)
      }
    }
  }
  browser()
  bind_rows(left, right, bind_rows(ret))
}