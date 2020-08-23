time_impute = function(df,
                       ...,
                       cols_to_impute = c('Value', 'lowerCI', 'upperCI'),
                       year_column = 'year'){
  
  # input handling
  dots = as.character(ensyms(...))
  
  # processing
  print('Missingness before imputation (%):')
  print(colMeans(is.na(df[, cols_to_impute])))
  
  # For progress bar, though it slows the algorithm surpisingly  
  # count = 0
  # total_combs = prod(apply(df %>%
  #                            select(...), 2, function(x) length(unique(x))))
  
  t0 = Sys.time()
  
  combinations = df %>% select(...) %>% distinct()
  
  thePlaceToBe = environment()
  apply(combinations, 1, function(current_comb){
    
    ## For progress bar too 
    # assign('count', count+1, envir = parent.frame(2))
    # cat(paste('\r', count, '/', total_combs))
    
    
    
    #make a bit more complex call that are not possible (or idk how to do)
    # with dots
    str1 = ''
    str2 = ''
    for (i in 1:length(current_comb)){
      if (is.numeric(current_comb[i])){
        str1 = paste(str1, paste0(dots[i], " == ", current_comb[i]), sep = " , ")
        str2 = paste(str2, paste0(dots[i], " == ", current_comb[i]), sep = " & ")
      } else {
        str1 = paste(str1, paste0(dots[i], " == '", current_comb[i], "'"), sep = " , ")
        str2 = paste(str2, paste0(dots[i], " == '", current_comb[i], "'"), sep = " & ")
      }
    }
    
    # because of how I implemented this there will be a , or & at the start
    str1 = stringr::str_replace(str1, ',', '')
    str2 = stringr::str_replace(str2, '&', '')
    
    #########################
    
    # assign1 = ''
    # assign2 = ''
    for (col in cols_to_impute){
      assign1 = paste0(col, ' = ifelse(',str2,',
                 na_locf(', col,'[',str2,']),', col,')')
      assign2 = paste0(col, ' = ifelse(',str2,',
                 na_interpolation(', col,'[',str2,']),', col,')')
      
      # to debug set up a toggle breakpoint and do cat(assign2) or cat(str2) to understand the code
      assign1 = trimws(assign1, whitespace = ',\n')
      assign2 = trimws(assign2, whitespace = ',\n')
      
      current_vals = as.vector(
        eval(
          parse(
            text = paste0('(df %>% filter(', str2, ') %>% select(Value))$Value')
          )
        )
      )
      
      ## stop if years are not sorted
      stopifnot(
        !is.unsorted(
          eval(
            parse(text = paste0('df %>%
      filter(', str2, ') %>%
      select(', year_column, ')')
            )
          )
        )
      )
      
      if (all(is.na(current_vals)) | sum(is.na(current_vals))==0){
        #if all values missing don't impute
        # or all values present pass
        # I do not need to make this explicit but well it does not hurt
        # next does not work using apply
        return()
      } else if (sum(!is.na(current_vals))== 1){
        #if single value is available use locf
        eval(parse(text = paste0("assign('df', value = df %>% mutate(",
                                 assign1,'), envir = thePlaceToBe)')))
      } else {
        # cat(paste('Before:', df %>% filter(eval(parse( text = str2))) %>% select(Value)), '\n')
        eval(parse(text = paste0("assign('df', value = df %>% mutate(",
                                          assign2,'), envir = thePlaceToBe)')))
        # cat(paste('After:', df %>% filter(eval(parse( text = str2))) %>% select(Value)), '\n\n')
      }
      # for debugging
      # df %>% mutate(eval(parse(text = str2))) %>% filter(eval(parse(text = str2)))
      # df %>% filter(eval(parse( text = str2))) %>% select(Value, lowerCI, upperCI, yearInterval)
      
    }
  }
  )
  
  print(Sys.time()-t0)
  print('Missingness after imputation (%):')
  print(colMeans(is.na(df[, cols_to_impute])))
  
  return(df)
  
}