create_verb_df <- function(verb = 'falar',
                           na_to_blank = TRUE){
  
  require(dplyr)
  
  # Get data using perl program
  x <- base::system(paste0('conjug ', verb), intern = TRUE)
  
  # Remove the verb name
  x <- x[-1]
  
  # Turn into dataframe
  first_entry <- unlist(lapply(strsplit(x, ' '), function(z){z[1]}))
  df <- expand.grid(verb = first_entry[1],
                    form = first_entry[1:length(first_entry)])
  
  # Define meaning of each form
  meanings <- 
    c( 'pres' = 'Presente', 
       'perf' = 'Perfeito', 
       'imp'  = 'Imperfeito', 
       'fut'  = 'Futuro',
       'mdp'  = 'Mais Que Perfeito', 
       'cpres'= 'Conjuntivo Presente', 
       'cimp' = 'Conjuntivo Imperfeito', 
       'cond' = 'Condicional',
       'cfut' = 'Conjuntivo Futuro', 
       'ivo'  = 'Imperativo', 
       'pp'   = 'Particípio Passado',
       'grd'  = 'Gerundivo')
  meanings_df <- data.frame(form = names(meanings),
                            meaning = meanings)
  meanings_df$form <- as.character(meanings_df$form)
  df$form <- as.character(df$form)
  df <- left_join(x = df,
                  y = meanings_df,
                  by = 'form')
  
  
  # Add columns for 1st, 2nd and 3rd persons, as well as 
  # singular (s) and plural (p)
  new_columns <- c(paste0('s',
                        1:3),
                   paste0('p',
                          1:3))
  new_columns <- c(new_columns)
  for (j in 1:length(new_columns)){
    df[,new_columns[j]] <- NA
  }
  df$other <- NA
  
  # Go through each row of the raw data and add to dataframe
  for (i in 1:nrow(df)){
    
    # Isolate the row
    this_row <- x[i]
    
    # Get the individual words
    the_words <- unlist(strsplit(x = this_row, split = ' '))
    the_words <- the_words[the_words != '']
    the_words <- the_words[2:length(the_words)]
    
    # Add those words to the dataframe
    if(length(the_words) == 6){
      df[i, new_columns] <- the_words
    } else 
    if(length(the_words) == 1){
      df[i, 'other'] <- the_words
    } 
  }
  for(j in 1:ncol(df)){
    df[,j] <- as.character(df[,j])
  }
  if(na_to_blank){
    for(j in 1:ncol(df)){
      df[,j][is.na(df[,j])] <- ''
    }
  }
  return(df)
}

