
# function to add new authors from a review file to the authors dataframe 

add_new_authors <- function(review_df, authors_df, llm_colnames = TRUE){
  
  # add new authors to author.csv table
  new_authors <- unique(strsplit(as.character(review_df$NEW_AUTHORS), split= ";") %>% unlist)
  new_authors <- trimws( new_authors[!is.na(new_authors)])
  new_authors <- data.frame(author_name = new_authors)
  
  # replicate the cleaning we did in 'clean_authors.r'
  names <- stringr::str_split(new_authors$author_name, ' |, ')
  new_authors$last_name <- unlist(lapply(names, FUN=function(x){x[[1]]}))
  
  new_authors$first_name <- unlist(lapply(names, FUN=function(x){
    if(length(x)>1){ return(gsub("\\.","", x[[2]]))
    }else{ return(NA)
    }}))
  
  new_authors$middle_name <- unlist(lapply(names, FUN=function(x){
    if(length(x)>2){ return(gsub("\\.","", paste(x[3:length(x)], collapse = " ")))
    }else{ return(NA)
    }}))
  rm(names)
  new_authors <- new_authors %>% # fix san juan, c. a. 
    mutate( first_name = ifelse(last_name=="San", "C", first_name),
            middle_name = ifelse(last_name=="San", "A", middle_name),
            extra = ifelse(last_name=="San", "San", NA),
            last_name = ifelse(last_name=="San", "Juan", last_name)
    )
  new_authors$author_id <- max(authors_df$author_id) + 1:nrow(new_authors)
  
  
  authors_df <- authors_df %>% full_join(new_authors)
  
  write.csv(authors_df, here("data","tables","authors.csv"), row.names = FALSE)
  
  if(llm_colnames){
    # Add these new authors and author_ids to columns X_LLM_Authors_IDs & X_LLM_Authors_Parsed
    for(i in 1:nrow(new_authors)){
      review_df <- review_df %>% 
        mutate(X_LLM_Authors_IDs = ifelse(grepl(new_authors$author_name[i], NEW_AUTHORS),
                                          paste(X_LLM_Authors_IDs, new_authors$author_id[i], sep = "; "),
                                          X_LLM_Authors_IDs),
               X_LLM_Authors_Parsed = ifelse(grepl(new_authors$author_name[i], NEW_AUTHORS),
                                             paste(X_LLM_Authors_Parsed, new_authors$author_name[i], sep = "; "),
                                             X_LLM_Authors_Parsed),
        )
    }
  }else{
    # Add these new authors and author_ids to columns Author_IDs & Authors
    for(i in 1:nrow(new_authors)){
      review_df <- review_df %>% 
        mutate(Author_IDs = ifelse(grepl(new_authors$author_name[i], NEW_AUTHORS),
                                          paste(Author_IDs, new_authors$author_id[i], sep = "; "),
                                   Author_IDs),
               Authors = ifelse(grepl(new_authors$author_name[i], NEW_AUTHORS),
                                             paste(Authors, new_authors$author_name[i], sep = "; "),
                                Authors),
        )
    }
  }
 
  
  return(list("new_authors" = new_authors,
              "authors" = authors_df,
              "review_df" = review_df %>% select(-NEW_AUTHORS)))
}