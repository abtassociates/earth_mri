# clean raw input file into csv tables 

# Publications

# review unique values for Publisher and clean them ---------------------
#write.csv(unique(child_pubs$X_LLM_Publisher), here("data","tables","manual_review", "child_publishers.csv"))

publisher_lu <- read.csv(here("data","tables","manual_review", "child_publishers.csv"))

child_pubs <- child_pubs %>% left_join(publisher_lu) %>%
  mutate(Publisher = case_when(!is.na(replacement) & replacement!="" ~ replacement,
                               !is.na(X_LLM_Publisher) ~ X_LLM_Publisher, # take value if available
                               grepl("dggs.alaska", `Publication Link` ) ~ "Alaska Division of Geological & Geophysical Surveys",
                               grepl("usgs.gov", `Publication Link` ) ~ "U.S. Geological Survey",
                               TRUE ~ NA)) %>%
  select(-replacement, - X_LLM_Publisher)

rm(publisher_lu)

# review missing publishers -------------------------------------------
#write.csv(child_pubs %>% filter(is.na(Publisher)), here("data","tables","manual_review", "child_no_publishers.csv"))
# lots to review, skipping for now since it won't affect the author analysis


# review duplicate links ------------------------------

length(unique(child_pubs$`Publication Link`)) #522 different links for Publications

unq_child <- child_pubs %>% select(`Publication Link`, Title, Publisher) %>% unique # but 530 different combos of link, title publisher
write.csv(unq_child, here("data","tables","manual_review", "child_unique.csv"))

child_pubs$row_id <- 1:nrow(child_pubs)
links <- unq_child$`Publication Link`[which(duplicated(unq_child$`Publication Link`))] # get duplicated links

dup_rows <- child_pubs %>% filter(`Publication Link`%in% links) %>%
  arrange(Title ) %>% select(row_id, `Publication Link`, Title, Publisher, `Parent Title`, `Parent Cited By`, everything())
#write.csv(x, here("data","tables","manual_review", "child_dup_links.csv"))

dup_rows <- read.csv(here("data","tables","manual_review", "child_dup_links.csv"))
dup_rows <- add_new_authors(review_df = dup_rows, 
                            authors_df = authors)
#dup_rows[["new_authors"]]
authors <- dup_rows[["authors"]]
dup_rows <- dup_rows[["review_df"]]

# drop rows from child_pubs and full join with dup_rows to make these replacements

colnames(child_pubs) <- gsub(" ", ".", colnames(child_pubs))

child_pubs <- child_pubs %>% filter(!row_id %in% dup_rows$row_id) %>% 
  full_join(dup_rows) %>% arrange(row_id) %>% select(-Notes,-row_id)
rm(dup_rows,links)
# create pub_id and merge parent_pub_id ---------------------------------

# Now, create the child pub_id with unique Link, Title and Publisher (which match the length of unique links)
length(unique(child_pubs$Publication.Link)) #529 different links for Publications

unq_child <- child_pubs %>% select(Publication.Link, Title, Publisher) %>% unique # 529 unique link/title/publisher, these will be our child ids
unq_child$pub_id <- max(parent_pubs$pub_id) + 1:nrow(unq_child)

child_pubs <- child_pubs %>% full_join(unq_child) %>% # get pub_id in child_pubs 
  left_join(parent_pubs %>% select(Publication_Link,pub_id) %>% rename(parent_pub_id = pub_id), 
            by = c("Parent.Publication.Link" = "Publication_Link"))

unq_child <- child_pubs %>% select(-Parent.Publication.Link, - Parent.Title, - Parent.Cited.By, - parent_cited_by_link) %>%
  group_by(pub_id) %>% mutate(parent_pub_id = paste0(unique(parent_pub_id), collapse = "; "),
                              X_LLM_Methods = paste0(unique(X_LLM_Methods), collapse = "; "),
                              X_LLM_Study_Area = paste0(unique(X_LLM_Study_Area), collapse = "; "),
                              X_LLM_Keywords = paste0(unique(X_LLM_Keywords), collapse = "; "),
                              X_LLM_Abstract = paste0(unique(X_LLM_Abstract), collapse = "; "),
                              X_LLM_Authors = paste0(unique(X_LLM_Authors), collapse = "; "),
                              X_LLM_Publisher = paste0(unique(X_LLM_Publisher), collapse = "; "),
                              X_LLM_Journal = paste0(unique(X_LLM_Journal), collapse = "; "),
                              X_LLM_Title = paste0(unique(X_LLM_Title), collapse = "; "),
                              Cited.By.Link = paste0(unique(Cited.By.Link), collapse = "; "),
                              child_position = paste0(unique(child_position), collapse = "; ")) %>% unique
unq_child$row_id <- 1:nrow(unq_child)

unq_dups <-unq_child[which(duplicated(unq_child %>% select(Publication.Link, Title, Publisher))),] 
unique(unq_dups$pub_id)
review_dups <- unq_child %>% filter(pub_id %in% unq_dups$pub_id)
#write.csv(review_dups, here("data","tables","manual_review",  "child_dups.csv"))

review_dups <- read.csv(here("data","tables","manual_review",  "child_dups.csv"))
review_dups <- add_new_authors(review_df = review_dups, 
                            authors_df = authors)
#review_dups[["new_authors"]]
authors <- review_dups[["authors"]]
review_dups <- review_dups[["review_df"]]

child_pubs <- unq_child %>% filter(!row_id %in% review_dups$row_id) %>%
  full_join(review_dups) %>% filter(is.na(Drop)) %>% select(-Drop, -Notes, -row_id)

rm(review_dups, unq_dups, unq_child)

# clean the authors ---------------------------------

# I want to use Authors_Clean instead of Authors_Parsed, because 
# I want to check before the AI made the Author Inventory
author_counts <- lapply(child_pubs$X_LLM_Authors_Clean, FUN = function(x){
  if(is.na(x)){return(0)
  }else{
    length(unlist(strsplit(x, split= ";")) ) 
    }
  })

child_pubs$count_authors <- unlist(author_counts)
table(child_pubs$count_authors)

# review 8 child publications with no authors ------------------------
# write.csv(child_pubs %>% filter(count_authors==0), here("data","tables","manual_review", "child_pubs_no_authors.csv"))

colnames(child_pubs)

# reviewed and added authors 
child_manual <- read.csv(here("data","tables","manual_review", "child_pubs_no_authors.csv")) 
child_manual <- add_new_authors(review_df = child_manual, 
                                 authors_df = authors)
#child_manual[["new_authors"]]
authors <- child_manual[["authors"]]
child_manual <- child_manual[["review_df"]] 

child_manual <- child_manual %>%
  mutate(child_position=as.character(child_position),
         parent_pub_id=as.character(parent_pub_id))

child_pubs <- child_pubs %>% filter(!pub_id %in% child_manual$pub_id)  %>% 
  full_join(child_manual)
rm(child_manual)

# recount authors
author_counts <- lapply(child_pubs$X_LLM_Authors_Parsed, FUN = function(x){
  if(is.na(x)){return(0)
  }else{
    length(unlist(strsplit(x, split= ";")) ) 
  }
})

child_pubs$count_authors_parsed <- unlist(author_counts)
table(child_pubs$count_authors_parsed) 

# now count the author IDs
author_id_counts <- lapply(child_pubs$X_LLM_Authors_IDs, FUN = function(x){
  if(is.na(x)){return(0)
  }else{
    length(unlist(strsplit(x, split= ";")) ) 
  }
})

child_pubs$count_author_ids <- unlist(author_id_counts)
rm(author_counts, author_id_counts)

# review  child publications with number of author ids NOT equal to number of authors in 'Authors_Clean' ------------
# This includes the 8 publications above
#write.csv(child_pubs %>% filter(count_authors != count_authors_parsed), here("data","tables","manual_review", "child_pubs_author_ids.csv"))

child_manual <- read.csv(here("data","tables","manual_review", "child_pubs_author_ids.csv")) %>% select(-X, - Notes)
child_manual <- add_new_authors(review_df = child_manual, 
                                authors_df = authors)
#View(child_manual[["new_authors"]])
authors <- child_manual[["authors"]]
child_manual <- child_manual[["review_df"]] 

# join with child_pubs
child_pubs <- child_pubs %>% filter(!pub_id %in% child_manual$pub_id) %>% full_join(child_manual) %>% 
  select(-count_authors, -count_authors_parsed, - count_author_ids, -Notes,
         -X_LLM_Authors, - X_LLM_Authors_Clean)
rm(child_manual)

colnames(child_pubs)





# clean Date/Year --------------------

# all those missing years are also missing dates so this captures all
# review child publications with missing date ----------------------------
#write.csv(child_pubs %>% filter(is.na(child_year_guess)), here("data","tables","manual_review","child_pubs_no_year.csv"))

child_manual <- read.csv(here("data","tables","manual_review", "child_pubs_no_year.csv")) %>%
  select(-X,-Notes)
child_manual <- add_new_authors(review_df = child_manual, 
                                authors_df = authors)
#View(child_manual[["new_authors"]])
authors <- child_manual[["authors"]]
child_manual <- child_manual[["review_df"]] 

# join with child_pubs
child_pubs <- child_pubs %>% filter(!pub_id %in% child_manual$pub_id) %>% full_join(child_manual) 
rm(child_manual)

# review all child_pubs -----------------
#write.csv(child_pubs , here("data","tables","manual_review","child_pubs.csv"))
child_manual <- read.csv(here("data","tables","manual_review", "child_pubs.csv")) %>%
  select(-Notes)
child_manual <- add_new_authors(review_df = child_manual, 
                                authors_df = authors, llm_colnames = FALSE)
#View(child_manual[["new_authors"]])
authors <- child_manual[["authors"]]
child_pubs <- child_manual[["review_df"]] 
rm(child_manual)

# review just the authors that appear in the parent publications
# create table of pub_ids and author_ids
library(tidyr)
child_pub_authors <- child_pubs %>% select(pub_id, Authors, Author_IDs) %>%
  separate_longer_delim(c(Authors, Author_IDs), delim = "; ") %>%
  mutate(Author_IDs = as.numeric(Author_IDs)) %>%
  left_join(authors, by = c("Authors" = "author_name")) %>%
  mutate(author_id = ifelse(is.na(author_id), Author_IDs, author_id)) %>%
  filter(!is.na(author_id)) %>% unique


authors_child <- authors %>% filter(author_id %in% child_pub_authors$author_id)
# review all authors that appear in parent publications
# write.csv(authors_child, here("data","tables","manual_review","authors_child.csv"))

# I cleaned up the author_name value and added column 'Notes' and 'replacement' to combine some authors
authors_child <- read.csv(here("data","tables","manual_review","authors_child.csv")) %>%
  filter(author_id != 935 &# remove: PhD
           author_id != 1691  # remove: Melissa R. which was added to author_id 1690
  )

child_pub_authors %>% filter(author_id != 935 & # remove: PhD
                             author_id != 1691, # remove: Melissa R. which was added to author_id 1690
                             )

replacements <- authors_child %>%  filter(!is.na(replacement))

for(i in 1:nrow(replacements)){ # replace the author_id in the parent_pubs_authors table
  child_pub_authors <- child_pub_authors %>%
    mutate(author_id = ifelse(author_id==replacements$author_id[i],
                            replacements$replacement[i], author_id))
} 
rm(replacements,i)
authors_child <- authors_child %>% filter(is.na(replacement)) # drop from authors_parent

# any parent replacements remaining?
replacements <- parent_replacements %>% filter(author_id %in% child_pub_authors$author_id)
all(!replacements$replacement %in% child_pub_authors$author_id)
# add them to child_authors and remove the original
authors_parent_replace <- authors_parent %>% filter(author_id %in% replacements$replacement)
authors_child <- authors_child %>% full_join(authors_parent_replace) %>% 
  filter(!author_id %in% replacements$author_id)

for(i in 1:nrow(replacements)){ # replace the author_id in the child_pubs_authors table
  child_pub_authors <- child_pub_authors %>%
    mutate(author_id = ifelse(author_id==replacements$author_id[i],
                              replacements$replacement[i], author_id))
} 
rm(authors_parent_replace, parent_replacements,replacements,i)

write.csv(authors_child, here("data","tables","child_authors.csv"))


# now for all rows of parent_pub_authors, replace author_name

child_pub_authors <- child_pub_authors %>% select(pub_id, author_id) %>% 
  mutate(author_id = as.numeric(trimws(author_id))) %>%
  left_join(authors_child, by= "author_id") %>%
  rename(Authors = author_name) %>%
  select(pub_id, Authors, author_id)

write.csv(parent_pub_authors, here("data","tables","child_pub_authors.csv"))

# Now lets summarise back to publications to replace in parent_pubs

child_pub_authors1 <- child_pub_authors %>% group_by(pub_id) %>%
  summarise(Authors = paste0(Authors, collapse = "; "),
            Author_IDs = paste0(author_id, collapse = "; "))

child_pubs <- child_pubs %>% select(-Authors, - Author_IDs) %>%
  left_join(child_pub_authors1, by = "pub_id") %>%
  rename(Year = child_year_guess,
         Publication_Link = Publication.Link,
         Cited_By = Cited.By,
         Cited_By_Link = Cited.By.Link)
rm(child_pub_authors1)

# save parent publications -------------------------

child_pubs <- child_pubs %>% select(pub_id, Year, Publisher, Title, Authors, Author_IDs, 
                                      Publication_Link, everything())
write.csv(child_pubs, here("data","tables","child_publications.csv"), row.names = FALSE)



