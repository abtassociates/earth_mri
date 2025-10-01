
### clean parent publications 

# clean the publishers -----------------------
parent_pubs <- parent_pubs %>% 
  mutate(Publisher = case_when(X_LLM_Publisher == "USGS" ~ "U.S. Geological Survey", # recode acroymns
                               X_LLM_Publisher == "AGU" ~ "American Geophyscial Union", 
                               !is.na(X_LLM_Publisher) ~ X_LLM_Publisher, # take value if available
                               grepl("dggs.alaska", `Publication Link` ) ~ "Alaska Division of Geological & Geophysical Surveys",
                               grepl("usgs.gov", `Publication Link` ) ~ "U.S. Geological Survey",
                               TRUE ~ NA))
# review X publications with no publishers -----------------------
#write.csv(parent_pubs %>% filter(is.na(Publisher)), here("data","tables","manual_review", "parent_pubs_publisher.csv"))

parent_manual <- read.csv(here("data","tables","manual_review", "parent_pubs_publisher.csv")) %>% 
  select(Title, X_LLM_Publisher, Publisher)

parent_pubs <- parent_pubs %>% left_join(parent_manual, by = c("Title", "X_LLM_Publisher")) %>%
  mutate(Publisher = ifelse(is.na(Publisher.y), Publisher.x, Publisher.y)) %>%
  select(-Publisher.x, - Publisher.y, -X_LLM_Publisher)

parent_pubs$pub_id <- 1:nrow(parent_pubs) # unique Title & Publisher
rm(parent_manual)

# clean the authors ---------------------------------

# I want to use Authors_Clean instead of Authors_Parsed, because 
# I want to check before the AI made the Author Inventory
author_counts <- lapply(parent_pubs$X_LLM_Authors_Clean, FUN = function(x){
  if(is.na(x)){return(0)
  }else{
    length(unlist(strsplit(x, split= ";")) ) 
    }
  })

parent_pubs$count_authors <- unlist(author_counts)
table(parent_pubs$count_authors)

# review 27 parent publications with no authors ------------------------
# write.csv(parent_pubs %>% filter(author_counts==0), here("data","tables","manual_review", "parent_pubs_no_authors.csv"))

# reviewed and added authors when available (23 publications, leaving only 4 publications with no authors)
# these all had organizational authors like USGS and no individuals listed
# for data sets I used Originators as Authors
parent_manual <- read.csv(here("data","tables","manual_review", "parent_pubs_no_authors.csv")) %>% 
  select(pub_id, NEW_AUTHORS )

parent_pubs <- parent_pubs %>% left_join(parent_manual, by = "pub_id") %>%
  mutate(X_LLM_Authors_Clean = ifelse(is.na(NEW_AUTHORS), X_LLM_Authors_Clean, NEW_AUTHORS)) %>%
  select(-NEW_AUTHORS)
rm(parent_manual)

# recount authors
author_counts <- lapply(parent_pubs$X_LLM_Authors_Parsed, FUN = function(x){
  if(is.na(x)){return(0)
  }else{
    length(unlist(strsplit(x, split= ";")) ) 
  }
})

parent_pubs$count_authors <- unlist(author_counts)
table(parent_pubs$count_authors) # checks out that only 4 are missing Authors now
rm(author_counts)

# now count the author IDs
author_id_counts <- lapply(parent_pubs$X_LLM_Authors_IDs, FUN = function(x){
  if(is.na(x)){return(0)
  }else{
    length(unlist(strsplit(x, split= ";")) ) 
  }
})

parent_pubs$count_author_ids <- unlist(author_id_counts)
rm(author_id_counts)

# review 45 parent publications with number of author ids NOT equal to number of authors in 'Authors_Clean' ------------
# This includes the 23 publications that added NEW_AUTHORS (above) 
#write.csv(parent_pubs %>% filter(count_authors != count_author_ids), here("data","tables","manual_review", "parent_pubs_author_ids.csv"))

# manually updated columns 'X_LLM_Authors_IDs' and 'X_LLM_Authors_Parsed' when an author in 'X_LLM_Authors_Clean' 
# was missing from the 'Parsed' version and that author could be looked up in the saved table 'authors.csv' 
# When it could not be found in that table, it was added to 'NEW_AUTHORS' column with the structure
# last, first separated by '; ' with spaces between initials
# Added 'Notes' column to specify the changes to each row

parent_manual <- read.csv(here("data","tables","manual_review", "parent_pubs_author_ids.csv")) %>% 
  select(pub_id, X_LLM_Authors_IDs, X_LLM_Authors_Parsed, NEW_AUTHORS )

source(here("scripts","add_new_authors.r"))

parent_manual <- add_new_authors(review_df = parent_manual, 
                                 authors_df = authors)
parent_manual[["new_authors"]]
authors <- parent_manual[["authors"]]
parent_manual <- parent_manual[["review_df"]]

# fix '; 1335' and '; MAUNE, DAVID F.'
parent_manual <- parent_manual %>% 
  mutate(X_LLM_Authors_IDs = ifelse(X_LLM_Authors_IDs == "; 1335", "1335", X_LLM_Authors_IDs),
         X_LLM_Authors_Parsed = ifelse(X_LLM_Authors_Parsed == "; MAUNE, DAVID F.", "MAUNE, DAVID F.", X_LLM_Authors_Parsed))

# join with parent_pubs
parent_pubs <- parent_pubs %>% left_join(parent_manual, by = "pub_id") %>%
  mutate(X_LLM_Authors_IDs = ifelse(is.na(X_LLM_Authors_IDs.y), X_LLM_Authors_IDs.x, X_LLM_Authors_IDs.y),
         X_LLM_Authors_Parsed = ifelse(is.na(X_LLM_Authors_Parsed.y), X_LLM_Authors_Parsed.x, X_LLM_Authors_Parsed.y)) %>%
  select(-X_LLM_Authors_IDs.x, -X_LLM_Authors_IDs.y, 
         -X_LLM_Authors_Parsed.x, -X_LLM_Authors_Parsed.y,
         -count_authors,-count_author_ids,  -X_LLM_Authors, - X_LLM_Authors_Clean)

rm(parent_manual)

parent_pubs <- parent_pubs %>% select(pub_id, Publisher, Title, X_LLM_Authors_Parsed, X_LLM_Authors_IDs,
                                      `Publication Link`, `Publication DOI`, `Alt Links`, everything()) %>%
  rename(Authors = X_LLM_Authors_Parsed,
         Author_IDs = X_LLM_Authors_IDs)
colnames(parent_pubs) <- gsub(" ", "_", colnames(parent_pubs))

# clean Date/Year --------------------

# all those missing years are also missing dates so this captures all
# review 45 parent publications with missing date ----------------------------
#write.csv(parent_pubs %>% filter(is.na(X_LLM_Date)), here("data","tables","manual_review","parent_pubs_no_date.csv"))

# manually reviewed adding 'X_LLM_Date' and adding/changing 'X_LLM_Year' when appropriate 
# changes in 'X_LLM_Year' occurred mostly because datasets used the End year of the data rather than the publication year
# while checking for the dates, I also checked the Authors 
# some authors were missing or extra (usually works cited rather than publication authors)
# In those cases I manually revised 'Authors' and 'Author_IDs' to contain the names and ids from 'authors.csv'
# for any authors that did not appear in 'authors.csv', I added new column 'NEW_AUTHORS' column with the structure
# last, first separated by '; ' with spaces between initials
# Added 'Notes' column to specify the changes to each row
# Added alt link for one of them

parent_manual <- read.csv(here("data","tables","manual_review", "parent_pubs_no_date.csv")) %>%
  select(pub_id, Authors, Author_IDs, NEW_AUTHORS, X_LLM_Date, X_LLM_Year, Alt_Links )

parent_manual <- add_new_authors(review_df = parent_manual, 
                                 authors_df = authors, llm_colnames=FALSE)
parent_manual[["new_authors"]]
authors <- parent_manual[["authors"]]
parent_manual <- parent_manual[["review_df"]]

parent_pubs <- parent_pubs %>% left_join(parent_manual, by = "pub_id") %>%
  mutate(Date = ifelse(is.na(X_LLM_Date.y), X_LLM_Date.x, X_LLM_Date.y),
         Year = ifelse(is.na(X_LLM_Year.y), X_LLM_Year.x, X_LLM_Year.y),
         Authors = ifelse(is.na(Authors.y), Authors.x, Authors.y),
         Author_IDs = ifelse(is.na(Author_IDs.y), Author_IDs.x, Author_IDs.y),
         Alt_Links = ifelse(is.na(Alt_Links.y), Alt_Links.x, Alt_Links.y)) %>%
  select(-X_LLM_Date.x, - X_LLM_Date.y,
         -X_LLM_Year.x, - X_LLM_Year.y,
         -Authors.x, - Authors.y,
         -Author_IDs.x, - Author_IDs.y,
         -Alt_Links.x, -Alt_Links.y) 

rm(parent_manual)
# Also I added 'Notes' to 'authors_v2.csv' that either provided the author_id to replace it with
# or details on information that I added (did not change the authors_name but did change the parsed information)
# we will need to review the authors entirely to capture more of these duplicates


# review just the authors that appear in the parent publications
# create table of pub_ids and author_ids
library(tidyr)
parent_pub_authors <- parent_pubs %>% select(pub_id, Authors, Author_IDs) %>%
  separate_longer_delim(c(Authors, Author_IDs), delim = "; ")


authors_parent <- authors %>% filter(author_id %in% parent_pub_authors$Author_IDs)

# review all authors that appear in parent publications
#write.csv(authors_parent, here("data","tables","manual_review","authors_parent.csv"))

# I cleaned up the author_name value and added column 'Notes' and 'replacement' to combine some authors
authors_parent <- read.csv(here("data","tables","manual_review","authors_parent.csv")) 

replacements <- authors_parent %>%  filter(!is.na(replacement))

for(i in 1:nrow(replacements)){ # replace the author_id in the parent_pubs_authors table
  parent_pub_authors <- parent_pub_authors %>%
    mutate(Author_IDs = ifelse(Author_IDs==replacements$author_id[i],
                            replacements$replacement[i], Author_IDs))
} 
parent_replacements <- replacements
rm(replacements,i)
authors_parent <- authors_parent %>% filter(is.na(replacement)) # drop from authors_parent
write.csv(authors_parent, here("data","tables","parent_authors.csv"))

# now for all rows of parent_pub_authors, replace author_name

parent_pub_authors <- parent_pub_authors %>% select(-Authors) %>% 
  mutate(Author_IDs = as.numeric(trimws(Author_IDs))) %>%
  left_join(authors_parent, by= c("Author_IDs"="author_id")) %>%
  rename(Authors = author_name) %>%
  select(pub_id, Authors, Author_IDs)

write.csv(parent_pub_authors, here("data","tables","parent_pub_authors.csv"))

# Now lets summarise back to publications to replace in parent_pubs

parent_pub_authors1 <- parent_pub_authors %>% group_by(pub_id) %>%
  summarise(Authors = paste0(Authors, collapse = "; "),
            Author_IDs = paste0(Author_IDs, collapse = "; "))

parent_pubs <- parent_pubs %>% select(-Authors, - Author_IDs) %>%
  left_join(parent_pub_authors1, by = "pub_id")
rm(parent_pub_authors1)

# save parent publications -------------------------

parent_pubs <- parent_pubs %>% select(pub_id, Year, Publisher, Title, Authors, Author_IDs, Date,
                                      Publication_Link, Publication_DOI, Alt_Links, everything())
write.csv(parent_pubs, here("data","tables","parent_publications.csv"), row.names = FALSE)


