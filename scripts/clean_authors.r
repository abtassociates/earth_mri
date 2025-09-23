
# clean raw input file into csv tables 

## Author Directory -----------
# seems pretty good already, but lets split the name into first/last
names <- stringr::str_split(author_directory$author_name, ' |, ')
max(unlist(lapply(names, FUN = length))) # max length = 7
min(unlist(lapply(names, FUN = length))) # min length = 1

author_directory$last_name <- unlist(lapply(names, FUN=function(x){x[[1]]}))
author_directory$first_name <- unlist(lapply(names, FUN=function(x){
  if(length(x)>1){ return(gsub("\\.","", x[[2]]))
    }else{ return(NA)
        }}))

author_directory$middle_name <- unlist(lapply(names, FUN=function(x){
  if(length(x)>2){ return(gsub("\\.","", paste(x[3:length(x)], collapse = " ")))
  }else{ return(NA)
  }}))

manual_rev <- author_directory %>% filter(grepl(" ", middle_name))
# commented out to not over-write the file
#dir.create(here("data","tables", "manual_review"), recursive = TRUE)
#write.csv(manual_rev, here("data","tables","manual_review","authors.csv"))

# After the manual review, join back to the authors
authors <- read.csv(here("data","tables","manual_review","authors.csv")) %>%
  select(-X) %>% full_join(author_directory, by=c("author_id", "author_name","canonical_key"))%>% 
  mutate(
  last_name = ifelse(is.na(last_name.x), last_name.y, last_name.x),
  first_name = ifelse(is.na(first_name.x), first_name.y, first_name.x),
  middle_name = ifelse(is.na(middle_name.x), middle_name.y, middle_name.x),
  extra = ifelse(extra=="", NA, extra)
) %>% select(-last_name.x, - last_name.y, -first_name.x, -first_name.y,
             -middle_name.x, -middle_name.y) %>% arrange(author_id)

# there are some duplicated authors (using first_name, middle_name, last_name & extra)
# not sure how to combine since they have multiple canonical_key values, leaving as is
dups  <- authors %>% select(first_name, middle_name, last_name, extra)
dups <- dups[which(duplicated(dups)),]
dups <- dups %>% left_join(authors) # how should we handle these? leaving for now
rm(dups)
## Author Merges Audit -----------------------

winners <- author_merges_audit$winner_key %>% unique
losers <- author_merges_audit$loser_key %>% unique

stopifnot(length(winners) == length(losers)) # losers and winners are same length
stopifnot( nrow(authors %>% filter(canonical_key %in% losers)) == 0) # and no losers appear

# save author table
rm(manual_rev,names, author_directory, author_merges_audit, winners, losers)
write.csv(authors, here("data","tables","authors.csv"), row.names = FALSE)
