## analyse oropouche

orderly_strict_mode()
orderly2::orderly_parameters(pathogen = "OROV")

orderly2::orderly_artefact(description = "inputs folder",
                           files = "inputs/")

# did not like latest(parameter:pathogen == this:pathogen) - fix in future - seems to have resolved itself for now...
orderly_dependency(
  name = "db_compilation",
  query = "latest(parameter:pathogen == this:pathogen)",
  files = c("inputs/articles.csv"="articles.csv",
            "inputs/parameters.csv"="parameters.csv",
            "inputs/outbreaks.csv"="outbreaks.csv"))


# read in data
articles <- read.csv("inputs/articles.csv")
outbreaks <- read.csv("inputs/outbreaks.csv")
parameters <- read.csv("inputs/parameters.csv")

# covidence 
covidence <- read.csv("covidence/orov_covidence.csv") %>%
  rename(CovID = Covidence..)
covidence$CovID %>% unique() %>% length()

# do we have all the articles we are expecting?
nrow(articles) == length(unique(covidence$CovID))


## firstly find the article with a bogus covidence ID
which(!(paste0("#",articles$covidence_id) %in% covidence$CovID))



## investigating what is duplicated
# articles[1,]
# ## this is a dummy one that I've worked on - omit 
# 
# # which ones are duplicated
# dup_cov <- articles$covidence_id[which(duplicated(articles$covidence_id))]
# articles[which(articles$covidence_id %in% dup_cov),]
# ## want the second article entry from both I think
# 
# outbreaks[which(outbreaks$covidence_id %in% dup_cov),]
# parameters[which(parameters$covidence_id %in% dup_cov),]
# ## Cov ID 30 - Anna-Maria - need to resolve
# ## Cov ID 109 - Joseph - need to remove one article row but the rest is fine 
# 
# 
# articles %>% filter(covidence_id!="999999",
#                     id!="6e85a97a-c427-4eef-b0e8-f55142802ef1",
#                     id!="23d7b6a2-bbbe-4f94-984f-da9fd0bab2d5")


