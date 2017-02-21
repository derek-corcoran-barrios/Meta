library(metagear)

# initialize bibliographic data and screening tasks
data(example_references_metagear)
effort_distribute(example_references_metagear, initialize = TRUE, reviewers = "marc", save_split = TRUE)

# initialize screener GUI
abstract_screener("effort_marc.csv", aReviewer = "marc")