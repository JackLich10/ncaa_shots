### Push newly scraped files

# Set up connection to repository folder
repo <- git2r::repository('./')
cred <- git2r::cred_token()
version <- "0.0.1"

### Update menu
git2r::add(repo, "data/*")
git2r::commit(repo, message = paste0("Scraped daily menu ", Sys.time(), " using version ", version)) # commit the staged files with the chosen message
git2r::pull(repo, credentials = cred) # pull repo (and pray there are no merge commits)
git2r::push(repo, credentials = git2r::cred_user_pass('JackLich10', Sys.getenv("GITHUB_PAT"))) # push commit
