#!/usr/bin/Rscript
# Sorry this isn't elegant but necessary for the cron tab to work
setwd("~/Projects/Graphics/2022-Tyler-3D_Graphical_Perception/")

# Set up authentication via ssh
cred <- git2r::cred_ssh_key("~/.ssh/id_rsa.pub", "~/.ssh/id_rsa")
repo <- git2r::repository()
git2r::config(repo = repo, global = F, "Susan-auto", "srvanderplas@gmail.com")

# Log job start
httr::POST("https://hc-ping.com/c3809b60-8e8b-448b-a371-31f40575480d/start")

# Check repo status
status <- git2r::status()

tmp <- status$unstaged
modified <- names(tmp) == "modified"
modified <- unlist(tmp[modified])

# If db has been modified
if (any(stringr::str_detect(modified, "(experiment_interface/.*\\.db)|(experiment_interface/codes.txt)"))) {

  # Add changed db to commit and commit
  git2r::add(repo = '.', "experiment_interface/*.db")
  git2r::add(repo = '.', "experiment_interface/codes.txt")
  try(git2r::commit(message = "Update data and codes"))

  # Update
  git2r::pull(repo = repo, credentials = cred)
  git2r::push(getwd(), credentials = cred)

  if (length(git2r::status()$unstaged$conflicted) > 0) {
    # Log merge conflict, signal failure (Susan gets an email)
    httr::POST("https://hc-ping.com/c3809b60-8e8b-448b-a371-31f40575480d/fail", body = "Merge conflict")
  } else {
    # Log success
    httr::POST("https://hc-ping.com/c3809b60-8e8b-448b-a371-31f40575480d", body = "Changes pushed")
  }
} else {
  # Log no changes
  httr::POST("https://hc-ping.com/c3809b60-8e8b-448b-a371-31f40575480d", body = "No changes")
}

git2r::config(repo = repo, global = F, "Susan Vanderplas", "srvanderplas@gmail.com")
