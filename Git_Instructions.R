#################################################################
# Title: Synchronizing GitHub and RStudio
# Purpose: "Cheat sheet" for pushing/pulling from GitHub to RStudio
# Adapted from YouTube: https://www.youtube.com/watch?v=kL6L2MNqPHg
# Written by : L. Pandori
# Created: 03/31/2020
# Last edited: 03/31/2020
################################################################

# To commit
  # Stage by checking box in Git tab in upper R window
  # Click 'commit'
  # Add comment in new window, click 'commit'
  # Close 2x windows, return to RStudio

# To return to a previous version 
  # Click 'dif' in Git tab in upper R window
  # Click history in new window
  # Copy SHA of version you would like to revert to
  # In main RStudio window... tools --> terminal --> new terminal
  # In terminal, type the next line with SHA substituted for SHA
      # git reset --hard SHA
  # Run code, previous version should appear in top R window

# To create a GitHub repository for a project (from local Git repository)
  library(usethis)
  ?use_github
  # scroll to link -- set up personal access token
  # in link, click 'generate new token'
  # in list, click 'repo', then 'generate token'
  # copy and paste into code (but don't distribute online!!!)
  # put it in your environment
  edit_r_environ()
  # in new window (.Renviron*), enter below with actual token
  GITHUB_PAT = 'TOKEN'
  # now restart R session to take effect
  library(usethis)
  # make repository
  use_github(protocol = 'https', auth_token = Sys.getenv('GITHUB_PAT'))
  # review name and description
  1
  # select '1' to confirm and press enter to run code
  # it will open your new reposiory in GitHub -- edit if necessary
  
  
  
