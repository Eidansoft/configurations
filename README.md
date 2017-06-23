# git-gui-tools
My personal tools for a better git-gui

## Have $FILENAMES variable for tools
In order to get on GIT-GUI Tools the $FILENAMES variable availablea and be able to execute commands to multiple files for example "rm $FILENAMES" to delete all selected files, you need to medify the file:

    Git_PATH/share/git-gui/lib/tools.tcl

In OSx with Git installed by [Brew](https://brew.sh), this file is at:

    /usr/local/git/share/git-gui/lib/tools.tcl

The modified lines are:

    70  global repo_config env current_diff_path selected_paths
    103 set env(FILENAMES) [array names selected_paths]
    126 unset env(FILENAMES)

## Set default commit message
In order to have the default branch name in all commits at Git-Gui you need also modify the files:
 * /usr/local/git/share/git-gui/lib/git-gui.tcl
 * /usr/local/git/share/git-gui/lib/commit.tcl

Adding to the end of git-gui.tcl the following to get the branch name when you open git-gui:

    $ui_comm insert end "[git branch | grep "*" | cut -d " " -f 2] " ; #I set the branch name by default in commits

And modifing to commit.tcl line 436 to get the branch name by default after every commit:

    $ui_comm insert end "[git branch | grep "*" | cut -d " " -f 2] " ; #I set the branch name by default in commits

## Configure KDiff3 as merge tool
Using [KDiff3](http://kdiff3.sourceforge.net) as Git-GUI Merge Tool
 * Add the KDiff3 directory to the Path environment variable
 * Add KDiff3 as your Git mergetool; From Git Bash run:
 
     git config --global merge.tool kdiff3

 * Add kdiff3 complete path to Git Config; From Git Bash run:
 
     git config --global mergetool.kdiff3.path "/ABSOLUTE_PATH_TO_KADIFF3/kdiff3.exe"

 * Go into Git GUI settings and set the mergetool to kdiff3 (if Git GUI doesn't pick up this setting from git config, which it should).
 * Regardless of what I tried and/or read online, setting all possible settings is the only way KDiff3 works every time I select "Run Merge Tool" from the Git GUI right-click menu when there is a merge conflict.

