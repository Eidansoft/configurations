# git-gui-tools
My personal tools for a better git-gui

In order to get on GIT-GUI Tools the $FILENAMES variable availablea and be able to execute commands to multiple files for example "rm $FILENAMES" to delete all selected files, you need to medify the file:

    Git_PATH/share/git-gui/lib/tools.tcl

In OSx with Git installed by brew, this file is at:

    /usr/local/git/share/git-gui/lib/tools.tcl

The modified lines are:

    70  global repo_config env current_diff_path selected_paths
    103 set env(FILENAMES) [array names selected_paths]
    126 unset env(FILENAMES)

In order to have the default branch name in all commits at Git-Gui you need also modify the files:
 * /usr/local/git/share/git-gui/lib/git-gui.tcl
 * /usr/local/git/share/git-gui/lib/commit.tcl

Adding to the end of git-gui.tcl the following to get the branch name when you open git-gui:

    $ui_comm insert end "[git branch | grep "*" | cut -d " " -f 2] " ; #I set the branch name by default in commits

And modifing to commit.tcl line 436 to get the branch name by default after every commit:

    $ui_comm insert end "[git branch | grep "*" | cut -d " " -f 2] " ; #I set the branch name by default in commits

