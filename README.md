# Personal system configurations

Steps to use it:
* Clone the project
* Link the bash-profile file

    $ ln -s path-to-project/configurations/bash_profile ~/.bash_profile

* Link the git-prompt.sh file

    $ ln -s path-to-project/configurations/git-prompt.sh ~/.git-prompt.sh

* Install brew

    $ /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

* Install gpg2

    $ brew install gpg2

* Generate Key

    $ ssh-keygen -t rsa

* Unencrypt config.gpg and link it

    $ unencryptFileGpg2 config.gpg
    $ ln -s path-to-project/configurations/config ~/.ssh/config

## Configure KDiff3 as merge tool
Using [KDiff3](http://kdiff3.sourceforge.net) as Git-GUI Merge Tool
 * Add the KDiff3 directory to the Path environment variable
 * Add KDiff3 as your Git mergetool; From Git Bash run:
 
     git config --global merge.tool kdiff3

 * Add kdiff3 complete path to Git Config; From Git Bash run:
 
     git config --global mergetool.kdiff3.path "/ABSOLUTE_PATH_TO_KADIFF3/kdiff3.exe"

 * Go into Git GUI settings and set the mergetool to kdiff3 (if Git GUI doesn't pick up this setting from git config, which it should).
 * Regardless of what I tried and/or read online, setting all possible settings is the only way KDiff3 works every time I select "Run Merge Tool" from the Git GUI right-click menu when there is a merge conflict.

