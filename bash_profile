#!/bin/bash
echo -n "Cargando mi /Users/alex/.bash_profile... "

# Elimino todos los alias, para recrearlos a continuacion
unalias -a

# Funcion para encriptar
function unencryptFileGpg2 {
    FILE=$1
    if [ "${FILE##*.}" != "gpg" ]; then
	echo "[INFO] El archivo no tiene extension *.gpg"
    else
	echo "[INFO] Desencriptando $FILE"
	gpg -o ${FILE%.*} --decrypt $FILE
	[ "$?" = 0 ] && echo "[INFO] $FILE desencriptado en ${FILE%.*}" || echo "[ERROR] Clave incorrerta"
    fi
}

function encryptFileGpg2 {
    FILE=$1
    echo "Encriptando $FILE"
    gpg -o ${FILE}.gpg --symmetric $FILE
    echo "[INFO] $FILE encriptado en ${FILE}.gpg"
}

# Creo funcion que me permite editar el .bash_profile y cargarlo directamente
function editBashProfile {
    /Applications/Emacs.app/Contents/MacOS/Emacs -nw /Users/alorente/.bash_profile
    source /Users/alorente/.bash_profile
}

# Funcion que incluye rutas al path sin repetir
function add_to_path {
    ruta=$1
    echo "$PATH" | grep -e ":\?${ruta}:" > /dev/null
    if [ "$?" != 0 ]; then
	export PATH="${ruta}:${PATH}"
    fi
}

# Creo funcion que abre el sublime para editar un archivo
function sublime {
    if [ "$1" == "" ]; then
	echo "[ERROR] Tienes que pasar el archivo a editar"
    else
	file=$1
	pushd $(pwd) > /dev/null
	echo "[INFO] Editando archivo $file ..."
	/Applications/Sublime\ Text.app/Contents/MacOS/Sublime\ Text $file &> /dev/null &
	popd > /dev/null
	echo "[INFO] Archivo $file abierto en ventana nueva con el Sublime"
    fi
}

# Creo funcion para el wireshark
function openWireshark {
	echo "Configurando el wireshark..."
	sudo chmod 644 /dev/bpf*
	wireshark
}

# Funcion para manejar los entornos virtuales
function activate_virtualenv {
    env=$1
    valid_envs=$(ls -la ~/.virtualenvs/ | grep $USER | tr -s " " | cut -d " " -f 9 | grep -v "\.")
    [ "$env" = "" ] && echo "[ERROR] Tienes que indicar el entorno a cargar. Entornos validos: "$valid_envs && return 1
    echo $valid_envs | tr " " "\n" | grep -w $env > /dev/null 2>&1
    [ "$?" != "0" ] && echo "[ERROR] <$env> No es un entorno virtual valido. Entornos validos: "$valid_envs && return 1
    source ~/.virtualenvs/$env/bin/activate
}

# Funcion para listar las tags (previa actualizacion) o si recibe un hash por parametro, lista las tags que lo incluyen
function list_tags(){
    commit=$1
    git fetch --prune origin "+refs/tags/*:refs/tags/*"
    [ "$commit" = "" ] && git tag || git tag --contains $commit
}

# Funcion para listar y seleccionar comodamente una branch
function select_git_branch(){
    branches=$(git branch | grep -v '*' | tr -d " ")
    [ "$branches" = "" ] && return 1
    options=($branches)
    PS3="What branch do you wanna select? "
    select opt in "${options[@]}"; do
        echo $opt
        break
    done
}

# Funcion para hacer checkout de una branch seleccionandola comodamente
function git_checkout_interactive(){
    echo "Possible branches:"
    selected_branch=$(select_git_branch)
    [ "$selected_branch" = "" ] && echo "[ERROR] No valid branch." && return 1
    echo "Checking out: $selected_branch"
    git checkout $selected_branch
}

# Funcion para ver las migraciones entre dos tags
function git_migrations_between(){
    new_tag=$1
    old_tag=$2
    git fetch
    [ "$new_tag" = "" -o "$old_tag" = "" ] && echo "[ERROR] Both the new and old tag must be provided." && return 1
    echo ""
    echo "The list of migrations between <$new_tag> and <$old_tag> are:"
    git diff $old_tag $new_tag  --name-status | grep -E '0[01][0-9]'
}

# Funcion para crear comodamente tags
function git_create_tag(){
    tag=$1
    [ "$tag" = "" ] && echo "[ERROR] You must provide the tag name." && return 1
    git push origin $(git rev-parse --abbrev-ref HEAD)
    git fetch --prune origin "+refs/tags/*:refs/tags/*"
    git tag -a $tag -m "created tag $tag"
    git push origin refs/tags/$tag
}

# Funcion para eliminar comodamente tags
function git_delete_tag(){
    tag=$1
    [ "$tag" = "" ] && echo "[ERROR] You must provide the tag name." && return 1
    git fetch --prune origin "+refs/tags/*:refs/tags/*"
    git push --delete origin refs/tags/$tag
    git tag --delete $tag
}

# Funcion para crear imagenes de docker
function docker_create_image(){
    folder=$1
    name=$2
    [ "$folder" = "" -o "$name" = "" ] && echo "[ERROR] The folder containing the Dockerfile and name for the image are mandatory." && return 1
    [ ! -d "$folder" ] && echo "[ERROR] The folder provided <$folder> is not valid." && return 1
    docker build $folder -t $name
}

function git_ancestor(){
    pushedrev="$(git rev-parse HEAD)"
    echo "Checking hash $pushedrev"
    basename=djezzy_golive_v3
    if ! baserev="$(git rev-parse --verify refs/heads/"$basename" 2>/dev/null)"; then
        echo "'$basename' is missing, call for help!"
        exit 1
    fi

    parents_of_commits_beyond_base="$(
      git rev-list --pretty=tformat:%P "$pushedrev" --not "$baserev" | grep -v '^commit '
    )"

    case "$parents_of_commits_beyond_base" in
        *\ *)          echo "must not push merge commits (rebase instead)"
               ;;
        *"$baserev"*)  echo "Everything is OK"
               ;;
        *)             echo "must descend from tip of '$basename'"
               ;;

    esac
}

function docker_clean_images(){
    # Clean all temporal or not tagged docker images
    images_ids=$(docker images | grep "<none>" | tr -s " " | cut -d " " -f 3)
    for image in $images_ids; do
        docker rmi $image
    done
}

function docker_clean_containers(){
    # Clean al already exited docker containers
    containers_ids=$(docker ps -a | grep "Exited" | tr -s " " | cut -d " " -f 1)
    for container in $containers_ids; do
        docker rm $container
    done
}

function git_delete_all_branches() {
    excluded_branches=("djezzy_golive_uat" "djezzy_golive_prod" "djezzy_golive_v3")
    all_branches=$(git branch | grep -v ^*)
    for branch in $all_branches
    do
        git branch -D
    done
}

function launch_cluster_ssh() {
    params=$1
    [ -x ~/personal/configurations/toggle_Terminal_maximize.sh ] && ~/personal/configurations/toggle_Terminal_maximize.sh || echo "[ERROR] The toggle maximization script is not properly configured."
    sleep 1
    num_screens=$(system_profiler SPDisplaysDataType | grep -v "^          " | grep "^        " | tr -s " " | wc -l)
    if [ "$num_screens" -gt "1" ]; then
        echo "Two screen detected, using the second one..."
        csshX --screen 2 $params
    else
        csshX $params
    fi
}

function git_commits_between(){
    newest_hash=$1
    oldest_hash=$2
    [ -z "$newest_hash" -o -z "$oldest_hash" ] && echo "[ERROR] The new and old hash params are mandatory." && return 1
    git log --oneline --format=%s --reverse ${oldest_hash}..${newest_hash} | grep -v "^Merge"
}

# Cargando las variables de entorno
add_to_path "/usr/local/opt/python/libexec/bin"
add_to_path "/usr/local/bin"
add_to_path "/usr/local/opt/python/libexec/bin"

# Aliases
alias ebp="editBashProfile"
alias emacs="/Applications/Emacs.app/Contents/MacOS/Emacs"
alias cdt="pushd ~/trabajo/dbss"
alias cdp="pushd ~/personal"
alias cb="git rev-parse --abbrev-ref HEAD"
alias cssh="launch_cluster_ssh $@"
alias dci="docker_create_image $@"
alias ddai="docker_clean_images"
alias ddac="docker_clean_containers"
alias ddi="docker rmi $1"
alias dvi="docker images"
alias dti="docker run -it --name test --rm $@"
alias dk="/Users/alorente/trabajo/dbss/docker/run.sh $@"
alias fsecurestop="launchctl list | grep secure && echo 'launchctl remove todos los listados'"
alias g="git $@"
alias ga="git add $@"
alias gb="git branch $@"
alias gc="git checkout $@"
alias gci="git_checkout_interactive"
alias gct="git_create_tag $@"
alias gdab="git_delete_all_branches $@"
alias gdt="git_delete_tag $@"
alias gcb="git_commits_between $@"
alias gclean="git clean -fdx; git checkout -- ."
alias gd="git diff $@"
alias gf="git fetch"
alias gg='git log --oneline --graph --pretty=format:"%Cred%h%Creset%C(yellow)%d%Creset %<(70,trunc)%s %Cgreen%<(10,trunc)%an%Creset %C(bold blue)%<(14,trunc)%ad%Creset" --topo-order'
alias gga='git log --oneline --graph --pretty=format:"%Cred%h%Creset%C(yellow)%d%Creset %<(70,trunc)%s %Cgreen%<(10,trunc)%an%Creset %C(bold blue)%<(14,trunc)%ad%Creset" --topo-order --all'
alias gh="git help $@"
alias glm="git_migrations_between $@"
alias glt="list_tags $@"
alias globalprotectstop="echo 'Stopping...' && launchctl remove com.paloaltonetworks.gp.pangps ; sleep 5 && launchctl remove com.paloaltonetworks.gp.pangpa ; sleep 5"
alias globalprotectstart="echo 'Starting...' && launchctl load /Library/LaunchAgents/com.paloaltonetworks.gp.pangpa.plist ; launchctl load /Library/LaunchAgents/com.paloaltonetworks.gp.pangps.plist"
alias gr="git remote -v"
alias gs="git status"
alias gsth="git stash $@"

# Configuro la busqueda en el historico de comandos para que pueda hacer Ctrl-s y buscar hacia delante (hacia atras ya lo hace por defecto con el Ctrl-r)
stty -ixon

# Configuro el prompt con el branch del GIT en uso
source ~/.git-prompt.sh
GIT_PS1_SHOWDIRTYSTATE=true
export PS1='\[\033[32m\]\w\[\033[33m\]$(__git_ps1)\[\033[00m\] $ '

echo "Cargado."
