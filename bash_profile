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

# Funcion que incluye rutas al path sin repetir, dando preferencia
# a la ultima added
function add_to_path {
    ruta=$(stat -f $1)
    [ -d "$ruta" ] || (echo "[ERROR] Path provided to be added <$ruta> to PATH does not exists." && return 1)
    echo "$PATH" | grep -e ":\?${ruta}:" > /dev/null
    if [ "$?" != 0 ]; then
	export PATH="${ruta}:${PATH}"
    fi
}

# Funcion que incluye rutas al cdpath sin repetir, dando preferencia
# a las que ya hay primero
export CDPATH="."
function add_to_cdpath {
    ruta=$(stat -f $1 )
    [ "$ruta" = "" -o "$ruta" = "." ] && echo "[ERROR] Path provided to be added to the CDPATH <$ruta> not valid." && return 1
    [ -d "$ruta" ] || (echo "[ERROR] Path provided to be added <$ruta> to CDPATH does not exists. " && return 1)
    echo "$CDPATH" | grep -e ":${ruta}:\?" > /dev/null
    if [ "$?" != 0 ]; then
        export CDPATH="${CDPATH}:${ruta}"
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
    valid_envs=$(ls -la ~/.pyenv/versions/ | grep $USER | tr -s " " | cut -d " " -f 9 | grep -v "\.")
    [ "$env" = "" ] && echo "[ERROR] Tienes que indicar el entorno a cargar. Entornos validos: "$valid_envs && return 1
    echo $valid_envs | tr " " "\n" | grep -w $env > /dev/null 2>&1
    [ "$?" != "0" ] && echo "[ERROR] <$env> No es un entorno virtual valido. Entornos validos: "$valid_envs && return 1
    source ~/.pyenv/versions/$env/bin/activate
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

function docker_clean(){
    docker container prune -f
    docker image prune --filter "dangling=true" -f
    docker images | tr -s ' ' | grep "<none>" | cut -d ' ' -f 3 | xargs -I % docker rmi %
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

function dpython2(){
    docker_params=$1
    docker run -it --rm --name testpython2 -v $(pwd):/mnt $docker_params eidansoft/python2 /bin/bash
}

function dpython3(){
    docker_params=$1
    docker run -it --rm --name testpython3 -v $(pwd):/mnt $docker_params eidansoft/python3 /bin/bash
}

function doctave(){
    echo ''
    echo 'REMMEMBER TO USE graphics_toolkit gnuplot IF YOU NEED PLOT'
    echo ''
    xhost +
    sleep 3
    docker run -it --name octave --rm -v $(pwd):/mnt -e DISPLAY=host.docker.internal:0 eidansoft/octave
}

function nas_ls(){
    ping -c 1 -t 1 nas > /dev/null
    [ "$?" != "0" ] && echo "[ERROR] Nas do not reply to ping, are you sure it's turned on?" && return 1
    rsync --list-only rsync://alex@nas:873/$@
}

# Cargando las variables de entorno
add_to_path /usr/local/opt/python/libexec/bin
add_to_path /usr/local/bin
add_to_path /usr/local/opt/python/libexec/bin

# Cargando variables del cdpath
add_to_cdpath ~
add_to_cdpath ~/trabajo
add_to_cdpath ~/personal

# Aliases
alias cd..="cd .."
alias cd...="cd ../.."
alias cd....="cd ../../.."
alias cdt="pushd ~/trabajo/dbss"
alias cdp="pushd ~/personal"
alias cb="git rev-parse --abbrev-ref HEAD"
alias cssh="launch_cluster_ssh $@"
alias cssh_alg_sit_wapps="csshX $@ allorent@10.96.3.2 allorent@10.96.3.3"
alias cssh_alg_sit_qapis="csshX $@ allorent@10.96.3.6 allorent@10.96.3.7"
alias cssh_alg_uat_wapps="csshX $@ allorent@10.96.3.66 allorent@10.96.3.67 allorent@10.96.3.68 allorent@10.96.3.69 allorent@10.96.3.70 allorent@10.96.3.71 allorent@10.96.3.72 allorent@10.96.3.73"
alias cssh_alg_uat_qapis="csshX $@ allorent@10.96.3.75 allorent@10.96.3.76 allorent@10.96.3.77 allorent@10.96.3.78"
alias cssh_alg_prod_wapps="csshX $@ allorent@10.96.2.8 allorent@10.96.2.9 allorent@10.96.2.10 allorent@10.96.2.11 allorent@10.96.2.12 allorent@10.96.2.13 allorent@10.96.2.14 allorent@10.96.2.15 allorent@10.96.2.16 allorent@10.96.2.17 allorent@10.96.2.18 allorent@10.96.2.19 allorent@10.96.2.20 allorent@10.96.2.21 allorent@10.96.2.22 allorent@10.96.2.23 allorent@10.96.2.24 allorent@10.96.2.25 allorent@10.96.2.26 allorent@10.96.2.27 allorent@10.96.2.28 allorent@10.96.2.29 allorent@10.96.2.30 allorent@10.96.2.31 allorent@10.96.2.32 allorent@10.96.2.33 allorent@10.96.2.34 allorent@10.96.2.35 allorent@10.96.2.36 allorent@10.96.2.37 allorent@10.96.2.38 allorent@10.96.2.39"
alias cssh_alg_prod_qapis="csshX $@ allorent@10.96.2.42 allorent@10.96.2.43 allorent@10.96.2.44 allorent@10.96.2.45 allorent@10.96.2.46 allorent@10.96.2.47 allorent@10.96.2.48 allorent@10.96.2.49"
alias cssh_ban_prodDC1_wapps="csshX $@ --ssh_args '-J bangla_prod_jump' alorente@10.74.8.21 alorente@10.74.8.22 alorente@10.74.8.23 alorente@10.74.8.24 alorente@10.74.8.25 alorente@10.74.8.26 alorente@10.74.8.27 alorente@10.74.8.28 alorente@10.74.8.29 alorente@10.74.8.30 alorente@10.74.8.31 alorente@10.74.8.32 alorente@10.74.8.33 alorente@10.74.8.34 alorente@10.74.8.35 alorente@10.74.8.36"
alias cssh_ban_prodDC1_qapis="csshX $@ --ssh_args '-J bangla_prod_jump' alorente@10.74.8.61 alorente@10.74.8.62 alorente@10.74.8.63 alorente@10.74.8.64 alorente@10.74.8.65 alorente@10.74.8.66 alorente@10.74.8.67 alorente@10.74.8.68 alorente@10.74.8.235 alorente@10.74.8.236 alorente@10.74.8.237 alorente@10.74.8.238"
alias cssh_ban_prodDC2_wapps="csshX $@ --ssh_args '-J bangla_prod_jump' alorente@10.74.9.21 alorente@10.74.9.22 alorente@10.74.9.23 alorente@10.74.9.24 alorente@10.74.9.25 alorente@10.74.9.26 alorente@10.74.9.27 alorente@10.74.9.28 alorente@10.74.9.29 alorente@10.74.9.30 alorente@10.74.9.31 alorente@10.74.9.32 alorente@10.74.9.33 alorente@10.74.9.34 alorente@10.74.9.35 alorente@10.74.9.36"
alias cssh_ban_prodDC2_qapis="csshX $@ --ssh_args '-J bangla_prod_jump' alorente@10.74.9.61 alorente@10.74.9.62 alorente@10.74.9.63 alorente@10.74.9.64 alorente@10.74.9.65 alorente@10.74.9.66 alorente@10.74.9.67 alorente@10.74.9.68"
alias dci="docker_create_image $@"
alias dclean="docker_clean"
alias ddi="docker rmi $1"
alias dvi="docker images"
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
alias gu="git fetch && git pull"
alias jupyter-start-locally="echo 'Launching Jupyter at localhost:8888' ; docker run -d --rm -p 8888:8888 --name jupyter -v /Users/alorente/personal/proyectos:/home/jovyan/work eidansoft/jupyter start-notebook.sh --ip 0.0.0.0"
alias notify_me_on_telegram='docker run -it --rm --name telegram -v /Users/alorente/.telegram_send_config_alex.conf:/home/telegram/conf eidansoft/telegram-send telegram-send --config conf "$([ $? = 0 ] && echo "" || echo "error: ") $(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*tg$//'\'')"'
alias vsc="/Applications/Visual\ Studio\ Code.app/Contents/MacOS/Electron $@"

# Configuro la busqueda en el historico de comandos para que pueda hacer Ctrl-s y buscar hacia delante (hacia atras ya lo hace por defecto con el Ctrl-r)
stty -ixon

# Configuro el prompt con el branch del GIT en uso
source ~/.git-prompt.sh
GIT_PS1_SHOWDIRTYSTATE=true
export PS1='\[\033[32m\]\w\[\033[33m\]$(__git_ps1)\[\033[00m\] $ '

echo "Cargado."
