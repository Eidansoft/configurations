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

function activate_virtualenv {
    env=$1
    valid_envs=$(ls -la ~/.virtualenvs/ | grep $USER | tr -s " " | cut -d " " -f 9 | grep -v "\.")
    [ "$env" = "" ] && echo "[ERROR] Tienes que indicar el entorno a cargar. Entornos validos: "$valid_envs && return 1
    echo $valid_envs | tr " " "\n" | grep -w $env > /dev/null 2>&1
    [ "$?" != "0" ] && echo "[ERROR] <$env> No es un entorno virtual valido. Entornos validos: "$valid_envs && return 1
    source ~/.virtualenvs/$env/bin/activate
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
alias dk="/Users/alorente/trabajo/dbss/docker/run.sh $@"
alias g="git $@"
alias ga="git add $@"
alias gb="git branch $@"
alias gc="git checkout $@"
alias gclean="git clean -f; git checkout -- ."
alias gd="git diff $@"
alias gf="git fetch"
alias gg='git log --oneline --graph --pretty=format:"%Cred%h%Creset%C(yellow)%d%Creset %<(70,trunc)%s %Cgreen%<(10,trunc)%an%Creset %C(bold blue)%<(14,trunc)%ad%Creset" --topo-order'
alias gga='git log --oneline --graph --pretty=format:"%Cred%h%Creset%C(yellow)%d%Creset %<(70,trunc)%s %Cgreen%<(10,trunc)%an%Creset %C(bold blue)%<(14,trunc)%ad%Creset" --topo-order --all'
alias gh="git help $@"
alias gr="git remote -v"
alias gs="git status"
alias gsth="git stash $@"

# Configuro el prompt con el branch del GIT en uso
source ~/.git-prompt.sh
GIT_PS1_SHOWDIRTYSTATE=true
export PS1='\[\033[32m\]\w\[\033[33m\]$(__git_ps1)\[\033[00m\] $ '

echo "Cargado."
