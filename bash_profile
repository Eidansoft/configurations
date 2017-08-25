#!/bin/bash
echo -n "Cargando mi /Users/alex/.bash_profile... "

# Elimino todos los alias, para recrearlos a continuacion
unalias -a

# Creo alias para hacer tunneling a una maquina de Necotex
function tunnelingNecotex {
	echo "Levantando tunneling en los puertos locales:"
	echo " - 8090 JasperReports"
	echo " - 2139 Samba Unidades de Red"
	echo " - 5180 Router Web Interface"
	echo " - 5780 NAS Web Interface"
	echo " - 8080, 5432, 19812, 19813, 19814 PowerShop"
	sudo ssh -L 8090:172.26.0.5:8090 -L 2139:172.26.0.7:139 -L 5180:172.26.0.1:80 -L 5780:172.26.0.7:80 -L 8080:172.26.0.5:8080 -L 5432:172.26.0.5:5432 -L 19812:172.26.0.5:19812 -L 19813:172.26.0.5:19813 -L 19814:172.26.0.5:19814 user@domain -p PORT
}

# Funcion para encriptar
function unencryptFileGpg2 {
    FILE=$1
    if [ "${FILE##*.}" != "gpg" ]; then
	echo "[INFO] El archivo no tiene extension *.gpg"
    else
	echo "[INFO] Desencriptando $FILE"
	gpg2 -o ${FILE%.*} --decrypt $FILE
	echo "[INFO] $FILE desencriptado en ${FILE%.*}"
    fi
}

function encryptFileGpg2 {
    FILE=$1
    echo "Encriptando $FILE"
    gpg2 -o ${FILE}.gpg --symmetric $FILE
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

function resetMyWorkingBranch {
    branch=$(git branch | grep "*" | cut -d " " -f 2)
    if [ "$branch" != "WB" ]; then
	echo "[ERROR] No estas en la WorkingBranch (WB)."
    else    # compruebo que no hay nada pendiente de commitear
	git status | grep "nothing added to commit but untracked files present" 2>&1 > /dev/null
	untrackedFiles="$?"
	git status | grep "nothing to commit, working tree clean" 2>&1 > /dev/null
	allClean="$?"
	if [ "$untrackedFiles" = "0" ]; then
	    git status
	    read -p "[WARNING] Tienes archivos untracked. Seguro que quieres hacer HardReset ? (Y/n)" confirmacion
	    if [ "$confirmacion" = "Y" -o "$confirmacion" = "y" ];then
		git reset --hard MY_DOCKER
	    else
		echo "[INFO]"
	    fi
	elif [ "allClean" = "0" ]; then
		git reset --hard MY_DOCKER
	else
	    echo "[ERROR] Hay cambios pendientes"
	fi
    fi
}

# Creo funcion para el wireshark
function openWireshark {
	echo "Configurando el wireshark..."
	sudo chmod 644 /dev/bpf*
	wireshark
}
# Cargando las variables de entorno
add_to_path "/usr/local/opt/python/libexec/bin"
add_to_path "/usr/local/bin"
add_to_path "/usr/local/opt/python/libexec/bin"

# Aliases
alias ebp="editBashProfile"
alias emacs="/Applications/Emacs.app/Contents/MacOS/Emacs"
alias cdt="cd ~/trabajo/dbss"
alias g="git $@"
alias gb="git branch $@"
alias gc="git checkout $@"
alias gf="git fetch"
alias gg="git log --graph --oneline"
alias gga="git log --graph --oneline --all"
alias gh="git help $@"
alias gr="git remote -v"
alias grwb="resetMyWorkingBranch"
alias gs="git status"
alias gsth="git stash $@"
echo "Cargado."

# Configuro el prompt con el branch del GIT en uso
source ~/.git-prompt.sh
GIT_PS1_SHOWDIRTYSTATE=true
export PS1='\[\033[32m\]\w\[\033[33m\]$(__git_ps1)\[\033[00m\] $ '
