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
	[ "${FILE##*.}" != "gpg" ] && echo "El archivo no tiene extension *.gpg" && exit 1
	echo "Desencriptando $FILE"
	gpg2 -o ${FILE%.*} --decrypt $FILE
	echo "$FILE desencriptado en ${FILE%.*}"
}

function encryptFileGpg2 {
	FILE=$1
	echo "Encriptando $FILE"
        gpg2 -o ${FILE}.gpg --symmetric $FILE
        echo "$FILE encriptado en ${FILE}.gpg"
}

# Creo funcion que abre el sublime para editar un archivo
function sublime {
	if [ "$1" == "" ]; then
		echo "ERROR: Tienes que pasar el archivo a editar"
	else
		file=$1
		pushd $(pwd) > /dev/null
		echo "Editando archivo $file ..."
		/Applications/Sublime\ Text.app/Contents/MacOS/Sublime\ Text $file &> /dev/null
		popd > /dev/null
		echo "FIN"
	fi
}

# Creo funcion para editar el .bash_profile con el comando "ebp"
function ebp {
	nano ~/.bash_profile
	. ~/.bash_profile
}

# Creo funcion para el wireshark
function openWireshark {
	echo "Configurando el wireshark..."
	sudo chmod 644 /dev/bpf*
	wireshark
}

# Configuro el PATH con GIT y MySQL
PATH=/usr/local/git/bin:$PATH
export PATH=/usr/local/mysql/bin:$PATH

echo "DONE!"

# Setting PATH for Python 3.6
# The original version is saved in .bash_profile.pysave
PATH="/Library/Frameworks/Python.framework/Versions/3.6/bin:${PATH}"
export PATH
