#/bin/sh

if [ "$1" == "" ]; then
	echo "Copy the beam file into the installation directory."
	echo "USAGE:"
	echo "$0 <*.beam>"
	exit
fi

[ -f "ebin/$1" ] && cp -i ebin/$1 ~/local/ejabberd/lib/ejabberd-16.04/ebin/ && echo OK

