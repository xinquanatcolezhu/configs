# Emacs User needs to use caps lock as a Ctrl key
if [ -f "/usr/bin/setxkbmap" ]; then
	setxkbmap -option "ctrl:swapcaps"
fi

alias drop="docker exec -it dropbox dropbox"
function d_s {
    docker run -d --restart=always --net="host" --name=dropbox -v local/dropbox-path/:/dbox/Dropbox \
    -v $HOME/.dropbox:/dbox/.dropbox -e DBOX_UID=965004366 -e DBOX_GID=965000513 \
    -e https_proxy=proxy_url -e http_proxy=proxy_url janeczku/dropbox
}
