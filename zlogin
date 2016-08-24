# display login banner of fortune and cowsay
if which fortune cowsay >/dev/null; then
  fortune | cowsay -f $(ls /usr/local/Cellar/cowsay/3.04/share/cows | gshuf -n1)
fi
