# TMUX
if which tmux 2>&1 >/dev/null; then
  # if no session is started, start a new session
  test -z ${TMUX} && (tmux -2 attach -t "default" || tmux -2 new-session -s "default")
fi
source ~/.bash_profile
