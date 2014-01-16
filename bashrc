# TMUX
if which tmux 2>&1 >/dev/null; then
  # if no session is started, start a new session
  tmux -2 new-session
fi
source ~/.bash_profile
