export CLICOLOR=1
##################################################
# Fancy PWD display function
##################################################
# The home directory (HOME) is replaced with a ~
# The last pwdmaxlen characters of the PWD are displayed
# Leading partial directory names are striped off
# /home/me/stuff          -> ~/stuff               if USER=me
# /usr/share/big_dir_name -> ../share/big_dir_name if pwdmaxlen=20
##################################################
bash_prompt_command() {
	    # How many characters of the $PWD should be kept
	    local pwdmaxlen=25
	    # Indicate that there has been dir truncation
	    local trunc_symbol=".."
	    local dir=${PWD##*/}
	    pwdmaxlen=$(( ( pwdmaxlen < ${#dir} ) ? ${#dir} : pwdmaxlen ))
	    NEW_PWD=${PWD/$HOME/~}
	    local pwdoffset=$(( ${#NEW_PWD} - pwdmaxlen ))
	    if [ ${pwdoffset} -gt "0" ]
	        then
	        NEW_PWD=${NEW_PWD:$pwdoffset:$pwdmaxlen}
	        NEW_PWD=${trunc_symbol}/${NEW_PWD#*/}
	    fi
}
#
bash_prompt() {
    local NONE='\[\033[0m\]'    # unsets color to term's fg color

        # regular colors
	local K='\[\033[0;30m\]'    # black
	local R='\[\033[0;31m\]'    # red
	local G='\[\033[0;32m\]'    # green
	local Y='\[\033[0;33m\]'    # yellow
	local B='\[\033[0;34m\]'    # blue
	local M='\[\033[0;35m\]'    # magenta
	local C='\[\033[0;36m\]'    # cyaN
	local W='\[\033[0;37m\]'    # white

	# empahsized (bolded) colors
	local EMK='\[\033[1;30m\]'
	local EMR='\[\033[1;31m\]'
	local EMG='\[\033[1;32m\]'
	local EMY='\[\033[1;33m\]'
	local EMB='\[\033[1;34m\]'
	local EMM='\[\033[1;35m\]'
	local EMC='\[\033[1;36m\]'
	local EMW='\[\033[1;37m\]'

	# background colors
	local BGK='\[\033[40m\]'
	local BGR='\[\033[41m\]'
	local BGG='\[\033[42m\]'
	local BGY='\[\033[43m\]'
	local BGB='\[\033[44m\]'
	local BGM='\[\033[45m\]'
	local BGC='\[\033[46m\]'
	local BGW='\[\033[47m\]'

  if [[ -s "$HOME/.host_prompt_colours" ]]
  then
    source "$HOME/.host_prompt_colours"
  else
    local PC1=$EMG
    local PC2=$EMC
  fi

	PS1="${EMC}[\t${EMC}] ${PC1}[${PC1}\u${PC1}@${PC1}\h ${PC2}\${NEW_PWD}\`git branch 2> /dev/null | grep -e ^* | sed -E  s/^\\\\\\\\\*\ \(.+\)$/\ \(\\\\\\\\\1\)\/\`${PC1}] ${EMC}\\$ ${EMC}${NONE}"
}

PROMPT_COMMAND=bash_prompt_command
bash_prompt
unset bash_prompt
shopt -s histappend
PROMPT_COMMAND="$PROMPT_COMMAND;history -a"

if [[ -s /usr/local/etc/bash_completion.d/git-completion.bash ]] ; then source /usr/local/etc/bash_completion.d/git-completion.bash ; fi

function g {
  if [[ $# > 0 ]]
  then
    git $@
  else
    git status
  fi
}

alias be='bundle exec'
alias ack-grep='ack'
export EDITOR="vim"
export GIT_EDITOR="vim"
export GEM_EDITOR="vim"
export PATH=b:bin:/Users/tim/bin:/Users/tim/.sp5repl:/Users/tim/node_modules/.bin:/usr/local/share/python:/usr/local/bin:$PATH
export PYTHONPATH=/usr/local/lib/python:$PYHTONPATH
if [[ -s "$HOME/.bash_private" ]]
then
  source "$HOME/.bash_private"
fi

source /usr/local/opt/chruby/share/chruby/chruby.sh
source /usr/local/opt/chruby/share/chruby/auto.sh

# TMUX
if which tmux 2>&1 >/dev/null; then
  # if no session is started, start a new session
  test -z ${TMUX} && tmux
fi

export DOCKER_HOST=tcp://172.16.42.43:4243

export SBT_OPTS="-Xmx1536M -XX:+UseConcMarkSweepGC -XX:+CMSClassUnloadingEnabled -XX:MaxPermSize=2G -Xss2M  -Duser.timezone=GMT"
shopt -s checkwinsize
stty stop undef #unmap ctrl-S
