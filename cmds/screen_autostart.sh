# Auto-screen invocation. see: http://taint.org/wk/RemoteLoginAutoScreen
# if we're coming from a remote SSH connection, in an interactive session
# then automatically put us into a screen(1) session.   Only try once
# -- if $STARTED_SCREEN is set, don't try it again, to avoid looping
# if screen fails for some reason.
echo $TERM
if [ "$TERM" != dumb -a "$TERM" != screen -a "$PS1" != "" -a "${STARTED_SCREEN:-x}" = x -a "${SSH_TTY:-x}" != x -a "$DISPLAY" == "" ]
then
  echo "Auto-starting screen."

  # Set the window title to HOSTNAME
  echo -ne "\e]2;$HOSTNAME\a"

  # If no session is running, set DISPLAY environment variable
  screen -ls | egrep "^No Sockets found" > /dev/null
  if [ $? = 0 ]; then
    export DISPLAY=:$(( $( (echo 5555; ls /tmp/.X11-unix/X* 2> /dev/null) | sed 's/^.*\/X//' | sort -n | tail -n 1) + 1))
    echo "No running screen found. DISPLAY set to $DISPLAY."
  fi
  STARTED_SCREEN=1 ; export STARTED_SCREEN
  screen -D -RR && exit 0
  # normally, execution of this rc script ends here...
  echo "Screen failed! Continuing with normal bash startup."
fi
# [end of auto-screen snippet]
