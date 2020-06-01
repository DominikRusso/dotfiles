if [[ $(ps --no-header --pid=$PPID --format=cmd) != "fish" ]]
then
    export SHELL=/usr/bin/fish
	exec fish
fi
