if [[ $1 = "-f" ]]
then stack build --fast && stack exec duckbot-exe
else stack test && stack build && stack exec duckbot-exe
fi

