Haste.js: ./lib/_main.js ./Src/Main.hs
	hastec -o ./lib/Haste.js '--start=$$HASTE_MAIN();' --with-js=./lib/_main.js ./Src/Main.hs

clean:
	rm -r main ./Src/*.hi ./Src/*.o
