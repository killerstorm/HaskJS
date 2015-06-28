main.js: _main.js main.hs
	hastec '--start=$$HASTE_MAIN();' --with-js=_main.js main.hs

clean:
	rm -r main main.hi main.o CoinKernel.hi TransactionGraph.hi CoinKernel.o TransactionGraph.o
