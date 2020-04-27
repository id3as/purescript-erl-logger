build:
	spago -x test.dhall build 

erl: build
	erlc -o ebin/ output/*/*.erl

test: erl
	erl -config test/sys.config -pa ebin -noshell -eval "(test_main@ps:main())()" -eval "init:stop()"



