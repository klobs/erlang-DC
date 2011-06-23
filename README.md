What is this?
-------------

This is piece of software can be used to do the add up and participant management work for the [DC-Protocol](https://github.com/klobs/dc--).

The featureset is the same, only this example has 10 times fewer lines of code and no deadlocks. (Sometimes there seem to be some raceconditions, though. I'll promise, I'll take a look at these)

How to install
--------------

The easiest way to install is the [rebar](https://bitbucket.org/basho/rebar/wiki/Home) utility.

If you have never used it, yet, just do the following:

	wget http://bitbucket.org/basho/rebar/downloads/rebar; chmod u+x rebar
	./rebar compile

How to run
----------

In the base directory do the following:
	
	erl -pa ebin

Then, at the erlang shell do the following:

	application:load(erlangDC), application:start(erlangDC).

