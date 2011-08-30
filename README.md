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

How to configure
----------------

In ebin/erlangDC.app, you can modify and add the environment variables
to change the behaviour of erlangDC. These are the available options:

 Key generation method. Default: keg_probab_fail_stop
 {keg_method,
	keg_null; keg_dc; keg_fail_stop_wc; keg_probab_fail_stop}
	
Key exchange method. Default: kex_fully_automatic
{kex_method,
	kex_manual; kex_fully_automatic}

Port to listen on. Default: 6768
{port, Port}

Minimum amount of acteve participants. Minimum secure default: 3
{min_active_participants, MinPart}

When to time in [ms] out for RT messages. Integer or infinity. Default:
infinity
{rt_timeout, RTTimeout}

Max allowed symbol length in [bytes]. Theoretical max = 65000. Default:
65000
{symbol_length, Length}

Time in [ms] to wait before new ticks get sent out. Integer. Default
20
{tick_timeout, TickTimeout}

Allow variable payload length: yes or no. Default: yes
{variable_payloads, yes; no}

How to run
----------

In the base directory do the following:
	
	erl -pa ebin

Then, at the erlang shell do the following:

	application:load(erlangDC), application:start(erlangDC).

