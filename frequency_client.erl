-module(frequency_client).
-export([loop/0]).


%% Start a client with:
%%   Client = spawn(frequency_client, loop, []).
%%
%% This client simply requests a frequency, holds it for 10 seconds, and then
%% deallocates it, waiting for 5 seconds before doing the whole thing again.
%% While trivial, spawning multiple client processes and using the observer
%% tool is more than sufficient for the purposes of this assignment. 
%%
%% As the server dynamically links to us when we request a frequency, we will
%% be automatically killed if the server fails while we hold a frequency.  If
%% the server fails while no link is in place (i.e. we do not hold a frequency)
%% then it is safe for us to remain up.  We will however fail if we attempt to 
%% make a request of the named process while it is unavailable.
loop() ->
    {ok, Freq} = frequency:allocate(),
    io:format("PID ~p: client was allocated frequency ~w~n", [self(), Freq]),
    timer:sleep(10000),
    frequency:deallocate(Freq),
    io:format("PID ~p: client deallocated frequency ~w~n", [self(), Freq]),
    timer:sleep(5000),
    loop().
