-module(frequency_supervisor).
-export([init/0]).


%% Start the system with:
%%   FreqSup = spawn(frequency_supervisor, init, []).
%%
%% This will start a frequency_supervisor process, which will in turn start and 
%% supervise a frequency server.
%% We trap exits so that we will receive an EXIT message if the frequency
%% server fails.
%% 
%% Stop the system with:
%%   FreqSup ! {request, self(), stop}.
%%
init() ->
    io:format("PID ~p: frequency_supervisor:init ~n", [self()]),
    process_flag(trap_exit, true),
    spawn_frequency_server(),
    loop().

spawn_frequency_server() ->
    register(frequency, spawn_link(frequency, init, [self()])).

%% On trapping an EXIT, respawn the frequency server.
%% Note the (naive) assumption that we would not receive a spurious/malicious 
%% message for a PID other than the one we are supervising.
loop() ->
    receive
        {request, _Pid, stop} ->
            frequency:stop();
        {'EXIT', Pid, _Reason} -> 
            io:format("PID ~p: frequency_supervisor:loop trapped EXIT of ~p~n", [self(), Pid]),
            spawn_frequency_server(),
            loop()    
    end.
