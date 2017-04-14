%% Based on code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson
%%
%% This is based on the 'hardened_frequency' sample code.

-module(frequency).
-export([allocate/0,deallocate/1,stop/0]).
-export([init/1]).


%% Initialize the server.
%% We trap exits so that we can be notified of client failures.  However, this 
%% does mean that we will receive an EXIT message as opposed to being 
%% automatically killed when our parent supervisor fails. 
init(SupervisorPid) ->
    io:format("PID ~p: frequency:init - being supervised by ~p~n", [self(), SupervisorPid]),
    process_flag(trap_exit, true),
    Frequencies = {get_frequencies(), []},
    loop(SupervisorPid, Frequencies).

% Hard Coded
get_frequencies() -> [10,11,12,13,14,15].

%% The Main Loop
%% Note the need to distinguish between EXIT messages from clients (in which
%% case we want to reclaim any allocated frequencies), and EXIT messages from
%% our parent supervisor (in which case we want to terminate).
%%
%% Any clients to which we are linked (i.e. they have an allocated frequency) 
%% will be automatically killed if we exit.  This ensures consistent state 
%% across the system if we are re-spawned.  Clients that have not been allocated 
%% a frequency can safely remain up. 
loop(SupervisorPid, Frequencies) ->
    receive
        {request, Pid, allocate} ->
            {NewFrequencies, Reply} = allocate(Frequencies, Pid),
            Pid ! {reply, Reply},
            loop(SupervisorPid, NewFrequencies);
        {request, Pid , {deallocate, Freq}} ->
            NewFrequencies = deallocate(Frequencies, Freq),
            Pid ! {reply, ok},
            loop(SupervisorPid, NewFrequencies);
        {request, Pid, stop} ->
            Pid ! {reply, stopped};
        {'EXIT', SupervisorPid, _Frequencies} ->
            io:format("PID ~p: frequency:loop stopping having trapped supervisor EXIT~n", [self()]);
        {'EXIT', Pid, _Reason} ->
            io:format("PID ~p: frequency:loop trapped EXIT of: ~p~n", [self(), Pid]),    
            NewFrequencies = exited(Frequencies, Pid), 
            loop(SupervisorPid, NewFrequencies)
    end.

%% Functional interface
allocate() -> 
    frequency ! {request, self(), allocate},
    receive 
        {reply, Reply} -> Reply
    end.

deallocate(Freq) -> 
    frequency ! {request, self(), {deallocate, Freq}},
    receive 
        {reply, Reply} -> Reply
    end.

stop() -> 
    frequency ! {request, self(), stop},
    receive 
        {reply, Reply} -> Reply
    end.


%% The Internal Help Functions used to allocate and
%% deallocate frequencies.

allocate({[], Allocated}, _Pid) ->
    {{[], Allocated}, {error, no_frequency}};
allocate({[Freq|Free], Allocated}, Pid) ->
    link(Pid),                                               
    {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}}.

deallocate({Free, Allocated}, Freq) ->
    {value,{Freq,Pid}} = lists:keysearch(Freq,1,Allocated),  
    unlink(Pid),                                            
    NewAllocated=lists:keydelete(Freq, 1, Allocated),
    {[Freq|Free],  NewAllocated}.

exited({Free, Allocated}, Pid) ->                
    case lists:keysearch(Pid,2,Allocated) of
        {value,{Freq,Pid}} ->
            NewAllocated = lists:keydelete(Freq,1,Allocated),
            {[Freq|Free],NewAllocated}; 
        false ->
            {Free,Allocated} 
    end.
