##### FutureLearn / University of Kent: Concurrent Programming in Erlang
### Week 2 Assignment


#### Adding a supervisor
Program a supervisor process that can be used to start the frequency server, and 
restart it whenever it has terminated unexpectedly.

In adding the supervisor you will need to think about the state of the server 
when it is restarted. The following discussion may help you to think about that.

In a typical client-server scenario, the clients are not under the control of 
the supervisor: any client can connect to the server as long as it knows the 
name (or the Pid) of the server. So, we can’t expect the supervisor to restart 
the clients; on the other hand we can ensure that if the server terminates 
unexpectedly then the clients do too.


#### Using the observer
Using the observer tool as described in the previous exercise, observe how your
system behaves when some of the constituent processes – including the supervisor
itself – are killed.

Recall that to run the observer, type `observer:start()` in the Erlang shell, 
and that we use `exit(Pid,kill)` to kill the process with pid Pid.
