
-module(pingpong).

-export([start/0, play/1, stop/0, ping_loop/1, pong_loop/0]).

start() ->
    register(ping, spawn(?MODULE, ping_loop, [0])),
    register(pong, spawn(?MODULE, pong_loop, [])).

play(N) ->
    ping ! N.

stop() ->
    ping ! stop,
    pong ! stop.


ping_loop(Total) ->
    receive
        stop ->
            io:format("Stopped abruptly~n");
        0 ->
            io:format("Ping! 0, Total: ~w~n", [Total]),
            ping_loop(Total);
        N ->
            io:format("Ping! ~w, Total: ~w~n", [N, Total + N]),
            pong ! (N-1),
            timer:sleep(1_000),
            ping_loop(Total + N)
    after
        20_000 ->
            io:format("Ping ended~n")
    end.

pong_loop() ->
    receive
        stop ->
            io:format("Stopped abruptly~n");
        0 ->
            io:format("Pong! 0~n"),
            pong_loop();
        N ->
            io:format("Pong! ~w~n", [N]),
            ping ! (N-1),
            timer:sleep(1_000),
            pong_loop()
    after
        20_000 ->
            io:format("Pong ended~n")
    end.