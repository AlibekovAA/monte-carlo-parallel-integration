-module(main).
-export([start/0]).

start() ->
    case file_reader:read_input("input.txt") of
        {error, Reason} ->
            io:format("Error reading file: ~p~n", [Reason]);
        {ok, {{{X1, Y1}, {X2, Y2}}, Points, Processes}} ->
            io:format("~nRead data from file:~n"),
            io:format("Function: sin(x) + 2 * cos(y)~n"),
            io:format("Bounds: X=[~p,~p], Y=[~p,~p]~n",
                     [X1, X2, Y1, Y2]),
            io:format("Number of points: ~p~n", [Points]),
            io:format("Number of processes: ~p~n", [Processes]),

            StartTime = erlang:system_time(millisecond),

            Result = monte_carlo:integrate(
                fun(X, Y) -> math:sin(X) + 2 * math:cos(Y) end,
                {X1, X2}, {Y1, Y2},
                Points,
                Processes
            ),

            EndTime = erlang:system_time(millisecond),
            TimeTaken = (EndTime - StartTime) / 1000,

            io:format("Integral result: ~p~n", [Result]),
            io:format("Time taken: ~.3f seconds~n", [TimeTaken]),

            ok
    end.
