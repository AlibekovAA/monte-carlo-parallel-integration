-module(monte_carlo).
-export([integrate/5]).

integrate(Function, {X1, X2}, {Y1, Y2}, NumPoints, NumProcesses) ->
    BasePointsPerProcess = NumPoints div NumProcesses,
    LastProcessPoints = BasePointsPerProcess + (NumPoints rem NumProcesses),
    Parent = self(),

    [spawn(fun() -> worker(Function, {X1, X2}, {Y1, Y2}, BasePointsPerProcess, Parent) end)
     || _ <- lists:seq(1, NumProcesses-1)],

    spawn(fun() -> worker(Function, {X1, X2}, {Y1, Y2}, LastProcessPoints, Parent) end),

    Sum = collect_results(NumProcesses, 0),

    Area = (X2 - X1) * (Y2 - Y1),
    (Area * Sum) / NumPoints.

worker(Function, {X1, X2}, {Y1, Y2}, NumPoints, Parent) ->
    Points = lists:seq(1, NumPoints),
    Sum = lists:foldl(fun(_, Acc) ->
        X = X1 + rand:uniform() * (X2 - X1),
        Y = Y1 + rand:uniform() * (Y2 - Y1),
        Acc + Function(X, Y)
    end, 0, Points),
    Parent ! {result, Sum}.

collect_results(0, Acc) -> Acc;
collect_results(N, Acc) ->
    receive
        {result, Sum} -> collect_results(N-1, Acc + Sum)
    end.
