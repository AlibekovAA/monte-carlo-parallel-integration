-module(file_reader).
-export([read_input/1]).

read_input(Filename) ->
    case file:read_file(Filename) of
        {ok, Binary} ->
            Content = binary_to_list(Binary),
            parse_content(string:tokens(Content, "\n"));
        {error, Reason} ->
            {error, Reason}
    end.

parse_content([BoundsLine, PointsLine, ProcessesLine]) ->
    Bounds = parse_bounds(remove_prefix(BoundsLine, "bounds:")),
    Points = parse_points(remove_prefix(PointsLine, "points:")),
    Processes = parse_processes(remove_prefix(ProcessesLine, "count_processes:")),
    {ok, {Bounds, Points, Processes}}.

remove_prefix(String, Prefix) ->
    case string:prefix(String, Prefix) of
        nomatch -> String;
        Rest -> string:trim(Rest)
    end.

parse_bounds(BoundsStr) ->
    CleanStr = string:trim(BoundsStr, both, "\r "),
    [X1Y1, X2Y2] = string:tokens(CleanStr, ","),
    {X1, Y1} = parse_coordinates(X1Y1),
    {X2, Y2} = parse_coordinates(X2Y2),
    {{X1, X2}, {Y1, Y2}}.

parse_coordinates(CoordStr) ->
    [X, Y] = string:tokens(string:trim(CoordStr), " "),
    {list_to_integer(X), list_to_integer(Y)}.

parse_points(PointsStr) ->
    CleanStr = string:trim(PointsStr, both, "\r "),
    list_to_integer(CleanStr).

parse_processes(ProcessesStr) ->
    CleanStr = string:trim(ProcessesStr, both, "\r "),
    list_to_integer(CleanStr).
