-module(esan_tests).

-compile(export_all).

test() ->
    {ok, Html1} = file:read_file("test/test.html"),
    {ok, Html2} = esan:sanitize(binary_to_list(Html1)),
    io:format("~p~n", [Html2]).
