-module(atomizer_test).

-include_lib("eunit/include/eunit.hrl").
-include("../src/atomizer.hrl").

-define(AtomFeedTest, "http://droid-toolbox.com/feed.atom").

atomizer_test() ->
    Res = atomizer:parse_url(?AtomFeedTest),
    io:format("Res: ~p", [Res]),
    
    ?assertEqual(Res#feed.title, "Recent Libraries"),
    ?assert(is_list(Res#feed.entries)).
