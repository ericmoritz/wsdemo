-module(packet_reader).

-export([open/1, next/1, foldl/3]).

open(Filename) ->
    {ok, FH} = file:open(Filename, [read, binary]),
    new(FH, <<>>).

new(FH, Rest) ->
    {?MODULE, FH, Rest}.

next({_, FH, Rest}) ->
    case file:read(FH, 2) of
        {ok, Size} ->
            read_packet(FH, <<Rest/binary, Size/binary>>);
        Other ->
            Other
    end.

foldl(Fun, Acc0, PR) ->
    case PR:next() of
        {ok, Packet, PR2} ->
            Acc1 = Fun(Packet, Acc0),
            PR2:foldl(Fun, Acc1);
        eof ->
            Acc0;
        Other ->
            Other
    end.

%% Internal

read_packet(FH, CurrentBin) ->
    case erlang:decode_packet(2, CurrentBin, []) of
        {ok, Packet, Rest} ->
            NewReader = new(FH, Rest),
            {ok, binary_to_term(Packet), NewReader};
        {more, Length} ->
            case file:read(FH, Length) of
                {ok, More} ->
                    read_packet(FH, <<CurrentBin/binary, More/binary>>);
                Else ->
                    Else
            end;
        {error, Reason} ->
            {error, Reason}
    end.
