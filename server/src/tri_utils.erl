-module(tri_utils).

-export([pack_json/1, unpack_json/1, atom_keys/1, ms/0, vect_transform/2]).


pack_json([{_Key, _Value}|_Is] = Items) ->
    NewItems = [{Key, pack_json(Value)} || {Key, Value} <- Items],
    {NewItems};
pack_json(Items) when is_list(Items) ->
    [pack_json(I) || I <- Items];
pack_json(Item) ->
    Item.

unpack_json({[{_Key, _Value}|_Is] = Items}) ->
    NewItems = [{Key, unpack_json(Value)} || {Key, Value} <- Items],
    NewItems;
unpack_json(Items) when is_list(Items) ->
    [unpack_json(I) || I <- Items];
unpack_json(Item) ->
    Item.

atom_keys(List) ->
    [{binary_to_atom(Key, utf8), Value} || {Key, Value} <- List].

ms() ->
    N = now(),
    element(2, N) * 1000 + trunc(element(3, N) / 1000).

vect_transform(Length, Angle) ->
    Rad = Angle / 57.3,
    X = Length * math:cos(Rad),
    Y = Length * math:sin(Rad),
    [X, Y].

