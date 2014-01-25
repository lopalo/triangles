-module(tri_utils).

-export([pack_json/1, unpack_json/1, atom_keys/1, filter_none/1]).


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

filter_none(none) -> false;
filter_none(_Val) -> true.


