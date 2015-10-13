%%----------------------------------------------------
%% @desc: record pretty print
%% @author chengcheng<cyoucmi@gmail.com>
%% @date: 2015年10月13日17:23:03 
%%----------------------------------------------------
-module(record_pretty).

-export([
        p/2
    ]).

p(Info, Module)->
    List = p1(Info, Module),
    lists:flatten(List).
    

p1(Tuple, Module) when is_tuple(Tuple)->
    case is_record_known(Tuple, Module) of
        false->
            p_tuple_raw(Tuple, Module);
        {_Name, RecordFields} ->
            p_record(Tuple, RecordFields, Module)
    end;

p1(List, Module) when is_list(List)->
    [
        "[",
        make_raw_list(List, Module),
        "]"
    ];

p1(Info, _Module)->
    io_lib:format("~p",[Info]).


p_tuple_raw(Tuple, Module)->
    [
        "{",
        make_raw_list(tuple_to_list(Tuple), Module),
        "}"
    ].

p_record(Record, RecordFields, Module)->
    Name = element(1, Record),
    L = 
    lists:foldl(
        fun(Index, Acc)->
                Elem = element(1 + Index, Record),
                Acc ++ 
                [
                    [atom_to_list(lists:nth(Index, RecordFields)) , " = ", p1(Elem, Module)],
                    ","
                ]
        end,
        [],
        lists:seq(1, length(RecordFields))
    ),
    L1 = case L =:= [] of
        true->
            L;
        false->
            lists:droplast(L)
    end,
    ["#", atom_to_list(Name), "{", L1, "}"].


make_raw_list(List, Module)->
    L1 = lists:map(
        fun(Ele) ->
                [p1(Ele, Module), ","]
        end,
        List),
    case L1 =:= [] of
        true->
            L1;
        false->
            [Last, _] = lists:last(L1),
            [lists:droplast(L1), Last]
    end.


is_record_known(Record, Module) ->
    Name = element(1, Record),
    Attrs = Module:module_info(attributes),
    case lists:keyfind(records_fields, 1, Attrs) of
        false -> false;
        {records_fields, Records} ->
            case lists:keyfind(Name, 1, Records) of
                false -> false;
                {Name, RecordFields} ->
                    case (tuple_size(Record) - 1) =:= length(RecordFields) of
                        false -> false;
                        true -> {Name, RecordFields}
                    end
            end
    end.




