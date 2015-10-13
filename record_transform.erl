%% Copyright (c) 2011-2012 Basho Technologies, Inc.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.

%% @doc The parse transform used for lager messages.
%% This parse transform rewrites functions calls to lager:Severity/1,2 into
%% a more complicated function that captures module, function, line, pid and
%% time as well. The entire function call is then wrapped in a case that
%% checks the lager_config 'loglevel' value, so the code isn't executed if
%% nothing wishes to consume the message.

%%----------------------------------------------------
%% @desc: record metadata into beam
%% @author chengcheng<cyoucmi@gmail.com>
%% @date: 2015年10月13日17:23:03 
%%----------------------------------------------------
-module(record_transform).
-export([parse_transform/2]).


%% @private
parse_transform(AST, _Options) ->
    erlang:put(records, []),
    walk_ast([], AST).


walk_ast(Acc, []) ->
    insert_record_attribute(Acc);

walk_ast(Acc, [{attribute, _, record, {Name, Fields}}=H|T]) ->
    FieldNames = lists:map(fun({record_field, _, {atom, _, FieldName}}) ->
                FieldName;
            ({record_field, _, {atom, _, FieldName}, _Default}) ->
                FieldName
        end, Fields),
    stash_record({Name, FieldNames}),
    walk_ast([H|Acc], T);

walk_ast(Acc, [H|T]) ->
    walk_ast([H|Acc], T).

stash_record(Record) ->
    Records = case erlang:get(records) of
        undefined ->
            [];
        R ->
            R
    end,
    erlang:put(records, [Record|Records]).

insert_record_attribute(AST) ->
    lists:foldl(fun({attribute, Line, module, _}=E, Acc) ->
                [E, {attribute, Line, records_fields, erlang:get(records)}|Acc];
            (E, Acc) ->
                [E|Acc]
        end, [], AST).
