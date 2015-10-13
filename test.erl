-module(test).

-export([test/0]).

-compile([{parse_transform, record_transform}]).

-record(test_record, {
        field_a, 
        field_b
    }).

test()->
    record_pretty:p({test_record, 1, 2}, ?MODULE).
