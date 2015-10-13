# erlang_record_pretty_print
tool for more pretty print erlang record, base on parse transform generates metadata such as field names.

Tips
-----
Read test.erl file for more information

Test
-----
```erlang
Erlang/OTP 17 [erts-6.4] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false]

Eshell V6.4  (abort with ^G)
1> test:test().
"#test_record{field_a = 1,field_b = 2}"
```