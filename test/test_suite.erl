-module(test_suite).

-author("me@danielparnell.com").

-include_lib("eunit/include/eunit.hrl").

all_test_() ->
  [
   {module, dcpu16_core_test},
   {module, dcpu16_asm_test}
  ].
