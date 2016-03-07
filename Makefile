PROJECT = erlang_ale_extension

DEPS = erlang_ale lager
dep_erlang_ale = git https://github.com/esl/erlang_ale.git master
dep_lager = git https://github.com/basho/lager 3.1.0

include erlang.mk
