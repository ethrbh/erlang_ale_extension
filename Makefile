PROJECT = erlang_ale_extension

DEPS = erlang_ale
#dep_erlang_ale = git https://github.com/esl/erlang_ale.git master
dep_erlang_ale = git https://github.com/ethrbh/erlang_ale.git update_for_otp-23

include erlang.mk
