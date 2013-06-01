#!/bin/sh

path=`dirname "${0}"`
link=`readlink "${0}"`
[ -n "${link}" ] && path=`dirname "${link}"`
cd "${path}"

erl -sname shutdown@localhost -smp disable -eval "rpc:call (ytmd@localhost, init, stop, []), init:stop ()." -noshell
