#!/bin/sh

path=`dirname "${0}"`
link=`readlink "${0}"`
[ -n "${link}" ] && path=`dirname "${link}"`
cd "${path}"

cd src
nice -n 5 erl -sname ytmd@localhost -smp disable -ytmd cfgfile "\"/etc/ytm.xml\"" -eval "application:start (ytmd)." -noshell
