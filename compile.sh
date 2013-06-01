 #!/bin/sh
 
path=`dirname "${0}"`
link=`readlink "${0}"`
[ -n "${link}" ] && path=`dirname "${link}"`
cd "${path}"

cd src
erl -eval "compile:file(conf),compile:file(html),compile:file(netw),compile:file(rssf),compile:file(text),compile:file(time),compile:file(ytmd),init:stop()." -noshell
cd ..
