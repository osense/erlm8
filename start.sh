#! /bin/bash

erl -boot start_sasl \
    -pa ebin deps/jiffy/ebin deps/jiffy/priv deps/ej/ebin \
    -setcookie erlm8cookie \
    -name erl8m \
    -eval "erlm8:start(), timer:sleep(infinity)."
