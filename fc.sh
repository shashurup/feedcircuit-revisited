#!/bin/sh
java -Dclojure.tools.logging.factory=clojure.tools.logging.impl/jul-factory \
     -Djava.util.logging.SimpleFormatter.format='%1$tF %1$tT %4$s: %5$s%6$s%n' \
     -jar $1 config &> log &
