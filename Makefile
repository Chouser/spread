.PHONY: all setup update test

all: setup test

setup:
	clojure -P

update:
	clojure -M:outdated --every --write

test:
	clojure -M:test

