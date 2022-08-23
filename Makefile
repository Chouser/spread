.PHONY: all setup update clean test

all: setup test

setup:
	clojure -P

update:
	clojure -M:outdated --every --write

clean:
	rm -rf target/public

test:
	clojure -M:test

