.EXPORT_ALL_VARIABLES:
.PHONY: test

test:
	clj -M:dev -m "cognitect.test-runner"

repl:
	clj -M:dev -m "tools.repl" -p 3001
