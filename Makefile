.EXPORT_ALL_VARIABLES:
.PHONY: test

test:
	clj -A:dev -m "cognitect.test-runner"
