.PHONY: run

all: semester.beam

semester.beam: semester.erl
	@echo Compiling
	@erlc semester.erl

run: semester.beam
	@./run-it
