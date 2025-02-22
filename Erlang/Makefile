ERLC = erlc
ESCRIPT = escript
SRC = file_reader.erl main.erl monte_carlo.erl
ESCRIPT_FILE = run.escript
ERLC_FLAGS = +native +{hipe,o3} +inline

.PHONY: all clean run rebuild

all: compile

compile:
	$(ERLC) $(ERLC_FLAGS) $(SRC)

run: compile
	$(ESCRIPT) $(ESCRIPT_FILE)

clean:
	@if exist *.beam (del /Q *.beam)

rebuild: clean all run
