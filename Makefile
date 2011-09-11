REBAR=./rebar
DATA=data
EX=../example


all: compile

compile: $(REBAR)
	@$(REBAR) compile

clean: $(REBAR)
	@$(REBAR) clean

test:
	erl -pa ebin -noshell -run hashtrie_test test -run erlang halt

test_file:
	erl -pa ebin -noshell -run hashtrie_test test_file -run erlang halt

plots:
	(cd $(DATA) && eplot -plot bar2d -margin 40 -x_label "#elements" -y_label "ratio" -width 800 -height 600 -o $(EX)/data_get.png dict_fetch.dat gb_trees_get.dat ht_get.dat pht_get.dat)
	(cd $(DATA) && eplot -plot bar2d -margin 40 -x_label "#elements" -y_label "ratio" -width 800 -height 600 -o $(EX)/data_put.png dict_store.dat gb_trees_insert.dat ht_put.dat pht_put.dat)
	(cd $(DATA) && eplot -plot bar2d -margin 40 -x_label "#elements" -y_label "ratio" -width 800 -height 600 -o $(EX)/data_update.png dict_update.dat gb_trees_update.dat ht_update.dat pht_update.dat)
