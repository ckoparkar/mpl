MPL=../build/bin/mpl
FLAGS=-default-type int64 -cc-opt -DINT_NO_CHECK

PROGRAMS = \
	benchrunner
        # msort \
	# nn

all: $(PROGRAMS)

$(PROGRAMS): %: src/* src/lib/*
	$(MPL) $(FLAGS) -cc-opt -DINT_NO_CHECK -output src/$*.exe src/$*.mlb
	@echo "successfully built $*"

.PHONY: clean
clean:
	rm -f $(PROGRAMS)
