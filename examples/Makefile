MPL=../build/bin/mpl
FLAGS=-default-type int64 -default-type word64

# PROGRAMS= \
# 	fib \
# 	random \
# 	primes \
# 	msort \
# 	dmm \
# 	ray \
# 	tokens \
# 	nn \
# 	dedup \
# 	buildtree \
# 	sbuildtree \
# 	qsort

PROGRAMS= \
	parfib

all: $(PROGRAMS)

$(PROGRAMS): %: src/* src/lib/*
	$(MPL) $(FLAGS) -output src/$*.exe src/$*.mlb
	@echo "successfully built $*"

.PHONY: clean
clean:
	rm -f $(PROGRAMS)
