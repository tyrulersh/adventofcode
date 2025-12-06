TEST_CASES = $(patsubst tst/%.input.txt,test-%,$(wildcard tst/*.input.txt))

all: test

safe: src/safe.hs
	ghc -o $@ $<

test: $(TEST_CASES)

test-%: tst/%.input.txt tst/%.expected.txt safe
	@(cmp --silent <(cat $< | ./safe) $(word 2,$^) && echo -e "\e[32mPASS\e[0m" $@) || true
	@(cmp --silent <(cat $< | ./safe) $(word 2,$^) || echo -e "\e[31mFAIL\e[0m" $@) && diff -u <(cat $< | ./safe) $(word 2,$^) || true

clean:
	rm -f safe src/safe.hi src/safe.o
