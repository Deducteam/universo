.PHONY: all
all: bin

.PHONY: bin
bin:
	@dune build

.PHONY: doc
doc:
	@dune build @doc

DKCHECK  = dkcheck
UNIVERSO = $(shell readlink -f _build/install/default/bin/universo)

MATITA_PATH=experiments/matita2
MODE=
TARGET=

.PHONY: theory
theory:
	$(DKCHECK) -e $(MATITA_PATH)/theory/cic.dk

.PHONY: univ
univ:
	$(DKCHECK) -e encodings/universo.dk

.PHONY: test
test: bin theory univ
	@$(UNIVERSO) -d s $(MODE) -I $(MATITA_PATH)/theory \
	--to-elaboration $(MATITA_PATH)/compatibility/in.dk \
	--of-universo    $(MATITA_PATH)/compatibility/out.dk \
	--to-theory      $(MATITA_PATH)/compatibility/theory.dk	\
	--theory         $(MATITA_PATH)/theory/cic.dk \
	$(MATITA_PATH)/input/test.dk -o $(MATITA_PATH)/output

.PHONY: test2
test2: bin theory univ
	@$(UNIVERSO) -d s $(MODE) -I $(MATITA_PATH)/theory \
	--to-elaboration $(MATITA_PATH)/compatibility/in.dk \
	--of-universo    $(MATITA_PATH)/compatibility/out.dk \
	--to-theory      $(MATITA_PATH)/compatibility/theory.dk	\
	--theory         $(MATITA_PATH)/theory/cic.dk \
	$(MATITA_PATH)/input/test.dk \
	$(MATITA_PATH)/input/test2.dk -o $(MATITA_PATH)/output

.PHONY: logic
logic: bin theory univ
	@$(UNIVERSO) -d s $(MODE) -I $(MATITA_PATH)/theory \
	--to-elaboration $(MATITA_PATH)/compatibility/in.dk \
	--of-universo    $(MATITA_PATH)/compatibility/out.dk \
	--to-theory      $(MATITA_PATH)/compatibility/theory.dk	\
	--theory         $(MATITA_PATH)/theory/cic.dk \
	$(MATITA_PATH)/input/matita_basics_logic.dk -o $(MATITA_PATH)/output

INPUT_DIR=$(MATITA_PATH)/input

FILES=experiments/matita2/input/matita_basics_logic.dk experiments/matita2/input/matita_basics_relations.dk experiments/matita2/input/matita_basics_bool.dk experiments/matita2/input/matita_arithmetics_nat.dk experiments/matita2/input/matita_arithmetics_div_and_mod.dk experiments/matita2/input/matita_basics_types.dk experiments/matita2/input/matita_arithmetics_bigops.dk experiments/matita2/input/matita_arithmetics_exp.dk experiments/matita2/input/matita_arithmetics_factorial.dk experiments/matita2/input/matita_arithmetics_minimization.dk experiments/matita2/input/matita_arithmetics_primes.dk experiments/matita2/input/matita_arithmetics_sigma_pi.dk experiments/matita2/input/matita_arithmetics_binomial.dk experiments/matita2/input/matita_arithmetics_bounded_quantifiers.dk experiments/matita2/input/matita_arithmetics_log.dk experiments/matita2/input/matita_arithmetics_chebyshev_chebyshev_psi.dk experiments/matita2/input/matita_arithmetics_gcd.dk experiments/matita2/input/matita_arithmetics_chebyshev_chebyshev_theta.dk experiments/matita2/input/matita_arithmetics_ord.dk experiments/matita2/input/matita_arithmetics_chebyshev_factorization.dk experiments/matita2/input/matita_arithmetics_chebyshev_psi_bounds.dk experiments/matita2/input/matita_arithmetics_sqrt.dk experiments/matita2/input/matita_arithmetics_chebyshev_bertrand.dk experiments/matita2/input/matita_basics_lists_list.dk experiments/matita2/input/matita_arithmetics_chebyshev_bertrand256.dk experiments/matita2/input/matita_arithmetics_congruence.dk experiments/matita2/input/matita_arithmetics_chinese_reminder.dk experiments/matita2/input/matita_arithmetics_permutation.dk experiments/matita2/input/matita_arithmetics_fermat_little_theorem.dk experiments/matita2/input/matita_arithmetics_iteration.dk experiments/matita2/input/matita_arithmetics_lstar.dk experiments/matita2/input/matita_arithmetics_pidgeon_hole.dk experiments/matita2/input/matita_basics_deqsets.dk experiments/matita2/input/matita_basics_lists_listb.dk experiments/matita2/input/matita_basics_finset.dk experiments/matita2/input/matita_basics_jmeq.dk experiments/matita2/input/matita_basics_sets.dk experiments/matita2/input/matita_basics_star.dk experiments/matita2/input/matita_basics_star1.dk experiments/matita2/input/matita_basics_vectors.dk experiments/matita2/input/matita_basics_vector_finset.dk experiments/matita2/input/matita_hints_declaration.dk

BUG=experiments/matita2/input/matita_basics_logic.dk experiments/matita2/input/matita_basics_relations.dk experiments/matita2/input/matita_basics_bool.dk experiments/matita2/input/matita_arithmetics_nat.dk experiments/matita2/input/matita_basics_types.dk experiments/matita2/input/matita_basics_lists_list.dk experiments/matita2/input/matita_basics_vectors.dk


FILES_LIGHT=experiments/matita2/input/matita_basics_logic.dk experiments/matita2/input/matita_basics_relations.dk experiments/matita2/input/matita_basics_bool.dk experiments/matita2/input/matita_arithmetics_nat.dk experiments/matita2/input/matita_arithmetics_div_and_mod.dk experiments/matita2/input/matita_basics_types.dk experiments/matita2/input/matita_arithmetics_bigops.dk experiments/matita2/input/matita_arithmetics_exp.dk experiments/matita2/input/matita_arithmetics_factorial.dk experiments/matita2/input/matita_arithmetics_minimization.dk experiments/matita2/input/matita_arithmetics_primes.dk experiments/matita2/input/matita_arithmetics_sigma_pi.dk experiments/matita2/input/matita_arithmetics_binomial.dk experiments/matita2/input/matita_arithmetics_bounded_quantifiers.dk experiments/matita2/input/matita_arithmetics_log.dk experiments/matita2/input/matita_arithmetics_chebyshev_chebyshev_psi.dk experiments/matita2/input/matita_arithmetics_gcd.dk experiments/matita2/input/matita_arithmetics_chebyshev_chebyshev_theta.dk experiments/matita2/input/matita_arithmetics_ord.dk experiments/matita2/input/matita_arithmetics_chebyshev_factorization.dk experiments/matita2/input/matita_arithmetics_chebyshev_psi_bounds.dk experiments/matita2/input/matita_arithmetics_sqrt.dk experiments/matita2/input/matita_arithmetics_chebyshev_bertrand.dk experiments/matita2/input/matita_basics_lists_list.dk experiments/matita2/input/matita_arithmetics_chebyshev_bertrand256.dk experiments/matita2/input/matita_arithmetics_congruence.dk experiments/matita2/input/matita_arithmetics_chinese_reminder.dk experiments/matita2/input/matita_arithmetics_permutation.dk experiments/matita2/input/matita_arithmetics_fermat_little_theorem.dk experiments/matita2/input/matita_arithmetics_iteration.dk experiments/matita2/input/matita_arithmetics_lstar.dk experiments/matita2/input/matita_arithmetics_pidgeon_hole.dk #experiments/matita2/input/matita_basics_deqsets.dk experiments/matita2/input/matita_basics_lists_listb.dk experiments/matita2/input/matita_basics_finset.dk experiments/matita2/input/matita_basics_jmeq.dk experiments/matita2/input/matita_basics_sets.dk experiments/matita2/input/matita_basics_star.dk experiments/matita2/input/matita_basics_star1.dk experiments/matita2/input/matita_basics_vectors.dk experiments/matita2/input/matita_basics_vector_finset.dk experiments/matita2/input/matita_hints_declaration.dk

.PHONY: matita
bug: bin theory univ
	@$(UNIVERSO) -d s $(MODE) -I $(MATITA_PATH)/theory \
	--to-elaboration $(MATITA_PATH)/compatibility/in.dk \
	--of-universo    $(MATITA_PATH)/compatibility/out.dk \
	--to-theory      $(MATITA_PATH)/compatibility/theory.dk	\
	--theory         $(MATITA_PATH)/theory/cic.dk \
	$(BUG) -o $(MATITA_PATH)/output

.PHONY: matita
matita: bin theory univ
	@$(UNIVERSO) -d s $(MODE) -I $(MATITA_PATH)/theory \
	--to-elaboration $(MATITA_PATH)/compatibility/in.dk \
	--of-universo    $(MATITA_PATH)/compatibility/out.dk \
	--to-theory      $(MATITA_PATH)/compatibility/theory.dk	\
	--theory         $(MATITA_PATH)/theory/cic.dk \
	$(FILES) -o $(MATITA_PATH)/output

.PHONY: matita-light
matita-light: bin theory univ
	@$(UNIVERSO) -d s $(MODE) -I $(MATITA_PATH)/theory \
	--to-elaboration  $(MATITA_PATH)/compatibility/in.dk \
	--of-universo     $(MATITA_PATH)/compatibility/out.dk \
	--to-theory       $(MATITA_PATH)/compatibility/theory.dk	\
	--theory          $(MATITA_PATH)/theory/cic.dk \
	$(FILES_LIGHT) -o $(MATITA_PATH)/output

.PHONY: check_input
check_input:
	cd $(MATITA_PATH)/input && make

.PHONY: check_test
check: $(TARGET)
	cd $(MATITA_PATH)/output && make

.PHONY: clean
clean:
	@dune clean
	cd $(MATITA_PATH)/input && make clean
	cd $(MATITA_PATH)/output && make clean
