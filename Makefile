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

MODE=
TARGET=

MATITA_PATH=experiments/matita2
MATITA_THEORY_DK=$(MATITA_PATH)/theory/cic.dk
MATITA_THEORY_DKO=$(MATITA_PATH)/theory/cic.dko

UNIVERSO_DK=encodings/universo.dk
UNIVERSO_DKO=encodings/universo.dko


$(MATITA_THEORY_DKO):
	$(DKCHECK) -e $(MATITA_THEORY_DK)

$(UNIVERSO_DKO):
	$(DKCHECK) -e $(UNIVERSO_DK)

MATITA_IN=$(MATITA_PATH)/input
MATITA_FILES=$(wildcard $(MATITA_IN)/*.dk)

.library_depend_dko: $(MATITA_FILES)
	@echo "[DKDEP (DK FILES)] $@"
	@$(DKDEP) -o $@ -I $(MATITA_PATH)/theory -I $(MATITA_IN) $^

ifneq ($(MAKECMDGOALS), clean)
	-include .library_depend_dko
endif

FILES=experiments/matita2/input/matita_basics_logic.dk experiments/matita2/input/matita_basics_relations.dk experiments/matita2/input/matita_basics_bool.dk experiments/matita2/input/matita_arithmetics_nat.dk experiments/matita2/input/matita_arithmetics_div_and_mod.dk experiments/matita2/input/matita_basics_types.dk experiments/matita2/input/matita_arithmetics_bigops.dk experiments/matita2/input/matita_arithmetics_exp.dk experiments/matita2/input/matita_arithmetics_factorial.dk experiments/matita2/input/matita_arithmetics_minimization.dk experiments/matita2/input/matita_arithmetics_primes.dk experiments/matita2/input/matita_arithmetics_sigma_pi.dk experiments/matita2/input/matita_arithmetics_binomial.dk experiments/matita2/input/matita_arithmetics_bounded_quantifiers.dk experiments/matita2/input/matita_arithmetics_log.dk experiments/matita2/input/matita_arithmetics_chebyshev_chebyshev_psi.dk experiments/matita2/input/matita_arithmetics_gcd.dk experiments/matita2/input/matita_arithmetics_chebyshev_chebyshev_theta.dk experiments/matita2/input/matita_arithmetics_ord.dk experiments/matita2/input/matita_arithmetics_chebyshev_factorization.dk experiments/matita2/input/matita_arithmetics_chebyshev_psi_bounds.dk experiments/matita2/input/matita_arithmetics_sqrt.dk experiments/matita2/input/matita_arithmetics_chebyshev_bertrand.dk experiments/matita2/input/matita_basics_lists_list.dk experiments/matita2/input/matita_arithmetics_chebyshev_bertrand256.dk experiments/matita2/input/matita_arithmetics_congruence.dk experiments/matita2/input/matita_arithmetics_chinese_reminder.dk experiments/matita2/input/matita_arithmetics_permutation.dk experiments/matita2/input/matita_arithmetics_fermat_little_theorem.dk experiments/matita2/input/matita_arithmetics_iteration.dk experiments/matita2/input/matita_arithmetics_lstar.dk experiments/matita2/input/matita_arithmetics_pidgeon_hole.dk experiments/matita2/input/matita_basics_deqsets.dk experiments/matita2/input/matita_basics_lists_listb.dk experiments/matita2/input/matita_basics_finset.dk experiments/matita2/input/matita_basics_jmeq.dk experiments/matita2/input/matita_basics_sets.dk experiments/matita2/input/matita_basics_star.dk experiments/matita2/input/matita_basics_star1.dk experiments/matita2/input/matita_basics_vectors.dk experiments/matita2/input/matita_basics_vector_finset.dk experiments/matita2/input/matita_hints_declaration.dk

FILES_LIGHT=experiments/matita2/input/matita_basics_logic.dk experiments/matita2/input/matita_basics_relations.dk experiments/matita2/input/matita_basics_bool.dk experiments/matita2/input/matita_arithmetics_nat.dk #experiments/matita2/input/matita_arithmetics_div_and_mod.dk experiments/matita2/input/matita_basics_types.dk experiments/matita2/input/matita_arithmetics_bigops.dk experiments/matita2/input/matita_arithmetics_exp.dk experiments/matita2/input/matita_arithmetics_factorial.dk experiments/matita2/input/matita_arithmetics_minimization.dk experiments/matita2/input/matita_arithmetics_primes.dk experiments/matita2/input/matita_arithmetics_sigma_pi.dk experiments/matita2/input/matita_arithmetics_binomial.dk experiments/matita2/input/matita_arithmetics_bounded_quantifiers.dk experiments/matita2/input/matita_arithmetics_log.dk experiments/matita2/input/matita_arithmetics_chebyshev_chebyshev_psi.dk experiments/matita2/input/matita_arithmetics_gcd.dk experiments/matita2/input/matita_arithmetics_chebyshev_chebyshev_theta.dk experiments/matita2/input/matita_arithmetics_ord.dk experiments/matita2/input/matita_arithmetics_chebyshev_factorization.dk experiments/matita2/input/matita_arithmetics_chebyshev_psi_bounds.dk experiments/matita2/input/matita_arithmetics_sqrt.dk experiments/matita2/input/matita_arithmetics_chebyshev_bertrand.dk experiments/matita2/input/matita_basics_lists_list.dk experiments/matita2/input/matita_arithmetics_chebyshev_bertrand256.dk experiments/matita2/input/matita_arithmetics_congruence.dk experiments/matita2/input/matita_arithmetics_chinese_reminder.dk experiments/matita2/input/matita_arithmetics_permutation.dk experiments/matita2/input/matita_arithmetics_fermat_little_theorem.dk experiments/matita2/input/matita_arithmetics_iteration.dk experiments/matita2/input/matita_arithmetics_lstar.dk experiments/matita2/input/matita_arithmetics_pidgeon_hole.dk experiments/matita2/input/matita_basics_deqsets.dk experiments/matita2/input/matita_basics_lists_listb.dk experiments/matita2/input/matita_basics_finset.dk experiments/matita2/input/matita_basics_jmeq.dk experiments/matita2/input/matita_basics_sets.dk experiments/matita2/input/matita_basics_star.dk experiments/matita2/input/matita_basics_star1.dk experiments/matita2/input/matita_basics_vectors.dk experiments/matita2/input/matita_basics_vector_finset.dk experiments/matita2/input/matita_hints_declaration.dk

.PHONY: matita
matita: bin $(MATITA_THEORY_DKO) $(UNIVERSO_DKO)
	@$(UNIVERSO) -d s $(MODE) -I $(MATITA_PATH)/theory \
	--to-elaboration $(MATITA_PATH)/compatibility/in.dk \
	--of-universo    $(MATITA_PATH)/compatibility/out.dk \
	--to-theory      $(MATITA_PATH)/compatibility/theory.dk	\
	--constraints    $(MATITA_PATH)/compatibility/constraints.dk \
	--theory         $(MATITA_PATH)/theory/cic.dk \
	$(FILES) -o $(MATITA_PATH)/output

.PHONY: matita-light
matita-light: bin $(MATITA_THEORY_DKO) $(UNIVERSO_DKO)
	@$(UNIVERSO) -d s $(MODE) \
	-I $(MATITA_PATH)/theory \
	-I encodings \
	--to-elaboration  $(MATITA_PATH)/compatibility/in.dk \
	--of-universo     $(MATITA_PATH)/compatibility/out.dk \
	--to-theory       $(MATITA_PATH)/compatibility/theory.dk \
	--constraints    $(MATITA_PATH)/compatibility/constraints.dk \
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
