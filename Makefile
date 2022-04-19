# FLP 2022
# Variant : DKA-2-MKA
# Author  : Jan Krejci
# Login   : xkrejc70

SRC = src
TESTS = tests
INVALID = ${TESTS}/invalid
PRINT = ${TESTS}/print
PRINT_OUT = ${TESTS}/print_out
MINIMIZE = ${TESTS}/minimize
MINIMIZE_OUT = ${TESTS}/minimize_out
MAIN = ${SRC}/Main.hs
BIN = flp21-fun
OUTPUT = output
arg ?= q	# default diff argument

default: clean
	ghc -i${SRC} -Wall -o ${BIN} ${MAIN}

clean:
	rm -rf ${SRC}/*.o ${SRC}/*.hi ${BIN} $(OUTPUT)

# tests
tests: test-print test-minimize

# invalid input format
test-invalid: default
	@for test in $(shell ls ${INVALID}); do\
		echo ;\
		echo "==================" $${test} "==================";\
		./flp21-fun -i ${INVALID}/$${test};\
	done

# print input (-i)
test-print: default
# @echo "\033[0m================== running print tests ==================\033[0m"
	@echo "================== running print tests =================="
	@for test in $(shell ls ${PRINT}); do\
		./flp21-fun -i ${PRINT}/$${test} >$(OUTPUT);\
		if (diff -$(arg) ${PRINT_OUT}/$${test}.out $(OUTPUT));\
			# then echo "\033[92m$${test}\033[0m";\
			then echo $${test};\
			# else echo "\033[031m$${test} Failed!\033[031m";\
			else echo "* $${test} Failed!";\
		fi;\
	done

# minimize (-t)
test-minimize: default
# @echo "\033[0m================== running minimize tests ==================\033[0m"
	@echo "================== running minimize tests =================="
	@for test in $(shell ls ${MINIMIZE}); do\
		./flp21-fun -t ${MINIMIZE}/$${test} >$(OUTPUT);\
		if (diff -$(arg) ${MINIMIZE_OUT}/$${test}.out $(OUTPUT));\
			# then echo "\033[92m$${test}\033[0m";\
			then echo $${test};\
			# else echo "\033[031m$${test} Failed!\033[031m";\
			else echo "* $${test} Failed!";\
		fi;\
	done