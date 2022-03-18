#---------------------------------------------------------------------------------------------------
# 4190.409 Compilers                     SnuPL/2 Term Project                            Spring 2022
#
# Author: Bernhard Egger <bernhard@csap.snu.ac.kr>
#


#
# variables
#

# directories
SRC_DIR=src
OBJ_DIR=obj
DEP_DIR=.deps

# compilation w/ automatic dependency generation
CC=g++
CCFLAGS=-std=c++11 -Wall -g -O0
DEPFLAGS=-MMD -MP -MT $@ -MF $(DEP_DIR)/$*.d

# sources for various targets
SCANNER=scanner.cpp
SOURCES=$(SCANNER)

# object files of various targets
DEPS=$(patsubst %.cpp,$(DEP_DIR)/%.d,$(SOURCES))
OBJ_SCANNER=$(patsubst %.cpp,$(OBJ_DIR)/%.o,$(SCANNER))

# Doxygen configuration file
DOXYFILE=doc/Doxyfile

#
# compilations rules
#
.PHONY: doc clean mrproper

all: test_scanner

$(OBJ_DIR)/%.o: $(SRC_DIR)/%.cpp | $(DEP_DIR) $(OBJ_DIR)
	$(CC) $(CCFLAGS) $(DEPFLAGS) -c -o $@ $<

$(DEP_DIR):
	@mkdir -p $(DEP_DIR)

$(OBJ_DIR):
	@mkdir -p $(OBJ_DIR)

-include $(DEPS)

test_scanner: $(OBJ_DIR)/test_scanner.o $(OBJ_SCANNER)
	$(CC) $(CCFLAGS) -o $@ $(OBJ_DIR)/test_scanner.o $(OBJ_SCANNER)

doc:
	doxygen $(DOXYFILE)

clean:
	rm -rf $(OBJ_DIR)/*.o $(DEP_DIR)

mrproper: clean
	rm -rf doc/html/* test_scanner
