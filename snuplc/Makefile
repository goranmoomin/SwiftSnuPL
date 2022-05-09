#---------------------------------------------------------------------------------------------------
# 4190.409 Compilers                     SnuPL/2 Term Project                           Spring 2022
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
BACKEND=backend.cpp
BASE=environment.cpp \
		 target.cpp \
		 $(BACKEND)
SCANNER=scanner.cpp
PARSER=parser.cpp \
			 type.cpp \
			 symtab.cpp \
			 data.cpp \
			 ast.cpp \
			 ir.cpp
SOURCES=$(BASE) $(SCANNER) $(PARSER)

# object files of various targets
DEPS=$(patsubst %.cpp,$(DEP_DIR)/%.d,$(SOURCES))
OBJ_SCANNER=$(patsubst %.cpp,$(OBJ_DIR)/%.o,$(BASE) $(SCANNER))
OBJ_PARSER=$(patsubst %.cpp,$(OBJ_DIR)/%.o,$(BASE) $(SCANNER) $(PARSER))

# Doxygen configuration file
DOXYFILE=doc/Doxyfile

#
# compilations rules
#
.PHONY: doc clean mrproper

all: test_ir

$(OBJ_DIR)/%.o: $(SRC_DIR)/%.cpp | $(DEP_DIR) $(OBJ_DIR)
	$(CC) $(CCFLAGS) $(DEPFLAGS) -c -o $@ $<

$(DEP_DIR):
	@mkdir -p $(DEP_DIR)

$(OBJ_DIR):
	@mkdir -p $(OBJ_DIR)

-include $(DEPS)

test_scanner: $(OBJ_DIR)/test_scanner.o $(OBJ_SCANNER)
	$(CC) $(CCFLAGS) -o $@ $(OBJ_DIR)/test_scanner.o $(OBJ_SCANNER)

test_parser: $(OBJ_DIR)/test_parser.o $(OBJ_PARSER)
	$(CC) $(CCFLAGS) -o $@ $(OBJ_DIR)/test_parser.o $(OBJ_PARSER)

test_semanal: $(OBJ_DIR)/test_semanal.o $(OBJ_PARSER)
	$(CC) $(CCFLAGS) -o $@ $(OBJ_DIR)/test_semanal.o $(OBJ_PARSER)

test_ir: $(OBJ_DIR)/test_ir.o $(OBJ_PARSER)
	$(CC) $(CCFLAGS) -o $@ $(OBJ_DIR)/test_ir.o $(OBJ_PARSER)

doc:
	doxygen $(DOXYFILE)

clean:
	rm -rf $(OBJ_DIR) $(DEP_DIR)

mrproper: clean
	rm -rf doc/html/* test_scanner test_parser test_semanal test_ir
