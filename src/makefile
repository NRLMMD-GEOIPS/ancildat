# # # This source code is protected under the license referenced at
# # # https://github.com/NRLMMD-GEOIPS.

REPO_DIR:=$(shell dirname $(realpath $(firstword $(MAKEFILE_LIST))))
REPO_DIR:=$(shell dirname $(realpath $(REPO_DIR)))

LIB=$(REPO_DIR)/ancildat/lib
INC=$(REPO_DIR)/ancildat/inc

FORTRAN_UTILS_LIB=${GEOIPS_DEPENDENCIES_DIR}/share/lib/fortran_utils
FORTRAN_UTILS_INC=${GEOIPS_DEPENDENCIES_DIR}/share/inc/fortran_utils

FINCS = -I. -I$(INC) -I${FORTRAN_UTILS_INC}
FLIBS = -L. -L$(LIB) -L${FORTRAN_UTILS_LIB}

ifeq ('$(GEOIPS_FORTRAN_COMPILER)','ifort')
FC = ifort
F2P_COMPILER_FLAGS = --fcompiler=intelem --compiler=intelem
FFLAGS = -warn all -fPIC -O3 -module $(INC) $(FLIBS) $(FINCS)
else
FC = gfortran
F2P_COMPILER_FLAGS = --fcompiler=gnu95
FFLAGS = -Wall -fPIC -O3 -J$(INC) $(FLIBS) $(FINCS)
endif

# F2PCFLAGS = --fcompiler=$(FCNAME) --f90flags='-fopenmp' -lgomp --quiet --opt="-O3" $(FLIBS) $(FINCS)
# F2PCFLAGS = --fcompiler=$(FCNAME) --quiet --opt="-O3" $(FLIBS) $(FINCS)
# F2PCFLAGS = --fcompiler=$(FCNAME) --compiler=$(CCNAME) --opt="-O3" $(FLIBS) $(FINCS)
F2PCFLAGS = $(F2P_COMPILER_FLAGS) --opt="-O3" $(FLIBS) $(FINCS)

#Libraries with Python bindings
$(LIB)/libancildat.so: ancildat.f90 $(FORTRAN_UTILS_LIB)/error_codes.o $(FORTRAN_UTILS_LIB)/errors.o $(FORTRAN_UTILS_LIB)/io_messages.o $(FORTRAN_UTILS_LIB)/datetime_utils.o $(FORTRAN_UTILS_LIB)/file_io_utils.o $(FORTRAN_UTILS_LIB)/prettyprint.o $(FORTRAN_UTILS_LIB)/string_operations.o $(FORTRAN_UTILS_LIB)/config.o

$(LIB)/ancildat.o: ancildat.f90 $(FORTRAN_UTILS_LIB)/error_codes.o $(FORTRAN_UTILS_LIB)/errors.o $(FORTRAN_UTILS_LIB)/io_messages.o $(FORTRAN_UTILS_LIB)/datetime_utils.o $(FORTRAN_UTILS_LIB)/file_io_utils.o $(FORTRAN_UTILS_LIB)/prettyprint.o $(FORTRAN_UTILS_LIB)/string_operations.o $(FORTRAN_UTILS_LIB)/config.o

#####################################################
### Required builds from other fortran packages
$(FORTRAN_UTILS_LIB)/config.o:
	echo "MUST build fortran_utils first"
	exit 1

$(FORTRAN_UTILS_LIB)/error_codes.o:
	echo "MUST build fortran_utils first"
	exit 1

$(FORTRAN_UTILS_LIB)/errors.o:
	echo "MUST build fortran_utils first"
	exit 1

$(FORTRAN_UTILS_LIB)/io_messages.o:
	echo "MUST build fortran_utils first"
	exit 1

$(FORTRAN_UTILS_LIB)/datetime_utils.o:
	echo "MUST build fortran_utils first"
	exit 1

$(FORTRAN_UTILS_LIB)/file_io_utils.o:
	echo "MUST build fortran_utils first"
	exit 1

$(FORTRAN_UTILS_LIB)/prettyprint.o:
	echo "MUST build fortran_utils first"
	exit 1

$(FORTRAN_UTILS_LIB)/string_operations.o:
	echo "MUST build fortran_utils first"
	exit 1

#####################################################
### Actual build commands for *.so (f2py) and *.o (gfortran) files

$(LIB)/lib%.so: %.f90
	mkdir -p $(INC) $(LIB)
	f2py $(F2PCFLAGS) -m lib$* -c $?
	touch $(LIB)/__init__.py
	mv lib$**.so $(LIB)/lib$*.so

$(LIB)/%.o: %.f90
	mkdir -p $(INC) $(LIB)
	touch $(LIB)/__init__.py
	# This also creates $(INC)/$*.mod
	$(FC) -c $(FFLAGS) $< -o $@

clean:
	rm -rfv $(REPO_DIR)/ancildat/lib/__pycache__/
	rm -fv $(REPO_DIR)/ancildat/lib/*
	rm -fv $(REPO_DIR)/ancildat/inc/*
