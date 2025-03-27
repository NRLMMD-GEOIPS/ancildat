# # # This source code is protected under the license referenced at
# # # https://github.com/NRLMMD-GEOIPS.

REPO_DIR:=$(shell dirname $(realpath $(firstword $(MAKEFILE_LIST))))

# Write to shared library location, where other packages can use these
# libraries during build.
SHARED_LIB=${GEOIPS_DEPENDENCIES_DIR}/share/lib/ancildat/
SHARED_INC=${GEOIPS_DEPENDENCIES_DIR}/share/inc/ancildat/

all:
	make -C src
	make -C src $(REPO_DIR)/ancildat/lib/ancildat.o
	# Copy to shared lib and inc for other packages build
	mkdir -p $(SHARED_LIB)
	mkdir -p $(SHARED_INC)
	cp $(REPO_DIR)/ancildat/lib/*.o $(SHARED_LIB)
	cp $(REPO_DIR)/ancildat/lib/*.so $(SHARED_LIB)
	cp $(REPO_DIR)/ancildat/inc/*.mod $(SHARED_INC)

clean:
	make -C src clean
	rm -fv ${GEOIPS_DEPENDENCIES_DIR}/share/lib/ancildat/*
	rm -fv ${GEOIPS_DEPENDENCIES_DIR}/share/inc/ancildat/*
