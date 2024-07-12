# # # Distribution Statement A. Approved for public release. Distribution unlimited.
# # #
# # # Author:
# # # Naval Research Laboratory, Marine Meteorology Division
# # #
# # # This program is free software: you can redistribute it and/or modify it under
# # # the terms of the NRLMMD License included with this program. This program is
# # # distributed WITHOUT ANY WARRANTY; without even the implied warranty of
# # # MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the included license
# # # for more details. If you did not receive the license, for more information see:
# # # https://github.com/U-S-NRL-Marine-Meteorology-Division/

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
