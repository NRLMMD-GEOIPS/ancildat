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

#!/bin/bash

if [[ -z "$GEOIPS_PACKAGES_DIR" ]]; then
    echo "Must define GEOIPS_PACKAGES_DIR environment variable prior to setting up geoips packages"
    exit 1
fi

if [[ -z "$GEOIPS_ANCILDAT" || -z "$GEOIPS_ANCILDAT_AUTOGEN" ]]; then
    echo "Must define GEOIPS_ANCILDAT and GEOIPS_ANCILDAT_AUTOGEN environment variable prior to building ancildat"
    echo "These values are hard coded into a fortran config.f90 file at build time"
    echo "The default locations would be $GEOIPS_OUTDIRS/ancildat and $GEOIPS_OUTDIRS/ancildat_autogen"
    echo "Alternative locations should be specified for shared ancillary data"
    exit 1
fi

if [[ "$1" == "install" ]]; then
    # For now, must pip install fortran_utils first, so it builds dependencies
    # where ancildat can find them. Otherwise pip install works to call make
    # for both.
    pip install -e $GEOIPS_PACKAGES_DIR/fortran_utils
    pip install -e $GEOIPS_PACKAGES_DIR/ancildat
else
    echo "UNRECOGNIZED COMMAND $1"
    exit 1
fi

