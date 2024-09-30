    # # # This source code is protected under the license referenced at
    # # # https://github.com/NRLMMD-GEOIPS.

ancildat GeoIPS Plugin
======================

The ancildat package is a GeoIPS-compatible plugin, intended to be used within the GeoIPS ecosystem.
Please see the
[GeoIPS Documentation](https://github.com/NRLMMD-GEOIPS/geoips#readme)
for more information on the GeoIPS plugin architecture and base infrastructure.

Package Overview
-----------------

The ancildat package includes fortran code with Python wrappers for reading
in and using various ancillary datasets within GeoIPS.  This includes
elevation, rayleigh scattering look up tables, climatology, etc.


System Requirements
---------------------

* geoips >= 1.12.1
* fortran_utils >= 1.12.1 (currently must pip install FIRST)
* Test data repos contained in $GEOIPS_TESTDATA_DIR for tests to pass.
* GEOIPS_ANCILDAT and GEOIPS_ANCILDAT_AUTOGEN

  * Must define GEOIPS_ANCILDAT and GEOIPS_ANCILDAT_AUTOGEN environment variables
    prior to building ancildat
  * These values are hard coded into a fortran config.f90 file at build time
  * The default locations would be:
  
    * $GEOIPS_OUTDIRS/ancildat
    * $GEOIPS_OUTDIRS/ancildat_autogen
  * Alternative locations should be specified if using shared ancillary data

IF REQUIRED: Install base geoips package
------------------------------------------------------------
SKIP IF YOU HAVE ALREADY INSTALLED BASE GEOIPS ENVIRONMENT

If GeoIPS Base is not yet installed, follow the
[installation instructions](https://github.com/NRLMMD-GEOIPS/geoips#installation)
within the geoips source repo documentation:

Install ancildat package
----------------------------
```bash
    # Assuming you followed the fully supported installation,
    # using $GEOIPS_PACKAGES_DIR and $GEOIPS_CONFIG_FILE:
    source $GEOIPS_CONFIG_FILE
    git clone https://github.com/NRLMMD-GEOIPS/fortran_utils $GEOIPS_PACKAGES_DIR/fortran_utils
    git clone https://github.com/NRLMMD-GEOIPS/ancildat $GEOIPS_PACKAGES_DIR/ancildat

    # NOTE: currently, fortran dependencies must be installed separately, initially
    # including in pyproject.toml resulted in incorrect installation paths.
    # More work required to get the pip dependencies working properly for fortran
    # installations via pyproject.toml with the poetry backend.
    pip install -e $GEOIPS_PACKAGES_DIR/fortran_utils
    pip install -e $GEOIPS_PACKAGES_DIR/ancildat

    # Set GEOIPS_ANCILDAT and GEOIPS_ANCILDAT_AUTOGEN
```

Test ancildat installation
-----------------------------
```bash

    # Ensure GeoIPS Python environment is enabled.

    # synth_green package is used for True Color and GeoColor
    # Test those products to test synth_green functionality
```
