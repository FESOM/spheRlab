# spheRlab README file

spheRlab is an R package with numerous functions related to spherical geometry. The package is intended in particular for analysis and plotting of geophysical unstructured-grid data, for example data produced by the Finite Element (or volumE) Sea-ice Ocean Model (FESOM).

The original development of spheRlab has been driven by personal technical needs to be addressed while working with unstructured-grid data in the context of climate model development and research. The package content is in particular shaped by data produced by the Finite Element (or volumE) Sea-ice Ocean Model (FESOM) of the Alfred Wegener Institute (AWI), and a combined usage with the Climate Data Operators (CDO).

The further development of spheRlab is hoped to get some boost by the move of the project to GitHub for public collaborative code sharing and development.

Starting with version 1.0.0 on 8 December 2016, the code is known to be rich in bugs and needs for improvements. Any help and other feedback is greatly appreciated, either directly on GitHub or via email to <helge.goessling@awi.de>!

What follows is a list of what needs and/or could be done to improve spheRlab:

## URGENT

* Add simple grid (so-called pi-grid) and a corresponding field for examples.

* Add examples - for (almost) every function.

* Implement automatic lon-lat labelling into sl.plot.lonlatgrid.

## NEXT

* Finish sl.polygon.polygon.intersect and sl.polygon.rotdir (or omit?).

* Update sl.plot.polygon.qad to handle polygons truncated into multiple visible parts also in polar and regpoly projections.

* Add function that writes filter information into a CDO-readable file (see corresponding script).

* Add decent coastline capabilities.

* Add more predefined colourbars, e.g. the one mentioned by Nicolay, and write a documentation for the pre-defined colourbars.

* Add other projections (robin, mercator, other?).

* Add Modified/Partial Hausdorff Distance.

* Group function index: plotting; spherical geometry; mostly internal functions; …

* Add fesom2cdo functionality: convert 3D field, stored in a single vector as done in FESOMv1, to a matrix where the second dimension is the depth index. Dummy nodes with NAs are added where the local ocean depth is exceeded.

* Write a decent vignette.

## WISHLIST

* Split sl.read.FESOM into a more basic read function and multiple grid analysis functions.

* Add grid generation capabilities.

* Maybe introduce objects (S3 or S4?)? -> grid; colourbar (with breaks?); plot specifics list; more?

* More argument testing at beginning of functions.

* Speed up things where possible.

* Move into 3D-field plotting and analysis; includes handling sections.
