# Introduction

A re-implementation in scala of the svLis set-theoretic solid modeller created by Dr Adrian Bowyer at the University of Bath.
Full details about svLis can be found at ***URL*** and the source code is available at http://github.com/***URL***.

# Roadmap

My intent is to build up the software in a way that at each stage the modeller is fully usable,
however the functionality will be limited at each stage.
I anticipate that the stages will be:
1. Single plane support with model division (currently in-progress)
1. Export model voxels in format suitable for 3D viewer
1. Add set operations to support multiple primitives in a set
1. Add additional primitives
1. Add primitive operations
1. Built-in 3D display
1. Model faceter to create polygonal models for display
1. Optimise model division to use sub-models returned by decision function

# svLis compatibility

The classes, class names, method, field and variable names have been chosen to match svLis,
however the sv_ prefix has been dropped and I have followed Scala naming conventions (e.g. Capitalised Classes).

Compatibility has been retained as closely as possible,
so that e.g. any functionality from svLis model.h and model.cxx is found in Model.scala.

Models contain sets rather than set lists, and set attributes have not been implemented.

Other differences to svLis:
- The sv_real class has been dropped and Double is used instead.
- geometry.h/geometry.cxx has been split into Point.scala and Plane.scala.
- prim.h/prim.cxx becomes Primitive.scala.

### Enums

In svLis, enums are all in enum_def.h, but in svscala,
common Enums are found in Enums.scala and enums that are class specific are in the files of the class.
Enum names use those from svLis reformatted for Scala naming conventions, except:
- mod_kind becomes ModelKind
- SV_EVERYTHING and SV_NOTHING are contained in Contents

### Model division

Parts of Model.redivide_r have been functionally decomposed into createDividedBoxes.

TODO divide will probably no longer be an instance method and divide/redivide logic may be separated
