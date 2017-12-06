# Introduction

A re-implementation in scala of the svLis set-theoretic solid modeller created by Dr Adrian Bowyer at the University of Bath.
Full details about svLis can be found at http://adrianbowyer.com/inge/svlis and the source code is available at https://github.com/AdrianRepRap/SvLis.

# How to use svLis

A simple program to construct an infinitely long cylinder.

```
import svlis.lib._

object Main extends App {
    // The axis is vertical in Z, going through point (0.2, 0.2, 0)
    val axis = new Line(Point.Z, new Point(0.2, 0.2, 0))
    val radius = 1.0
    // Create a svlis set
    val cyl = Solid.cylinder(axis, radius)
    // A bounding box is required so that svLis know which part of space we are interested in
    val box = new Box(new Point(-1, -1, -1), new Point(1, 1, 1))
    // A model is a set withing a bounding box
    val model = Model(cyl, box)
    // The model could now be displayed, exported to VRML - see examples directory
}

```

See the scala/examples directory for how these models can be displayed and longer examples.

# Project roadmap

The intent is to build up the software in a way that at each stage the modeller is fully usable,
however the functionality will be limited at each stage.
The anticipated work stages are:
1. Single plane support with model division
1. Export model voxels in format suitable for 3D viewer
1. Add set operations to support multiple primitives in a set
1. Add additional primitives (currently in-progress)
1. Add primitive transformations
1. Built-in 3D display
1. Model faceter to create polygonal models for display
1. Support set colouring
1. Efficiently redivide an already divided model
1. Optimise model division to use sub-models returned by decision function

# svLis compatibility

The classes, class names, method, field and variable names have been chosen to match svLis,
however the sv_ prefix has been dropped and Scala naming conventions are followed (e.g. Capitalised Classes).

Compatibility has been retained as closely as possible,
so that e.g. any functionality from svLis model.h and model.cxx is found in Model.scala.

The methods in scala classes are ordered to match those from the C++ files to make it easier to compare the two codebases.

Differences to svLis:
- The inner data structs for primitives, sets and models have not been used, the classes have the data as direct immutable members instead.
- Models contain sets rather than set lists.
- Set attributes have not been implemented.
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
