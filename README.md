3D Marker Generator
=========

This code generates a board of fiducial markers.
The board is placed in a 3D environment at the origin of the coordinate system, and a camera takes pictures of the board from different positions and angles.
The fiducial markers are generated as in the [ArUco](http://www.uco.es/investiga/grupos/ava/node/26 "ArUco library") library.

![Example 1](https://raw.github.com/oetr/3D-Marker-Generator/master/images/Examples.png)

# Requirements
You need to have a Mac in order to be able to run this code without modification.
In this version, Racket calls difectly to MacOS-specific GUI framework by using libffi.
However, the markers are drawn by using OpenGL, which is platform-independent.

# Installation
Get the latest version of Racket from http://racket-lang.org/.
You are good to go!

# License
Copyright (c) 2012 Peter Samarin

This code is distributed under the GNU Lesser General Public License (LGPL).
This means that you can link the code into proprietary applications, provided you follow the rules stated in the LGPL.
You can also modify this code; if you distribute a modified version, you must distribute it under the terms of the LGPL, which in particular means that you must release the source code for the modified software.