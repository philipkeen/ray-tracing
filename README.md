A simple Scala project to produce ray-traced images.

_To Run_

The project reads in scene information from a file (unfortunately there is no GUI).
This can be written as a simple text fle.  It must begin with the following information:
```
lightSource
  location: // (x, y, z) coordinates of the light source
  radius: // decimal number
  ambience: // decimal in range [0, 1] the closer to 0, the darker the shadows
renderSettings
  maxReflections: // non-negative integer
  maxShadowRaysPerPoint: // positive integer
  antiAliasing: // true/false
  defaultColour: // colour in hexadecimal format - eg. ffffff
```
For example:
```$xslt
lightSource
  location: (-200, -200, 200)
  radius: 10.25
  ambience: 0.2
renderSettings
  maxReflections: 2
  maxShadowRaysPerPoint: 2
  antiAliasing: true
  defaultColour: ffffff
```
Note the fields for `lightSource` and `renderSettings` must be indented.

The objects in the scene can follow the `lightSource` and `renderSettings`

In the base directory of the project, start sbt
```$xslt
>sbt
```
