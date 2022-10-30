# pentultimate_solver

## Contents

* [Introduction](#introduction)
* [Demo Video](#demo-video)
* [Terminology](#terminology)
  * [Organism](#organism)
  * [Species](#species)
  * [Half Address (Orientations)](#half-address-orientation)
  * [Origin Orientation](#origin-orientation)
  * [Genus](#genus)
  * [Full Address](#full-address)
  * [Family](#family)
* [Interface](#interface)
  * [Key Presses](#key-presses)
  * [Mouse](#mouse)
  * [Default Species](#default-species)
* [File Menu](#file-menu)
  * [Preferences](#preferences)
  * [Randomize](#randomize)
  * [Solve](#solve)
  * [Save Puzzle State](#save-puzzle-state)
  * [Load Puzzle State](#load-puzzle-state)
  * [Quit](#quit)
* [Puzzle Designs](#puzzle-designs)
  * [Original Super Dodecahedron](#original-super-dodecahedron)
  * [Custom Super Dodecahedron](#custom-super-dodecahedron)
  * [Super Icosahedron](#super-icosahedron)
  * [Rhombic Triacontahedron](#rhombic-triacontahedron)
  * [Rounded Rhombic Triacontahedron](#rounded-rhombic-triacontahedron)


## Introduction

Pentultimate Solver is a puzzle application to interact with a virtual [Pentultimate](https://www.twistypuzzles.com/cgi-bin/puzzle.cgi?pkey=1741), a twisty puzzle invented by Jason Smith.

This app is written in Rust, using the [`bevy`](https://docs.rs/bevy) game engine, with the [`egui`](https://docs.rs/egui) crate used for UI elements.

## Demo Video

There's a demo video covering most of the information in this readme on YouTube:

<div style="text-align:center"><a href="https://youtu.be/rNOOAjImquU">
  <img
    src="/assets/images/thumbnail.png"
    alt="The Pentultimate puzzle framed in the center, against a gray background"
    title="The Pentultimate Solver demo video on YouTube"
    width="300px"/>
</a></div>

## Terminology

Most of the data for the transformations is organized in a multidimensional array behind the scenes. To help keep track of what the various array indices affect, I adopted use of the biological taxonomy terms.

### Organism

An "organism index" is a discreet rotation of a piece (pentagonal or triangular) relative to the default rotation for its current position. A "transformation organism" is a single, specific transformation.

### Species

A "species index" is a discreet position of a piece (pentagonal or triangular) relative to the [origin](#origin-orientation). A "transformation species" is a group of five transformation organisms, grouped together based on shared relation with the pentagon specified by the corresponding species index.

### Half Address (Orientation)

Together, an organism index and a species index constitute a "half address", which can be used to specify the current position and rotation (collectively known as an "orientation") of any of the pieces of the puzzle (pentagonal or triangular). The 60 possible orientations for the pentagonal pieces can be thought of a discreet quaternion of sorts: they can either specify an orientation, or 3D rotation to apply to an orientation, transforming it into another orientation.

### Origin Orientation

The orientation specified by half address `(0, 0)` is known as the "origin" orientation. This is used to put the puzzle state into a "standardized form" (when the pentagon with index 0 is at the origin orientation). Without having a "standardized form" for a puzzle state, comparing puzzle states for equivalence would be extremely inefficient.

### Genus

![A vertical list of left-aligned terms, with gray text on a slightly darker gray background: "Reorientation", "Simple", "Swap2PentPairs", "CommutePentTrioTriangle", "CommutePentTrioLine", "Rotate2PentPairs", "RotatePentPair", "RotatePent", "CommuteTriTrioFar", "CommuteTriTrioNear", "CommuteTriTrioAcute", "RotateTriPairFar", "RotateTriPairNear". "Simple" is in white text.](/assets/images/genera.png)

A "genus" (plural "genera") is a broader category of transformation, comprising of twelve transformation species (one for each pentagonal species index). Listed above are the transformation genera. `Reorientation` moves the camera position to a specific orientation; `Simple` rotates a hemisphere of the puzzle either 0*τ*, 0.2*τ*, 0.4*τ*, 0.6*τ*, or 1*τ* radians (where *τ* radians is a full rotation, or 360 degrees); and all the other genera are known as "complex" genera, which comprise of a specific sequence of `Simple` transformations.

### Full Address

Together, a genus and a half address constitute a "full address", which specify a specific transformation that can be applied to the puzzle. Aside from any identity transformations (`(Reorientation, 0, 0)` and `(Simple, *, 0)` all don't affect the puzzle state nor the camera orientation), a full address uniquely identifies a transformation. There are some complex transformations that produce the same net effect change to the puzzle state, but through a different sequence of `Simple` transformations to get there.

### Family

It was a half lie when I said the list in the image is the transformation genera. Truly, it's the list of transformation families, or groupings of genera, where each family has a genus of the same name. The non-complex transformation families `Reorientation` and `Simple` both consist of a single genus.

A family consists of:
1. The genus of that name
2. The "mirror" of that genus
3. The "inverse" of that genus (as long as it doesn't overlap with either of the previous two genera)
4. The inverse's mirror (as long as it doesn't overlap with either of the previous three genera)

Here, the "mirror" is constructed by mirroring horizontally each comprising `Simple` transformation, and the "inverse" is constructed by performing the inverse of each comprising `Simple` transformations in the opposite order (the undo functionality just does the inverse of the performed transformation).

## Interface

### Key Presses

The following is the default keypress input mapping, which can be configured in the preferences.

| Action						| Mapping	| Description	|
| -----------------------------	| ---------	| -------------	|
| `Pentagon1`					| `Numpad0`	| Perform the transformation specified by the [toggles](#toggles) at the 1st of 6 pentagons (see [Default Species](#default-species) |
| `Pentagon2`					| `Numpad1`	| Perform the transformation specified by the [toggles](#toggles) at the 2nd of 6 pentagons (see [Default Species](#default-species) |
| `Pentagon3`					| `Numpad4`	| Perform the transformation specified by the [toggles](#toggles) at the 3rd of 6 pentagons (see [Default Species](#default-species) |
| `Pentagon4`					| `Numpad5`	| Perform the transformation specified by the [toggles](#toggles) at the 4th of 6 pentagons (see [Default Species](#default-species) |
| `Pentagon5`					| `Numpad6`	| Perform the transformation specified by the [toggles](#toggles) at the 5th of 6 pentagons (see [Default Species](#default-species) |
| `Pentagon6`					| `Numpad3`	| Perform the transformation specified by the [toggles](#toggles) at the 6th of 6 pentagons (see [Default Species](#default-species) |
| `RecenterCamera`				| `Space`	| Reorient the camera to be centered on the closest pentagon, with a side of this pentagon facing directly up |
| `Undo`						| `Ctrl+Z`	| Undo the most recent transformation |
| `Redo`						| `Ctrl+Y`	| Redo the most recent transformation |
| `CycleFamilyUp`				| `Up`		| Cycle the current transformation [family](#family) up once |
| `CycleFamilyDown`				| `Down`	| Cycle the current transformation [family](#family) down once |
| `CycleGenusUpWithinFamily`	| `Right`	| Cycle the current transformation genus within the current [family](#family) "up" once |
| `CycleGenusDownWithinFamily`	| `Left`	| Cycle the current transformation genus within the current [family](#family) "down" once |
| `EnableRecentering`			| `C`		| Enable camera recentering as part of transformations |
| `EnableModifiers`				| `F`		| Enable `RotateTwice` and `CounterClockwise` to affect the specified transformation |
| `RotateTwice`					| `D`		| On `Simple` transformations, rotate the pentagon twice; on other transformations, use organism index 2 without `CounterClockwise`/3 with `CounterClockwise` |
| `CounterClockwise`			| `S`		| On `Simple` transformations, rotate the pentagon counter clockwise; on other transformations, use organism index 4 without `RotateTwice`/3 with `RotateTwice` |
| `AlternateHemisphere`			| `A`		| Perform the transformation centered on the pentagon/species opposite the pentagon specified by keypress |

### Mouse

Hold down the scroll wheel to "pan" the puzzle (really this rotates about the axis perpendicular to the mouse travel direction).

Rolling the scroll wheel up rotates the puzzle clockwise (relative to the camera), and vice versa with rolling the scroll wheel down.

The mouse can also be used to interact with the file menu, the preferences, and any active debug tools.

### Default Species

![An orthographic projection of an icosidodecahedron, centered on one of the pentagonal faces, with one of its sides facing up. The six visible pentagons are filled with a translucent white, and numbered. The one in the center says "#0". Starting at the 12:00 position and progressing clockwise, the other pentagons read "#5", "#6", "#3", "#1", and "#4". The triangles are filled with a translucent grey.](/assets/images/numpad_preview.png)

For `Simple` transformations, each transformation affects a hemisphere of the puzzle, centered at one of the twelve pentagons. Six of these are directly affected by the `Pentagon*` keypress actions, and the other six are affected when `AlternateHemisphere` is enabled. Under the default input configuration and default species list for these six pentagons, the image above describes which numpad keys affect which pentagons.

## File Menu

The File menu is somewhat of a misnomer, since only three of the six items in it are related to a "file," but it gets the job done.

### Preferences

* `file_menu`: Preferences associated with the File menu
  * `randomization_params`: Preferences associated with the Randomize button
    * `random_transformation_genera`: Which transformation genera to be used in the shuffle; the default is `[ "Simple" ]`
    * `random_transformation_count`: How many transformations to apply in the shuffle; the default is `30`
    * `randomization_type`: Either `FromCurrent` (apply the transformations to the current puzzle state and transformation stack), `FromSolved` (apply the transformation to the solved puzzle state, building up a fresh transformation stack), or `FromSolvedNoStack` (same as `FromSolved`, but with no stack recorded for the applied transformations); the default is `FromSolved`
* `input`: Preferences associated with keyboard input
  * `default_species`: Which species indices are affected by the pentagon keys by default; default is `[0, 5, 10, 1, 8, 4]`
  * `key_presses`: See [Key Presses](#key-presses)
* `light_and_camera`: Preferences associated with the light and camera
  * `light_pos`: The position of the light within the scene; the default is `[ 0.0, 0.0, 10.0 ]`
  * `camera_pos`: The position of the camera within the scene; the default is `[ 0.0, 0.0, 10.0 ]`
  * `persp_proj`: A struct mirroring [`bevy::render::camera::PerspectiveProjection`](https://docs.rs/bevy/0.7.0/bevy/render/camera/struct.PerspectiveProjection.html)
    * `fov`: The field of view of the camera; the default is `0.5`
    * `aspect_ratio`: The aspect ratio of the camera; the default is `1.0`
    * `near`: The near clipping plane; the default is `1.0`
    * `far`: The far clipping plane; the default is `1000.0`
  * `light_illuminance`: The light's strength; the default is `4.0`
* `puzzle`: Preferences associated with the puzzle appearance
  * `color`: Preferences associated with the colors used
    * `colors_with_mat`: Preferences about colors with associated materials
    * `polyhedron_to_colors`: A map of polyhedron to colors for each face of that polyhedron
  * `design`: See [Puzzle Designs](#puzzle-designs); the default is `CustomSuperDodecahedron`
* `speed`: Preferences associated with speed
  * `camera`: Preferences associated with the camera speed
    * `pan_speed`: How fast the camera rotates with the scroll wheel held down (a number in the range of 1 and 100); the default is `50`
    * `roll_speed`: How fast the camera rotates when the scroll wheel rolls (a number in the range of 1 and 100); the default is `50`
  * `animation`: Preferences associated with animation speed
    * `rotation_millis`: How many milliseconds a fifth of a rotation takes; the default is `250`
    * `uniform_transformation_duration`: Whether or not all transformations take `rotation_millis` milliseconds vs `rotation_millis` for each fifth of a rotation; the default is `false`
    * `animate_undo_and_redo`: Whether or not the undo and redo operations are animated vs immediate; the default is `true`
  * `solver`: Preferences associated with the solver functionality
    * `cycle_duration_millis`: How many milliseconds are devoted to working on the solution per frame/update cycle; the default is `30.0`
* `tools`: Which tool debug modes are active; the default is `[]` (no modes)
* `ui`: Preferences associated with the visual application UI
  * `window_mode`: An enum mirroring [`bevy::window::WindowMode`](https://docs.rs/bevy/0.7.0/bevy/window/enum.WindowMode.html); the default is `Windowed`

### Randomize

This randomizes the puzzle. This is parametrized by `file_menu.randomization_params` in the preferences

### Solve

This starts the process of generating a sequence of transformations to apply to the puzzle to put it into a solved state. Before the solution is ready, further input is not accepted, aside from adjustment of the preferences. Once the solution is ready, the transformations are sequentially applied to the puzzle, appearing in the stack.

### Save Puzzle State

Using the [`serde`](https://docs.rs/serde) crate for serialization and the [`rfd`](https://docs.rs/rfd) crate for the file dialog, this saves the current puzzle state and transformation stack to a file in the `saves` directory. The following are the supported file types:

* `.bc`: Serialize using the [`bincode`](https://docs.rs/bincode) crate
* `.json`: Serialize using the [`serde_json`](https://docs.rs/serde_json) crate
* `.json5`: Serialize using the [`json5`](https://docs.rs/json5) crate
* `.ron`: Serialize using the [`ron`](https://docs.rs/ron) crate
* ~~`.toml`: Serialize using the [`toml`](https://docs.rs/toml) crate~~ The `toml` crate is too limited in what types it can serialize and deserialize, so it's not currently supported

### Load Puzzle State

This loads up a previous puzzle state and stack saved by the "Save Puzzle State" button.

### Quit

This quits the application, which can be useful if the application is currently in fullscreen.

## Puzzle Designs

All the puzzle meshes are generated at runtime, programmatically, not imported from 3D object files.

### Original Super Dodecahedron

This is the commercially available design from manufacturer Mf8, hence the "original" prefix. The term "super" indicates how the pentagonal pieces have extra detail on them such that there's only one correct orientation for that piece, as opposed to 5 (given the 5-fold rotational symmetry).

![A dodecahedron with a different color on each face. In the center of each face, there is a pentagonal hold punched out, revealing a hollow puzzle center. Along the exposed depth of the shell of these holes are five circles, colored based on the colors of the adjacent faces of the dodecahedron](/assets/images/puzzle_designs/original_super_dodecahedron.png)

### Custom Super Dodecahedron

This was the first design I programmed for the puzzle. It is a custom design that I came up with because I figured it'd be easier to program than the original design.

![A dodecahedron with a five-pointed star pattern on each face. Each face has a clear primary color, as well as 5 secondary colors for the 5 adjacent faces](/assets/images/puzzle_designs/custom_super_dodecahedron.png)

### Super Icosahedron

Instead of having flat faces for the pentagonal pieces flush with pyramidal caps on the triangular pieces, as is the case with the dodecahedron puzzle designs, we can instead base the geometry around the dodecahedron's [dual polyhedron](https://en.wikipedia.org/wiki/Dual_polyhedron), the icosahedron, yielding flat faces for the triangular pieces flush with pyramidal caps on the pentagonal pieces. To distinguish when the triangle are in the correct orientations, extra color was added to indicate the proper orientation.

![An icosahedron with each face tessellated into 4 smaller equilateral triangles. The central smaller equliateral triangle on each face is also subdivided into 4 smaller yet equilateral triangles. Each face has a clear primary color, as well as 3 secondary colors for the 3 adjacent faces](/assets/images/puzzle_designs/super_icosahedron.png)

### Rhombic Triacontahedron

By using the rhombic triacontahedron as the base polyhedron for the design, only one color is needed per face, which yields a rather clean look.

![A rhombic triacontahedron with each face having a unique, solid color, such that collectively the color wheel is present.](/assets/images/puzzle_designs/rhombic_triacontahedron.png)

### Rounded Rhombic Triacontahedron

By taking the previous design and "sanding down" any portion of the puzzle further out from the puzzle's center than where the edge between two rhombi meets one of the cuts through the puzzle's center, this design is created.

![A collection of colored discs on a sphere such that each one is "kissing" 4 neighbors. Each disc corresponds to a face on a rhombic triacontahedron](/assets/images/puzzle_designs/rounded_rhombic_triacontahedron.png)