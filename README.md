# VDHL image generator

Generates a VDHL black and white matrix from an image.

The script generates a VDHL matrix of 0 (black) and 1 (white) from an image.

The image doesn't need to be black and white. If the image have colors or
grey shades, it will be converted to black and white (using color intensities)
before being converted to the VDHL matrix.

## Example

This image:

![Example image](image.png)

will result in this VDHL matrix:

```vhdl
(11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111), 
(11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111), 
(11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111), 
(11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111), 
(11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111), 
(11111000011111111111100001111111000000000000000001111110000111111111111111100001111111111111111111111000000001111111111111111), 
(11111000011111111111100001111111000000000000000001111110000111111111111111100001111111111111111111100000000000011111111111111), 
(11111000011111111111100001111111000000000000000001111110000111111111111111100001111111111111111110000000000000000111111111111), 
(11111000011111111111100001111111000011111111111111111110000111111111111111100001111111111111111100000011111100000011111111111), 
(11111000011111111111100001111111000011111111111111111110000111111111111111100001111111111111111100001111111111000011111111111), 
(11111000011111111111100001111111000011111111111111111110000111111111111111100001111111111111111000011111111111100001111111111), 
(11111000011111111111100001111111000011111111111111111110000111111111111111100001111111111111110000011111111111100000111111111), 
(11111000011111111111100001111111000011111111111111111110000111111111111111100001111111111111110000111111111111110000111111111), 
(11111000011111111111100001111111000011111111111111111110000111111111111111100001111111111111110000111111111111110000111111111), 
(11111000011111111111100001111111000011111111111111111110000111111111111111100001111111111111100001111111111111111000011111111), 
(11111000011111111111100001111111000011111111111111111110000111111111111111100001111111111111100001111111111111111000011111111), 
(11111000000000000000000001111111000000000000000011111110000111111111111111100001111111111111100001111111111111111000011111111), 
(11111000000000000000000001111111000000000000000011111110000111111111111111100001111111111111100001111111111111111000011111111), 
(11111000000000000000000001111111000000000000000011111110000111111111111111100001111111111111100001111111111111111000011111111), 
(11111000011111111111100001111111000011111111111111111110000111111111111111100001111111111111100001111111111111111000011111111), 
(11111000011111111111100001111111000011111111111111111110000111111111111111100001111111111111100001111111111111111000011111111), 
(11111000011111111111100001111111000011111111111111111110000111111111111111100001111111111111100001111111111111111000011111111), 
(11111000011111111111100001111111000011111111111111111110000111111111111111100001111111111111110000111111111111110000111111111), 
(11111000011111111111100001111111000011111111111111111110000111111111111111100001111111111111110000111111111111110000111111111), 
(11111000011111111111100001111111000011111111111111111110000111111111111111100001111111111111110000011111111111100000111111111), 
(11111000011111111111100001111111000011111111111111111110000111111111111111100001111111111111111000011111111111100001111111111), 
(11111000011111111111100001111111000011111111111111111110000111111111111111100001111111111111111100001111111111000011111111111), 
(11111000011111111111100001111111000011111111111111111110000111111111111111100001111111111111111100000011111100000011111111111), 
(11111000011111111111100001111111000000000000000001111110000000000000000111100000000000000001111110000000000000000111111111111), 
(11111000011111111111100001111111000000000000000001111110000000000000000111100000000000000001111111100000000000011111111111111), 
(11111000011111111111100001111111000000000000000001111110000000000000000111100000000000000001111111111000000001111111111111111), 
(11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111), 
(11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111), 
(11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111), 
(11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111);
```

## Download

A Linux x64 precompiled executable can be downloaded
[here](https://github.com/RaphaelJ/vdhl-image/releases/tag/v0.1.0.0).

You will need to have the DevIL image library installed on your system. The
library is present in most distribution package repositories.

## Usage

You need to execute the vdhl-image command with the image you want to convert
as first command argument:

```bash
./vdhl-image image.png
```

The VDHL matrix will be generated on the standard output.

## Compiling

To compile the library You need to have the GHC Haskell Compiler and the DeVIL
image library.

In the root directory of this repository, execute the following commands:

```bash
cabal sandbox init
cabal update
cabal install --only-dependencies
cabal build
```

The `vdhl-image` executable will be generated in the `dist/build/vdhl-image/`
directory.
