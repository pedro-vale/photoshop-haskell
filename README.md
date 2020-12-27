PPM image format manipulation.

# Description

This project was done for a university assignment for `Princípios de Programação` course.

# Build project

```
$ stack build
```

# Run project

```
$ stack exec -- photoshop-haskell <input_file> <output_file> <flags>
```


| Arguments     | Description                                           |
|:-------------:|:-----------------------------------------------------:|
| `input_file`  | Input file name.                                      |
| `output_file` | Output file name.                                     |
| `flags`       | Flags to select modifications to be applied on image. |


| Flag name | Description      |
|:---------:|:----------------:|
| `-fh`     | Horizontal flip  |
| `-fv`     | Vertical flip    |
| `-gs`     | Gray scale       |
| `-rc`     | Red scale        |
| `-bc`     | Blue scale       |
| `-gc`     | Green scale      |
| `-hw`     | Crop half width  |
| `-hh`     | Crop half height |

# Run example
```
$ stack exec -- photoshop-haskell ./assets/input.ppm output.ppm -fh -gs -hw
```
