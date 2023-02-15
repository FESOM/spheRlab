# spheRlab README file

spheRlab is an R package with numerous functions related to spherical geometry. The package is intended in particular for analysis and plotting of geophysical unstructured-grid data, for example data produced by the Finite Element (or volumE) Sea-ice Ocean Model (FESOM).

The original development of spheRlab has been driven by personal technical needs to be addressed while working with unstructured-grid data in the context of climate model development and research. The package content is in particular shaped by data produced by the Finite Element (or volumE) Sea-ice Ocean Model (FESOM) of the Alfred Wegener Institute (AWI), and a combined usage with the Climate Data Operators (CDO).

The further development of spheRlab is hoped to get some boost by the move of the project to GitHub for public collaborative code sharing and development.

Any help and other feedback is greatly appreciated, either directly on GitHub or via email to <helge.goessling@awi.de>!

## Usage in Docker

If you are experiencing difficulties with a local installation, you can also consider running spheRlab via a container.

For example, you could use `docker` like this:

```console
$ docker run -it --rm ghcr.io/fesom/spherlab:master R 
```

This launches you into an interactive R session with spheRlab already installed!

If you need access to specific data:

```console
$ docker run -it --rm -v "$(pwd):/app" ghcr.io/fesom/spherelab:master R
```

This would mount your current working directory into the container. If instead you are using `singularity`, you could use:

```console
$ singularity run --pwd /app -B $(pwd):/app -w docker://ghcr.io/fesom/spherelab:master R
```
