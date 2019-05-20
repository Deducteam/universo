Command line options
====================

The available options for Universo can be printed with `--help`. Three options are mandatory:

- `-o <path>`: takes a directory. Files will be generated in this directory
- `--theory <file>`: theory file used to type check the terms. It should respect the constraints mentioned in TODO
- `--config <file>`: a configuration file for Universo. Some examples can be found in the `example` directory

`universo` takes several files as argument and output generated files in the `-o` directory. Because some steps described in [What is Universo](about.md) are modular, Universo offers a way to select the step to process.

#### Mode selection

By default, Universo will process for each files steps 1 and 2 (order given on the command line). Then it will process steps 3 and finally it will process step 4.

- `--elab-only`: only step 1 is performed
- `--check-only` (requires step 1): only step 2 is performed
- `--solve-only` (requires step 2): only step 3 is performed
- `--simplify <path>` (requires step 3): only step 4 is performed: For each module `A.dk` given on the command line, generates a new `A.dk` file where universes are replaced by the solution given at step 3.

Each time, it is assumed that the files given are the original ones.

**Default mode**: Only steps 1, 2 and 3 are done.

#### Debugging

With an erroneous configuration file, strange errors might appear quickly. In that case two options are given:

- `-l`: Print logging informations for Universo only
- `-d <flags>`: Print debugging informations for Universo and Dedukti. See [Dedukti](https://github.com/Deducteam/Dedukti/blob/master/README.md)

### External dependencies

- `-I <path>`: add directories where external dependencies can be found. These files won't be processed by Universo.
