Cellgen: OpenMP-like support for the Cell processor
Scott Schneider, scschnei@cs.vt.edu
http://www.cs.vt.edu/~scschnei/cellgen

Compiling
---------

Cellgen uses Boost.Spirit for parsing, which uses extensively nested
templates. When debugging information is turned on (-g), each level of
nesting is not compiled away, and remains in the executable.
Consequently, including debugging information makes an order of
magnitude difference in executable size.

Cellgen also relies on other Boost libraries, but they should be
installed on most Linux systems.

Other than that, a simple "make" should do.


Usage
-----

cellgen foo.cellgen [-n <# SPEs>] [-I <include file for PPE/SPE]

Accepts only a single input file at a time, and it must end with
.cellgen. For the file "foo.cellgen", the files "foo.c" and "spu/foo_spe.c"
are generated where "foo.c" runs on the PPE, and "spu/foo_spe.c" contains
all of the offloaded SPE code.

Currently, only for loops are supported. In order to offload a for loop,
use the following pragma:

	#pragma cell SPE_start(<start>) SPE_stop(<stop>)
	{
		int i;
		for (i = SPE_start; i < SPE_stop; i++) {
			// ...
		}
	}

SPE_start and SPE_stop are keywords, and must be used as the iteration
conditions in the for loop. Note that the braces around the code to be
offloaded are required. Private and shared data are indicated with the
following commands:

	#pragma cell ... private(<variables>) shared(<variables>)

Currently, the private and shared commands require the programmer to
give an alias to an existing value, including the type. For example:

	int* array;

	#pragma cell ... shared(int* a = array)

Each SPE thread gets its own copy of a private variable, which also
means no buffering is performed. Buffering is geneated for all shared
variables. Cellgen determines if a variable is in (data only needs to go
from main memory to the SPE), out (data only needs to go from the SPE to
main memory), or inout (data needs to go from both main memory to the
SPE and the SPE to main memory). In and out variables use double
buffering, while inout variables use triple buffering.

For examples, check the unit_test directory.
