Cellgen: OpenMP-like support for the Cell processor
Scott Schneider, http://www.cs.vt.edu/~scschnei
See the project page, http://www.cs.vt.edu/~scschnei/cellgen, for publications.

Compiling
---------

Cellgen uses Boost.Spirit for parsing, which uses extensively nested templates. When debugging
information is turned on (-g), each level of nesting is not compiled away, and remains in the
executable. Consequently, including debugging information makes an order of magnitude difference
in executable size.

Cellgen also relies on other Boost libraries, but they should be installed on most Linux systems.

Other than that, a simple "make" should do.

Usage
-----

cellgen foo.cellgen [-n <# SPEs>] [-I <include file for PPE/SPE>]

Brief Programming Tutorital
---------------------------

Cellgen shares semantics with OpenMP, but legal OpenMP code is not legal Cellgen code, and
vice-versa. This section presents a brief tutorial of Cellgen, which serves to both provide
the reader with an intuitive feel for the programming model, and to highlight supported features.

Regarding mechanics, Cellgen is a source-to-source compiler: it accepts C code and emits
C code. The current workflow requires a programmer to call "cellgen" on a "*.cellgen" file,
which will produce code for both the PPE and SPE. Currently, we rely on the sophisticated Make
files provided by the IBM SDK to produce executable code.

In all of these code examples, we assume the Cellgen blocks of code reside in a legal C program.

The Basics
----------

All Cellgen code is preceded by a "#pragma cell" directive.  Cellgen ignores all other lines of
code until it reaches that pragma.  The Cellgen code is also enclosed in braces. The simplest
Cellgen code transfers no data in or out of the SPE:

#pragma cell
{
  printf("Hello world");
}

This code will print the string "Hello world" from each SPE. All code within a Cellgen region
will be executed on the SPE, and all code outside will be executed on the PPE. In code terms:

printf("I will always execute on the PPE.");

#pragma cell
{
  printf("I will always execute on each SPE.");
}

In the previous two examples, the SPEs all behaved the same. While the Cellgen model is to
distribute the same code to each SPE, the power comes from giving each SPE different data. In
the following example, each SPE executes different parts of the iteration space for a loop.

#pragma cell SPE_start(0) SPE_stop(10)
{
  int i;
  for (i = SPE_start; i < SPE_stop; ++i) {
    printf("iteration %d\n, i);
  }
}

We've introduced two new directives: "SPE_start" and "SPE_stop".  Conceptually, "SPE_start"
and "SPE_stop" are the starting and stopping conditions for the entire iteration space of the
loop. Each SPE executes a subset of the iteration space [0--10).

Computations with Flat Arrays
-----------------------------

None of the prior examples performed any interestion computations or even transferred any data
beyond loop parameters. The following example multiplies each element of a single-dimensional
array by a constant.

int vector[N];
int factor; // presumabley set elsewhere

#pragma cell SPE_start(0) SPE_stop(N) shared(int* v = vector) private(int f = factor)
{
  int i;
  for (i = SPE_start; i < SPE_stop; ++i) {
    v[i] = v[i] * f;
  }
}

This code sample introduces several new concepts. First, in order to pass data into a Cell
region, we must specify if it is "shared" or "private". Variables declared "shared" will have
their data distributed among all SPEs, streamed in or out as needed. Cellgen performs reference
analysis to determine how to stream the variables. In this example, the data for "vector"
will be both streamed in and out of the SPEs; its result will be visible to code beyond the
Cell region. Variables declared "private" will be transferred to each SPE once, and all SPEs
will have their own local copy.

Each SPE will carry out its computation in parallel, and there is an implicit barrier at the end
of the Cell region. Note that all of the iterations of the loop are *independent*. Currently,
Cellgen can only handle independent loops.

Reductions
----------

The result from the previous example was an entire array. Cellgen can also handle reductions,
where the computation relies on a large dataset, but the result is reduced to a single value.

int vector[N];
int sum = 0;

#pragma cell SPE_start(0) SPE_stop(N) shared(int* v = vector) reduction(+: int s = sum)
{
  int i;
  for (i = SPE_start; i < SPE_stop; ++i) {
    s += v[i];
  }
}

After all SPEs have finished, "sum" contains the summation of all elements of "vector". Cellgen
supports reductions for addition ("+") and multiplication ("*").

Multidimensional Arrays
-----------------------

Dense matrices are usually implemented with multidimensional arrays in C. Cellgen can handle
multidimensional arrays, but it requires more information than with flat arrays, and some
programmer assistance is required with column accesses.

To start with, we shall consider row accesses. The following code multiplies each element of
a 3-dimensional array by a constant factor:

int matrix[N1][N2][N3];
int factor; 

#pragma cell SPE_start(0) SPE_stop(N1) shared(int* m = matrix[N1][N2][N3]) \
                                        private(int f = factor, \
                                               int N1 = N1, \
                                               int N2 = N2, \
                                               int N3 = N3)
{
  int i, j, k;
  for (i = SPE_start; i < SPE_stop; ++i) {
    for (j = 0; j < N2; ++j) {
      for (k = 0; k < N3; ++k) {
        m[i][j][k] = m[i][j][k] * f;
      }
    }
  }
}

Cellgen needs to know the dimensions of the matrix, which are provided in the "shared"
directive. The dimensions can be either constants or variables only known at runtime. Cellgen
requires the matrix dimensions so that it can compute addresses for the DMAs which will get
and put values in main memory. Currently, if the dimensions are variables, the programmer must
explicitly pass them as "private" variables.  This peculiarity will be fixed in a later version.

Column accesses currently require more work from the programmer.  Because DMA lists work best
with addresses that are 16-byte aligned, Cellgen expects the programmer to pad their data. The
same computation, but accessing columns:

typedef struct int16b_t {
  int num;
  char pad[12];
};

int16b_t matrix[N1][N2][N3];
int factor; 

#pragma cell SPE_start(0) SPE_stop(N1) shared(int16b_t* m = matrix[N1][N2][N3]) \
                                        private(int f = factor, \
                                               int N1 = N1, \
                                               int N2 = N2, \
                                               int N3 = N3)
{
  int i, j, k;
  for (i = SPE_start; i < SPE_stop; ++i) {
    for (j = 0; j < N3; ++j) {
      for (k = 0; k < N1; ++k) {
        m[k][i][j].num = m[k][i][j].num * f;
      }
    }
  }
}

In the future, Cellgen will handle data padding so that row and column accesses appear the same
to programmers.

Further examples are available in the "unit_tests" directory.
