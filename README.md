# Parallel programming with Fortran and MPI

*Programmation parallèle en Fortran avec MPI : exercices et travaux pratiques du cours de programmation avancée.*

Coursework from the **Advanced Programming / HPC** course, engineering programme at ENSGMM (UNSTIM, Benin), September – October 2026.
These are learning exercises, ordered from first steps to dynamic load balancing.

## Contents

| Folder | What it shows |
|---|---|
| `01-fortran-basics/` | Modern Fortran: dynamic allocation with error checks, matrix operations, matrix inversion with **LAPACK** (`dgetrf`/`dgetri`) |
| `02-mpi-point-to-point/` | `MPI_Init`, ranks and size, blocking `MPI_Send`/`MPI_Recv`, matrix exchange between two ranks, distributing the rows of a matrix read from a file across 4 ranks, with a global error check |
| `03-mpi-collectives/` | `MPI_Scatter` / `MPI_Gather`: the root sends one matrix to each worker, the workers square it, and the root collects the results |
| `04-master-worker/` | **Dynamic load balancing.** The master hands the next work item to whichever worker finishes first (`MPI_ANY_SOURCE` + `status%MPI_SOURCE`) and stops the workers with a dedicated tag. Applied to 150 independent systems solved with Newton's method (`mpi_f08`) |
| `05-exercises/` | Tutorial answers, for example why output from P processes does not follow rank order, and how to force it |

## Build and run

Requires gfortran and an MPI implementation (Open MPI or MPICH), plus LAPACK for the inversion example.

```bash
mpif90 -O2 04-master-worker/newton_dynamic.f90 -o newton_dynamic
mpirun -np 4 ./newton_dynamic              # 1 master + 3 workers

gfortran 01-fortran-basics/matrix_inverse_lapack.f90 -llapack -o inverse && ./inverse
```

## Notes

- These programs are exercises: they favour clarity (heavily commented code) over performance.
- No performance measurements are claimed here.
