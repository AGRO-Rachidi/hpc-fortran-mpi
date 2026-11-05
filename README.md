# Fortran + MPI

My exercises and lab work from the advanced programming / HPC course at ENSGMM (Benin), September–October 2026. Comments in the code are in French, the way I wrote them while learning.

The folders follow the order of the course:

- `01-fortran-basics`: allocation with error checks, small matrix operations, and inverting a matrix with LAPACK (`dgetrf` + `dgetri`).
- `02-mpi-point-to-point`: first MPI programs, ranks and sizes, and two ranks exchanging matrices with `MPI_Send` / `MPI_Recv`.
- `03-mpi-collectives`: the root scatters one matrix to each worker, the workers square it, and the root gathers the results.
- `04-master-worker`: the part I spent the most time on. The master gives the next task to whichever worker answers first (`MPI_ANY_SOURCE`, then `status%MPI_SOURCE`) and sends a stop tag when there is nothing left. `newton_dynamic.f90` uses this to solve 150 independent problems with Newton's method.
- `05-exercises`: answers to tutorial questions, for example why the output of several processes doesn't come out in rank order.

To build and run (needs gfortran, Open MPI or MPICH, and LAPACK for the inversion example):

```bash
mpif90 -O2 04-master-worker/newton_dynamic.f90 -o newton_dynamic
mpirun -np 4 ./newton_dynamic        # 1 master + 3 workers

gfortran 01-fortran-basics/matrix_inverse_lapack.f90 -llapack -o inverse
./inverse
```

These are course exercises, so I haven't measured any speed-up.
