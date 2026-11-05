! Give the program its name. The name after END PROGRAM must match.
program mpi_matrice
    ! Import the standard Fortran name for the terminal output unit.
    use iso_fortran_env, only: output_unit
    ! Import the MPI routines and constants, including MPI_Init and MPI_COMM_WORLD.
    use mpi
    ! Require declarations for all variables; this catches many spelling mistakes.
    implicit none

    ! PARAMETER makes this a constant: this exercise requires exactly four ranks.
    integer, parameter :: required_processes = 4
    ! MPI message tag: a small integer used to identify this kind of message.
    integer, parameter :: message_tag = 100
    ! ierr receives an MPI routine's error code; rank is this process's number;
    ! process_count is the total number of MPI processes started.
    integer :: ierr, rank, process_count
    ! file_unit identifies the opened file; io_status reports file I/O success;
    ! local_error and global_error tell all ranks whether setup failed.
    integer :: file_unit, io_status, local_error, global_error
    ! Dimensions read from the file and the current 1-based row number.
    integer :: row_count, column_count, row_index
    ! The MPI rank that sends the current row and the rank that receives it.
    integer :: sender, receiver
    ! MPI_Recv writes details about the received message into this array.
    integer :: receive_status(MPI_STATUS_SIZE)
    ! A row has column_count integers; its size is known only after reading
    ! the file header, so allocate it later.
    integer, allocatable :: row(:)
    ! Track whether OPEN succeeded so we only CLOSE a file we actually opened.
    logical :: file_is_open

    ! Start the MPI environment. Every MPI program must call this first.
    call MPI_Init(ierr)
    ! Ask MPI for this process's rank, numbered 0 through process_count - 1.
    call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)
    ! Ask MPI how many processes belong to the global communicator.
    call MPI_Comm_size(MPI_COMM_WORLD, process_count, ierr)

    ! Stop early unless the program was started with exactly four processes.
    if (process_count /= required_processes) then
        ! Print the instruction once, rather than once per process.
        if (rank == 0) then
            ! A is a character string and I0 prints an integer without padding.
            write(*, '(A,I0,A)') 'Run this program with exactly ', &
                required_processes, ' processes.'
        end if
        ! Shut down MPI cleanly before leaving the program.
        call MPI_Finalize(ierr)
        ! STOP ends this process; every rank takes this branch.
        stop
    end if

    ! Initialize the file unit to a recognizable value before OPEN.
    file_unit = -1
    ! At this point no file has been opened.
    file_is_open = .false.
    ! Zero means this process has not detected a setup error.
    local_error = 0

    ! Each rank opens its own read-only connection to the shared input file.
    ! NEWUNIT chooses a safe unit number; OLD requires an existing file;
    ! IOSTAT returns an error code instead of stopping on an open error.
    open(newunit=file_unit, file='matrix.txt', status='old', &
         action='read', iostat=io_status)
    ! IOSTAT equal to zero means OPEN succeeded.
    if (io_status == 0) then
        ! Remember to close this file later.
        file_is_open = .true.
            ! Read N, the size of this square matrix (N rows and N columns).
            read(file_unit, *, iostat=io_status) row_count
            ! Nonzero IOSTAT means the header could not be read as an integer.
        if (io_status /= 0) then
            local_error = 1
            ! A matrix must have a positive size.
            else if (row_count <= 0) then
            local_error = 1
            else
                ! This input format describes a square matrix, so rows = columns.
                column_count = row_count
        end if
    else
        ! OPEN failed, for example because matrix.txt is missing.
        local_error = 1
    end if

    ! Combine all ranks' error flags using the maximum: if any rank has 1,
    ! then every rank receives global_error = 1. All ranks must call this.
    call MPI_Allreduce(local_error, global_error, 1, MPI_INTEGER, MPI_MAX, &
                       MPI_COMM_WORLD, ierr)

    ! If any process could not set up its input file, all processes stop.
    if (global_error /= 0) then
        ! Only rank 0 prints the shared error message.
        if (rank == 0) then
            write(*, '(A)') 'Could not open matrix.txt or read its dimensions.'
            write(*, '(A)') 'The first line must contain the positive matrix size N.'
        end if
        ! Close the file on this rank only if its OPEN succeeded.
        if (file_is_open) close(file_unit)
        ! Finish the MPI environment on every rank.
        call MPI_Finalize(ierr)
        ! Nonzero STOP status tells the operating system the run failed.
        stop 1
    end if

    ! Make room for one matrix row, with one integer per column.
    allocate(row(column_count))

    ! Fortran arrays start at index 1 by default, so rows are 1 to row_count.
    do row_index = 1, row_count
        ! Since every process opened the same file and follows this loop,
        ! each reads the same next row from its own file connection.
        read(file_unit, *, iostat=io_status) row
        ! A bad/missing row means the file does not match its dimensions.
        if (io_status /= 0) then
            ! Report which process and row failed.
            write(*, '(A,I0,A,I0)') 'Rank ', rank, &
                ' could not read matrix row ', row_index
            ! Terminate the whole MPI job; other ranks may be waiting for messages.
            call MPI_Abort(MPI_COMM_WORLD, 1, ierr)
        end if

        ! Assign rows in round-robin order. MOD returns a remainder:
        ! row 1 -> rank 0, row 2 -> rank 1, row 3 -> rank 2, row 4 -> rank 3,
        ! row 5 -> rank 0, and so on.
        sender = mod(row_index - 1, process_count)
        ! The next rank in the ring receives; MOD wraps rank 3 back to rank 0.
        receiver = mod(row_index, process_count)

        ! Only the designated sender transmits the row for this iteration.
        if (rank == sender) then
            ! MPI_Send(buffer, count, datatype, destination, tag, communicator, error)
            ! buffer=row, count=column_count integers, destination=receiver,
            ! tag=message_tag, communicator=MPI_COMM_WORLD, ierr=error code.
            call MPI_Send(row, column_count, MPI_INTEGER, receiver, message_tag, &
                          MPI_COMM_WORLD, ierr)
        ! Only the designated receiver accepts this row and displays it.
        else if (rank == receiver) then
            ! MPI_Recv(buffer, count, datatype, source, tag, communicator,
            !          status, error): expect column_count integers from sender.
            call MPI_Recv(row, column_count, MPI_INTEGER, sender, message_tag, &
                          MPI_COMM_WORLD, receive_status, ierr)
            ! List-directed output prints the receiving process and the row.
            write(*, *) 'Process ', rank, ' displays row ', row_index, ': ', row
            ! Ask Fortran to flush terminal output so the line appears promptly.
            flush(output_unit)
        end if
    end do

    ! Close this rank's file connection after all rows have been processed.
    close(file_unit)
    ! Shut down MPI after all communication is complete.
    call MPI_Finalize(ierr)
! Mark the end of the program.
end program mpi_matrice