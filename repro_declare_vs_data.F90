module bench_mod
  use iso_fortran_env, only: int64, real64
  implicit none

  integer, parameter :: variant_declare = 1
  integer, parameter :: variant_data    = 2
  integer, parameter :: nvariants       = 2

  type :: bench_stats
    real(real64) :: entry_sum = 0.0_real64
    real(real64) :: exit_sum  = 0.0_real64
    real(real64) :: total_sum = 0.0_real64
    integer      :: calls     = 0
  end type bench_stats

  type(bench_stats), save :: stats(nvariants)
  real(real64), save      :: call_t0(nvariants) = 0.0_real64
  real(real64), save      :: exit_t0(nvariants) = 0.0_real64
  logical, save           :: timing_enabled     = .true.

contains

  real(real64) function wall_time()
    integer(int64) :: count
    integer(int64) :: rate

    call system_clock(count, rate)
    wall_time = real(count, real64) / real(rate, real64)
  end function wall_time

  subroutine set_timing(flag)
    logical, intent(in) :: flag

    timing_enabled = flag
  end subroutine set_timing

  subroutine reset_stats()
    stats(:)%entry_sum = 0.0_real64
    stats(:)%exit_sum  = 0.0_real64
    stats(:)%total_sum = 0.0_real64
    stats(:)%calls     = 0
    call_t0(:)         = 0.0_real64
    exit_t0(:)         = 0.0_real64
  end subroutine reset_stats

  subroutine note_call_start(variant)
    integer, intent(in) :: variant

    if (.not. timing_enabled) return
    call_t0(variant) = wall_time()
  end subroutine note_call_start

  subroutine note_entry(variant)
    integer, intent(in) :: variant

    if (.not. timing_enabled) return
    stats(variant)%entry_sum = stats(variant)%entry_sum + (wall_time() - call_t0(variant))
  end subroutine note_entry

  subroutine note_exit_start(variant)
    integer, intent(in) :: variant

    if (.not. timing_enabled) return
    exit_t0(variant) = wall_time()
  end subroutine note_exit_start

  subroutine note_call_end(variant)
    integer, intent(in) :: variant
    real(real64)        :: t1

    if (.not. timing_enabled) return

    t1 = wall_time()
    stats(variant)%exit_sum  = stats(variant)%exit_sum + (t1 - exit_t0(variant))
    stats(variant)%total_sum = stats(variant)%total_sum + (t1 - call_t0(variant))
    stats(variant)%calls     = stats(variant)%calls + 1
  end subroutine note_call_end

  subroutine print_stats(label, variant)
    character(len=*), intent(in) :: label
    integer, intent(in)          :: variant
    real(real64)                 :: avg_entry
    real(real64)                 :: avg_exit
    real(real64)                 :: avg_total

    if (stats(variant)%calls == 0) then
      write (*,'(a)') trim(label)//': no timed calls'
      return
    end if

    avg_entry = stats(variant)%entry_sum / real(stats(variant)%calls, real64)
    avg_exit  = stats(variant)%exit_sum  / real(stats(variant)%calls, real64)
    avg_total = stats(variant)%total_sum / real(stats(variant)%calls, real64)

    write (*,'(a)') trim(label)
    write (*,'(a,f12.6,a)') '  avg entry = ', avg_entry, ' s'
    write (*,'(a,f12.6,a)') '  avg exit  = ', avg_exit,  ' s'
    write (*,'(a,f12.6,a)') '  avg total = ', avg_total, ' s'
    write (*,'(a,f12.6,a)') '  sum entry = ', stats(variant)%entry_sum, ' s'
    write (*,'(a,f12.6,a)') '  sum exit  = ', stats(variant)%exit_sum,  ' s'
    write (*,'(a,i0)')      '  calls     = ', stats(variant)%calls
  end subroutine print_stats

end module bench_mod

module repro_kernels
  use iso_fortran_env, only: real32
  use bench_mod,      only: note_entry, note_exit_start, variant_data, variant_declare
  implicit none

contains

  subroutine lsmruc_declare(a, b, c, d, n)
    implicit none

    integer, intent(in)        :: n
    real(real32), intent(in)   :: a(n), b(n)
    !$acc declare copyin(a,b)
    real(real32), intent(inout):: c(n)
    !$acc declare copy(c)
    real(real32), intent(out)  :: d(n)
    !$acc declare copy(d)
    real(real32)               :: tmp1(n), tmp2(n)
    !$acc declare create(tmp1,tmp2)
    integer                    :: i

    call note_entry(variant_declare)

    !$acc parallel loop present(a,b,c,tmp1,tmp2)
    do i = 1, n
      tmp1(i) = a(i) + 2.0_real32 * b(i)
      tmp2(i) = c(i) - 0.25_real32 * a(i)
    end do

    !$acc parallel loop present(c,d,tmp1,tmp2)
    do i = 1, n
      d(i) = tmp1(i) + tmp2(i)
      c(i) = 0.5_real32 * tmp1(i) - tmp2(i)
    end do

    call note_exit_start(variant_declare)
  end subroutine lsmruc_declare

  subroutine lsmruc_data(a, b, c, d, n)
    implicit none

    integer, intent(in)         :: n
    real(real32), intent(in)    :: a(n), b(n)
    real(real32), intent(inout) :: c(n)
    real(real32), intent(out)   :: d(n)
    real(real32)                :: tmp1(n), tmp2(n)
    integer                     :: i

    !$acc data copyin(a,b) copy(c,d) create(tmp1,tmp2)
    call note_entry(variant_data)

    !$acc parallel loop present(a,b,c,tmp1,tmp2)
    do i = 1, n
      tmp1(i) = a(i) + 2.0_real32 * b(i)
      tmp2(i) = c(i) - 0.25_real32 * a(i)
    end do

    !$acc parallel loop present(c,d,tmp1,tmp2)
    do i = 1, n
      d(i) = tmp1(i) + tmp2(i)
      c(i) = 0.5_real32 * tmp1(i) - tmp2(i)
    end do

    call note_exit_start(variant_data)
    !$acc end data
  end subroutine lsmruc_data

end module repro_kernels

program declare_vs_data_repro
  use iso_fortran_env, only: real32, real64
  use openacc
  use bench_mod
  use repro_kernels
  implicit none

  integer                 :: n
  integer                 :: repeats
  integer                 :: i
  real(real32), allocatable :: a(:), b(:), c_decl(:), d_decl(:), c_data(:), d_data(:)
  real(real64)            :: c_diff
  real(real64)            :: d_diff

  n       = 26214400
  repeats = 200
  call parse_args(n, repeats)

  allocate(a(n), b(n), c_decl(n), d_decl(n), c_data(n), d_data(n))

  call init_inputs(a, b, c_decl, c_data, d_decl, d_data, n)

  call acc_init(acc_device_nvidia)

  !$acc enter data copyin(a,b,c_decl,c_data) create(d_decl,d_data)

  call set_timing(.false.)
  call lsmruc_declare(a, b, c_decl, d_decl, n)
  call lsmruc_data(a, b, c_data, d_data, n)
  call set_timing(.true.)

  call init_inputs(a, b, c_decl, c_data, d_decl, d_data, n)
  !$acc update device(a,b,c_decl,c_data,d_decl,d_data)

  call reset_stats()

  do i = 1, repeats
    call note_call_start(variant_declare)
    call lsmruc_declare(a, b, c_decl, d_decl, n)
    call note_call_end(variant_declare)
  end do

  do i = 1, repeats
    call note_call_start(variant_data)
    call lsmruc_data(a, b, c_data, d_data, n)
    call note_call_end(variant_data)
  end do

  !$acc update self(c_decl,c_data,d_decl,d_data)
  !$acc exit data delete(a,b,c_decl,c_data,d_decl,d_data)

  c_diff = maxval(abs(real(c_decl, real64) - real(c_data, real64)))
  d_diff = maxval(abs(real(d_decl, real64) - real(d_data, real64)))

  write (*,'(a)') 'OpenACC declare-vs-data reproducer'
  write (*,'(a,i0)') '  n       = ', n
  write (*,'(a,i0)') '  repeats = ', repeats
  write (*,'(a)') '  host arrays are made resident before timed calls via enter data'
  write (*,'(a)') '  timed entry  = host call start -> first executable after mapping setup'
  write (*,'(a)') '  timed exit   = just before return/end data -> host call return'
  write (*,*)

  call print_stats('declare version', variant_declare)
  write (*,*)
  call print_stats('data version', variant_data)
  write (*,*)
  write (*,'(a,es12.4)') 'max abs(c_decl - c_data) = ', c_diff
  write (*,'(a,es12.4)') 'max abs(d_decl - d_data) = ', d_diff

  call acc_shutdown(acc_device_nvidia)

contains

  subroutine parse_args(n, repeats)
    integer, intent(inout) :: n
    integer, intent(inout) :: repeats
    character(len=64)      :: arg

    if (command_argument_count() >= 1) then
      call get_command_argument(1, arg)
      read (arg, *) n
    end if

    if (command_argument_count() >= 2) then
      call get_command_argument(2, arg)
      read (arg, *) repeats
    end if
  end subroutine parse_args

  subroutine init_inputs(a, b, c_decl, c_data, d_decl, d_data, n)
    integer, intent(in)          :: n
    real(real32), intent(out)    :: a(n), b(n), c_decl(n), c_data(n), d_decl(n), d_data(n)
    integer                      :: i

    do i = 1, n
      a(i)      = 0.001_real32 * real(i, real32)
      b(i)      = 1.0_real32 / real(i + 7, real32)
      c_decl(i) = 0.5_real32 + 0.0001_real32 * real(mod(i, 97), real32)
      c_data(i) = c_decl(i)
      d_decl(i) = 0.0_real32
      d_data(i) = 0.0_real32
    end do
  end subroutine init_inputs

end program declare_vs_data_repro
