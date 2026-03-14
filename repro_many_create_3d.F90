module bench_many_mod
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

end module bench_many_mod

module many_create_3d_kernels
  use iso_c_binding, only: c_associated, c_null_ptr, c_ptr, c_size_t
  use iso_fortran_env, only: int64, real32
  use openacc, only: acc_device_nvidia, acc_get_device_num, acc_get_property, acc_property_free_memory
  use bench_many_mod, only: note_entry, note_exit_start, variant_data, variant_declare
  implicit none

  interface
    function acc_malloc_c(bytes) bind(c, name="acc_malloc") result(devptr)
      use iso_c_binding, only: c_ptr, c_size_t
      implicit none

      integer(c_size_t), value :: bytes
      type(c_ptr)              :: devptr
    end function acc_malloc_c

    subroutine acc_free_c(devptr) bind(c, name="acc_free")
      use iso_c_binding, only: c_ptr
      implicit none

      type(c_ptr), value :: devptr
    end subroutine acc_free_c
  end interface

contains

  subroutine lsmruc_many_declare(a, b, c, d, ni, nk, nj)
    implicit none

    integer, intent(in)         :: ni, nk, nj
    real(real32), intent(in)    :: a(ni,nk,nj), b(ni,nk,nj)
    !$acc declare copyin(a,b)
    real(real32), intent(inout) :: c(ni,nk,nj)
    !$acc declare copy(c)
    real(real32), intent(out)   :: d(ni,nk,nj)
    !$acc declare copy(d)
    real(real32) :: t001(ni,nk,nj), t002(ni,nk,nj), t003(ni,nk,nj), t004(ni,nk,nj), t005(ni,nk,nj), t006(ni,nk,nj), t007(ni,nk,nj), t008(ni,nk,nj)
    real(real32) :: t009(ni,nk,nj), t010(ni,nk,nj), t011(ni,nk,nj), t012(ni,nk,nj), t013(ni,nk,nj), t014(ni,nk,nj), t015(ni,nk,nj), t016(ni,nk,nj)
    real(real32) :: t017(ni,nk,nj), t018(ni,nk,nj), t019(ni,nk,nj), t020(ni,nk,nj), t021(ni,nk,nj), t022(ni,nk,nj), t023(ni,nk,nj), t024(ni,nk,nj)
    real(real32) :: t025(ni,nk,nj), t026(ni,nk,nj), t027(ni,nk,nj), t028(ni,nk,nj), t029(ni,nk,nj), t030(ni,nk,nj), t031(ni,nk,nj), t032(ni,nk,nj)
    real(real32) :: t033(ni,nk,nj), t034(ni,nk,nj), t035(ni,nk,nj), t036(ni,nk,nj), t037(ni,nk,nj), t038(ni,nk,nj), t039(ni,nk,nj), t040(ni,nk,nj)
    real(real32) :: t041(ni,nk,nj), t042(ni,nk,nj), t043(ni,nk,nj), t044(ni,nk,nj), t045(ni,nk,nj), t046(ni,nk,nj), t047(ni,nk,nj), t048(ni,nk,nj)
    real(real32) :: t049(ni,nk,nj), t050(ni,nk,nj), t051(ni,nk,nj), t052(ni,nk,nj), t053(ni,nk,nj), t054(ni,nk,nj), t055(ni,nk,nj), t056(ni,nk,nj)
    real(real32) :: t057(ni,nk,nj), t058(ni,nk,nj), t059(ni,nk,nj), t060(ni,nk,nj), t061(ni,nk,nj), t062(ni,nk,nj), t063(ni,nk,nj), t064(ni,nk,nj)
    real(real32) :: t065(ni,nk,nj), t066(ni,nk,nj), t067(ni,nk,nj), t068(ni,nk,nj), t069(ni,nk,nj), t070(ni,nk,nj), t071(ni,nk,nj), t072(ni,nk,nj)
    real(real32) :: t073(ni,nk,nj), t074(ni,nk,nj), t075(ni,nk,nj), t076(ni,nk,nj), t077(ni,nk,nj), t078(ni,nk,nj), t079(ni,nk,nj), t080(ni,nk,nj)
    real(real32) :: t081(ni,nk,nj), t082(ni,nk,nj), t083(ni,nk,nj), t084(ni,nk,nj), t085(ni,nk,nj), t086(ni,nk,nj), t087(ni,nk,nj), t088(ni,nk,nj)
    real(real32) :: t089(ni,nk,nj), t090(ni,nk,nj), t091(ni,nk,nj), t092(ni,nk,nj), t093(ni,nk,nj), t094(ni,nk,nj), t095(ni,nk,nj), t096(ni,nk,nj)
    real(real32) :: t097(ni,nk,nj), t098(ni,nk,nj), t099(ni,nk,nj), t100(ni,nk,nj), t101(ni,nk,nj), t102(ni,nk,nj), t103(ni,nk,nj), t104(ni,nk,nj)
    !$acc declare &
    !$acc  create(t001,t002,t003,t004,t005,t006,t007,t008) &
    !$acc  create(t009,t010,t011,t012,t013,t014,t015,t016) &
    !$acc  create(t017,t018,t019,t020,t021,t022,t023,t024) &
    !$acc  create(t025,t026,t027,t028,t029,t030,t031,t032) &
    !$acc  create(t033,t034,t035,t036,t037,t038,t039,t040) &
    !$acc  create(t041,t042,t043,t044,t045,t046,t047,t048) &
    !$acc  create(t049,t050,t051,t052,t053,t054,t055,t056) &
    !$acc  create(t057,t058,t059,t060,t061,t062,t063,t064) &
    !$acc  create(t065,t066,t067,t068,t069,t070,t071,t072) &
    !$acc  create(t073,t074,t075,t076,t077,t078,t079,t080) &
    !$acc  create(t081,t082,t083,t084,t085,t086,t087,t088) &
    !$acc  create(t089,t090,t091,t092,t093,t094,t095,t096) &
    !$acc  create(t097,t098,t099,t100,t101,t102,t103,t104)
    integer :: i, j, k

    call note_entry(variant_declare)

    !$acc parallel loop collapse(3) gang vector present(a,b,c, &
    !$acc  t001,t002,t003,t004,t005,t006,t007,t008, &
    !$acc  t009,t010,t011,t012,t013,t014,t015,t016, &
    !$acc  t017,t018,t019,t020,t021,t022,t023,t024, &
    !$acc  t025,t026,t027,t028,t029,t030,t031,t032)
    do j = 1, nj
      do k = 1, nk
        do i = 1, ni
          t001(i,k,j) = a(i,k,j) + 0.001_real32 * b(i,k,j) + 0.0001_real32 * c(i,k,j)
          t002(i,k,j) = t001(i,k,j) + 0.002_real32 * b(i,k,j)
          t003(i,k,j) = t002(i,k,j) - 0.003_real32 * a(i,k,j)
          t004(i,k,j) = t003(i,k,j) + 0.0004_real32 * c(i,k,j)
          t005(i,k,j) = t004(i,k,j) + 0.005_real32
          t006(i,k,j) = t005(i,k,j) + 0.006_real32 * b(i,k,j)
          t007(i,k,j) = t006(i,k,j) - 0.007_real32 * a(i,k,j)
          t008(i,k,j) = t007(i,k,j) + 0.0008_real32 * c(i,k,j)
          t009(i,k,j) = t008(i,k,j) + 0.009_real32
          t010(i,k,j) = t009(i,k,j) + 0.010_real32 * b(i,k,j)
          t011(i,k,j) = t010(i,k,j) - 0.011_real32 * a(i,k,j)
          t012(i,k,j) = t011(i,k,j) + 0.0012_real32 * c(i,k,j)
          t013(i,k,j) = t012(i,k,j) + 0.013_real32
          t014(i,k,j) = t013(i,k,j) + 0.014_real32 * b(i,k,j)
          t015(i,k,j) = t014(i,k,j) - 0.015_real32 * a(i,k,j)
          t016(i,k,j) = t015(i,k,j) + 0.0016_real32 * c(i,k,j)
          t017(i,k,j) = t016(i,k,j) + 0.017_real32
          t018(i,k,j) = t017(i,k,j) + 0.018_real32 * b(i,k,j)
          t019(i,k,j) = t018(i,k,j) - 0.019_real32 * a(i,k,j)
          t020(i,k,j) = t019(i,k,j) + 0.0020_real32 * c(i,k,j)
          t021(i,k,j) = t020(i,k,j) + 0.021_real32
          t022(i,k,j) = t021(i,k,j) + 0.022_real32 * b(i,k,j)
          t023(i,k,j) = t022(i,k,j) - 0.023_real32 * a(i,k,j)
          t024(i,k,j) = t023(i,k,j) + 0.0024_real32 * c(i,k,j)
          t025(i,k,j) = t024(i,k,j) + 0.025_real32
          t026(i,k,j) = t025(i,k,j) + 0.026_real32 * b(i,k,j)
          t027(i,k,j) = t026(i,k,j) - 0.027_real32 * a(i,k,j)
          t028(i,k,j) = t027(i,k,j) + 0.0028_real32 * c(i,k,j)
          t029(i,k,j) = t028(i,k,j) + 0.029_real32
          t030(i,k,j) = t029(i,k,j) + 0.030_real32 * b(i,k,j)
          t031(i,k,j) = t030(i,k,j) - 0.031_real32 * a(i,k,j)
          t032(i,k,j) = t031(i,k,j) + 0.0032_real32 * c(i,k,j)
        end do
      end do
    end do
    !$acc end parallel loop

    !$acc parallel loop collapse(3) gang vector present(a,b,c, &
    !$acc  t024,t032,t033,t034,t035,t036,t037,t038,t039,t040, &
    !$acc  t041,t042,t043,t044,t045,t046,t047,t048, &
    !$acc  t049,t050,t051,t052,t053,t054,t055,t056, &
    !$acc  t057,t058,t059,t060,t061,t062,t063,t064)
    do j = 1, nj
      do k = 1, nk
        do i = 1, ni
          t033(i,k,j) = t032(i,k,j) + 0.0033_real32 * a(i,k,j) + 0.0003_real32 * t024(i,k,j)
          t034(i,k,j) = t033(i,k,j) + 0.0034_real32 * t024(i,k,j)
          t035(i,k,j) = t034(i,k,j) - 0.035_real32 * b(i,k,j)
          t036(i,k,j) = t035(i,k,j) + 0.0036_real32 * c(i,k,j)
          t037(i,k,j) = t036(i,k,j) + 0.037_real32 * a(i,k,j)
          t038(i,k,j) = t037(i,k,j) + 0.0038_real32 * t024(i,k,j)
          t039(i,k,j) = t038(i,k,j) - 0.039_real32 * b(i,k,j)
          t040(i,k,j) = t039(i,k,j) + 0.0040_real32 * c(i,k,j)
          t041(i,k,j) = t040(i,k,j) + 0.041_real32 * a(i,k,j)
          t042(i,k,j) = t041(i,k,j) + 0.0042_real32 * t024(i,k,j)
          t043(i,k,j) = t042(i,k,j) - 0.043_real32 * b(i,k,j)
          t044(i,k,j) = t043(i,k,j) + 0.0044_real32 * c(i,k,j)
          t045(i,k,j) = t044(i,k,j) + 0.045_real32 * a(i,k,j)
          t046(i,k,j) = t045(i,k,j) + 0.0046_real32 * t024(i,k,j)
          t047(i,k,j) = t046(i,k,j) - 0.047_real32 * b(i,k,j)
          t048(i,k,j) = t047(i,k,j) + 0.0048_real32 * c(i,k,j)
          t049(i,k,j) = t048(i,k,j) + 0.049_real32 * a(i,k,j)
          t050(i,k,j) = t049(i,k,j) + 0.0050_real32 * t024(i,k,j)
          t051(i,k,j) = t050(i,k,j) - 0.051_real32 * b(i,k,j)
          t052(i,k,j) = t051(i,k,j) + 0.0052_real32 * c(i,k,j)
          t053(i,k,j) = t052(i,k,j) + 0.053_real32 * a(i,k,j)
          t054(i,k,j) = t053(i,k,j) + 0.0054_real32 * t024(i,k,j)
          t055(i,k,j) = t054(i,k,j) - 0.055_real32 * b(i,k,j)
          t056(i,k,j) = t055(i,k,j) + 0.0056_real32 * c(i,k,j)
          t057(i,k,j) = t056(i,k,j) + 0.057_real32 * a(i,k,j)
          t058(i,k,j) = t057(i,k,j) + 0.0058_real32 * t024(i,k,j)
          t059(i,k,j) = t058(i,k,j) - 0.059_real32 * b(i,k,j)
          t060(i,k,j) = t059(i,k,j) + 0.0060_real32 * c(i,k,j)
          t061(i,k,j) = t060(i,k,j) + 0.061_real32 * a(i,k,j)
          t062(i,k,j) = t061(i,k,j) + 0.0062_real32 * t024(i,k,j)
          t063(i,k,j) = t062(i,k,j) - 0.063_real32 * b(i,k,j)
          t064(i,k,j) = t063(i,k,j) + 0.0064_real32 * c(i,k,j)
        end do
      end do
    end do
    !$acc end parallel loop

    !$acc parallel loop collapse(3) gang vector present(a,b,c, &
    !$acc  t048,t064,t065,t066,t067,t068,t069,t070,t071,t072, &
    !$acc  t073,t074,t075,t076,t077,t078,t079,t080, &
    !$acc  t081,t082,t083,t084,t085,t086,t087,t088, &
    !$acc  t089,t090,t091,t092,t093,t094,t095,t096)
    do j = 1, nj
      do k = 1, nk
        do i = 1, ni
          t065(i,k,j) = t064(i,k,j) + 0.0065_real32 * b(i,k,j) + 0.0005_real32 * t048(i,k,j)
          t066(i,k,j) = t065(i,k,j) + 0.0066_real32 * t048(i,k,j)
          t067(i,k,j) = t066(i,k,j) - 0.067_real32 * c(i,k,j)
          t068(i,k,j) = t067(i,k,j) + 0.0068_real32 * a(i,k,j)
          t069(i,k,j) = t068(i,k,j) + 0.069_real32 * b(i,k,j)
          t070(i,k,j) = t069(i,k,j) + 0.0070_real32 * t048(i,k,j)
          t071(i,k,j) = t070(i,k,j) - 0.071_real32 * c(i,k,j)
          t072(i,k,j) = t071(i,k,j) + 0.0072_real32 * a(i,k,j)
          t073(i,k,j) = t072(i,k,j) + 0.073_real32 * b(i,k,j)
          t074(i,k,j) = t073(i,k,j) + 0.0074_real32 * t048(i,k,j)
          t075(i,k,j) = t074(i,k,j) - 0.075_real32 * c(i,k,j)
          t076(i,k,j) = t075(i,k,j) + 0.0076_real32 * a(i,k,j)
          t077(i,k,j) = t076(i,k,j) + 0.077_real32 * b(i,k,j)
          t078(i,k,j) = t077(i,k,j) + 0.0078_real32 * t048(i,k,j)
          t079(i,k,j) = t078(i,k,j) - 0.079_real32 * c(i,k,j)
          t080(i,k,j) = t079(i,k,j) + 0.0080_real32 * a(i,k,j)
          t081(i,k,j) = t080(i,k,j) + 0.081_real32 * b(i,k,j)
          t082(i,k,j) = t081(i,k,j) + 0.0082_real32 * t048(i,k,j)
          t083(i,k,j) = t082(i,k,j) - 0.083_real32 * c(i,k,j)
          t084(i,k,j) = t083(i,k,j) + 0.0084_real32 * a(i,k,j)
          t085(i,k,j) = t084(i,k,j) + 0.085_real32 * b(i,k,j)
          t086(i,k,j) = t085(i,k,j) + 0.0086_real32 * t048(i,k,j)
          t087(i,k,j) = t086(i,k,j) - 0.087_real32 * c(i,k,j)
          t088(i,k,j) = t087(i,k,j) + 0.0088_real32 * a(i,k,j)
          t089(i,k,j) = t088(i,k,j) + 0.089_real32 * b(i,k,j)
          t090(i,k,j) = t089(i,k,j) + 0.0090_real32 * t048(i,k,j)
          t091(i,k,j) = t090(i,k,j) - 0.091_real32 * c(i,k,j)
          t092(i,k,j) = t091(i,k,j) + 0.0092_real32 * a(i,k,j)
          t093(i,k,j) = t092(i,k,j) + 0.093_real32 * b(i,k,j)
          t094(i,k,j) = t093(i,k,j) + 0.0094_real32 * t048(i,k,j)
          t095(i,k,j) = t094(i,k,j) - 0.095_real32 * c(i,k,j)
          t096(i,k,j) = t095(i,k,j) + 0.0096_real32 * a(i,k,j)
        end do
      end do
    end do
    !$acc end parallel loop

    !$acc parallel loop collapse(3) gang vector present(a,c,d, &
    !$acc  t008,t024,t040,t072,t088,t096, &
    !$acc  t097,t098,t099,t100,t101,t102,t103,t104)
    do j = 1, nj
      do k = 1, nk
        do i = 1, ni
          t097(i,k,j) = t096(i,k,j) + 0.0097_real32 * a(i,k,j) + 0.0007_real32 * t072(i,k,j)
          t098(i,k,j) = t097(i,k,j) + 0.0098_real32 * t088(i,k,j)
          t099(i,k,j) = t098(i,k,j) - 0.0099_real32 * t040(i,k,j)
          t100(i,k,j) = t099(i,k,j) + 0.0100_real32 * t088(i,k,j)
          t101(i,k,j) = t100(i,k,j) - 0.0101_real32 * t040(i,k,j)
          t102(i,k,j) = t101(i,k,j) + 0.0102_real32 * t088(i,k,j)
          t103(i,k,j) = t102(i,k,j) - 0.0103_real32 * t040(i,k,j)
          t104(i,k,j) = t103(i,k,j) + 0.0104_real32 * t088(i,k,j)
          d(i,k,j) = t104(i,k,j) + 0.1_real32 * t072(i,k,j) + 0.01_real32 * t040(i,k,j) + 0.001_real32 * t008(i,k,j)
          c(i,k,j) = 0.5_real32 * t088(i,k,j) - 0.25_real32 * t096(i,k,j) + 0.05_real32 * t024(i,k,j)
        end do
      end do
    end do
    !$acc end parallel loop

    call note_exit_start(variant_declare)
  end subroutine lsmruc_many_declare

  subroutine lsmruc_many_data(a, b, c, d, ni, nk, nj)
    implicit none

    integer, intent(in)         :: ni, nk, nj
    real(real32), intent(in)    :: a(ni,nk,nj), b(ni,nk,nj)
    real(real32), intent(inout) :: c(ni,nk,nj)
    real(real32), intent(out)   :: d(ni,nk,nj)
    real(real32) :: t001(ni,nk,nj), t002(ni,nk,nj), t003(ni,nk,nj), t004(ni,nk,nj), t005(ni,nk,nj), t006(ni,nk,nj), t007(ni,nk,nj), t008(ni,nk,nj)
    real(real32) :: t009(ni,nk,nj), t010(ni,nk,nj), t011(ni,nk,nj), t012(ni,nk,nj), t013(ni,nk,nj), t014(ni,nk,nj), t015(ni,nk,nj), t016(ni,nk,nj)
    real(real32) :: t017(ni,nk,nj), t018(ni,nk,nj), t019(ni,nk,nj), t020(ni,nk,nj), t021(ni,nk,nj), t022(ni,nk,nj), t023(ni,nk,nj), t024(ni,nk,nj)
    real(real32) :: t025(ni,nk,nj), t026(ni,nk,nj), t027(ni,nk,nj), t028(ni,nk,nj), t029(ni,nk,nj), t030(ni,nk,nj), t031(ni,nk,nj), t032(ni,nk,nj)
    real(real32) :: t033(ni,nk,nj), t034(ni,nk,nj), t035(ni,nk,nj), t036(ni,nk,nj), t037(ni,nk,nj), t038(ni,nk,nj), t039(ni,nk,nj), t040(ni,nk,nj)
    real(real32) :: t041(ni,nk,nj), t042(ni,nk,nj), t043(ni,nk,nj), t044(ni,nk,nj), t045(ni,nk,nj), t046(ni,nk,nj), t047(ni,nk,nj), t048(ni,nk,nj)
    real(real32) :: t049(ni,nk,nj), t050(ni,nk,nj), t051(ni,nk,nj), t052(ni,nk,nj), t053(ni,nk,nj), t054(ni,nk,nj), t055(ni,nk,nj), t056(ni,nk,nj)
    real(real32) :: t057(ni,nk,nj), t058(ni,nk,nj), t059(ni,nk,nj), t060(ni,nk,nj), t061(ni,nk,nj), t062(ni,nk,nj), t063(ni,nk,nj), t064(ni,nk,nj)
    real(real32) :: t065(ni,nk,nj), t066(ni,nk,nj), t067(ni,nk,nj), t068(ni,nk,nj), t069(ni,nk,nj), t070(ni,nk,nj), t071(ni,nk,nj), t072(ni,nk,nj)
    real(real32) :: t073(ni,nk,nj), t074(ni,nk,nj), t075(ni,nk,nj), t076(ni,nk,nj), t077(ni,nk,nj), t078(ni,nk,nj), t079(ni,nk,nj), t080(ni,nk,nj)
    real(real32) :: t081(ni,nk,nj), t082(ni,nk,nj), t083(ni,nk,nj), t084(ni,nk,nj), t085(ni,nk,nj), t086(ni,nk,nj), t087(ni,nk,nj), t088(ni,nk,nj)
    real(real32) :: t089(ni,nk,nj), t090(ni,nk,nj), t091(ni,nk,nj), t092(ni,nk,nj), t093(ni,nk,nj), t094(ni,nk,nj), t095(ni,nk,nj), t096(ni,nk,nj)
    real(real32) :: t097(ni,nk,nj), t098(ni,nk,nj), t099(ni,nk,nj), t100(ni,nk,nj), t101(ni,nk,nj), t102(ni,nk,nj), t103(ni,nk,nj), t104(ni,nk,nj)
    integer :: i, j, k

    !$acc data copyin(a,b) copy(c,d) &
    !$acc  create(t001,t002,t003,t004,t005,t006,t007,t008) &
    !$acc  create(t009,t010,t011,t012,t013,t014,t015,t016) &
    !$acc  create(t017,t018,t019,t020,t021,t022,t023,t024) &
    !$acc  create(t025,t026,t027,t028,t029,t030,t031,t032) &
    !$acc  create(t033,t034,t035,t036,t037,t038,t039,t040) &
    !$acc  create(t041,t042,t043,t044,t045,t046,t047,t048) &
    !$acc  create(t049,t050,t051,t052,t053,t054,t055,t056) &
    !$acc  create(t057,t058,t059,t060,t061,t062,t063,t064) &
    !$acc  create(t065,t066,t067,t068,t069,t070,t071,t072) &
    !$acc  create(t073,t074,t075,t076,t077,t078,t079,t080) &
    !$acc  create(t081,t082,t083,t084,t085,t086,t087,t088) &
    !$acc  create(t089,t090,t091,t092,t093,t094,t095,t096) &
    !$acc  create(t097,t098,t099,t100,t101,t102,t103,t104)
    call note_entry(variant_data)

    !$acc parallel loop collapse(3) gang vector present(a,b,c, &
    !$acc  t001,t002,t003,t004,t005,t006,t007,t008, &
    !$acc  t009,t010,t011,t012,t013,t014,t015,t016, &
    !$acc  t017,t018,t019,t020,t021,t022,t023,t024, &
    !$acc  t025,t026,t027,t028,t029,t030,t031,t032)
    do j = 1, nj
      do k = 1, nk
        do i = 1, ni
          t001(i,k,j) = a(i,k,j) + 0.001_real32 * b(i,k,j) + 0.0001_real32 * c(i,k,j)
          t002(i,k,j) = t001(i,k,j) + 0.002_real32 * b(i,k,j)
          t003(i,k,j) = t002(i,k,j) - 0.003_real32 * a(i,k,j)
          t004(i,k,j) = t003(i,k,j) + 0.0004_real32 * c(i,k,j)
          t005(i,k,j) = t004(i,k,j) + 0.005_real32
          t006(i,k,j) = t005(i,k,j) + 0.006_real32 * b(i,k,j)
          t007(i,k,j) = t006(i,k,j) - 0.007_real32 * a(i,k,j)
          t008(i,k,j) = t007(i,k,j) + 0.0008_real32 * c(i,k,j)
          t009(i,k,j) = t008(i,k,j) + 0.009_real32
          t010(i,k,j) = t009(i,k,j) + 0.010_real32 * b(i,k,j)
          t011(i,k,j) = t010(i,k,j) - 0.011_real32 * a(i,k,j)
          t012(i,k,j) = t011(i,k,j) + 0.0012_real32 * c(i,k,j)
          t013(i,k,j) = t012(i,k,j) + 0.013_real32
          t014(i,k,j) = t013(i,k,j) + 0.014_real32 * b(i,k,j)
          t015(i,k,j) = t014(i,k,j) - 0.015_real32 * a(i,k,j)
          t016(i,k,j) = t015(i,k,j) + 0.0016_real32 * c(i,k,j)
          t017(i,k,j) = t016(i,k,j) + 0.017_real32
          t018(i,k,j) = t017(i,k,j) + 0.018_real32 * b(i,k,j)
          t019(i,k,j) = t018(i,k,j) - 0.019_real32 * a(i,k,j)
          t020(i,k,j) = t019(i,k,j) + 0.0020_real32 * c(i,k,j)
          t021(i,k,j) = t020(i,k,j) + 0.021_real32
          t022(i,k,j) = t021(i,k,j) + 0.022_real32 * b(i,k,j)
          t023(i,k,j) = t022(i,k,j) - 0.023_real32 * a(i,k,j)
          t024(i,k,j) = t023(i,k,j) + 0.0024_real32 * c(i,k,j)
          t025(i,k,j) = t024(i,k,j) + 0.025_real32
          t026(i,k,j) = t025(i,k,j) + 0.026_real32 * b(i,k,j)
          t027(i,k,j) = t026(i,k,j) - 0.027_real32 * a(i,k,j)
          t028(i,k,j) = t027(i,k,j) + 0.0028_real32 * c(i,k,j)
          t029(i,k,j) = t028(i,k,j) + 0.029_real32
          t030(i,k,j) = t029(i,k,j) + 0.030_real32 * b(i,k,j)
          t031(i,k,j) = t030(i,k,j) - 0.031_real32 * a(i,k,j)
          t032(i,k,j) = t031(i,k,j) + 0.0032_real32 * c(i,k,j)
        end do
      end do
    end do
    !$acc end parallel loop

    !$acc parallel loop collapse(3) gang vector present(a,b,c, &
    !$acc  t024,t032,t033,t034,t035,t036,t037,t038,t039,t040, &
    !$acc  t041,t042,t043,t044,t045,t046,t047,t048, &
    !$acc  t049,t050,t051,t052,t053,t054,t055,t056, &
    !$acc  t057,t058,t059,t060,t061,t062,t063,t064)
    do j = 1, nj
      do k = 1, nk
        do i = 1, ni
          t033(i,k,j) = t032(i,k,j) + 0.0033_real32 * a(i,k,j) + 0.0003_real32 * t024(i,k,j)
          t034(i,k,j) = t033(i,k,j) + 0.0034_real32 * t024(i,k,j)
          t035(i,k,j) = t034(i,k,j) - 0.035_real32 * b(i,k,j)
          t036(i,k,j) = t035(i,k,j) + 0.0036_real32 * c(i,k,j)
          t037(i,k,j) = t036(i,k,j) + 0.037_real32 * a(i,k,j)
          t038(i,k,j) = t037(i,k,j) + 0.0038_real32 * t024(i,k,j)
          t039(i,k,j) = t038(i,k,j) - 0.039_real32 * b(i,k,j)
          t040(i,k,j) = t039(i,k,j) + 0.0040_real32 * c(i,k,j)
          t041(i,k,j) = t040(i,k,j) + 0.041_real32 * a(i,k,j)
          t042(i,k,j) = t041(i,k,j) + 0.0042_real32 * t024(i,k,j)
          t043(i,k,j) = t042(i,k,j) - 0.043_real32 * b(i,k,j)
          t044(i,k,j) = t043(i,k,j) + 0.0044_real32 * c(i,k,j)
          t045(i,k,j) = t044(i,k,j) + 0.045_real32 * a(i,k,j)
          t046(i,k,j) = t045(i,k,j) + 0.0046_real32 * t024(i,k,j)
          t047(i,k,j) = t046(i,k,j) - 0.047_real32 * b(i,k,j)
          t048(i,k,j) = t047(i,k,j) + 0.0048_real32 * c(i,k,j)
          t049(i,k,j) = t048(i,k,j) + 0.049_real32 * a(i,k,j)
          t050(i,k,j) = t049(i,k,j) + 0.0050_real32 * t024(i,k,j)
          t051(i,k,j) = t050(i,k,j) - 0.051_real32 * b(i,k,j)
          t052(i,k,j) = t051(i,k,j) + 0.0052_real32 * c(i,k,j)
          t053(i,k,j) = t052(i,k,j) + 0.053_real32 * a(i,k,j)
          t054(i,k,j) = t053(i,k,j) + 0.0054_real32 * t024(i,k,j)
          t055(i,k,j) = t054(i,k,j) - 0.055_real32 * b(i,k,j)
          t056(i,k,j) = t055(i,k,j) + 0.0056_real32 * c(i,k,j)
          t057(i,k,j) = t056(i,k,j) + 0.057_real32 * a(i,k,j)
          t058(i,k,j) = t057(i,k,j) + 0.0058_real32 * t024(i,k,j)
          t059(i,k,j) = t058(i,k,j) - 0.059_real32 * b(i,k,j)
          t060(i,k,j) = t059(i,k,j) + 0.0060_real32 * c(i,k,j)
          t061(i,k,j) = t060(i,k,j) + 0.061_real32 * a(i,k,j)
          t062(i,k,j) = t061(i,k,j) + 0.0062_real32 * t024(i,k,j)
          t063(i,k,j) = t062(i,k,j) - 0.063_real32 * b(i,k,j)
          t064(i,k,j) = t063(i,k,j) + 0.0064_real32 * c(i,k,j)
        end do
      end do
    end do
    !$acc end parallel loop

    !$acc parallel loop collapse(3) gang vector present(a,b,c, &
    !$acc  t048,t064,t065,t066,t067,t068,t069,t070,t071,t072, &
    !$acc  t073,t074,t075,t076,t077,t078,t079,t080, &
    !$acc  t081,t082,t083,t084,t085,t086,t087,t088, &
    !$acc  t089,t090,t091,t092,t093,t094,t095,t096)
    do j = 1, nj
      do k = 1, nk
        do i = 1, ni
          t065(i,k,j) = t064(i,k,j) + 0.0065_real32 * b(i,k,j) + 0.0005_real32 * t048(i,k,j)
          t066(i,k,j) = t065(i,k,j) + 0.0066_real32 * t048(i,k,j)
          t067(i,k,j) = t066(i,k,j) - 0.067_real32 * c(i,k,j)
          t068(i,k,j) = t067(i,k,j) + 0.0068_real32 * a(i,k,j)
          t069(i,k,j) = t068(i,k,j) + 0.069_real32 * b(i,k,j)
          t070(i,k,j) = t069(i,k,j) + 0.0070_real32 * t048(i,k,j)
          t071(i,k,j) = t070(i,k,j) - 0.071_real32 * c(i,k,j)
          t072(i,k,j) = t071(i,k,j) + 0.0072_real32 * a(i,k,j)
          t073(i,k,j) = t072(i,k,j) + 0.073_real32 * b(i,k,j)
          t074(i,k,j) = t073(i,k,j) + 0.0074_real32 * t048(i,k,j)
          t075(i,k,j) = t074(i,k,j) - 0.075_real32 * c(i,k,j)
          t076(i,k,j) = t075(i,k,j) + 0.0076_real32 * a(i,k,j)
          t077(i,k,j) = t076(i,k,j) + 0.077_real32 * b(i,k,j)
          t078(i,k,j) = t077(i,k,j) + 0.0078_real32 * t048(i,k,j)
          t079(i,k,j) = t078(i,k,j) - 0.079_real32 * c(i,k,j)
          t080(i,k,j) = t079(i,k,j) + 0.0080_real32 * a(i,k,j)
          t081(i,k,j) = t080(i,k,j) + 0.081_real32 * b(i,k,j)
          t082(i,k,j) = t081(i,k,j) + 0.0082_real32 * t048(i,k,j)
          t083(i,k,j) = t082(i,k,j) - 0.083_real32 * c(i,k,j)
          t084(i,k,j) = t083(i,k,j) + 0.0084_real32 * a(i,k,j)
          t085(i,k,j) = t084(i,k,j) + 0.085_real32 * b(i,k,j)
          t086(i,k,j) = t085(i,k,j) + 0.0086_real32 * t048(i,k,j)
          t087(i,k,j) = t086(i,k,j) - 0.087_real32 * c(i,k,j)
          t088(i,k,j) = t087(i,k,j) + 0.0088_real32 * a(i,k,j)
          t089(i,k,j) = t088(i,k,j) + 0.089_real32 * b(i,k,j)
          t090(i,k,j) = t089(i,k,j) + 0.0090_real32 * t048(i,k,j)
          t091(i,k,j) = t090(i,k,j) - 0.091_real32 * c(i,k,j)
          t092(i,k,j) = t091(i,k,j) + 0.0092_real32 * a(i,k,j)
          t093(i,k,j) = t092(i,k,j) + 0.093_real32 * b(i,k,j)
          t094(i,k,j) = t093(i,k,j) + 0.0094_real32 * t048(i,k,j)
          t095(i,k,j) = t094(i,k,j) - 0.095_real32 * c(i,k,j)
          t096(i,k,j) = t095(i,k,j) + 0.0096_real32 * a(i,k,j)
        end do
      end do
    end do
    !$acc end parallel loop

    !$acc parallel loop collapse(3) gang vector present(a,c,d, &
    !$acc  t008,t024,t040,t072,t088,t096, &
    !$acc  t097,t098,t099,t100,t101,t102,t103,t104)
    do j = 1, nj
      do k = 1, nk
        do i = 1, ni
          t097(i,k,j) = t096(i,k,j) + 0.0097_real32 * a(i,k,j) + 0.0007_real32 * t072(i,k,j)
          t098(i,k,j) = t097(i,k,j) + 0.0098_real32 * t088(i,k,j)
          t099(i,k,j) = t098(i,k,j) - 0.0099_real32 * t040(i,k,j)
          t100(i,k,j) = t099(i,k,j) + 0.0100_real32 * t088(i,k,j)
          t101(i,k,j) = t100(i,k,j) - 0.0101_real32 * t040(i,k,j)
          t102(i,k,j) = t101(i,k,j) + 0.0102_real32 * t088(i,k,j)
          t103(i,k,j) = t102(i,k,j) - 0.0103_real32 * t040(i,k,j)
          t104(i,k,j) = t103(i,k,j) + 0.0104_real32 * t088(i,k,j)
          d(i,k,j) = t104(i,k,j) + 0.1_real32 * t072(i,k,j) + 0.01_real32 * t040(i,k,j) + 0.001_real32 * t008(i,k,j)
          c(i,k,j) = 0.5_real32 * t088(i,k,j) - 0.25_real32 * t096(i,k,j) + 0.05_real32 * t024(i,k,j)
        end do
      end do
    end do
    !$acc end parallel loop

    call note_exit_start(variant_data)
    !$acc end data
  end subroutine lsmruc_many_data

  subroutine allocator_polluter(ni, nk, nj, passes)
    implicit none

    integer, intent(in) :: ni, nk, nj, passes
    integer, parameter  :: max_chunks = 48
    integer(int64), parameter :: min_chunk_bytes = 8_int64 * 1024_int64 * 1024_int64
    integer(int64), parameter :: align_bytes     = 4096_int64
    type(c_ptr)         :: bufs(max_chunks)
    integer(int64)      :: free_bytes
    integer(int64)      :: target_bytes
    integer(int64)      :: remaining_bytes
    integer(int64)      :: request_bytes
    integer(int64)      :: baseline_bytes
    integer(int64)      :: perturb_bytes
    integer(int64)      :: max_chunk_bytes
    integer             :: allocated_chunks
    integer             :: devnum
    integer             :: ipass
    integer             :: ichunk
    integer             :: nchunks
    integer             :: slots_left

    if (passes <= 0) return
    if (ni <= 0 .or. nk <= 0 .or. nj <= 0) return

    devnum = acc_get_device_num(acc_device_nvidia)
    if (devnum < 0) return

    bufs(:) = c_null_ptr

    do ipass = 1, passes
      free_bytes   = acc_get_property(devnum, acc_device_nvidia, acc_property_free_memory)
      target_bytes = free_bytes * 8_int64 / 10_int64
      if (target_bytes < min_chunk_bytes) cycle

      nchunks = int(min(int(max_chunks, int64), max(1_int64, target_bytes / min_chunk_bytes)))
      remaining_bytes = target_bytes
      allocated_chunks = 0
      bufs(:) = c_null_ptr

      do ichunk = 1, nchunks
        slots_left = nchunks - ichunk + 1
        baseline_bytes = remaining_bytes / int(slots_left, int64)
        perturb_bytes  = max(1_int64, baseline_bytes / 5_int64)

        select case (mod(ichunk - 1, 4))
        case (0)
          request_bytes = baseline_bytes + perturb_bytes
        case (1)
          request_bytes = baseline_bytes - perturb_bytes / 2_int64
        case (2)
          request_bytes = baseline_bytes + perturb_bytes / 3_int64
        case default
          request_bytes = baseline_bytes - perturb_bytes
        end select

        if (slots_left > 1) then
          max_chunk_bytes = remaining_bytes - min_chunk_bytes * int(slots_left - 1, int64)
          request_bytes = min(request_bytes, max_chunk_bytes)
        else
          request_bytes = remaining_bytes
        end if

        request_bytes = max(min_chunk_bytes, align_down_int64(request_bytes, align_bytes))

        do while (request_bytes >= min_chunk_bytes)
          bufs(ichunk) = acc_malloc_c(int(request_bytes, c_size_t))
          if (c_associated(bufs(ichunk))) exit
          request_bytes = align_down_int64(request_bytes / 2_int64, align_bytes)
        end do

        if (.not. c_associated(bufs(ichunk))) exit

        allocated_chunks = ichunk
        remaining_bytes = max(0_int64, remaining_bytes - request_bytes)
        if (remaining_bytes < min_chunk_bytes) exit
      end do

      do ichunk = allocated_chunks, 1, -1
        if (c_associated(bufs(ichunk))) call acc_free_c(bufs(ichunk))
      end do
    end do
  end subroutine allocator_polluter

  integer(int64) function align_down_int64(value, alignment)
    implicit none

    integer(int64), intent(in) :: value
    integer(int64), intent(in) :: alignment

    if (alignment <= 0_int64) then
      align_down_int64 = value
    else
      align_down_int64 = (value / alignment) * alignment
    end if
  end function align_down_int64

end module many_create_3d_kernels

program many_create_3d_repro
  use iso_fortran_env, only: real32, real64
  use openacc
  use bench_many_mod
  use many_create_3d_kernels
  implicit none

  integer, parameter :: jitter_count = 8
  integer, parameter :: jitter_ni_delta(jitter_count) = [0, 3, 9, -4, 7, -8, 5, -2]
  integer, parameter :: jitter_nk_delta(jitter_count) = [0, 0, 0, 1, -1, 2, -1, 1]
  integer, parameter :: jitter_nj_delta(jitter_count) = [0, -3, -9, 2, 5, -4, 8, -6]

  integer :: ni
  integer :: nk
  integer :: nj
  integer :: repeats
  integer :: polluter_passes
  integer :: jitter_enable
  integer :: resident_map_count
  integer :: resident_map_elems
  integer :: resident_map_gap
  integer :: ni_alloc
  integer :: nk_alloc
  integer :: nj_alloc
  integer :: ni_call
  integer :: nk_call
  integer :: nj_call
  integer(int64) :: resident_map_bytes
  integer :: i
  integer :: j
  integer :: k
  real(real32), allocatable :: a(:,:,:), b(:,:,:), c_decl(:,:,:), d_decl(:,:,:), c_data(:,:,:), d_data(:,:,:)
  real(real32), allocatable :: resident_map_pool(:,:)
  real(real64) :: c_diff
  real(real64) :: d_diff

  ni      = 500
  nk      = 6
  nj      = 500
  repeats = 100
  polluter_passes = 1
  jitter_enable = 1
  resident_map_count = 0
  resident_map_elems = 256
  resident_map_gap   = 17
  call parse_args(ni, nk, nj, repeats, polluter_passes, jitter_enable, resident_map_count, resident_map_elems)
  call get_alloc_shape(ni, nk, nj, jitter_enable, ni_alloc, nk_alloc, nj_alloc)
  resident_map_bytes = int(max(0, resident_map_count), int64) * int(max(0, resident_map_elems), int64) * &
                       int(storage_size(0.0_real32) / 8, int64)

  allocate(a(ni_alloc,nk_alloc,nj_alloc), b(ni_alloc,nk_alloc,nj_alloc), &
           c_decl(ni_alloc,nk_alloc,nj_alloc), d_decl(ni_alloc,nk_alloc,nj_alloc), &
           c_data(ni_alloc,nk_alloc,nj_alloc), d_data(ni_alloc,nk_alloc,nj_alloc))

  call init_inputs(a, b, c_decl, c_data, d_decl, d_data, ni_alloc, nk_alloc, nj_alloc)

  call acc_init(acc_device_nvidia)

  !$acc enter data copyin(a,b,c_decl,c_data) create(d_decl,d_data)
  call setup_resident_mapping_stress(resident_map_pool, resident_map_count, resident_map_elems, resident_map_gap)

  call set_timing(.false.)
  call get_call_shape(ni, nk, nj, 1, jitter_enable, ni_call, nk_call, nj_call)
  call lsmruc_many_declare(a(1:ni_call,1:nk_call,1:nj_call), b(1:ni_call,1:nk_call,1:nj_call), &
                           c_decl(1:ni_call,1:nk_call,1:nj_call), d_decl(1:ni_call,1:nk_call,1:nj_call), &
                           ni_call, nk_call, nj_call)
  call lsmruc_many_data(a(1:ni_call,1:nk_call,1:nj_call), b(1:ni_call,1:nk_call,1:nj_call), &
                        c_data(1:ni_call,1:nk_call,1:nj_call), d_data(1:ni_call,1:nk_call,1:nj_call), &
                        ni_call, nk_call, nj_call)
  call set_timing(.true.)

  call init_inputs(a, b, c_decl, c_data, d_decl, d_data, ni_alloc, nk_alloc, nj_alloc)
  !$acc update device(a,b,c_decl,c_data,d_decl,d_data)

  call reset_stats()

  do i = 1, repeats
    call get_call_shape(ni, nk, nj, i, jitter_enable, ni_call, nk_call, nj_call)
    call note_call_start(variant_declare)
    call lsmruc_many_declare(a(1:ni_call,1:nk_call,1:nj_call), b(1:ni_call,1:nk_call,1:nj_call), &
                             c_decl(1:ni_call,1:nk_call,1:nj_call), d_decl(1:ni_call,1:nk_call,1:nj_call), &
                             ni_call, nk_call, nj_call)
    call note_call_end(variant_declare)
    call allocator_polluter(ni_call, nk_call, nj_call, polluter_passes)
  end do

  do i = 1, repeats
    call get_call_shape(ni, nk, nj, i, jitter_enable, ni_call, nk_call, nj_call)
    call note_call_start(variant_data)
    call lsmruc_many_data(a(1:ni_call,1:nk_call,1:nj_call), b(1:ni_call,1:nk_call,1:nj_call), &
                          c_data(1:ni_call,1:nk_call,1:nj_call), d_data(1:ni_call,1:nk_call,1:nj_call), &
                          ni_call, nk_call, nj_call)
    call note_call_end(variant_data)
    if (i < repeats) call allocator_polluter(ni_call, nk_call, nj_call, polluter_passes)
  end do

  !$acc update self(c_decl,c_data,d_decl,d_data)
  call teardown_resident_mapping_stress(resident_map_pool, resident_map_count, resident_map_elems)
  !$acc exit data delete(a,b,c_decl,c_data,d_decl,d_data)

  c_diff = maxval(abs(real(c_decl, real64) - real(c_data, real64)))
  d_diff = maxval(abs(real(d_decl, real64) - real(d_data, real64)))

  write (*,'(a)') 'OpenACC many-create 3D declare-vs-data reproducer'
  write (*,'(a,i0)') '  ni      = ', ni
  write (*,'(a,i0)') '  nk      = ', nk
  write (*,'(a,i0)') '  nj      = ', nj
  write (*,'(a,i0)') '  repeats = ', repeats
  if (jitter_enable /= 0) then
    write (*,'(a)') '  shape jitter between timed calls = enabled (8-state deterministic cycle)'
    write (*,'(a,i0,a,i0,a,i0)') '  allocated envelope = ', ni_alloc, ' x ', nk_alloc, ' x ', nj_alloc
  else
    write (*,'(a)') '  shape jitter between timed calls = disabled'
  end if
  if (resident_map_count > 0 .and. resident_map_elems > 0) then
    write (*,'(a,i0,a,i0,a,i0,a)') '  resident mapping stress = ', resident_map_count, ' extra mappings x ', &
                                    resident_map_elems, ' reals each (gap ', resident_map_gap, ' reals)'
    write (*,'(a,f10.3,a)') '  resident mapping stress bytes = ', real(resident_map_bytes, real64) / 1048576.0_real64, ' MiB'
  else
    write (*,'(a)') '  resident mapping stress = disabled'
  end if
  if (polluter_passes > 0) then
    write (*,'(a,i0)') '  polluter passes between timed calls = ', polluter_passes
    write (*,'(a)') '  untimed polluter uses acc_get_property(...) and uneven acc_malloc chunks'
    write (*,'(a)') '  target per pass = 80% of currently free device memory'
  else
    write (*,'(a)') '  polluter between timed calls = disabled'
  end if
  write (*,'(a)') '  local scratch set = 104 separate 3D automatic arrays'
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

  subroutine parse_args(ni, nk, nj, repeats, polluter_passes, jitter_enable, resident_map_count, resident_map_elems)
    integer, intent(inout) :: ni
    integer, intent(inout) :: nk
    integer, intent(inout) :: nj
    integer, intent(inout) :: repeats
    integer, intent(inout) :: polluter_passes
    integer, intent(inout) :: jitter_enable
    integer, intent(inout) :: resident_map_count
    integer, intent(inout) :: resident_map_elems
    character(len=64)      :: arg

    if (command_argument_count() >= 1) then
      call get_command_argument(1, arg)
      read (arg, *) ni
    end if

    if (command_argument_count() >= 2) then
      call get_command_argument(2, arg)
      read (arg, *) nk
    end if

    if (command_argument_count() >= 3) then
      call get_command_argument(3, arg)
      read (arg, *) nj
    end if

    if (command_argument_count() >= 4) then
      call get_command_argument(4, arg)
      read (arg, *) repeats
    end if

    if (command_argument_count() >= 5) then
      call get_command_argument(5, arg)
      read (arg, *) polluter_passes
    end if

    if (command_argument_count() >= 6) then
      call get_command_argument(6, arg)
      read (arg, *) jitter_enable
    end if

    if (command_argument_count() >= 7) then
      call get_command_argument(7, arg)
      read (arg, *) resident_map_count
    end if

    if (command_argument_count() >= 8) then
      call get_command_argument(8, arg)
      read (arg, *) resident_map_elems
    end if
  end subroutine parse_args

  subroutine setup_resident_mapping_stress(resident_map_pool, resident_map_count, resident_map_elems, resident_map_gap)
    real(real32), allocatable, intent(inout) :: resident_map_pool(:,:)
    integer, intent(in)                      :: resident_map_count
    integer, intent(in)                      :: resident_map_elems
    integer, intent(in)                      :: resident_map_gap
    integer                                  :: map_stride
    integer                                  :: imap

    if (resident_map_count <= 0 .or. resident_map_elems <= 0) return

    map_stride = resident_map_elems + max(1, resident_map_gap)
    allocate(resident_map_pool(map_stride, resident_map_count))
    resident_map_pool(:,:) = 0.0_real32

    do imap = 1, resident_map_count
      !$acc enter data create(resident_map_pool(1:resident_map_elems,imap))
    end do
  end subroutine setup_resident_mapping_stress

  subroutine teardown_resident_mapping_stress(resident_map_pool, resident_map_count, resident_map_elems)
    real(real32), allocatable, intent(inout) :: resident_map_pool(:,:)
    integer, intent(in)                      :: resident_map_count
    integer, intent(in)                      :: resident_map_elems
    integer                                  :: imap

    if (.not. allocated(resident_map_pool)) return

    do imap = resident_map_count, 1, -1
      !$acc exit data delete(resident_map_pool(1:resident_map_elems,imap))
    end do

    deallocate(resident_map_pool)
  end subroutine teardown_resident_mapping_stress

  subroutine get_alloc_shape(ni_base, nk_base, nj_base, jitter_enable, ni_out, nk_out, nj_out)
    integer, intent(in)  :: ni_base
    integer, intent(in)  :: nk_base
    integer, intent(in)  :: nj_base
    integer, intent(in)  :: jitter_enable
    integer, intent(out) :: ni_out
    integer, intent(out) :: nk_out
    integer, intent(out) :: nj_out
    integer              :: idx

    ni_out = ni_base
    nk_out = nk_base
    nj_out = nj_base

    if (jitter_enable == 0) return

    do idx = 1, jitter_count
      ni_out = max(ni_out, max(16, ni_base + jitter_ni_delta(idx)))
      nk_out = max(nk_out, max(1, nk_base + jitter_nk_delta(idx)))
      nj_out = max(nj_out, max(16, nj_base + jitter_nj_delta(idx)))
    end do
  end subroutine get_alloc_shape

  subroutine get_call_shape(ni_base, nk_base, nj_base, call_index, jitter_enable, ni_out, nk_out, nj_out)
    integer, intent(in)  :: ni_base
    integer, intent(in)  :: nk_base
    integer, intent(in)  :: nj_base
    integer, intent(in)  :: call_index
    integer, intent(in)  :: jitter_enable
    integer, intent(out) :: ni_out
    integer, intent(out) :: nk_out
    integer, intent(out) :: nj_out
    integer              :: idx

    if (jitter_enable == 0) then
      ni_out = ni_base
      nk_out = nk_base
      nj_out = nj_base
      return
    end if

    idx = mod(call_index - 1, jitter_count) + 1
    ni_out = max(16, ni_base + jitter_ni_delta(idx))
    nk_out = max(1, nk_base + jitter_nk_delta(idx))
    nj_out = max(16, nj_base + jitter_nj_delta(idx))
  end subroutine get_call_shape

  subroutine init_inputs(a, b, c_decl, c_data, d_decl, d_data, ni, nk, nj)
    integer, intent(in)       :: ni
    integer, intent(in)       :: nk
    integer, intent(in)       :: nj
    real(real32), intent(out) :: a(ni,nk,nj), b(ni,nk,nj), c_decl(ni,nk,nj), c_data(ni,nk,nj), d_decl(ni,nk,nj), d_data(ni,nk,nj)
    integer                   :: i
    integer                   :: j
    integer                   :: k

    do j = 1, nj
      do k = 1, nk
        do i = 1, ni
          a(i,k,j)      = 0.001_real32 * real(i, real32) + 0.01_real32 * real(k, real32)
          b(i,k,j)      = 0.0005_real32 * real(j, real32) + 1.0_real32 / real(i + 3 * k + 5 * j, real32)
          c_decl(i,k,j) = 0.25_real32 + 0.0001_real32 * real(mod(i + 7 * k + 11 * j, 97), real32)
          c_data(i,k,j) = c_decl(i,k,j)
          d_decl(i,k,j) = 0.0_real32
          d_data(i,k,j) = 0.0_real32
        end do
      end do
    end do
  end subroutine init_inputs

end program many_create_3d_repro
