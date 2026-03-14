module bench_many_mod
  use iso_fortran_env, only: int64, real64
  implicit none

  integer, parameter :: variant_declare = 1
  integer, parameter :: variant_data    = 2
  integer, parameter :: nvariants       = 2

  type :: bench_stats
    real(real64) :: entry_sum = 0.0_real64
    real(real64) :: exit_sum  = 0.0_real64
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
    stats(variant)%calls     = stats(variant)%calls + 1
  end subroutine note_call_end

  subroutine print_stats(label, variant)
    character(len=*), intent(in) :: label
    integer, intent(in)          :: variant
    real(real64)                 :: avg_entry
    real(real64)                 :: avg_exit

    if (stats(variant)%calls == 0) then
      write (*,'(a)') trim(label)//': no timed calls'
      return
    end if

    avg_entry = stats(variant)%entry_sum / real(stats(variant)%calls, real64)
    avg_exit  = stats(variant)%exit_sum  / real(stats(variant)%calls, real64)

    write (*,'(a)') trim(label)
    write (*,'(a,f12.3,a)') '  avg entry = ', avg_entry * 1.0e6_real64, ' us'
    write (*,'(a,f12.3,a)') '  avg exit  = ', avg_exit  * 1.0e6_real64, ' us'
  end subroutine print_stats

end module bench_many_mod

module many_create_3d_kernels
  use iso_fortran_env, only: real32
  use bench_many_mod, only: note_entry, note_exit_start, variant_data, variant_declare
  implicit none

contains

  subroutine lsmruc_declare(a, b, c, d, ni, nk, nj)
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

    !$acc serial present( &
    !$acc  t001,t002,t003,t004,t005,t006,t007,t008, &
    !$acc  t009,t010,t011,t012,t013,t014,t015,t016, &
    !$acc  t017,t018,t019,t020,t021,t022,t023,t024, &
    !$acc  t025,t026,t027,t028,t029,t030,t031,t032, &
    !$acc  t033,t034,t035,t036,t037,t038,t039,t040, &
    !$acc  t041,t042,t043,t044,t045,t046,t047,t048, &
    !$acc  t049,t050,t051,t052,t053,t054,t055,t056, &
    !$acc  t057,t058,t059,t060,t061,t062,t063,t064, &
    !$acc  t065,t066,t067,t068,t069,t070,t071,t072, &
    !$acc  t073,t074,t075,t076,t077,t078,t079,t080, &
    !$acc  t081,t082,t083,t084,t085,t086,t087,t088, &
    !$acc  t089,t090,t091,t092,t093,t094,t095,t096, &
    !$acc  t097,t098,t099,t100,t101,t102,t103,t104)
    t001(1,1,1) = 1.0_real32;  t002(1,1,1) = 2.0_real32;  t003(1,1,1) = 3.0_real32;  t004(1,1,1) = 4.0_real32
    t005(1,1,1) = 5.0_real32;  t006(1,1,1) = 6.0_real32;  t007(1,1,1) = 7.0_real32;  t008(1,1,1) = 8.0_real32
    t009(1,1,1) = 9.0_real32;  t010(1,1,1) = 10.0_real32; t011(1,1,1) = 11.0_real32; t012(1,1,1) = 12.0_real32
    t013(1,1,1) = 13.0_real32; t014(1,1,1) = 14.0_real32; t015(1,1,1) = 15.0_real32; t016(1,1,1) = 16.0_real32
    t017(1,1,1) = 17.0_real32; t018(1,1,1) = 18.0_real32; t019(1,1,1) = 19.0_real32; t020(1,1,1) = 20.0_real32
    t021(1,1,1) = 21.0_real32; t022(1,1,1) = 22.0_real32; t023(1,1,1) = 23.0_real32; t024(1,1,1) = 24.0_real32
    t025(1,1,1) = 25.0_real32; t026(1,1,1) = 26.0_real32; t027(1,1,1) = 27.0_real32; t028(1,1,1) = 28.0_real32
    t029(1,1,1) = 29.0_real32; t030(1,1,1) = 30.0_real32; t031(1,1,1) = 31.0_real32; t032(1,1,1) = 32.0_real32
    t033(1,1,1) = 33.0_real32; t034(1,1,1) = 34.0_real32; t035(1,1,1) = 35.0_real32; t036(1,1,1) = 36.0_real32
    t037(1,1,1) = 37.0_real32; t038(1,1,1) = 38.0_real32; t039(1,1,1) = 39.0_real32; t040(1,1,1) = 40.0_real32
    t041(1,1,1) = 41.0_real32; t042(1,1,1) = 42.0_real32; t043(1,1,1) = 43.0_real32; t044(1,1,1) = 44.0_real32
    t045(1,1,1) = 45.0_real32; t046(1,1,1) = 46.0_real32; t047(1,1,1) = 47.0_real32; t048(1,1,1) = 48.0_real32
    t049(1,1,1) = 49.0_real32; t050(1,1,1) = 50.0_real32; t051(1,1,1) = 51.0_real32; t052(1,1,1) = 52.0_real32
    t053(1,1,1) = 53.0_real32; t054(1,1,1) = 54.0_real32; t055(1,1,1) = 55.0_real32; t056(1,1,1) = 56.0_real32
    t057(1,1,1) = 57.0_real32; t058(1,1,1) = 58.0_real32; t059(1,1,1) = 59.0_real32; t060(1,1,1) = 60.0_real32
    t061(1,1,1) = 61.0_real32; t062(1,1,1) = 62.0_real32; t063(1,1,1) = 63.0_real32; t064(1,1,1) = 64.0_real32
    t065(1,1,1) = 65.0_real32; t066(1,1,1) = 66.0_real32; t067(1,1,1) = 67.0_real32; t068(1,1,1) = 68.0_real32
    t069(1,1,1) = 69.0_real32; t070(1,1,1) = 70.0_real32; t071(1,1,1) = 71.0_real32; t072(1,1,1) = 72.0_real32
    t073(1,1,1) = 73.0_real32; t074(1,1,1) = 74.0_real32; t075(1,1,1) = 75.0_real32; t076(1,1,1) = 76.0_real32
    t077(1,1,1) = 77.0_real32; t078(1,1,1) = 78.0_real32; t079(1,1,1) = 79.0_real32; t080(1,1,1) = 80.0_real32
    t081(1,1,1) = 81.0_real32; t082(1,1,1) = 82.0_real32; t083(1,1,1) = 83.0_real32; t084(1,1,1) = 84.0_real32
    t085(1,1,1) = 85.0_real32; t086(1,1,1) = 86.0_real32; t087(1,1,1) = 87.0_real32; t088(1,1,1) = 88.0_real32
    t089(1,1,1) = 89.0_real32; t090(1,1,1) = 90.0_real32; t091(1,1,1) = 91.0_real32; t092(1,1,1) = 92.0_real32
    t093(1,1,1) = 93.0_real32; t094(1,1,1) = 94.0_real32; t095(1,1,1) = 95.0_real32; t096(1,1,1) = 96.0_real32
    t097(1,1,1) = 97.0_real32; t098(1,1,1) = 98.0_real32; t099(1,1,1) = 99.0_real32; t100(1,1,1) = 100.0_real32
    t101(1,1,1) = 101.0_real32; t102(1,1,1) = 102.0_real32; t103(1,1,1) = 103.0_real32; t104(1,1,1) = 104.0_real32
    !$acc end serial

    !$acc parallel loop collapse(3) gang vector present(a,b,c,d,t052,t104)
    do j = 1, nj
      do k = 1, nk
        do i = 1, ni
          d(i,k,j) = a(i,k,j) + b(i,k,j) + 1.0e-6_real32 * t104(1,1,1)
          c(i,k,j) = c(i,k,j) + 1.0e-6_real32 * t052(1,1,1)
        end do
      end do
    end do
    !$acc end parallel loop

    call note_exit_start(variant_declare)
  end subroutine lsmruc_declare

  subroutine lsmruc_data(a, b, c, d, ni, nk, nj)
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

    !$acc serial present( &
    !$acc  t001,t002,t003,t004,t005,t006,t007,t008, &
    !$acc  t009,t010,t011,t012,t013,t014,t015,t016, &
    !$acc  t017,t018,t019,t020,t021,t022,t023,t024, &
    !$acc  t025,t026,t027,t028,t029,t030,t031,t032, &
    !$acc  t033,t034,t035,t036,t037,t038,t039,t040, &
    !$acc  t041,t042,t043,t044,t045,t046,t047,t048, &
    !$acc  t049,t050,t051,t052,t053,t054,t055,t056, &
    !$acc  t057,t058,t059,t060,t061,t062,t063,t064, &
    !$acc  t065,t066,t067,t068,t069,t070,t071,t072, &
    !$acc  t073,t074,t075,t076,t077,t078,t079,t080, &
    !$acc  t081,t082,t083,t084,t085,t086,t087,t088, &
    !$acc  t089,t090,t091,t092,t093,t094,t095,t096, &
    !$acc  t097,t098,t099,t100,t101,t102,t103,t104)
    t001(1,1,1) = 1.0_real32;  t002(1,1,1) = 2.0_real32;  t003(1,1,1) = 3.0_real32;  t004(1,1,1) = 4.0_real32
    t005(1,1,1) = 5.0_real32;  t006(1,1,1) = 6.0_real32;  t007(1,1,1) = 7.0_real32;  t008(1,1,1) = 8.0_real32
    t009(1,1,1) = 9.0_real32;  t010(1,1,1) = 10.0_real32; t011(1,1,1) = 11.0_real32; t012(1,1,1) = 12.0_real32
    t013(1,1,1) = 13.0_real32; t014(1,1,1) = 14.0_real32; t015(1,1,1) = 15.0_real32; t016(1,1,1) = 16.0_real32
    t017(1,1,1) = 17.0_real32; t018(1,1,1) = 18.0_real32; t019(1,1,1) = 19.0_real32; t020(1,1,1) = 20.0_real32
    t021(1,1,1) = 21.0_real32; t022(1,1,1) = 22.0_real32; t023(1,1,1) = 23.0_real32; t024(1,1,1) = 24.0_real32
    t025(1,1,1) = 25.0_real32; t026(1,1,1) = 26.0_real32; t027(1,1,1) = 27.0_real32; t028(1,1,1) = 28.0_real32
    t029(1,1,1) = 29.0_real32; t030(1,1,1) = 30.0_real32; t031(1,1,1) = 31.0_real32; t032(1,1,1) = 32.0_real32
    t033(1,1,1) = 33.0_real32; t034(1,1,1) = 34.0_real32; t035(1,1,1) = 35.0_real32; t036(1,1,1) = 36.0_real32
    t037(1,1,1) = 37.0_real32; t038(1,1,1) = 38.0_real32; t039(1,1,1) = 39.0_real32; t040(1,1,1) = 40.0_real32
    t041(1,1,1) = 41.0_real32; t042(1,1,1) = 42.0_real32; t043(1,1,1) = 43.0_real32; t044(1,1,1) = 44.0_real32
    t045(1,1,1) = 45.0_real32; t046(1,1,1) = 46.0_real32; t047(1,1,1) = 47.0_real32; t048(1,1,1) = 48.0_real32
    t049(1,1,1) = 49.0_real32; t050(1,1,1) = 50.0_real32; t051(1,1,1) = 51.0_real32; t052(1,1,1) = 52.0_real32
    t053(1,1,1) = 53.0_real32; t054(1,1,1) = 54.0_real32; t055(1,1,1) = 55.0_real32; t056(1,1,1) = 56.0_real32
    t057(1,1,1) = 57.0_real32; t058(1,1,1) = 58.0_real32; t059(1,1,1) = 59.0_real32; t060(1,1,1) = 60.0_real32
    t061(1,1,1) = 61.0_real32; t062(1,1,1) = 62.0_real32; t063(1,1,1) = 63.0_real32; t064(1,1,1) = 64.0_real32
    t065(1,1,1) = 65.0_real32; t066(1,1,1) = 66.0_real32; t067(1,1,1) = 67.0_real32; t068(1,1,1) = 68.0_real32
    t069(1,1,1) = 69.0_real32; t070(1,1,1) = 70.0_real32; t071(1,1,1) = 71.0_real32; t072(1,1,1) = 72.0_real32
    t073(1,1,1) = 73.0_real32; t074(1,1,1) = 74.0_real32; t075(1,1,1) = 75.0_real32; t076(1,1,1) = 76.0_real32
    t077(1,1,1) = 77.0_real32; t078(1,1,1) = 78.0_real32; t079(1,1,1) = 79.0_real32; t080(1,1,1) = 80.0_real32
    t081(1,1,1) = 81.0_real32; t082(1,1,1) = 82.0_real32; t083(1,1,1) = 83.0_real32; t084(1,1,1) = 84.0_real32
    t085(1,1,1) = 85.0_real32; t086(1,1,1) = 86.0_real32; t087(1,1,1) = 87.0_real32; t088(1,1,1) = 88.0_real32
    t089(1,1,1) = 89.0_real32; t090(1,1,1) = 90.0_real32; t091(1,1,1) = 91.0_real32; t092(1,1,1) = 92.0_real32
    t093(1,1,1) = 93.0_real32; t094(1,1,1) = 94.0_real32; t095(1,1,1) = 95.0_real32; t096(1,1,1) = 96.0_real32
    t097(1,1,1) = 97.0_real32; t098(1,1,1) = 98.0_real32; t099(1,1,1) = 99.0_real32; t100(1,1,1) = 100.0_real32
    t101(1,1,1) = 101.0_real32; t102(1,1,1) = 102.0_real32; t103(1,1,1) = 103.0_real32; t104(1,1,1) = 104.0_real32
    !$acc end serial

    !$acc parallel loop collapse(3) gang vector present(a,b,c,d,t052,t104)
    do j = 1, nj
      do k = 1, nk
        do i = 1, ni
          d(i,k,j) = a(i,k,j) + b(i,k,j) + 1.0e-6_real32 * t104(1,1,1)
          c(i,k,j) = c(i,k,j) + 1.0e-6_real32 * t052(1,1,1)
        end do
      end do
    end do
    !$acc end parallel loop

    call note_exit_start(variant_data)
    !$acc end data
  end subroutine lsmruc_data

end module many_create_3d_kernels

program many_create_3d_repro
  use iso_fortran_env, only: error_unit, real32
  use openacc
  use bench_many_mod
  use many_create_3d_kernels
  implicit none

  integer :: ni
  integer :: nk
  integer :: nj
  integer :: repeats
  integer :: resident_map_count
  integer :: resident_map_elems
  integer :: resident_map_gap
  logical :: show_help

  real(real32), allocatable :: a(:,:,:), b(:,:,:), c_decl(:,:,:), d_decl(:,:,:), c_data(:,:,:), d_data(:,:,:)
  real(real32), allocatable :: resident_map_pool(:,:)
  integer      :: i

  ni = 500
  nk = 6
  nj = 500
  repeats = 100
  resident_map_count = 20000
  resident_map_elems = 64
  resident_map_gap   = 17

  call parse_args(ni, nk, nj, repeats, resident_map_count, resident_map_elems, resident_map_gap, show_help)
  if (show_help) go to 900

  allocate(a(ni,nk,nj), b(ni,nk,nj), c_decl(ni,nk,nj), d_decl(ni,nk,nj), c_data(ni,nk,nj), d_data(ni,nk,nj))
  allocate(resident_map_pool(resident_map_elems + resident_map_gap, resident_map_count))

  call init_inputs(a, b, c_decl, c_data, d_decl, d_data)
  resident_map_pool(:,:) = 0.0_real32

  call acc_init(acc_device_nvidia)

  !$acc enter data copyin(a,b,c_decl,c_data) create(d_decl,d_data)
  call setup_resident_mapping_stress(resident_map_pool)

  call set_timing(.false.)
  call lsmruc_declare(a, b, c_decl, d_decl, ni, nk, nj)
  call lsmruc_data(a, b, c_data, d_data, ni, nk, nj)
  call set_timing(.true.)

  call init_inputs(a, b, c_decl, c_data, d_decl, d_data)
  !$acc update device(a,b,c_decl,c_data,d_decl,d_data)

  call reset_stats()

  do i = 1, repeats
    call note_call_start(variant_declare)
    call lsmruc_declare(a, b, c_decl, d_decl, ni, nk, nj)
    call note_call_end(variant_declare)
  end do

  do i = 1, repeats
    call note_call_start(variant_data)
    call lsmruc_data(a, b, c_data, d_data, ni, nk, nj)
    call note_call_end(variant_data)
  end do

  call teardown_resident_mapping_stress(resident_map_pool)
  !$acc exit data delete(a,b,c_decl,c_data,d_decl,d_data)

  write (*,'(a)') 'OpenACC declare-vs-data present-table reproducer'
  write (*,'(a,i0,a,i0,a,i0,a,i0)') '  ni = ', ni, ', nk = ', nk, ', nj = ', nj, ', repeats = ', repeats
  write (*,'(a,i0,a,i0,a,i0,a)') '  resident mappings = ', resident_map_count, ' x ', resident_map_elems, ' reals (gap ', resident_map_gap, ')'
  write (*,*)

  call print_stats('declare version', variant_declare)
  write (*,*)
  call print_stats('data version', variant_data)

  call acc_shutdown(acc_device_nvidia)

900 continue

contains

  subroutine parse_args(ni, nk, nj, repeats, resident_map_count, resident_map_elems, resident_map_gap, show_help)
    integer, intent(inout) :: ni
    integer, intent(inout) :: nk
    integer, intent(inout) :: nj
    integer, intent(inout) :: repeats
    integer, intent(inout) :: resident_map_count
    integer, intent(inout) :: resident_map_elems
    integer, intent(inout) :: resident_map_gap
    logical, intent(out)   :: show_help
    integer                :: iarg
    character(len=128)     :: arg
    character(len=128)     :: key
    character(len=128)     :: value
    integer                :: eq

    show_help = .false.
    iarg = 1
    do while (iarg <= command_argument_count())
      call get_command_argument(iarg, arg)
      eq = index(arg, '=')

      if (eq > 0) then
        key = trim(arg(:eq-1))
        value = trim(arg(eq+1:))
      else
        key = trim(arg)
        value = ''
      end if

      select case (trim(key))
      case ('-h', '--help')
        call print_help()
        show_help = .true.
        return
      case ('--ni')
        call parse_integer_option(iarg, key, value, ni)
      case ('--nk')
        call parse_integer_option(iarg, key, value, nk)
      case ('--nj')
        call parse_integer_option(iarg, key, value, nj)
      case ('--repeats')
        call parse_integer_option(iarg, key, value, repeats)
      case ('--resident-map-count')
        call parse_integer_option(iarg, key, value, resident_map_count)
      case ('--resident-map-elems')
        call parse_integer_option(iarg, key, value, resident_map_elems)
      case ('--resident-map-gap')
        call parse_integer_option(iarg, key, value, resident_map_gap)
      case default
        write (error_unit,'(a,a)') 'Unknown option: ', trim(arg)
        call print_help()
        error stop 1
      end select

      iarg = iarg + 1
    end do

    if (ni <= 0 .or. nk <= 0 .or. nj <= 0 .or. repeats <= 0) then
      write (error_unit,'(a)') 'ni, nk, nj, and repeats must be positive.'
      error stop 1
    end if

    if (resident_map_count < 0 .or. resident_map_elems <= 0 .or. resident_map_gap < 0) then
      write (error_unit,'(a)') 'resident_map_count must be nonnegative; resident_map_elems must be positive; resident_map_gap must be nonnegative.'
      error stop 1
    end if
  end subroutine parse_args

  subroutine parse_integer_option(iarg, key, value, target)
    integer, intent(inout)     :: iarg
    character(len=*), intent(in) :: key
    character(len=*), intent(in) :: value
    integer, intent(inout)     :: target
    character(len=128)         :: next_arg
    integer                    :: ios

    if (len_trim(value) == 0) then
      if (iarg >= command_argument_count()) then
        write (error_unit,'(a,a)') 'Missing value for option ', trim(key)
        error stop 1
      end if
      iarg = iarg + 1
      call get_command_argument(iarg, next_arg)
    else
      next_arg = value
    end if

    read (next_arg, *, iostat=ios) target
    if (ios /= 0) then
      write (error_unit,'(a,a,a,a)') 'Invalid integer for option ', trim(key), ': ', trim(next_arg)
      error stop 1
    end if
  end subroutine parse_integer_option

  subroutine print_help()
    write (*,'(a)') 'Usage: ./repro_many_create_3d [options]'
    write (*,'(a)') 'Options:'
    write (*,'(a)') '  -h, --help                 Show this help text and exit.'
    write (*,'(a)') '  --ni N                     Horizontal i extent. Default: 500'
    write (*,'(a)') '  --nk N                     Vertical extent. Default: 6'
    write (*,'(a)') '  --nj N                     Horizontal j extent. Default: 500'
    write (*,'(a)') '  --repeats N                Timed calls per variant. Default: 100'
    write (*,'(a)') '  --resident-map-count N     Extra long-lived resident mappings. Default: 20000'
    write (*,'(a)') '  --resident-map-elems N     Reals per extra resident mapping. Default: 64'
    write (*,'(a)') '  --resident-map-gap N       Gap between mapped slices in the backing pool. Default: 17'
    write (*,'(a)') 'Values may be passed as --option VALUE or --option=VALUE.'
  end subroutine print_help

  subroutine setup_resident_mapping_stress(resident_map_pool)
    real(real32), intent(inout) :: resident_map_pool(:,:)
    integer                     :: imap

    do imap = 1, resident_map_count
      !$acc enter data create(resident_map_pool(1:resident_map_elems,imap))
    end do
  end subroutine setup_resident_mapping_stress

  subroutine teardown_resident_mapping_stress(resident_map_pool)
    real(real32), allocatable, intent(inout) :: resident_map_pool(:,:)
    integer                                  :: imap

    do imap = resident_map_count, 1, -1
      !$acc exit data delete(resident_map_pool(1:resident_map_elems,imap))
    end do

    deallocate(resident_map_pool)
  end subroutine teardown_resident_mapping_stress

  subroutine init_inputs(a, b, c_decl, c_data, d_decl, d_data)
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
