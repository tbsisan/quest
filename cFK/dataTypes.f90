MODULE dataTypes
IMPLICIT NONE
!ya
INTEGER, PARAMETER :: BR=8
INTEGER, PARAMETER :: RR=4
INTEGER, PARAMETER :: BIGREAL=selected_real_kind(16,100)
INTEGER, PARAMETER :: SMREAL=selected_real_kind(4,5)
integer, parameter :: INT64 = selected_int_kind(18)
integer, parameter :: INT32 = selected_int_kind(9)
integer, parameter :: IEEE32 = selected_real_kind(  6,  37 )
integer, parameter :: IEEE64 = selected_real_kind( 15, 307 )
END MODULE dataTypes
