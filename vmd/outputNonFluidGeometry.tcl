set geomOutputFile [lindex $argv 0]
set outputFileChannel [open $geomOutputFile w]

set cnt [atomselect top {segname "CH[0-9]"}]
set membraneBarriers [atomselect top {segname SLB BSL}]
set pistons [atomselect top {segname "[BT]PS"}]
set cntExtent [measure minmax $cnt]
set barrierExtent [measure minmax $membraneBarriers]
set pistonExtent [measure minmax $pistons]
set cntBtm [lindex $cntExtent 0 2]
set cntTop [lindex $cntExtent 1 2]
set barrierBtm [lindex $barrierExtent 0 2]
set barrierTop [lindex $barrierExtent 1 2]
set pistonBtm [lindex $pistonExtent 0 2]
set pistonTop [lindex $pistonExtent 1 2]

# write nonFluid geometry out to a file
puts $outputFileChannel [list $cntBtm $cntTop]
puts $outputFileChannel [list $barrierBtm $barrierTop]
puts $outputFileChannel [list $pistonBtm $pistonTop]
close $outputFileChannel
exit
