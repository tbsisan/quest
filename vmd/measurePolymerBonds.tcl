# Run with vmd -dispdev text -psf [psffilename] -dcd [dcdfilename] -e measurePolymerBonds.tcl
# Processes a dcd data file from a simulation of a polymer.  Atoms must be spatially ordered be index number.
# Outputs the data for all the bond lengths, angles, and dihedrals, to polymerData directory.

# Get the info needed to name the output files for the bondlengths, angles, and dihedrals.
set tframes [molinfo top get numframes]
set files [molinfo top get filename]
set dcdpath [lindex $files 0 1]
set fn [file tail $dcdpath]
set fl [file rootname $fn]
set bondFile [open "/projects/p20200/polymerData/$fl.bonds" w]
set angleFile [open "/projects/p20200/polymerData/$fl.angles" w]
set dihedralFile [open "/projects/p20200/polymerData/$fl.dihedrals" w]

# Select the polymer atoms.  TODO: need better way to select rather than just the non CA atoms.
set polymer [atomselect top "not name CA"]
set bondPairs {}
set angleTrios {}
set dihedralQuads {}
set polymerAtomInds [$polymer get index]
set polymerNum [llength $polymerAtomInds]
set polymerNumMinus1 [expr $polymerNum-1]

# Build lists of pairs, trios, and quads
for {set atom1 0} {$atom1<$polymerNumMinus1 } {incr atom1} {
    set atom2 [expr $atom1+1]
    set atom3 [expr $atom1+2]
    set atom4 [expr $atom1+3]
    set atomInd1 [lindex $polymerAtomInds $atom1] 
    if {$atom1 < $polymerNumMinus1} {
        set atomInd2 [lindex $polymerAtomInds $atom2] 
        lappend bondPairs [list $atomInd1 $atomInd2]
    }
    if {$atom2 < [expr $polymerNumMinus1-1]} {
        set atomInd3 [lindex $polymerAtomInds $atom3] 
        lappend angleTrios [list $atomInd1 $atomInd2 $atomInd3]
    }
    if {$atom3 < [expr $polymerNumMinus1-2]} {
        set atomInd4 [lindex $polymerAtomInds $atom4] 
        lappend dihedralQuads [list $atomInd1 $atomInd2 $atomInd3 $atomInd4]
    }
}

# For every frame, measure the bond lengths and angles, and write to the data files.
for {set i 1} {$i < $tframes} {incr i 1} {
	set bondLengths {}
	foreach pair $bondPairs {
	    lappend bondLengths [measure bond $pair frame $i]
	}
    puts $bondFile $bondLengths
	set angles {}
	foreach trio $angleTrios {
	    lappend angles [measure angle $trio frame $i]
	}
    puts $angleFile $angles
	set dihedrals {}
	foreach quad $dihedralQuads {
	    lappend dihedrals [measure dihed $quad frame $i]
	}
    puts $dihedralFile $dihedrals
}
close $bondFile
close $angleFile
close $dihedralFile
# makeMov

exit;
