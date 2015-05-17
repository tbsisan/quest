# Get the atom indexes for the fluid molecules.  This is so a program which reads NAMD dcd files can known
# which data corresponds to the fluid atoms.  Used by my matlab file:
# vmd -dispdev text -eofexit -pdb $pdbFile -e outputFluidIndexes.tcl -args $projStor/indexes/$canonicalLeaf.oi >> $vmdOutFile

#mol new [file normalize $pdbFile] type pdb autobonds off waitfor all
# Open output channel to write list of fluid atom indexes.
set indexOutputFile [lindex $argv 0]
set outputFilechannel [open $indexOutputFile w]
#set oif [regsub {pdb$} $pdbFile "oi"]
# TODO: find a better way to get fluid atoms, like using segnames or hijack an element symbol
set fluids [atomselect top "not name CA and not name SA and not name BA and not name NA"]
set fluidIndexes [$fluids get index]
set namdIndexes $fluidIndexes
set listIndex 0
foreach vmdIndex $fluidIndexes { 
    lset namdIndexes $listIndex [expr $vmdIndex+1] 
    incr listIndex
}
puts $outputFileChannel $namdIndexes
close $outputFileChannel
puts "wrote to $indexOutputFile"
exit
