# Get the atom indexes for the fluid molecules.  This is so a program which reads NAMD dcd files can known
# which data corresponds to the fluid atoms.  Used by my matlab file:
# vmd -dispdev text -eofexit -pdb $pdbFile -e outputFluidIndexes.tcl -args $projStor/indexes/$canonicalLeaf.oi >> $vmdOutFile

#mol new [file normalize $pdbFile] type pdb autobonds off waitfor all
# Open output channel to write list of fluid atom indexes.
proc listPlusOne {inList} {
    # Change from vmd indexes to namd indexes.
    set namdIndexes $inList
    set listIndex 0
    foreach vmdIndex $inList { 
        lset namdIndexes $listIndex [expr $vmdIndex+1] 
        incr listIndex
    }
    return $namdIndexes
}
proc writeIndexes {outputFile vmdSelection} {
    set outputHandler [open $outputFile w]
    set selectionIndexes [$vmdSelection get index]
    set namdSelectionIndexes [listPlusOne $selectionIndexes]
    puts $outputHandler $namdSelectionIndexes
    puts "Wrote to $outputFile"
    close $outputHandler
}
set fluidIndexOutputFile [lindex $argv 0]
set fluidSelection [atomselect top {not name "[CBNS]A"}]
writeIndexes $fluidIndexOutputFile $fluidSelection

set pistonIndexOutputFile [lindex $argv 1]
set pistonSelection [atomselect top {segname "[TB]PS"}]
writeIndexes $pistonIndexOutputFile $pistonSelection
#set oif [regsub {pdb$} $pdbFile "oi"]
# TODO: find a better way to get fluid atoms, like using segnames or hijack an element symbol
exit
