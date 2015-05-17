# Call like: vmd -dispdev text -psf psfs/cnt60_nm88_cgpsemb50a10u20d100p90.psf -dcd /projects/p20200/XiEffects/changeh/projScience/namdState/cnt60_nm88_cgpsemb50a10u20d100p90_ens1_T64_coul_runcgp6_langevin5all.dcd -e movieController.tcl -args $start $skip $end $movDir $imgDir
# If the argument list isn't 5 elements, the end elements are set to the empty string
lassign $::argv startFrame skipFrame endFrame movDir imgDir
puts "movieController.tcl run with passed in args: $startFrame, $skipFrame, $endFrame, $movDir, $imgDir"
source /home/tbs246/md/vmd/movieRoutines.tcl
setupPolymer
setupPolymerEnd
makeMov $startFrame $skipFrame $endFrame "End" $movDir $imgDir
set zoom 1.75
setupPolymerLong $zoom
makeMov $startFrame $skipFrame $endFrame "Long" $movDir $imgDir
exit;
