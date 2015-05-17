# Procedures for making movies in VMD. 
# Check your ~/.vmdrc file  to see if any changes to the display style are already made.
# For example, if your .vmdrc file has any scence rotations, the below procs won't rotate correctly.

proc usefulVMDcommands {} {
	# This procedure just here for reference and learning purposes
	$mySelection set radius 1
	mol selection "oxygen"; # what does this do
	color Name OT grey
	color Segname TW1 yellow
	set cmol [molinfo top]
	mol representation VDW
	mol delrep 0 $cmol
	mol addrep $cmol
	color Display {Background} white
	color Name C black
    mol smoothrep 0 0 3
}

proc setupPolymer {} {
    # Draw a CNT in "line" represention with a polymer in CPK form.
    display projection orthographic
    mol modselect 0 0 "name CA"; # keep the carbon selection with default display
    mol addrep 0; # add a new rep for beautifying the polymer
    mol modselect 1 0 "not name CA"; # assign the non-carbon atoms to the new rep
    mol modcolor 1 0 ResID; # Color polymer by ResID
    mol modstyle 1 0 CPK 2.0 0.2 10.0 10.0; # Display polymer with the CPK style
}

proc setupPolymerEnd {{scaling 1.0}} {
    # Look down the end of a CNT.
    # If not set, scaling takes the defaul value 1.0
    # By default, we view down the axis of the CNT, assuming it hasn't been changed in .vmdrc
    display height 1.0
    # display resize 500 500
    scale by $scaling
}

proc setupPolymerLong {{scaling 1.0}} {
    # Put cnt axis horizontal (left to right).
    # If not set, scaling takes the defaul value 1.0
    display resetview; # go back to initial view angle
    rotate y by 90
    # display resize 1200 200
    scale by $scaling
}

proc getFileLeaf {molid} {
	set files [molinfo $molid get filename]
	set fullPath [lindex $files 0 1]
	set fn [file tail $fullPath]
	set fl [file rootname $fn]
    return $fl
}

proc writeFramePpmImgs {startFrame skipFrame endFrame imgRootName} {
	set frameCount 0
	set writingDir [pwd]
	for {set currentFrame $startFrame} {$currentFrame < [expr $endFrame+1]} {incr currentFrame $skipFrame} {
		animate goto $currentFrame
		set tganame snap.[format "%04d" $currentFrame].tga
		#  set pngname /home/tbs246/Dropbox/LichterGroup/nuthesis/pres/mov/snap55.[format "%04d" $i].png
		set ppmname $imgRootName.[format "%04d" $frameCount].ppm
		render TachyonInternal $tganame
		exec convert $tganame $ppmname
		# puts "tried to write $ppmname"
		file delete $tganame
		incr frameCount
		#rotate y by 20
    }
    puts "Wrote $frameCount .ppm images into dir:$writingDir"
    return $frameCount
}

proc convertPpmsToMpeg {movName lastFrameAsStr} {
    # Assume the file ppmtompeg config file ppmtompeg.prm is in curr directory.
    # First, make the config file for the ppmtompeg command.
    puts "\tConverting all .ppm images for $movName into .mpeg movie with ppmtompeg command."
    puts "\tWriting frame files up to last frame identifier $lastFrameAsStr"
	exec awk -v movName=$movName -v lastfr=$lastFrameAsStr {{ sub(/floppy/,movName); sub(/1000/,lastfr); print }} <ppmtompeg.prm >ppmtompeg_tcl.prm
	exec -ignorestderr ppmtompeg ppmtompeg_tcl.prm
}

proc convertMpegToMp4 {movName} {
    puts "\tConverting .mpeg into .mp4 file with ffmpeg"
	exec -ignorestderr ffmpeg -r 10 -i ${movName}.mpeg -acodec aac -ab 128kb -vcodec mpeg4 -b 1200k -mbd 2 -cmp 2 -subcmp 2 -r 10 -y ${movName}.mp4
}

proc checkNumFrames {endFrame} {
	set totalFrames [molinfo top get numframes]
    if {$totalFrames < $endFrame} {
        puts "WARNING: In makeMov tcl procedure, last requested frame $endFrame is greater than number of frames $totalFrames."
        puts "\tSetting endFrame to last frame."
        set endFrame $totalFrames
    }
    return $endFrame
}

proc makeMov {startFrame skipFrame endFrame namePrefix {saveMovsDir "~/movs"} {saveImgsDir "~/movs"}} {
	puts "Making Movie"
	puts "\tAssuming presence of awk, ppmtompeg, and ffmpeg commands"
    puts "\tThe movie directory parameter ($saveMovsDir), should be an absolute path unless set to ~/movs (the default)"
    set fl [getFileLeaf top]
	set movName ${namePrefix}_$fl
	set sys [atomselect top all]
	set initialDir [pwd]
	cd $saveImgsDir
    set endFrame [checkNumFrames $endFrame]
    set frameCountWritten [writeFramePpmImgs $startFrame $skipFrame $endFrame $movName]
	set lastFrame [format "%04d" [expr $frameCountWritten-1]]
    convertPpmsToMpeg $movName $lastFrame
    convertMpegToMp4 $movName
    if {![string equal $saveMovsDir "~/movs"]} {
        puts "\tCopying movie files into dir: $saveMovsDir, and deleting originals"
        # note the file command probably doesn't expand ~ to the home directory, assuming absolute paths used
	    file copy -force ${movName}.mpeg ${saveMovsDir}/${movName}.mpeg
	    file copy -force ${movName}.mp4 ${saveMovsDir}/${movName}.mp4
        file delete ${movName}.mpeg ${movName}.mp4
    }
	cd $initialDir
}
