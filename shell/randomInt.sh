#!/usr/bin/bash
function getRandomInt() {

    # Produce a random integer of about 9 digits built from from 3 sources, so hopefully random

    processRandom=$(($RANDOM+$RANDOM+$RANDOM+$RANDOM)); #bashed built in random function from processid + other stuff?
    # each $RANDOM gives from 0 - 32767 (256*256)

    uRandom=$( od -N3 -An -i /dev/urandom | tr -d ' '); #grab 3 bytes of entropy from /dev/urandom
    # each byte can be from 0 to 255, so three bytes is 256^3 = 16777216

    nanoRandom=$( date +%N | sed 's/^0*//'); #nanosecond counter goes from 1 - 10^9

    randomSeed=`expr $processRandom + $uRandom + $nanoRandom`;
    echo $randomSeed
}
