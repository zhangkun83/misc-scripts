#!/bin/bash
# Use imagemagick to vertically stretch a portion of the image, which
# is the rectangle (0, START_Y) to (WIDTH, END_Y)
set -e
if [[ "$#" -lt 4 ]]; then
    echo "Usage: <INPUT> <START_Y> <END_Y> <RATIO_PERCENTAGE>"
    exit 1;
fi
INPUT="$1"
shift
START_Y="$1"
shift
END_Y="$1"
shift
RATIO="$1"

OUTPUT="${INPUT}.mod.jpg"

INPUT_W=$(identify -format "%w" "$INPUT")
INPUT_H=$(identify -format "%h" "$INPUT")
# $(( )) is the arithmetic expansion

REGION_ORIG_H=$(( $END_Y - $START_Y ))
REGION_NEW_H=$(( $REGION_ORIG_H * $RATIO / 100  ))
H_DELTA=$(( ${REGION_NEW_H} - ${REGION_ORIG_H} ))

REGION_FILE=$(mktemp)

# line 1: define output canvas
# line 2, 3: the bottom portion unstreched, but shifted down by H_DELTA
# line 4, 5: the stretched portion
# line 6: the top portion unstreched
convert -size ${INPUT_W}x$(( ${INPUT_H} + ${H_DELTA} )) xc:none \
        \( "$INPUT" -crop ${INPUT_W}x$(( $INPUT_H - $END_Y ))!+0+${END_Y} \) \
                                                   -geometry +0+$(( $END_Y + $H_DELTA )) -composite \
        \( "$INPUT" -crop ${ORIG_W}x${REGION_ORIG_H}!+0+${START_Y} \
        -resize ${ORIG_W}x${REGION_NEW_H}! \) -geometry +0+${START_Y} -composite \
        \( "$INPUT" -crop ${INPUT_W}x${START_Y}!+0+0 \) -geometry +0+0 -composite \
        "$OUTPUT"
