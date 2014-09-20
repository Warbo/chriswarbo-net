#!/bin/sh
MIME=`file -b --mime-type $1`
DATA=`base64 -w 0 $1`
echo "<img alt='$1' src='data:$MIME;base64,$DATA' />"
