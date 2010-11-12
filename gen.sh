#!/bin/bash

wget -c -nv --no-parent --mirror ftp://unicode.org/Public/MAPPINGS/

mkdir generated_tables/

for FILE in \
	./unicode.org/Public/MAPPINGS/ISO8859/8859-*.TXT \
	./unicode.org/Public/MAPPINGS/OBSOLETE/EASTASIA/JIS/SHIFTJIS.TXT \
	./unicode.org/Public/MAPPINGS/OBSOLETE/EASTASIA/OTHER/BIG5.TXT
do
	#BASENAME1=${FILE/*\//}
	#BASENAME=${BASENAME1/%.TXT/}
	BASENAME=$(basename ${FILE} .TXT)
	M="to_unicode_table-${BASENAME}"
	(
	echo "-module('$M')."
	echo
	echo "% Automatically generated file by to_unicode/gen.sh from ${FILE}"
	echo "% at $(date) using file version ${VERSION} with checksum"
	echo "% md5sum $(md5sum ${FILE})"
	echo
	echo "-export([map/1, m/1])."
	echo
	echo "map(Input) -> lists:map(fun m/1, Input)."
	echo
	awk '!/^#|^$/ {sub(/^0x/,"16#",$1); sub(/^0x/,"16#",$2); print "m(" $1 ") -> " $2 "; % " $0 }' "$FILE"
	echo "m(_) -> throw(badarg)."
	echo
	) > "generated_tables/${M}.erl"
	echo "generated_tables/${M}.erl"
	erlc "generated_tables/${M}.erl"
done
