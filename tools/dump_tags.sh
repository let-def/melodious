#!/bin/sh

if [ -z "$1" ]
then
  cat <<END
Usage: $0 <sqlite-args>
Usually args just contains path to the sqlite database file.

Dump tags from database as json.
END
else
  {
    sqlite3 "$@" <<END
.mode tabs
select song,key,value from tags order by song,key;
END
  } | {
    lsong=""
    printf '{'
    while read song key value; do
      if ! [[ "x$lsong" = "x$song" ]]
      then
        [[ "x$lsong" = "x" ]] || printf '\n  },'
        printf '\n  "%s":\n  {\n    %s:\t"%s"' "$song" "$key" "$value"
        lsong=$song
      else 
        printf ',\n    %s:\t"%s"' "$key" "$value"
      fi
    done
    printf '\n  }\n}\n'
  }
fi
