#!/bin/sh
if [[ -z "$MELODIOUS" ]]; then
  MELODIOUS=http://lakaban.net/melodious
fi

ACTION=$1

get_tags()
{
  OGG=$1
  TAG=$2

  if [[ -z "$TAG" ]]; then
    vorbiscomment "$OGG"
  else
    cat "$TAG"
  fi
}

process_tags()
{
  while read tag; do echo >&2 -- "$tag"; printf -- '-F\0%s\0' "$tag"; done
}

upload_data()
{
  OGG=$1

  echo "Uploading: " "$OGG"
  xargs -0 curl -F "ogg=@$OGG" -# -o /dev/null "$MELODIOUS/upload/song"
}

check_data()
{
  md5sum -b -- "$1" | {
    read a b;
    curl -s "$MELODIOUS/upload/check?hash=$a"
  } | {
    read v;
    test "$v" -eq 1
  }
}

if [[ -z "$ACTION" ]]; then
  cat <<END
Configuration:
  Set MELODIOUS variable to override melodious instance, e.g: 
  $ export MELODIOUS='http://melo.yawdp.com'

Usage: $0 <file.ogg> [<file.tag>]
  Upload file.ogg, then tag with file.tag if given or use vorbiscomment
  to extract embedded tags. Stop if file is already hosted.

Usage: $0 force <file.ogg> [<file.tag>]
  Upload file.ogg, then tag with file.tag if given or use vorbiscomment
  to extract embedded tags.

Usage: $0 check <file.ogg>
  Check if file is already hosted.

TODO: $0 remote <file.ogg>
  Check if the server already has tags for <file.ogg>.

TODO: $0 update <file.ogg> <file.tag>
  Don't upload file if not needed, only update tags.
END
  exit 0
fi

case "x$ACTION" in
xcheck)
  check_data "$2"
  ;;

xremote)
  echo TODO
  exit 1
  ;;

xupdate)
  echo TODO
  exit 1
  ;;

force)
  get_tags "$2" "$3" | process_tags | upload_data "$2"
  ;;

*)
  if (check_data "$1"); then
    echo Skip "$1".
  else
    get_tags "$1" "$2" | process_tags | upload_data "$1"
  fi
  ;;
esac
