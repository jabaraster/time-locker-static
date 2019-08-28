#!/bin/sh
elm-format elm/Index.elm --output elm/Index.elm --yes \
  && elm-format elm/Api.elm --output elm/Api.elm --yes \
  && elm-format elm/Line.elm --output elm/Line.elm --yes \
  && elm-format elm/Types.elm --output elm/Types.elm --yes \
  && elm-format elm/RemoteResource.elm --output elm/RemoteResource.elm --yes \
  && elm make elm/Index.elm --output=./.work/index.js \
  && elm make elm/Line.elm --output=./.work/line.js \
  && uglifyjs .work/index.js --compress 'pure_funcs="Elm.Index.init,F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' \
  | uglifyjs --mangle --output=./js/index.min.js \
  && uglifyjs .work/line.js --compress 'pure_funcs="Elm.Index.init,F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' \
  | uglifyjs --mangle --output=./js/line.min.js \
  && aws s3 cp js/index.min.js s3://static.time-locker.jabara.info/js/