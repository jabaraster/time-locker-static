lessc less/common.less > .work/common.css \
  && cleancss .work/common.css --output css/common.min.css \
  && aws s3 cp css/common.css s3://static.time-locker.jabara.info/css/