reg.finalizer(
  e = loadNamespace("base"),
  onexit = TRUE,
  f = function(x) {
    recordings.stop()
    recordings.save("/tmp/recordings.rds")
  }
)

recordings.start()
