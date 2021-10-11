q = 1L
f = function(x) {
  s = 0L
  for (i in 1:x)
    s = s+q
  s
}

for (i in 1:5)
  print(system.time(print(f(10000000L))))

q=1.1

for (i in 1:5)
  print(system.time(print(f(10000000L))))
