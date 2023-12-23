//Constants:

pi = 3.14159265359

//Functions

fn remove(l, x):
   return [ y | y <- l, y != x ]

fn removeAt(l, index):
   newL = []
   for i = 0; i < #l; i+1:
      if i != index:
         newL = newL + [l!i]
   return newL

fn removeFirst(l, x):
   if l == []:
      return []
   if l!0 == x:
      return l.removeAt(0)
   return [l!0] + removeFirst(l.removeAt(0), x)

fn reverse(list):
   if list == []:
      return []
   return reverse(list.removeAt(0)) + [list!0]

fn list(start, end, inc):
   if inc == 0:
      return []
   if (start >= end && inc > 0) || (start <= end && inc < 0):
      return []
   return [start] + list(start+inc, end, inc)

fn abs(x):
   return x < 0 ? -x : x

fn toList(s):
   res = []
   for i = 0; i < #s; i+1:
      res = res + [s!i]
   return res

fn split(s, x):
   res = []
   curr = []
   for el <- s.toList():
      if x == el:
         res = res + [curr]
         curr = []
      else:
         curr = curr + [el]
   return res + [curr]

fn contains(l,x):
   if l == [] or l == "":
      return false
   if l!0 == x:
      return true
   return contains(l.removeAt(0), x)