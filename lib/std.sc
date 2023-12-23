//Constants:

pi = 3.14159265359

//Functions

fn remove(l, x):
   newL = []
   i = 0
   while i < #l:
      if l!i != x:
         newL = newL + [l!i]
      i = i+1
   return newL

fn removeAt(l, index):
   newL = []
   i = 0
   while i < #l:
      if i != index:
         newL = newL + [l!i]
      i = i+1
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
   return [list!(#list-1)] + list.removeAt(#list-1)

fn list(start, end, inc):
   if inc == 0:
      return []
   if (start >= end && inc > 0) || (start <= end && inc < 0):
      return []
   return [start] + list(start+inc, end, inc)

fn abs(x):
   if x < 0:
      return -x
   return x

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