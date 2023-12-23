//import statements get followed by a path to the file. 
//(std is just an alias for the standard library)
import std

//assign values to variables:
a = 5
b = 13.4
c = "hello"
d = true
e = [1, 3.5, "world", [1,2,3]]

// print operator:
>> a + b    //gives 18.4
>> !d       //gives false
>> #e       //gives 4 (length of e)
>> e!0      //gives 1 (element 0 of e)
>> type(a)  //gives "int"
>> float(a) //gives 5.0 (casts to float)


//list comprehension:
odd_squares = [x*x | x <- [1,2,3,4,5,6,7] | x*x % 2 != 0] 
>> odd_squares //gives [1,9,25,49]


//functions:
// implementation of quicksort:
fn qs(l):
   if l == []:
      return []
   smaller = qs([x | x <- l.removeAt(0) | x <= l!0])
   bigger = qs([x | x <- l.removeAt(0) | x > l!0])
   return smaller + [l!0] + bigger

>> qs([5,3,1,4,2]) //gives [1,2,3,4,5]


// loops:

//while:
l = [1,2,3]
while l != []:
   >> l!0
   l = l.removeAt(0)

//for:
for i = 0; i < 10; i+1:
   >> i
   break

//for-each:
s = "hello"
for x <- s.toList():
   >> x