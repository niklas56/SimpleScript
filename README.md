# Simple Script

## Introduction

 A simple interpreter for  a custom programming language ("Simple Script") written in Haskell. Its syntax and functionality is very similar to Python. 
 I build this project to learn the basics of Haskell.

## Features
Simple Script is an imperative dynamically typed language, supporting the following types:

| Type     | Description            |
|----------|------------------------|
| int      | Integer Numbers        |
| float    | Floating Point Numbers |
| str      | Strings                |
| bool     | Booleans               |
| list     | Lists                  |
| null     | Null type

and operators:
| Operator         | Description                |
|------------------|----------------------------|
| +                | Addition and Concatenation |
| -                | Subtraction                |
| *                | Multiplication             |
| /                | Division                   |
| ^                | Power                      |
| %                | Modulo                     |
| ==               | Equal                      |
| !=               | Not equal                  |
| <                | Bigger                     |
| >                | Smaller                    |
| <=               | Smaller or equal           |  
| >=               | Bigger or equal            |
| ==               | Comparison                 |
| && / "and"       | And                        |
| \|\| / "or"      | Or                         |
| !                | Not                        |
| [list] ! [index] | Access list at index       |
| #                | size of list/string        |



## Example
The following example should demonstrate the important syntax and features:

    //import statements get followed by a path to the file.
	//(std is just an alias for the standard library)
	import  std

	//assign values to variables:
	a = 5
	b = 13.4
	c = "hello"
	d = true
	e = [1, 3.5, "world", [1,2,3]]

	// print values:
	>> a + b  	//gives 18.4
	>> !d  		//gives false
	>> #e  		//gives 4 (length of e)
	>> e!0 		//gives 1 (element 0 of e)
	>> type(a) 	//gives "int"
	>> float(a) //gives 5.0 (casts to float)


	//list comprehension:
	odd_squares = [x*x | x <- [1,2,3,4,5,6,7] | x*x % 2 != 0]
	>> odd_squares  //gives [1,9,25,49]


	//functions:
	// implementation of quicksort:
	fn qs(l):
		if  l == []:
			return []
			smaller = qs([x | x <- l.removeAt(0) | x <= l!0])
			bigger = qs([x | x <- l.removeAt(0) | x > l!0])
		return  smaller + [l!0] + bigger

	>> qs([5,3,1,4,2]) //gives [1,2,3,4,5]


	//loops:
	//while:
	l = [1,2,3]
	while  l != []:
		>> l!0
		l = l.removeAt(0)

	//for:
	for  i = 0; i < 10; i+1:
		>> i
		break
	
	//for-each:
	s = "hello"
	for  x <- s.toList():
		>> x

Besides that you can use the `os()` function to execute commands and get the stdout as string or use `error()` to raise an error and stop execution. For example:

	os("ls") // gives the content of the current directory as string
	error("My error-Message")

stdin can be read using the `input()` function.

There is also support for the ternary operator:

    >> true ? 5 : 10  // gives 5

To write content to a file there a built-in operators:

    "Hello World!" -> "test.txt" //overwrites the content
    "Hello World!" --> "test.txt //appends to the content

Or use `<-` to assign file-content to a variable:

    x <- "test.txt"

## Installation

If you have `ghc` on your system you can install the language by running the `install.sh` script.
The vscode extension can be installed by copying the `simple-script (extension)` folder into `~/.vscode/extensions`.