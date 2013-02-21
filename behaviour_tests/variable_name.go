package main

import "fmt"

type /*foo */ T /*bar*/ struct{}

var a,
	b = 1, 2
var x, y = 1, 2
var z = 3
var (
	a = 1
	b = 2
)

func simple() int {
	return 0
}

func f1(
	x int) (
	y int) {
	f := func(
		a int) (
		b int) {
		return a
	}
	return f(
		x)
	_ = T{}
}

func f2(x, /* Wow! */ //
	y int) (u, // Even across multilines with comments
	v bool) { /* This ROCKS! */
	_ = func( // Cool, man.
		a, // Foo
		b int) (
		u, /* Comment */
		v int) {
		return a,
			b
	}
	x,
		y := 1,
		2
	z :=
		3
}

// Comment
/* This
   is a
   Comment */
func (t *T) method(
	x int,
	y int) (u int,
	v int) {
	_ = func(a int,
		b int) (
		u int,
		v int) {
		fmt.Println(a, b)
		return a,
			b
	}
}
