package main

import (
	"example.com/demo/pkg/util"
	"example.com/demo/store"
)

func main() {
	opened := store.Open("demo")
	defer opened.Close()
	_ = util.Fetch
}
