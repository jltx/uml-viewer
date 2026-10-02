package store

import "testing"

func TestOpen(t *testing.T) {
	opened := Open("  Demo ")
	rows := Query(opened, "  find   all ")
	if len(rows) != 1 || rows[0].String() != "find all,demo" {
		t.Fatalf("unexpected rows: %v", rows)
	}
	if opened.String() != "store:demo" {
		t.Fatalf("unexpected store string: %q", opened.String())
	}
	if normalize(" a  b ") != "a b" {
		t.Fatal("normalize did not collapse whitespace")
	}
	if err := opened.Close(); err != nil {
		t.Fatal(err)
	}
}
