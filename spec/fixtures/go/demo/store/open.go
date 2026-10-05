package store

import (
	"strings"

	"example.com/demo/internal/util"
)

type Store struct {
	name string
}

func Open(name string) *Store {
	return &Store{name: util.Trim(strings.ToLower(name))}
}

func (s *Store) Close() error {
	s.name = ""
	return nil
}
