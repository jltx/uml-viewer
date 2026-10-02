package store

import "strings"

type Row struct {
	values []string
}

type Cache[K comparable, V any] struct {
	entries map[K]V
}

func Query(store *Store, text string) []Row {
	return []Row{{values: []string{normalize(text), store.name}}}
}

func (s Store) String() string {
	return "store:" + s.name
}

func normalize(text string) string {
	return strings.Join(strings.Fields(text), " ")
}

func (r Row) String() string {
	return strings.Join(r.values, ",")
}

func (c *Cache[K, V]) Get(key K) (V, bool) {
	value, found := c.entries[key]
	return value, found
}
