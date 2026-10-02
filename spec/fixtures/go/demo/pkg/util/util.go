package util

import "net/http"

func Fetch(address string) (*http.Response, error) {
	return http.Get(address)
}
