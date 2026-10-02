//go:build windows

package store

func platformName() string {
	return "windows"
}

func windowsOnly() bool {
	return true
}
