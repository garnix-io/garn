package server_test

import (
	"github.com/garnix-io/garner/examples/go-http-backend/server"
	"testing"
)

func TestAdd(t *testing.T) {
	t.Run("returns Pepper's score", func(t *testing.T) {
		got := server.Add(1, 2)
		want := 3
		if got != want {
			t.Errorf("got %q, want %q", got, want)
		}
	})
}
