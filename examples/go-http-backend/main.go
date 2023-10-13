package main

import (
	"fmt"
	"github.com/garnix-io/garner/examples/go-http-backend/server"
	"github.com/gorilla/mux"
	"net/http"
)

func handleRequest(w http.ResponseWriter, req *http.Request) {
	fmt.Fprintf(w, "Hello from go: %v\n", server.Add(1, 2))
}

func main() {
	r := mux.NewRouter()
	r.HandleFunc("/", handleRequest)
	http.Handle("/", r)
	fmt.Println("Backend listening on port 3000")
	http.ListenAndServe(":3000", nil)
}
