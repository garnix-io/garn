package main

import (
    "fmt"
    "net/http"
)

func handleRequest(w http.ResponseWriter, req *http.Request) {
    fmt.Fprintf(w, "Hello from go: %v\n", Add(1, 2))
}

func main() {
    http.HandleFunc("/", handleRequest)
    http.ListenAndServe(":3000", nil)
}
