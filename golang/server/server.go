// server
package main

import (
	"flag"
	"fmt"
	"io"
	"net"
	"os"
	"strconv"
)

var (
	port = flag.Int("port", 1337, "Port at which to listen")
)

func main() {
	flag.Parse()

	l, err := net.Listen("tcp", ":"+strconv.Itoa(*port))
	if err != nil {
		fmt.Errorf("Listening Error: %s\n", err)
		return
	}
	defer l.Close()

	c, err := l.Accept()
	if err != nil {
		fmt.Errorf("Accept Error: %s\n", err)
		return
	}

	_, err = io.Copy(os.Stdout, c)
	if err != nil {
		fmt.Errorf("Copy Error: %s\n", err)
		return
	}
}
