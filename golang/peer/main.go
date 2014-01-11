package main

import (
	"flag"
	"fmt"
	"net"
	"os"
	"strconv"
)

var port = flag.Int("p", 1337, "Port at which to listen")

var shuttingDown = false
var done = make(chan bool)

func main() {
	flag.Parse()

	l, err := net.Listen("tcp", ":"+strconv.Itoa(*port))
	if err != nil {
		fmt.Fprintf(os.Stderr, "Listening Error: %s\n", err)
		return
	}
	defer l.Close()
	fmt.Printf("[Global] Started listening on %v\n", l.Addr())

	go accepter(l)

	parseStdIO()

	// shut down
	m.Lock()
	if len(nodes) == 0 {
		return
	}
	shuttingDown = true
	m.Unlock()
	disconnectAllNodes()
	<-done
}
