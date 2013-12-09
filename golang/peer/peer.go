// server
package main

import (
	"bufio"
	"flag"
	"fmt"
	"math/rand"
	"net"
	"os"
	"strconv"
	"strings"
	"sync"

	p "../peer/protocol"
)

var (
	port = flag.Int("p", 1337, "Port at which to listen")
)

var nodes []*p.Node
var m = new(sync.RWMutex)

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
}

func accepter(l net.Listener) {
	for {
		c, err := l.Accept()
		if err != nil {
			fmt.Fprintf(os.Stderr, "Accept Error: %s\n", err)
			return
		}
		fmt.Printf("[Global] New Connection from %v\n", c.RemoteAddr())

		_ = p.NewConnection(c, forwardConnection, nil)

	}
}

func forwardConnection(c *p.Connection, msg *p.Message) {
	m.RLock()
	defer m.RUnlock()

	var n *p.Node
	switch msg.Action {
	case p.ActionHelloCW, p.ActionHelloCCW:
		for _, _n := range nodes {
			if _n.Loc == msg.DstLoc {
				n = _n
				break
			}
		}
	case p.ActionSplitEdge:
		// looking for a node that is free
		var freenodes []*p.Node
		for _, n := range nodes {
			if n.State == p.StateFree {
				freenodes = append(freenodes, n)
			}
		}

		if len(freenodes) == 0 {
			fmt.Printf("[Global] Rcv from %v, No Free Node: Closing Conn: %s\n", c.Remote(), msg)
			c.Close()
			return
		}

		n = freenodes[rand.Intn(len(freenodes))]
	default:
		fmt.Printf("[Global] Rcv Invalid from %v: %s\n", c.Remote(), msg)
		return
	}

	if n == nil {
		fmt.Printf("[Global] Rcv from %v -> Loc#%d not found: %s\n", c.Remote(), msg.DstLoc, msg)
		return
	}

	fmt.Printf("[Global] Rcv from %v -> Loc#%d: %s\n", c.Remote(), n.Loc, msg)

	n.MessageCallback(c, msg)
}

func parseStdIO() {
	scanner := bufio.NewScanner(os.Stdin)

	for scanner.Scan() {
		text := scanner.Text()
		if idx := strings.Index(text, " "); idx != -1 {
			command := text[:idx]
			text = text[idx+1:]
			switch command {
			case "c":
				connectNewNode(text)
			case "d":
				disconnectNode(text)
			}
		} else {
			switch text {
			case "cycle":
				createCycleNode()
			case "q":
				return
			case "l":
				listNodes()
			}
		}
	}
	if err := scanner.Err(); err != nil {
		fmt.Fprintln(os.Stderr, "Error reading stdio:", err)
	}
}

func listNodes() {
	m.RLock()
	defer m.RUnlock()

	for _, n := range nodes {
		fmt.Println(n)
	}
}

func createCycleNode() {
	c, err := net.Dial("tcp", "127.0.0.1:"+strconv.Itoa(*port))
	if err != nil {
		fmt.Fprintf(os.Stderr, "Dial Error: %s\n", err)
		return
	}

	n := p.NewNode(localAddress())
	fmt.Printf("[Node#%d] New Node -> Connected to %s\n", n.Loc, c.RemoteAddr())
	m.Lock()
	nodes = append(nodes, n)
	m.Unlock()

	conn := p.NewConnection(c, nil, nil)
	n.SetPrev(conn, localAddress(), n.Loc)
	n.SendPrev(p.NewHelloCWMessage(localAddress(), n.Loc, n.Loc))
}

func disconnectNode(loc string) {
	iLoc, err := strconv.Atoi(loc)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Parse Error: %s\n", err)
		return
	}
	var n *p.Node

	m.RLock()
	for _, n_ := range nodes {
		if n_.Loc == p.Location(iLoc) {
			n = n_
			break
		}
	}
	m.RUnlock()
	if n == nil {
		fmt.Fprintf(os.Stderr, "Node#%s not found\n", loc)
		return
	}

	n.InitiateMergeEdge()
}

func connectNewNode(addr string) {
	addrparts := strings.Split(addr, " ")
	if len(addrparts) != 2 {
		fmt.Fprintln(os.Stderr, "Parameter does not have form 'ip port'")
		return
	}
	port, err := strconv.Atoi(addrparts[1])
	if err != nil {
		fmt.Fprintf(os.Stderr, "Parsing Error: Port (%s) is not a number\n", addrparts[1])
		return
	}

	c, err := net.Dial("tcp", addrparts[0]+":"+addrparts[1])
	if err != nil {
		fmt.Fprintf(os.Stderr, "Dial Error: %s\n", err)
		return
	}

	n := p.NewNode(localAddress())
	fmt.Printf("[Node#%d] New Node -> Connected to %s\n", n.Loc, c.RemoteAddr())
	m.Lock()
	nodes = append(nodes, n)
	m.Unlock()

	conn := p.NewConnection(c, nil, nil)
	n.SetOther(conn, p.NewAddress(addrparts[0], port), 255)
	n.SendOther(p.NewSplitEdgeMessage(localAddress(), n.Loc))
}

func localAddress() *p.Address {
	return p.NewAddress("127.0.0.1", *port)
}
