package main

import (
	"fmt"
	"net"
	"os"
	"sync"
)

var nodes []*Node
var m = new(sync.RWMutex)

func accepter(l net.Listener) {
	for {
		c, err := l.Accept()
		if err != nil {
			fmt.Fprintf(os.Stderr, "Accept Error: %s\n", err)
			return
		}
		fmt.Printf("[Global] New Connection from %v\n", c.RemoteAddr())

		_ = NewConnection(c, forwardConnection, nil)

	}
}

func forwardConnection(c *Connection, msg *Message) {
	m.RLock()
	defer m.RUnlock()

	var n *Node
	switch msg.Action {
	case ActionHelloCW, ActionHelloCCW:
		for _, _n := range nodes {
			if _n.Loc == msg.DstLoc {
				n = _n
				break
			}
		}
	case ActionSplitEdge:
		// looking for a node that is free
		var freenodes []*Node
		for _, n := range nodes {
			if n.State == StateFree {
				freenodes = append(freenodes, n)
			}
		}

		if len(freenodes) == 0 {
			fmt.Printf("[Global] from %v, No Free Node: Closing Conn: %s\n", c.Remote(), msg)
			c.Close()
			return
		}

		n = freenodes[r.Intn(len(freenodes))]
	default:
		fmt.Printf("[Global] invalid from %v: %s\n", c.Remote(), msg)
		return
	}

	if n == nil {
		fmt.Printf("[Global] from %v -> Loc#%d not found: %s\n", c.Remote(), msg.DstLoc, msg)
		return
	}

	fmt.Printf("[Global] from %v -> Loc#%d: %s\n", c.Remote(), n.Loc, msg)

	n.MessageCallback(c, msg)
}

func createCycleNode() {
	n := NewCycleNode(localAddress(), uniqueLocation(), removeNode, graphCallback)
	if n == nil {
		return
	}

	m.Lock()
	nodes = append(nodes, n)
	m.Unlock()
}

func disconnectAllNodes() {
	m.RLock()
	defer m.RUnlock()

	for _, n := range nodes {
		n.InitiateMergeEdge()
	}
}

func uniqueLocation() Location {
	m.RLock()
	defer m.RUnlock()
	length := len(nodes)

	if length >= 255 {
		fmt.Fprintln(os.Stderr, "WELL THAT ARE A FUCKTON OF NODES!")
		return Location(r.Intn(255))
	}
outer:
	for {
		l := Location(r.Intn(255))
		for _, n := range nodes {
			if l == n.Loc {
				continue outer
			}
		}
		return l
	}

}

func localAddress() *Address {
	return NewAddress("127.0.0.1", *port)
}

func removeNode(n *Node) {
	m.Lock()
	defer m.Unlock()
	if len(nodes) == 0 {
		return
	}
	for i, _n := range nodes {
		if _n == n {
			nodes[i] = nodes[len(nodes)-1]
			nodes = nodes[:len(nodes)-1]
		}
	}

	if shuttingDown && len(nodes) == 0 {
		done <- true
	}
}
