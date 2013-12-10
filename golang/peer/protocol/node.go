package protocol

import (
	"bytes"
	"fmt"
	"math/rand"
	"net"
	"os"
	"strconv"
	"sync"
	"time"
)

const (
	// message actions
	StateFree = iota
	StateSplitting
	StateMerging
	StateDone
)

const RETRY_DELAY = 1 * time.Second

type NodeState int

func (n NodeState) String() string {
	switch n {
	case StateFree:
		return "StateFree"
	case StateSplitting:
		return "StateSplitting"
	case StateMerging:
		return "StateMerging"
	case StateDone:
		return "StateDone"
	}
	return "StateUnknown"
}

type remoteNode struct {
	loc  Location
	addr *Address
	c    *Connection
}

func (rn *remoteNode) String() string {
	if rn == nil {
		return "None"
	} else if rn.loc == 255 {
		return "??"
	}
	return strconv.Itoa(int(rn.loc))
}

func (rn *remoteNode) isConn(c *Connection) bool {
	if rn != nil && rn.c == c {
		return true
	}
	return false
}

type NodeAttr struct {
	Addr *Address
	Loc  Location
}

type Node struct {
	State     NodeState
	Addr      *Address
	Loc       Location
	OtherNode *remoteNode
	NextNode  *remoteNode
	PrevNode  *remoteNode

	cleanF func(*Node)
	graphF func([]*NodeAttr)
	m      *sync.Mutex
}

func NewNode(lAddr *Address, rAddr *Address, loc Location, clean func(*Node), graph func(g []*NodeAttr)) *Node {
	n := &Node{
		State:  StateDone,
		Addr:   lAddr,
		Loc:    loc,
		cleanF: clean,
		graphF: graph,
		m:      new(sync.Mutex),
	}

	return n.initiateSplitEdge(rAddr)
}

func NewCycleNode(lAddr *Address, loc Location, clean func(*Node), graph func(g []*NodeAttr)) *Node {
	c, err := connectTo(lAddr)
	if err != nil {
		fmt.Fprintf(os.Stderr, "NewCycleNode: Dial Error: %s\n", err)
		return nil
	}

	n := &Node{
		State:  StateSplitting,
		Addr:   lAddr,
		Loc:    loc,
		cleanF: clean,
		graphF: graph,
		m:      new(sync.Mutex),
	}
	fmt.Printf("[Node#%d] NewCycleNode -> Connected to %s\n", n.Loc, c.Remote())

	n.m.Lock()
	defer n.m.Unlock()
	n.setPrev(c, n.Addr, n.Loc)
	n.sendPrev(NewHelloCWMessage(n.Addr, n.Loc, n.Loc))

	return n
}

func (n *Node) initiateSplitEdge(rAddr *Address) *Node {
	n.m.Lock()
	defer n.m.Unlock()
	fmt.Println("----------------------------------------")
	fmt.Println(n)
	fmt.Printf("[Node#%d] Initiating SplitEdge\n", n.Loc)
	c, err := connectTo(rAddr)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Dial Error: %s\n", err)
		return nil
	}

	fmt.Printf("[Node#%d] Connected to %s\n", n.Loc, c.Remote())
	n.setState(StateSplitting)
	n.setOther(c, rAddr, 255)
	n.sendOther(NewSplitEdgeMessage(n.Addr, n.Loc))

	return n
}

func (n *Node) InitiateGraph() {
	n.m.Lock()
	defer n.m.Unlock()
	fmt.Println("----------------------------------------")
	fmt.Println(n)
	fmt.Printf("[Node#%d] Initiating Graph\n", n.Loc)
	content := unparseAddress(n.Addr)
	content = append(content, unparseLocation(n.Loc)...)
	n.sendNext(NewGraphMessage(n.Addr, n.Loc, content))
}

func (n *Node) InitiateBroadcast() {
	n.m.Lock()
	defer n.m.Unlock()
	fmt.Println("----------------------------------------")
	fmt.Println(n)
	fmt.Printf("[Node#%d] Initiating Broadcast\n", n.Loc)
	n.sendNext(NewBroadcastMessage(n.Addr, n.Loc, []byte("test")))
}

func (n *Node) InitiateMergeEdge() {
	n.m.Lock()
	defer n.m.Unlock()
	fmt.Println("----------------------------------------")
	fmt.Println(n)
	fmt.Printf("[Node#%d] Initiating MergeEdge\n", n.Loc)
	if n.NextNode == nil {
		fmt.Printf("[Node#%d] !!!!!!!!!!!!!!!!!!!! NEXTNODE IS NIL !!!!!!!!!!!!!\n", n.Loc)
		return
	}
	n.sendPrev(NewMergeEdgeMessage(n.NextNode.addr, n.NextNode.loc))
	n.setState(StateMerging)
}

func (n *Node) sendPrev(m *Message) {
	fmt.Printf("[Node#%d] -> PREV: %s\n", n.Loc, m)
	n.PrevNode.c.SendMessage(m)
}

func (n *Node) sendNext(m *Message) {
	fmt.Printf("[Node#%d] -> NEXT: %s\n", n.Loc, m)
	n.NextNode.c.SendMessage(m)
}

func (n *Node) sendOther(m *Message) {
	fmt.Printf("[Node#%d] -> OTHER: %s\n", n.Loc, m)
	n.OtherNode.c.SendMessage(m)
}

func (n *Node) setPrev(c *Connection, addr *Address, loc Location) {
	//oldNode := n.PrevNode
	if c != nil {
		n.PrevNode = &remoteNode{
			loc:  loc,
			addr: addr,
			c:    c,
		}
		c.SetCallbacks(n.MessageCallback, n.closeCallback)
	} else {
		n.PrevNode = nil
	}
	//fmt.Printf("[Node#%d] PrevNode: %s -> %s\n", n.Loc, oldNode, n.PrevNode)
	fmt.Println(n)
}

func (n *Node) setNext(c *Connection, addr *Address, loc Location) {
	//oldNode := n.NextNode
	if c != nil {
		n.NextNode = &remoteNode{
			loc:  loc,
			addr: addr,
			c:    c,
		}
		c.SetCallbacks(n.MessageCallback, n.closeCallback)
	} else {
		n.NextNode = nil
	}
	//fmt.Printf("[Node#%d] NextNode: %s -> %s\n", n.Loc, oldNode, n.NextNode)
	fmt.Println(n)
}

func (n *Node) setOther(c *Connection, addr *Address, loc Location) {
	//oldNode := n.OtherNode
	if c != nil {
		n.OtherNode = &remoteNode{
			loc:  loc,
			addr: addr,
			c:    c,
		}
		c.SetCallbacks(n.MessageCallback, n.closeCallback)
	} else {
		n.OtherNode = nil
	}
	//fmt.Printf("[Node#%d] OtherNode: %s -> %s\n", n.Loc, oldNode, n.OtherNode)
	fmt.Println(n)
}

func (n *Node) setState(s NodeState) {
	n.State = s
	fmt.Println(n)
}

func (n *Node) closeCallback(c *Connection) {
	n.m.Lock()
	defer n.m.Unlock()
	fmt.Printf("[Node#%d]%s connection closed\n", n.Loc, n.identifyConnection(c))
	if n.PrevNode.isConn(c) {
		n.setPrev(nil, nil, 0)
	}
	if n.NextNode.isConn(c) {
		n.setNext(nil, nil, 0)
	}
	if n.OtherNode.isConn(c) {
		if n.State == StateSplitting {
			go func(addr *Address) {
				time.Sleep(RETRY_DELAY/2 + time.Duration(rand.Intn(int(RETRY_DELAY/2))))
				n.initiateSplitEdge(addr)
			}(n.OtherNode.addr)
		}
		n.setOther(nil, nil, 0)
	}
}

func (n *Node) MessageCallback(c *Connection, m *Message) {
	n.m.Lock()
	defer n.m.Unlock()
	fmt.Println("----------------------------------------")
	fmt.Println(n)
	fmt.Printf("[Node#%d] <-%s: %s\n", n.Loc, n.identifyConnection(c), m)

	switch m.Action {
	case ActionSplitEdge:
		if n.State == StateFree {
			n.setState(StateSplitting)
			n.setOther(c, m.Addr, m.Loc)
			n.sendOther(NewHelloCCWMessage(n.Addr, n.Loc, m.Loc))
			n.sendNext(NewRedirectMessage(m.Addr, m.Loc))
		} else {
			fmt.Printf("[Node#%d] Could not handle msg\n", n.Loc)
		}
	case ActionMergeEdge:
		if n.NextNode.isConn(c) {
			if *n.NextNode.addr == *n.Addr && n.NextNode.loc == n.Loc {
				n.PrevNode.c.Close()
				n.NextNode.c.Close()
				n.setState(StateDone)
				n.cleanF(n)
			} else if n.State == StateFree {
				c, err := connectTo(m.Addr)
				if err != nil {
					fmt.Fprintf(os.Stderr, "[Node#%d] !! Dial Error: %s\n", n.Loc, err)
					n.sendNext(NewTryLaterMessage())
				} else {
					n.sendNext(NewShutdownMessage())
					n.setNext(c, m.Addr, m.Loc)
					n.sendNext(NewHelloCCWMessage(n.Addr, n.Loc, m.Loc))
				}
			} else {
				n.sendNext(NewTryLaterMessage())
			}
		} else {
			fmt.Fprintf(os.Stderr, "[Node#%d] !! not from NEXT\n", n.Loc)
		}
	case ActionRedirect:
		if n.PrevNode.isConn(c) {
			c, err := connectTo(m.Addr)
			if err != nil {
				fmt.Fprintf(os.Stderr, "[Node#%d] !! Dial Error: %s\n", n.Loc, err)
				n.sendPrev(NewCancelMessage())
			} else {
				n.sendPrev(NewShutdownMessage())
				n.setPrev(c, m.Addr, m.Loc)
				n.sendPrev(NewHelloCWMessage(n.Addr, n.Loc, m.Loc))
			}
		} else if n.NextNode.isConn(c) {
			fmt.Fprintf(os.Stderr, "[Node#%d] !! from NEXT, should be PREV\n", n.Loc)
			n.sendNext(NewCancelMessage())
		} else {
			fmt.Fprintf(os.Stderr, "[Node#%d] !! from OTHER, should be PREV\n", n.Loc)
			n.sendOther(NewCancelMessage())
		}
	case ActionHelloCW:
		if n.State == StateSplitting {
			n.setNext(c, m.Addr, m.SrcLoc)
			n.setState(StateFree)
		} else {
			fmt.Printf("[Node#%d] Could not handle msg\n", n.Loc)
		}
	case ActionHelloCCW:
		if n.OtherNode.isConn(c) {
			n.setOther(nil, nil, 0)
		}
		if n.PrevNode != nil {
			n.sendPrev(NewShutdownMessage())
		}
		n.setPrev(c, m.Addr, m.SrcLoc)
		/* WATT
		if n.State == StateMerging {
			n.setState(StateFree)
		}
		*/
	case ActionTryLater:
		n.setState(StateFree)
		go func() {
			time.Sleep(RETRY_DELAY/2 + time.Duration(rand.Intn(int(RETRY_DELAY/2))))
			n.InitiateMergeEdge()
		}()
		fmt.Printf("[Node#%d] Retrying later...\n", n.Loc)
	case ActionCancel:
		if n.State == StateSplitting && n.NextNode.isConn(c) {
			n.OtherNode.c.Close()
		} else {
			fmt.Printf("[Node#%d] Could not handle msg\n", n.Loc)
		}
	case ActionShutdown:
		if n.State == StateSplitting && n.NextNode.isConn(c) {
			n.NextNode, n.OtherNode = n.OtherNode, n.NextNode
			n.OtherNode.c.Close()
			n.setState(StateFree)
		} else if n.State == StateMerging && n.NextNode.isConn(c) {
			n.PrevNode.c.Close()
			n.NextNode.c.Close()
			n.setState(StateDone)
			n.cleanF(n)
		} else {
			fmt.Printf("[Node#%d] Could not handle msg\n", n.Loc)
		}
	case ActionBroadcast:
		if *m.Addr == *n.Addr && m.Loc == n.Loc {
			fmt.Printf("[Node#%d] We sent this broadcast, don't forward\n", n.Loc)
		} else if n.NextNode.isConn(c) {
			n.sendPrev(m)
		} else if n.PrevNode.isConn(c) {
			n.sendNext(m)
		}
	case ActionGraph:
		if *m.Addr == *n.Addr && m.Loc == n.Loc {
			fmt.Printf("[Node#%d] We sent this broadcast, don't forward\n", n.Loc)
			if len(m.Content)%7 != 0 {
				fmt.Printf("[Node#%d] Size of Graph mod 7 should be 0 (len=%d)\n", n.Loc, len(m.Content))
			} else {
				var graph []*NodeAttr
				b := bytes.NewReader(m.Content)
				for {
					addr, err := parseAddress(b)
					if err != nil {
						break
					}
					loc, err := parseLocation(b)
					if err != nil {
						break
					}
					graph = append(graph, &NodeAttr{addr, loc})
				}
				n.graphF(graph)
			}
		} else {
			content := m.Content
			content = append(content, unparseAddress(n.Addr)...)
			content = append(content, unparseLocation(n.Loc)...)
			if n.NextNode.isConn(c) {
				n.sendPrev(NewGraphMessage(m.Addr, m.Loc, content))
			} else if n.PrevNode.isConn(c) {
				n.sendNext(NewGraphMessage(m.Addr, m.Loc, content))
			}
		}
	}
}

func (n *Node) identifyConnection(c *Connection) string {
	var from string
	if n.PrevNode.isConn(c) {
		from += " PREV"
	}
	if n.NextNode.isConn(c) {
		from += " NEXT"
	}
	if n.OtherNode.isConn(c) {
		from += " OTHER"
	}
	if from == "" {
		from = " NEW(" + c.c.RemoteAddr().String() + ")"
	}
	return from
}

func connectTo(addr *Address) (*Connection, error) {
	c, err := net.Dial("tcp", addr.ip+":"+strconv.Itoa(addr.port))
	if err != nil {
		return nil, err
	}
	return NewConnection(c, nil, nil), nil
}

func (n *Node) String() string {
	return fmt.Sprintf("[Node#%d] [%s] %s<->%s (%s)", n.Loc, n.State, n.PrevNode, n.NextNode, n.OtherNode)
}
