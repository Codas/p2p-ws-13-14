package main

import (
	"fmt"
	"math/rand"
	"os"
	"sync"
	"time"
)

const RETRY_DELAY = 1 * time.Second

type CleanCallbackFunc func(*Node)
type NodeMessageCallbackFunc func(*Node, *Message)

const (
	// node states
	StateFree = iota
	StateSplitting
	StateMerging
	StateDone
)

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
	c    *Connection
	addr *Address
	loc  Location
}

func (rn *remoteNode) String() string {
	if rn == nil {
		return "None"
	} else if rn.loc == 255 {
		return "??"
	}
	return fmt.Sprintf("%d", rn.loc)
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
	m         *sync.Mutex

	cleanCB   CleanCallbackFunc
	messageCB NodeMessageCallbackFunc

	verbose bool
}

func NewNode(lAddr *Address, rAddr *Address, loc Location, verbose bool, clean CleanCallbackFunc, message NodeMessageCallbackFunc) *Node {
	n := &Node{
		State: StateDone,
		Addr:  lAddr,
		Loc:   loc,
		m:     new(sync.Mutex),

		cleanCB:   clean,
		messageCB: message,

		verbose: verbose,
	}

	return n.initiateSplitEdge(rAddr)
}

func NewCycleNode(lAddr *Address, loc Location, verbose bool, clean CleanCallbackFunc, message NodeMessageCallbackFunc) *Node {
	n := &Node{
		State: StateSplitting,
		Addr:  lAddr,
		Loc:   loc,
		m:     new(sync.Mutex),

		cleanCB:   clean,
		messageCB: message,

		verbose: verbose,
	}

	c, err := ConnectTo(lAddr, n.MessageCallback, n.closeCallback)
	if err != nil {
		fmt.Fprintf(os.Stderr, "NewCycleNode: Dial Error: %s\n", err)
		return nil
	}

	n.println(fmt.Sprintf("[Node#%d] NewCycleNode -> Connected to %s", n.Loc, c.Remote()))

	n.m.Lock()
	defer n.m.Unlock()
	n.setPrev(c, n.Addr, n.Loc)
	n.sendPrev(NewHelloCWMessage(n.Addr, n.Loc, n.Loc))

	return n
}

func (n *Node) initiateSplitEdge(rAddr *Address) *Node {
	n.m.Lock()
	defer n.m.Unlock()
	n.println("----------------------------------------")
	n.println(n)
	n.println(fmt.Sprintf("[Node#%d] Initiating SplitEdge", n.Loc))
	c, err := ConnectTo(rAddr, n.MessageCallback, n.closeCallback)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Dial Error: %s\n", err)
		return nil
	}

	n.println(fmt.Sprintf("[Node#%d] Connected to %s", n.Loc, c.Remote()))
	n.setState(StateSplitting)
	n.setOther(c, rAddr, 255)
	n.sendOther(NewSplitEdgeMessage(n.Addr, n.Loc))

	return n
}

func (n *Node) InitiateGraph() {
	n.m.Lock()
	defer n.m.Unlock()
	n.println("----------------------------------------")
	n.println(n)
	n.println(fmt.Sprintf("[Node#%d] Initiating Graph", n.Loc))
	content := unparseAddress(n.Addr)
	content = append(content, unparseLocation(n.Loc)...)
	n.sendNext(NewGraphMessage(n.Addr, n.Loc, content))
}

func (n *Node) InitiateBroadcast(text string) {
	n.m.Lock()
	defer n.m.Unlock()
	n.println("----------------------------------------")
	n.println(n)
	n.println(fmt.Sprintf("[Node#%d] Initiating Broadcast", n.Loc))
	n.sendNext(NewBroadcastMessage(n.Addr, n.Loc, []byte(text)))
}

func (n *Node) InitiateMergeEdge() {
	n.m.Lock()
	defer n.m.Unlock()
	n.println("----------------------------------------")
	n.println(n)
	n.println(fmt.Sprintf("[Node#%d] Initiating MergeEdge", n.Loc))
	n.sendPrev(NewMergeEdgeMessage(n.NextNode.addr, n.NextNode.loc))
	n.setState(StateMerging)
}

func (n *Node) SetVerbosity(verbose bool) {
	n.verbose = verbose
}

func (n *Node) sendPrev(m *Message) {
	n.println(fmt.Sprintf("[Node#%d] -> PREV: %s", n.Loc, m))
	n.PrevNode.c.SendMessage(m)
}

func (n *Node) sendNext(m *Message) {
	n.println(fmt.Sprintf("[Node#%d] -> NEXT: %s", n.Loc, m))
	n.NextNode.c.SendMessage(m)
}

func (n *Node) sendOther(m *Message) {
	n.println(fmt.Sprintf("[Node#%d] -> OTHER: %s", n.Loc, m))
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
	//n.println(fmt.Sprintf("[Node#%d] PrevNode: %s -> %s", n.Loc, oldNode, n.PrevNode))
	n.println(n)
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
	//n.println(fmt.Sprintf("[Node#%d] NextNode: %s -> %s", n.Loc, oldNode, n.NextNode))
	n.println(n)
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
	//n.println(fmt.Sprintf("[Node#%d] OtherNode: %s -> %s", n.Loc, oldNode, n.OtherNode))
	n.println(n)
}

func (n *Node) setState(s NodeState) {
	n.State = s
	n.println(n)
}

func (n *Node) closeCallback(c *Connection) {
	n.m.Lock()
	defer n.m.Unlock()
	n.println(fmt.Sprintf("[Node#%d]%s connection closed", n.Loc, n.identifyConnection(c)))
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
	n.println("----------------------------------------")
	n.println(n)
	n.println(fmt.Sprintf("[Node#%d] <-%s: %s", n.Loc, n.identifyConnection(c), m))

	switch m.Action {
	case ActionSplitEdge:
		if n.State == StateFree {
			n.setState(StateSplitting)
			n.setOther(c, m.Addr, m.Loc)
			n.sendOther(NewHelloCCWMessage(n.Addr, n.Loc, m.Loc))
			n.sendNext(NewRedirectMessage(m.Addr, m.Loc))
		} else {
			n.println(fmt.Sprintf("[Node#%d] Could not handle msg", n.Loc))
		}
	case ActionMergeEdge:
		if n.NextNode.isConn(c) {
			if *n.NextNode.addr == *n.Addr && n.NextNode.loc == n.Loc {
				n.PrevNode.c.Close()
				n.NextNode.c.Close()
				n.setState(StateDone)
				n.cleanCB(n)
			} else if n.State == StateFree {
				c, err := ConnectTo(m.Addr, n.MessageCallback, n.closeCallback)
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
			c, err := ConnectTo(m.Addr, n.MessageCallback, n.closeCallback)
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
			n.println(fmt.Sprintf("[Node#%d] Could not handle msg", n.Loc))
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
		n.println(fmt.Sprintf("[Node#%d] Retrying later...", n.Loc))
	case ActionCancel:
		if n.State == StateSplitting && n.NextNode.isConn(c) {
			n.OtherNode.c.Close()
		} else {
			n.println(fmt.Sprintf("[Node#%d] Could not handle msg", n.Loc))
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
			n.cleanCB(n)
		} else {
			n.println(fmt.Sprintf("[Node#%d] Could not handle msg", n.Loc))
		}
	case ActionBroadcast:
		if *m.Addr == *n.Addr && m.Loc == n.Loc {
			// we sent this broadcast, don't forward
		} else if n.NextNode.isConn(c) {
			n.sendPrev(m)
		} else if n.PrevNode.isConn(c) {
			n.sendNext(m)
		}
		n.messageCB(n, m)
	case ActionGraph:
		if *m.Addr == *n.Addr && m.Loc == n.Loc {
			// we sent this graph request, it's complete
			n.messageCB(n, m)
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
	case ActionFish:
		n.messageCB(n, m)
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

func (n *Node) println(a ...interface{}) {
	if n.verbose {
		fmt.Println(a...)
	}
}

func (n *Node) String() string {
	return fmt.Sprintf("[Node#%d] [%s] %s<->%s (%s)", n.Loc, n.State, n.PrevNode, n.NextNode, n.OtherNode)
}
