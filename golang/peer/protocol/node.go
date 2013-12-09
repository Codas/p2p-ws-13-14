package protocol

import (
	"fmt"
	"math/rand"
	"net"
	"os"
	"strconv"
	"time"
)

const (
	// message actions
	StateFree = iota
	StateSplitting
	StateMerging
	StateDone
)

var r = rand.New(rand.NewSource(time.Now().UnixNano()))

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

type Node struct {
	State     NodeState
	Addr      *Address
	Loc       Location
	OtherNode *remoteNode
	NextNode  *remoteNode
	PrevNode  *remoteNode

	cleanF func(*Node)
}

func NewNode(addr *Address, clean func(*Node)) *Node {
	return &Node{
		State:  StateSplitting,
		Addr:   addr,
		Loc:    Location(r.Intn(255)),
		cleanF: clean,
	}
}

func (n *Node) InitiateBroadcast() {
	fmt.Println("----------------------------------------")
	fmt.Println(n)
	fmt.Printf("[Node#%d] Initiating Broadcast\n", n.Loc)
	n.SendNext(NewBroadcastMessage(n.Addr, n.Loc, []byte("test")))
}

func (n *Node) InitiateMergeEdge() {
	fmt.Println("----------------------------------------")
	fmt.Println(n)
	fmt.Printf("[Node#%d] Initiating Merge\n", n.Loc)
	n.SendPrev(NewMergeEdgeMessage(n.NextNode.addr, n.NextNode.loc))
	n.SetState(StateMerging)
}

func (n *Node) SendPrev(m *Message) {
	fmt.Printf("[Node#%d] -> PREV: %s\n", n.Loc, m)
	n.PrevNode.c.SendMessage(m)
}

func (n *Node) SendNext(m *Message) {
	fmt.Printf("[Node#%d] -> NEXT: %s\n", n.Loc, m)
	n.NextNode.c.SendMessage(m)
}

func (n *Node) SendOther(m *Message) {
	fmt.Printf("[Node#%d] -> OTHER: %s\n", n.Loc, m)
	n.OtherNode.c.SendMessage(m)
}

func (n *Node) SetPrev(c *Connection, addr *Address, loc Location) {
	//oldNode := n.PrevNode
	if c != nil {
		n.PrevNode = &remoteNode{
			loc:  loc,
			addr: addr,
			c:    c,
		}
		c.SetCallbacks(n.MessageCallback, n.CloseCallback)
	} else {
		n.PrevNode = nil
	}
	//fmt.Printf("[Node#%d] PrevNode: %s -> %s\n", n.Loc, oldNode, n.PrevNode)
	fmt.Println(n)
}

func (n *Node) SetNext(c *Connection, addr *Address, loc Location) {
	//oldNode := n.NextNode
	if c != nil {
		n.NextNode = &remoteNode{
			loc:  loc,
			addr: addr,
			c:    c,
		}
		c.SetCallbacks(n.MessageCallback, n.CloseCallback)
	} else {
		n.NextNode = nil
	}
	//fmt.Printf("[Node#%d] NextNode: %s -> %s\n", n.Loc, oldNode, n.NextNode)
	fmt.Println(n)
}

func (n *Node) SetOther(c *Connection, addr *Address, loc Location) {
	//oldNode := n.OtherNode
	if c != nil {
		n.OtherNode = &remoteNode{
			loc:  loc,
			addr: addr,
			c:    c,
		}
		c.SetCallbacks(n.MessageCallback, n.CloseCallback)
	} else {
		n.OtherNode = nil
	}
	//fmt.Printf("[Node#%d] OtherNode: %s -> %s\n", n.Loc, oldNode, n.OtherNode)
	fmt.Println(n)
}

func (n *Node) SetState(s NodeState) {
	n.State = s
	fmt.Println(n)
}

func (n *Node) CloseCallback(c *Connection) {
	fmt.Printf("[Node#%d]%s connection closed\n", n.Loc, n.identifyConnection(c))
	if n.PrevNode.isConn(c) {
		n.SetPrev(nil, nil, 0)
	}
	if n.NextNode.isConn(c) {
		n.SetNext(nil, nil, 0)
	}
	if n.OtherNode.isConn(c) {
		n.SetOther(nil, nil, 0)
	}
}

func (n *Node) MessageCallback(c *Connection, m *Message) {
	fmt.Println("----------------------------------------")
	fmt.Println(n)
	fmt.Printf("[Node#%d] <-%s: %s\n", n.Loc, n.identifyConnection(c), m)

	switch m.Action {
	case ActionSplitEdge:
		if n.State == StateFree {
			n.SetState(StateSplitting)
			n.SetOther(c, m.Addr, m.Loc)
			n.SendOther(NewHelloCCWMessage(n.Addr, n.Loc, m.Loc))
			n.SendNext(NewRedirectMessage(m.Addr, m.Loc))
		} else {
			fmt.Printf("[Node#%d] Could not handle msg\n", n.Loc)
		}
	case ActionMergeEdge:
		if n.State == StateFree && n.NextNode.isConn(c) {
			c, err := connectTo(m.Addr)
			if err != nil {
				fmt.Fprintf(os.Stderr, "[Node#%d] !! Dial Error: %s\n", n.Loc, err)
				n.SendNext(NewTryLaterMessage())
			} else {
				n.SendNext(NewShutdownMessage())
				n.SetNext(c, m.Addr, m.Loc)
				n.SendNext(NewHelloCCWMessage(n.Addr, n.Loc, m.Loc))
			}
		} else {
			n.SendPrev(NewTryLaterMessage())
			fmt.Printf("[Node#%d] Could not handle %s\n", n.Loc, m)
		}
	case ActionRedirect:
		if n.PrevNode.isConn(c) {
			c, err := connectTo(m.Addr)
			if err != nil {
				fmt.Fprintf(os.Stderr, "[Node#%d] !! Dial Error: %s\n", n.Loc, err)
				n.SendPrev(NewCancelMessage())
			} else {
				n.SendPrev(NewShutdownMessage())
				n.SetPrev(c, m.Addr, m.Loc)
				n.SendPrev(NewHelloCWMessage(n.Addr, n.Loc, m.Loc))
			}
		} else if n.NextNode.isConn(c) {
			fmt.Fprintf(os.Stderr, "[Node#%d] !! from NEXT, should be PREV\n", n.Loc)
			n.SendNext(NewCancelMessage())
		} else {
			fmt.Fprintf(os.Stderr, "[Node#%d] !! from OTHER, should be PREV\n", n.Loc)
			n.SendOther(NewCancelMessage())
		}
	case ActionHelloCW:
		if n.State == StateSplitting {
			n.SetNext(c, m.Addr, m.SrcLoc)
			n.SetState(StateFree)
		} else {
			fmt.Printf("[Node#%d] Could not handle msg\n", n.Loc)
		}
	case ActionHelloCCW:
		if n.OtherNode.isConn(c) {
			n.SetOther(nil, nil, 0)
		}
		if n.PrevNode != nil {
			n.SendPrev(NewShutdownMessage())
		}
		n.SetPrev(c, m.Addr, m.SrcLoc)
	case ActionTryLater:
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
			n.SetState(StateFree)
		} else if n.State == StateMerging && n.NextNode.isConn(c) {
			n.PrevNode.c.Close()
			n.NextNode.c.Close()
			//n.SetPrev(nil, nil, 0)
			//n.SetNext(nil, nil, 0)
			n.SetState(StateDone)
			n.cleanF(n)
		} else {
			fmt.Printf("[Node#%d] Could not handle msg\n", n.Loc)
		}
	case ActionBroadcast:
		if *m.Addr == *n.Addr && m.Loc == n.Loc {
			fmt.Printf("[Node#%d] Don't forward, because we initially sent it\n", n.Loc)
		} else if n.NextNode.isConn(c) {
			n.SendPrev(m)
		} else if n.PrevNode.isConn(c) {
			n.SendNext(m)
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
