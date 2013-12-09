package protocol

import (
	"fmt"
	"math/rand"
	"net"
	"os"
	"strconv"
)

const (
	// message actions
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
	loc  Location
	addr *Address
	c    *Connection
}

func (r *remoteNode) String() string {
	if r == nil {
		return "None"
	}
	damn := ""
	if r.addr == nil {
		damn = "!"
	}
	return strconv.Itoa(int(r.loc)) + damn
}

func (r *remoteNode) isConn(c *Connection) bool {
	if r != nil && r.c == c {
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
}

func NewNode(addr *Address) *Node {
	return &Node{
		State: StateSplitting,
		Addr:  addr,
		Loc:   Location(rand.Intn(255)),
	}
}

func (n *Node) InitiateMergeEdge() {
	fmt.Printf("[Node#%d] Initiating Merge\n", n.Loc)
	n.SendPrev(NewMergeEdgeMessage(n.NextNode.addr, n.NextNode.loc))
	n.State = StateMerging
	fmt.Println(n)
}

func (n *Node) SendPrev(m *Message) {
	fmt.Printf("[Node#%d] Send to PREV: %s\n", n.Loc, m)
	n.PrevNode.c.SendMessage(m)
}

func (n *Node) SendNext(m *Message) {
	fmt.Printf("[Node#%d] Send to NEXT: %s\n", n.Loc, m)
	n.NextNode.c.SendMessage(m)
}

func (n *Node) SendOther(m *Message) {
	fmt.Printf("[Node#%d] Send to OTHER: %s\n", n.Loc, m)
	n.OtherNode.c.SendMessage(m)
}

func (n *Node) SetPrev(c *Connection, addr *Address, loc Location) {
	oldNode := n.PrevNode
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
	fmt.Printf("[Node#%d] PrevNode: %s -> %s\n", n.Loc, oldNode, n.PrevNode)
}

func (n *Node) SetNext(c *Connection, addr *Address, loc Location) {
	oldNode := n.NextNode
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
	fmt.Printf("[Node#%d] NextNode: %s -> %s\n", n.Loc, oldNode, n.NextNode)
}

func (n *Node) SetOther(c *Connection, addr *Address, loc Location) {
	oldNode := n.OtherNode
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
	fmt.Printf("[Node#%d] OtherNode: %s -> %s\n", n.Loc, oldNode, n.OtherNode)
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
	fmt.Println(n)
}

func (n *Node) MessageCallback(c *Connection, m *Message) {
	fmt.Println(n)
	fmt.Printf("[Node#%d] Rcv from%s: %s\n", n.Loc, n.identifyConnection(c), m)

	switch m.Action {
	case ActionSplitEdge:
		if n.State == StateFree {
			n.State = StateSplitting
			n.SetOther(c, m.Addr, m.Loc)
			n.SendOther(NewHelloCCWMessage(n.Addr, n.Loc, m.Loc))
			n.SendNext(NewRedirectMessage(m.Addr, m.Loc))
		} else {
			fmt.Printf("[Node#%d] Could not handle %s\n", m)
		}
	case ActionMergeEdge:
		if n.State == StateFree && n.NextNode.isConn(c) {
			c, err := connectTo(m.Addr)
			if err != nil {
				fmt.Fprintf(os.Stderr, "[Node#%d] !! Dial Error: %s\n", err)
				n.SendPrev(NewTryLaterMessage())
			} else {
				n.SendNext(NewShutdownMessage())
				n.SetNext(c, m.Addr, m.Loc)
				n.SendNext(NewHelloCWMessage(n.Addr, n.Loc, m.Loc))
			}
		} else {
			n.SendPrev(NewTryLaterMessage())
			fmt.Printf("[Node#%d] Could not handle %s\n", m)
		}
	case ActionRedirect:
		if n.PrevNode.isConn(c) {
			c, err := connectTo(m.Addr)
			if err != nil {
				fmt.Fprintf(os.Stderr, "[Node#%d] !! Dial Error: %s\n", err)
				n.SendPrev(NewCancelMessage())
			} else {
				n.SendPrev(NewShutdownMessage())
				n.SetPrev(c, m.Addr, m.Loc)
				n.SendPrev(NewHelloCCWMessage(n.Addr, n.Loc, m.Loc))
			}
		} else if n.NextNode.isConn(c) {
			fmt.Fprintf(os.Stderr, "[Node#%d] !! received from NEXT, should be PREV\n", m)
			n.SendNext(NewCancelMessage())
		} else {
			fmt.Fprintf(os.Stderr, "[Node#%d] !! received from OTHER, should be PREV\n", m)
			n.SendOther(NewCancelMessage())
		}
	case ActionHelloCW:
		if n.State == StateSplitting {
			n.SetNext(c, m.Addr, m.SrcLoc)
			n.State = StateFree
		} else {
			fmt.Printf("[Node#%d] Could not handle %s\n", m)
		}
	case ActionHelloCCW:
		if n.State == StateSplitting && n.OtherNode.isConn(c) {
			if n.PrevNode != nil {
				n.SendPrev(NewShutdownMessage())
			}
			n.SetPrev(c, m.Addr, m.SrcLoc)
			n.SetOther(nil, nil, 0)
		} else {
			fmt.Printf("[Node#%d] Could not handle %s\n", m)
		}
	case ActionTryLater:
	case ActionCancel:
		if n.State == StateSplitting && n.NextNode.isConn(c) {
			n.OtherNode.c.Close()
		} else {
			fmt.Printf("[Node#%d] Could not handle %s\n", m)
		}
	case ActionShutdown:
		if n.State == StateSplitting && n.NextNode.isConn(c) {
			n.NextNode, n.OtherNode = n.OtherNode, n.NextNode
			n.OtherNode.c.Close()
			n.State = StateFree
		} else if n.State == StateMerging && n.NextNode.isConn(c) {
			n.PrevNode.c.Close()
			n.NextNode.c.Close()
			n.SetPrev(nil, nil, 0)
			n.SetNext(nil, nil, 0)
			n.State = StateDone
		} else {
			fmt.Printf("[Node#%d] Could not handle %s\n", m)
		}
	case ActionMessage:
	}

	fmt.Println(n)
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
