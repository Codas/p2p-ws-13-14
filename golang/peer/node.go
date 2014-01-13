package main

import (
	"fmt"
	"os"
	"sync"
	"time"
)

const RETRY_DELAY = 2 * time.Second
const RETRY_VARIANCE = 200 * time.Millisecond

type CleanCallbackFunc func(*Node)
type NodeMessageCallbackFunc func(*Node, *Message)

const (
	// node states
	StateFree = iota
	StateSplitting
	//StateSplittingPrev
	StateMerging
	StateDone
)

type NodeState int

// Node State signal correct forwarding of a message in any direction
func (n NodeState) Ready() bool {
	return n == StateFree
	switch n {
	case StateSplitting, StateDone:
		return false
	}
	return true
}

func (n NodeState) String() string {
	switch n {
	case StateFree:
		return "StateFree"
	case StateSplitting:
		return "StateSplitting"
	//case StateSplittingPrev:
	//	return "StateSplittingPrev"
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
	return fmt.Sprintf("%d:%d", rn.addr.Port, rn.loc)
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
	State         NodeState
	ShutdownState int
	InitState     int

	Addr      *Address
	Loc       Location
	OtherNode *remoteNode
	NextNode  *remoteNode
	PrevNode  *remoteNode
	m         *sync.Mutex

	cleanCB   CleanCallbackFunc
	messageCB NodeMessageCallbackFunc

	backlog string
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
		State:     StateSplitting,
		InitState: 1,

		Addr: lAddr,
		Loc:  loc,
		m:    new(sync.Mutex),

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
	if n.State != StateDone {
		return nil
	}
	n.println("----------------------------------------")
	n.println(n)
	n.println(fmt.Sprintf("[Node#%d] Initiating SplitEdge: %s", n.Loc, rAddr))
	c, err := ConnectTo(rAddr, n.MessageCallback, n.closeCallback)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Dial Error: %s\n", err)
		return nil
	}
	m := NewSplitEdgeMessage(n.Addr, n.Loc)
	c.SendMessage(m)
	n.println(fmt.Sprintf("[Node#%d] -> sending %s", n.Loc, m))
	//n.setState(StateSplitter)
	//n.setOther(c, rAddr, 255)
	//n.sendOther(NewSplitEdgeMessage(n.Addr, n.Loc))

	// reschedule splitedge if it didnt work
	go func(addr *Address) {
		time.Sleep(RETRY_DELAY + time.Duration(r.Int63n(int64(RETRY_VARIANCE))))
		n.initiateSplitEdge(addr)
	}(rAddr)

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
	if n.State == StateDone {
		// TODO: perhaps cleanup and return?
		return
	} else if n.State != StateFree {
		n.println(fmt.Sprintf("[Node#%d] Retrying MergeEdge later (state is %s)", n.Loc, n.State))
		go func() {
			time.Sleep(RETRY_DELAY + time.Duration(r.Int63n(int64(RETRY_VARIANCE))))
			n.InitiateMergeEdge()
		}()
		return
	}
	n.println("----------------------------------------")
	n.println(n)
	n.println(fmt.Sprintf("[Node#%d] Initiating MergeEdge", n.Loc))
	n.sendPrev(NewMergeEdgeMessage(n.NextNode.addr, n.NextNode.loc))
	n.setState(StateMerging)
}

func (n *Node) SetVerbosity(verbose bool) {
	n.verbose = verbose
}

func (n *Node) SendPrev(m *Message) {
	n.m.Lock()
	defer n.m.Unlock()
	n.sendPrev(m)
}

func (n *Node) SendNext(m *Message) {
	n.m.Lock()
	defer n.m.Unlock()
	n.sendNext(m)
}

func (n *Node) sendSpecial(to string, c *Connection, m *Message) {
	n.println(fmt.Sprintf("[Node#%d] -> %s: %s", n.Loc, to, m))
	if c == nil {
		fmt.Fprintf(os.Stderr, "[Node#%d] ERROR -> %s (is nil): %s\n", n.Loc, to, m)
		return
	}
	c.SendMessage(m)
}

func (n *Node) sendPrev(m *Message) {
	n.println(fmt.Sprintf("[Node#%d] -> PREV: %s", n.Loc, m))
	if n.PrevNode == nil || n.PrevNode.c == nil {
		fmt.Fprintf(os.Stderr, "[Node#%d] ERROR -> PREV (is nil): %s\n", n.Loc, m)
		return
	}
	n.PrevNode.c.SendMessage(m)
}

func (n *Node) sendNext(m *Message) {
	n.println(fmt.Sprintf("[Node#%d] -> NEXT: %s", n.Loc, m))
	if n.NextNode == nil || n.NextNode.c == nil {
		fmt.Fprintf(os.Stderr, "[Node#%d] ERROR -> PREV (is nil): %s\n", n.Loc, m)
		return
	}
	n.NextNode.c.SendMessage(m)
}

func (n *Node) sendOther(m *Message) {
	n.println(fmt.Sprintf("[Node#%d] -> OTHER: %s", n.Loc, m))
	if n.OtherNode == nil || n.OtherNode.c == nil {
		fmt.Fprintf(os.Stderr, "[Node#%d] ERROR -> PREV (is nil): %s\n", n.Loc, m)
		return
	}
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
	if n.PrevNode.isConn(c) {
		n.println(fmt.Sprintf("[Node#%d] PREV connection closed", n.Loc))
		n.setPrev(nil, nil, 0)
	}
	if n.NextNode.isConn(c) {
		n.println(fmt.Sprintf("[Node#%d] NEXT connection closed", n.Loc))
		n.setNext(nil, nil, 0)
	}
	if n.OtherNode.isConn(c) {
		n.println(fmt.Sprintf("[Node#%d] OTHER connection closed", n.Loc))
		n.setOther(nil, nil, 0)
	}
}

func (n *Node) MessageCallback(c *Connection, m *Message) {
	if m.Action == ActionRandomWalk && m.Hops <= 1 {
		m.Action = ActionSplitEdge
	}

	n.m.Lock()
	defer n.m.Unlock()
	n.println("----------------------------------------")
	n.println(n)
	n.println(fmt.Sprintf("[Node#%d] <-%s: %s", n.Loc, n.identifyConnection(c), m))

	switch m.Action {
	case ActionSplitEdge:
		if n.State == StateFree {
			if c, err := ConnectTo(m.Addr, n.MessageCallback, n.closeCallback); err == nil {
				// TODO: n.setState(StateSplittingPrev)
				n.setState(StateSplitting)
				n.setOther(c, m.Addr, m.Loc)
				n.sendOther(NewHelloCCWMessage(n.Addr, n.Loc, m.Loc))
				n.sendNext(NewRedirectMessage(m.Addr, m.Loc))
			} else {
				fmt.Fprintf(os.Stderr, "[Node#%d] !! Dial Error: %s\n", n.Loc, err)
			}
		} else {
			n.println(fmt.Sprintf("[Node#%d] Could not handle msg", n.Loc))
		}
	case ActionMergeEdge:
		if n.NextNode.isConn(c) {
			if *n.NextNode.addr == *n.Addr && n.NextNode.loc == n.Loc {
				// shortcut for a cycle node
				n.PrevNode.c.Close()
				n.NextNode.c.Close()
				n.setState(StateDone)
				n.cleanCB(n)
			} else if n.State == StateFree {
				if c, err := ConnectTo(m.Addr, n.MessageCallback, n.closeCallback); err == nil {
					old := n.NextNode.c
					n.setNext(c, m.Addr, m.Loc)
					n.sendNext(NewHelloCCWMessage(n.Addr, n.Loc, m.Loc))
					n.sendSpecial("OLD NEXT", old, NewShutdownMessage())
				} else {
					fmt.Fprintf(os.Stderr, "[Node#%d] !! Dial Error: %s\n", n.Loc, err)
					n.sendNext(NewTryLaterMessage())
				}
			} else {
				n.sendNext(NewTryLaterMessage())
			}
		} else {
			fmt.Fprintf(os.Stderr, "[Node#%d] !! not from NEXT\n", n.Loc)
			//fmt.Printf("[Node#%d] ------ could not handle (%s) (not from NEXT)\n", n.Loc, m)
			n.printBacklog()
		}
	case ActionRedirect:
		if n.PrevNode.isConn(c) {
			c, err := ConnectTo(m.Addr, n.MessageCallback, n.closeCallback)
			if err != nil {
				fmt.Fprintf(os.Stderr, "[Node#%d] !! Dial Error: %s\n", n.Loc, err)
				n.sendPrev(NewCancelMessage())
			} else {
				old := n.PrevNode.c

				n.setPrev(c, m.Addr, m.Loc)
				n.sendPrev(NewHelloCWMessage(n.Addr, n.Loc, m.Loc))
				n.sendSpecial("OLD PREV", old, NewShutdownMessage())
			}
		} else {
			fmt.Fprintf(os.Stderr, "[Node#%d] !! not from PREV\n", n.Loc)
			//fmt.Printf("[Node#%d] ------ could not handle (%s) (not from PREV)\n", n.Loc, m)
			n.printBacklog()
			if n.NextNode.isConn(c) {
				n.sendNext(NewCancelMessage())
			} else {
				n.sendOther(NewCancelMessage())
			}
		}
	case ActionHelloCW:
		// accept always?
		if n.State == StateDone {
			n.setNext(c, m.Addr, m.SrcLoc)
			n.setState(StateSplitting)
			n.InitState = 2
		} else if n.State == StateSplitting && n.InitState == 1 {
			n.setNext(c, m.Addr, m.SrcLoc)
			n.setState(StateFree)
			n.InitState = 0
		} else {
			n.println(fmt.Sprintf("[Node#%d] Could not handle msg", n.Loc))
			//fmt.Printf("[Node#%d] ------ could not handle (%s)\n", n.Loc, m)
			n.printBacklog()
		}
	case ActionHelloCCW:
		if n.State == StateSplitting && n.InitState == 2 {
			n.setState(tateFree)
			n.InitState = 0
		} else if n.State == StateDone {
			n.setState(StateSplitting)
			n.InitState = 1
		}
		if n.OtherNode.isConn(c) {
			n.setOther(nil, nil, 0)
		}
		if n.PrevNode != nil {
			n.sendPrev(NewShutdownMessage())
		}
		n.setPrev(c, m.Addr, m.SrcLoc)
	case ActionTryLater:
		n.setState(StateFree)
		go func() {
			time.Sleep(RETRY_DELAY + time.Duration(r.Int63n(int64(RETRY_VARIANCE))))
			n.InitiateMergeEdge()
		}()
		n.println(fmt.Sprintf("[Node#%d] Retrying later...", n.Loc))
	case ActionCancel:
		// TODO: if n.State == StateSplittingPrev && n.NextNode.isConn(c) {
		if n.State == StateSplitting && n.NextNode.isConn(c) {
			n.OtherNode.c.Close()
			n.setState(StateFree)
		} else {
			n.println(fmt.Sprintf("[Node#%d] Could not handle msg", n.Loc))
			//fmt.Printf("[Node#%d] ------ could not handle (%s)\n", n.Loc, m)
			n.printBacklog()
		}
	case ActionShutdown:
		// TODO: if n.State == StateSplittingPrev && n.NextNode.isConn(c) {
		if n.State == StateSplitting && n.NextNode.isConn(c) {
			n.NextNode, n.OtherNode = n.OtherNode, n.NextNode
			n.setState(StateFree)
			n.OtherNode.c.Close()
		} else if n.State == StateMerging && n.PrevNode.isConn(c) {
			if n.ShutdownState == 2 {
				n.PrevNode.c.Close()
				n.NextNode.c.Close()
				n.setState(StateDone)
				n.cleanCB(n)
				n.ShutdownState = 0
			} else {
				n.ShutdownState = 1
			}
		} else if n.State == StateMerging && n.NextNode.isConn(c) {
			if n.ShutdownState == 1 {
				n.PrevNode.c.Close()
				n.NextNode.c.Close()
				n.setState(StateDone)
				n.cleanCB(n)
				n.ShutdownState = 0
			} else {
				n.ShutdownState = 2
			}
		} else {
			n.println(fmt.Sprintf("[Node#%d] Could not handle msg", n.Loc))
			//fmt.Printf("[Node#%d] ------ could not handle (%s)\n", n.Loc, m)
			n.printBacklog()
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
			gm := NewGraphMessage(m.Addr, m.Loc, content)
			if n.NextNode.isConn(c) {
				n.sendPrev(gm)
			} else if n.PrevNode.isConn(c) {
				n.sendNext(gm)
			}
		}
	case ActionFish, ActionRandomWalk:
		n.messageCB(n, m)
	default:
		//fmt.Printf("[Node#%d] ------ could not handle (%s)\n", n.Loc, m)
		n.printBacklog()
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
	n.backlog += fmt.Sprintln(a...)
	if n.verbose {
		fmt.Println(a...)
	}
}

func (n *Node) printBacklog() {
	fmt.Println("[ # # # # # # # # # # # # # # # # # # # # # # # # # # ]")
	fmt.Print(n.backlog)
}

func (n *Node) String() string {
	if n.State == StateFree {
		n.backlog = ""
	}
	return fmt.Sprintf("[Node#%d] [%s] %s<->%s (%s)", n.Loc, n.State, n.PrevNode, n.NextNode, n.OtherNode)
}
