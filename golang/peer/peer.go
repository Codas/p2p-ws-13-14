package main

import (
	"bytes"
	"fmt"
	"math"
	"math/rand"
	"net"
	"os"
	"sync"
	"time"
)

const HYSTERESIS = 0.5
const FISH_INTERVAL = 1000 * time.Millisecond

var r = rand.New(rand.NewSource(time.Now().UnixNano()))

type GraphCallbackFunc func([]*NodeAttr)

type Peer struct {
	l    net.Listener
	addr *Address

	nodes []*Node
	m     *sync.RWMutex

	graphCB GraphCallbackFunc

	fishticker  *time.Ticker
	networksize float32
	water, fish float32
	strength    uint32

	shutdown bool
	done     chan bool

	verbosity int
}

func NewPeer(port, verbosity int, graph GraphCallbackFunc) *Peer {
	l, err := net.Listen("tcp", fmt.Sprintf(":%d", port))
	if err != nil {
		fmt.Fprintf(os.Stderr, "Listening Error: %s\n", err)
		return nil
	}
	p := &Peer{
		l:    l,
		addr: NewAddress(LocalIP, port),
		m:    new(sync.RWMutex),

		networksize: 1,
		water:       1,
		fish:        1,
		strength:    r.Uint32(),

		done:    make(chan bool),
		graphCB: graph,

		verbosity: verbosity,
	}
	p.println(fmt.Sprintf("[Global] Started listening on %v", l.Addr()))

	go p.acceptLoop()
	go p.fishLoop()

	return p
}

// if addr = nil, we add a cycle node
func (p *Peer) AddNode(addr *Address) {
	var n *Node
	if addr == nil {
		n = NewCycleNode(p.addr, p.uniqueLocation(), p.verbosity > 1, p.removeNode, p.messageCB)
	} else {
		n = NewNode(p.addr, addr, p.uniqueLocation(), p.verbosity > 1, p.removeNode, p.messageCB)
	}
	if n == nil {
		return
	}

	p.m.Lock()
	p.nodes = append(p.nodes, n)
	p.m.Unlock()
}

func (p *Peer) DisconnectNode(l Location) bool {
	p.m.RLock()
	defer p.m.RUnlock()

	// disconnect random node
	if l == 255 {
		p.nodes[r.Intn(len(p.nodes))].InitiateMergeEdge()
		return true
	}

	for _, n := range p.nodes {
		if n.Loc == l {
			n.InitiateMergeEdge()
			return true
		}
	}
	return false
}

func (p *Peer) DisconnectAllNodes() {
	p.m.RLock()
	defer p.m.RUnlock()

	for _, n := range p.nodes {
		n.InitiateMergeEdge()
	}
}

func (p *Peer) ListNodes() {
	p.m.RLock()
	defer p.m.RUnlock()

	if len(p.nodes) == 0 {
		fmt.Println("[Global] No nodes currently active")
		return
	}

	for _, n := range p.nodes {
		fmt.Printf(" - %s\n", n)
	}
}

func (p *Peer) Broadcast(text string) {
	p.m.RLock()
	defer p.m.RUnlock()

	if len(p.nodes) == 0 {
		return
	}

	p.nodes[0].InitiateBroadcast(text)
}

func (p *Peer) GenerateGraph() {
	p.m.RLock()
	defer p.m.RUnlock()

	if len(p.nodes) == 0 {
		return
	}

	p.nodes[0].InitiateGraph()
}

func (p *Peer) Shutdown() {
	defer p.l.Close()

	p.m.Lock()
	if len(p.nodes) == 0 {
		return
	}
	p.shutdown = true
	p.m.Unlock()
	p.DisconnectAllNodes()
	<-p.done
}

func (p *Peer) SetVerbosityLevel(level int) {
	p.m.RLock()
	defer p.m.RUnlock()

	p.verbosity = level
	for _, n := range p.nodes {
		n.SetVerbosity(level > 1)
	}
}

func (p *Peer) messageCB(n *Node, m *Message) {
	switch m.Action {
	case ActionBroadcast:
		p.println(fmt.Sprintf("[Global] [BROADCAST] %s", string(m.Content)))
	case ActionGraph:
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
		p.graphCB(graph)
	case ActionFish:
		p.water += m.Water
		if p.strength < m.Strength {
			p.strength = m.Strength
			p.fish = m.Fish
		} else if p.strength == m.Strength {
			p.fish += m.Fish
		} else {
			// discard arriving fish
		}
		// apply HYSTERESIS to new estimated networksize
		p.networksize = p.networksize*(1-HYSTERESIS) + p.water/p.fish*HYSTERESIS
		p.println(fmt.Sprintf("[Global] [FISH] new n=%.2f [w=%.3f/f=%.3f]", p.networksize, p.water, p.fish))
	case ActionRandomWalk:
		m.Hops -= 1

		// looking for a node that is free
		var freenodes []*Node
		for _, n := range p.nodes {
			if n.State == StateFree {
				freenodes = append(freenodes, n)
			}
		}

		if len(freenodes) == 0 {
			p.println(fmt.Sprintf("[Global] No Free Node: %s", m))
			return
		}

		node := freenodes[r.Intn(len(freenodes))]
		if r.Intn(2) == 0 {
			if node == n {
				node.sendPrev(m)
			} else {
				node.SendPrev(m)
			}
		} else {
			if node == n {
				node.sendNext(m)
			} else {
				node.SendNext(m)
			}
		}

	}
}

func (p *Peer) fishLoop() {
	// TODO: perhaps adjust timer -> race to equilibrium?
	p.fishticker = time.NewTicker(FISH_INTERVAL)
	for _ = range p.fishticker.C {
		p.m.RLock()
		// calculate degree (collect all real neighbour addresses)
		var addresses []*Address
		for _, n := range p.nodes {
			if n.State == StateFree {
				addresses = p.addUniqueAddress(addresses, n.PrevNode.addr)
				addresses = p.addUniqueAddress(addresses, n.NextNode.addr)
			}
		}
		if len(addresses) != 0 {
			waterpart := p.water / float32(len(addresses)+1)
			fishpart := p.fish / float32(len(addresses)+1)
			p.water -= waterpart
			p.fish -= fishpart

			// TODO: to whom do we want to send it? (only real neighbours? or possibly also to a different nodes of this very peer?)
			// atm send only to different peers
			address := addresses[r.Intn(len(addresses))]
			for _, n := range p.nodes {
				if n.State == StateFree {
					if n.PrevNode.addr == address {
						n.sendPrev(NewFishMessage(waterpart, fishpart, p.strength))
						break
					} else if n.NextNode.addr == address {
						n.sendNext(NewFishMessage(waterpart, fishpart, p.strength))
						break
					}
				}
			}
		}
		p.m.RUnlock()
	}
}

func (p *Peer) addUniqueAddress(addresses []*Address, address *Address) []*Address {
	if *address == *p.addr {
		return addresses
	}
	for _, a := range addresses {
		if *a == *address {
			return addresses
		}
	}
	return append(addresses, address)
}

func (p *Peer) acceptLoop() {
	for {
		c, err := p.l.Accept()
		if err != nil {
			fmt.Fprintf(os.Stderr, "Accept Error: %s\n", err)
			return
		}
		p.println(fmt.Sprintf("[Global] New Connection from %v", c.RemoteAddr()))

		NewConnection(c, p.handleIncomingConnection, nil)
	}
}

func (p *Peer) handleIncomingConnection(c *Connection, msg *Message) {
	p.m.RLock()
	defer p.m.RUnlock()

	var n *Node
	switch msg.Action {
	case ActionHelloCW, ActionHelloCCW:
		for _, _n := range p.nodes {
			if _n.Loc == msg.DstLoc {
				n = _n
				break
			}
		}
	case ActionSplitEdge:
		c.Close()
		// create with extra hop
		hops := Hops(3*math.Log(float64(p.networksize))) + 1
		if hops > 1 {
			rwm := NewRandomWalkMessage(msg.Addr, msg.Loc, hops)
			// handle as if it came from some random node
			p.messageCB(nil, rwm)
			return
		}

		// looking for a node that is free
		var freenodes []*Node
		for _, n := range p.nodes {
			if n.State == StateFree {
				freenodes = append(freenodes, n)
			}
		}

		if len(freenodes) == 0 {
			p.println(fmt.Sprintf("[Global] from %v, No Free Node: Closing Conn: %s", c.Remote(), msg))
			c.Close()
			return
		}

		n = freenodes[r.Intn(len(freenodes))]
	default:
		fmt.Fprintf(os.Stderr, "[Global] invalid from %v: %s\n", c.Remote(), msg)
		return
	}

	if n == nil {
		fmt.Fprintf(os.Stderr, "[Global] from %v -> Loc#%d not found: %s\n", c.Remote(), msg.DstLoc, msg)
		return
	}

	p.println(fmt.Sprintf("[Global] from %v -> Loc#%d: %s", c.Remote(), n.Loc, msg))

	n.MessageCallback(c, msg)
}

func (p *Peer) uniqueLocation() Location {
	p.m.RLock()
	defer p.m.RUnlock()

	if len(p.nodes) >= 255 {
		fmt.Fprintln(os.Stderr, "WELL THAT ARE A FUCKTON OF NODES!")
		return Location(r.Intn(255))
	}
outer:
	for {
		l := Location(r.Intn(255))
		for _, n := range p.nodes {
			if l == n.Loc {
				continue outer
			}
		}
		return l
	}

}

func (p *Peer) removeNode(n *Node) {
	p.m.Lock()
	defer p.m.Unlock()
	if len(p.nodes) == 0 {
		return
	}
	for i, _n := range p.nodes {
		if _n == n {
			p.nodes[i] = p.nodes[len(p.nodes)-1]
			p.nodes = p.nodes[:len(p.nodes)-1]
		}
	}

	if p.shutdown && len(p.nodes) == 0 {
		p.done <- true
	}
}

func (p *Peer) println(a ...interface{}) {
	if p.verbosity > 0 {
		fmt.Println(a...)
	}
}
