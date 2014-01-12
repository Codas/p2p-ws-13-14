package main

import (
	"fmt"
	"math/rand"
	"net"
	"os"
	"sync"
	"time"
)

const HYSTERESIS = 0.5
const FISH_INTERVAL = 1000 * time.Millisecond

var r = rand.New(rand.NewSource(time.Now().UnixNano()))

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
}

func NewPeer(port int, graph GraphCallbackFunc) *Peer {
	l, err := net.Listen("tcp", fmt.Sprintf(":%d", port))
	if err != nil {
		fmt.Fprintf(os.Stderr, "Listening Error: %s\n", err)
		return nil
	}
	fmt.Printf("[Global] Started listening on %v\n", l.Addr())

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
	}

	go p.acceptLoop()

	return p
}

// if addr = nil, we add a cycle node
func (p *Peer) AddNode(addr *Address) {
	var n *Node
	if addr == nil {
		n = NewCycleNode(p.addr, p.uniqueLocation(), p.removeNode, p.graphCB, p.fishCB)
	} else {
		n = NewNode(p.addr, addr, p.uniqueLocation(), p.removeNode, p.graphCB, p.fishCB)
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

// TODO: perhaps adjust timer
func (p *Peer) fishLoop() {
	p.fishticker = time.NewTicker(FISH_INTERVAL)
	for _ = range p.fishticker.C {
		p.m.RLock()
		// calculate degree (number of real neighbors)
		var addresses []*Address
		for _, n := range p.nodes {
			addresses = p.addUniqueAddress(addresses, n.PrevNode.addr)
			addresses = p.addUniqueAddress(addresses, n.NextNode.addr)
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
				if n.PrevNode.addr == address {
					n.sendPrev(NewFishMessage(waterpart, fishpart, p.strength))
					break
				} else if n.PrevNode.addr == address {
					n.sendNext(NewFishMessage(waterpart, fishpart, p.strength))
					break
				}
			}
		}
		p.m.RUnlock()
	}
}

func (p *Peer) addUniqueAddress(addresses []*Address, address *Address) []*Address {
	if address == p.addr {
		return addresses
	}
	for _, a := range addresses {
		if a == address {
			return addresses
		}
	}
	return append(addresses, address)
}

func (p *Peer) fishCB(water, fish float32, strength uint32) {
	p.water += water
	if p.strength < strength {
		p.strength = strength
		p.fish = fish
	} else if p.strength == strength {
		p.fish += fish
	} else {
		// discard arriving fish
	}
	// apply HYSTERESIS to new estimated networksize
	p.networksize = p.networksize*(1-HYSTERESIS) + p.water/p.fish*HYSTERESIS
	fmt.Printf("[Global] New estimated Networksize: %.2f\n", p.networksize)
}

func (p *Peer) acceptLoop() {
	for {
		c, err := p.l.Accept()
		if err != nil {
			fmt.Fprintf(os.Stderr, "Accept Error: %s\n", err)
			return
		}
		fmt.Printf("[Global] New Connection from %v\n", c.RemoteAddr())

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
		// looking for a node that is free
		var freenodes []*Node
		for _, n := range p.nodes {
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
