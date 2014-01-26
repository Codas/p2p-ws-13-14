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
const FISH_INTERVAL = 250 * time.Millisecond

var r = rand.New(rand.NewSource(time.Now().UnixNano()))

type GraphCallbackFunc func([]*NodeAttr)
type SearchCallbackFunc func(string)

type Peer struct {
	l    net.Listener
	addr *Address

	nodes []*Node
	m     *sync.RWMutex

	content [][]byte

	graphCB  GraphCallbackFunc
	searchCB SearchCallbackFunc

	fishticker  *time.Ticker
	networksize float32
	t           float32
	water, fish float32
	wd1, wd2    float32
	strength    uint32
	degree      int

	shutdown bool
	done     chan bool

	verbosity int
}

func NewPeer(port, verbosity int, graph GraphCallbackFunc, search SearchCallbackFunc) *Peer {
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
		t:           0,
		water:       1,
		wd1:         0,
		wd2:         0,
		fish:        1,
		strength:    r.Uint32(),

		done:     make(chan bool),
		graphCB:  graph,
		searchCB: search,

		verbosity: verbosity,
	}
	p.println(1, fmt.Sprintf("[Global] Started listening on %v", l.Addr()))

	go p.acceptLoop()
	go p.fishLoop()

	return p
}

// if addr = nil, we add a cycle node
func (p *Peer) AddNode(addr *Address) {
	var n *Node
	if addr == nil {
		n = NewCycleNode(p.addr, p.uniqueLocation(), p.verbosity >= 3, p.removeNode, p.messageCB, p.degreeCB)
	} else {
		n = NewNode(p.addr, addr, p.uniqueLocation(), p.verbosity >= 3, p.removeNode, p.messageCB, p.degreeCB)
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

func (p *Peer) CheckNodes() {
	p.m.RLock()
	defer p.m.RUnlock()

	if len(p.nodes) == 0 {
		fmt.Println("[Global] No nodes currently active")
		return
	}

	for _, n := range p.nodes {
		if n.PrevNode == nil || n.PrevNode.c == nil {
			fmt.Println(fmt.Sprintf("[Global] NODE PREV IS NIL: %s", n))
		}
		if n.NextNode == nil || n.NextNode.c == nil {
			fmt.Println(fmt.Sprintf("[Global] NODE NEXT IS NIL: %s", n))
		}
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

func (p *Peer) StoreContent(content string) {
	// TODO: use better calculation
	hops := Hops(12)
	p.messageCB(nil, nil, NewCastStoreMessage(hops, []byte(content)))
}

func (p *Peer) SearchContent(content string) {
	// TODO: use better calculation
	hops := Hops(12)
	p.messageCB(nil, nil, NewCastSearchMessage(p.addr, hops, []byte(content)))
}

func (p *Peer) Shutdown() {
	defer p.l.Close()

	p.m.Lock()
	if len(p.nodes) == 0 {
		p.m.Unlock()
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
		n.SetVerbosity(level >= 3)
	}
}

func (p *Peer) messageCB(n *Node, nremote *remoteNode, m *Message) {
	switch m.Action {
	case ActionBroadcast:
		p.println(1, fmt.Sprintf("[Global] [BROADCAST] %s", string(m.Content)))
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
		p.wd1 += m.WD1
		p.wd2 += m.WD2
		if p.strength < m.Strength {
			p.strength = m.Strength
			p.fish = m.Fish
		} else if p.strength == m.Strength {
			p.fish += m.Fish
		} else {
			// discard arriving fish
		}
		d1 := p.wd1 / p.fish
		d2 := p.wd2 / p.fish
		// apply HYSTERESIS to new estimated networksize
		p.networksize = p.networksize*(1-HYSTERESIS) + p.water/p.fish*HYSTERESIS
		p.t = p.t*(1-HYSTERESIS) + (d1*d1)/(d2-2*d1)*HYSTERESIS
		p.println(2, fmt.Sprintf("[Global] [FISH] new n=%.2f, t=%.2f [w=%.3f/d1=%.3f/d2=%.3f/f=%.3f]", p.networksize, p.t, p.water, d1, d2, p.fish))
	case ActionRandomWalk:
		m.Hops -= 1

		// looking for a node that is free
		var freenodes []*Node
		p.m.RLock()
		for _, n := range p.nodes {
			if n.State.Ready() {
				freenodes = append(freenodes, n)
			}
		}
		p.m.RUnlock()

		if len(freenodes) == 0 {
			p.println(3, fmt.Sprintf("[Global] ActionRandomWalk: No Free Node: %s", m))
			return
		}

		node := freenodes[r.Intn(len(freenodes))]
		if r.Intn(2) == 0 {
			go node.SendPrev(m)
		} else {
			go node.SendNext(m)
		}
	case ActionCastStore:
		// store content here
		p.m.Lock()
		p.content = append(p.content, m.Content)
		p.m.Unlock()
		p.println(1, fmt.Sprintf("[Global] Storing Content: %s", string(m.Content)))

		if m.Hops < 2 {
			return
		}

		// get remote peers
		var filterAddr *Address
		if nremote != nil {
			// we didnt start this store request, filter neighbour where this came from
			filterAddr = nremote.addr
		}
		addresses := p.remotePeers(filterAddr)
		if len(addresses) == 0 {
			println(fmt.Sprintf("[Global] Can't resend CastStore (no remote peers) %s", string(m.Content)))
			return
		}

		hops := m.Hops - 1
		if len(addresses) == 1 {
			// send to 1 random neighbour
			address := addresses[r.Intn(len(addresses))]
			p.sendToAddress(*address, NewCastStoreMessage(hops, m.Content))
		} else {
			// send to 2 random neighbours
			hop := []Hops{hops / 2, hops - hops/2}
			perm := r.Perm(len(addresses))
			for i := range hop {
				if hop[i] > 0 {
					p.sendToAddress(*addresses[perm[i]], NewCastStoreMessage(hop[i], m.Content))
				}
			}
		}
	case ActionCastSearch:
		// lookup content here
		p.m.RLock()
		var response [][]byte
		for _, c := range p.content {
			if bytes.Contains(bytes.ToLower(c), bytes.ToLower(m.Content)) {
				response = append(response, c)
			}
		}
		if len(response) > 0 {
			// reply
			var reply []byte
			for _, c := range response {
				reply = append(reply, c...)
				reply = append(reply, ',', ' ')
			}
			replymsg := NewCastReplyMessage(reply[:len(reply)-2])
			if !p.sendToAddress(*m.Addr, replymsg) {
				// not already connected to searching peer, connect
				if c, err := ConnectTo(m.Addr, p.handleIncomingConnection, nil); err == nil {
					c.SendMessage(replymsg)
					c.Close()
				} else {
					fmt.Fprintf(os.Stderr, "[Global] !! Dial Error for ActionCastReply: %s\n", err)
				}
			}
		}
		p.m.RUnlock()
		p.println(1, fmt.Sprintf("[Global] Looking up content: %s", string(m.Content)))

		if m.Hops < 2 {
			return
		}

		// get remote peers
		var filterAddr *Address
		if nremote != nil {
			// we didnt start this search request, filter neighbour where this came from
			filterAddr = nremote.addr
		}
		addresses := p.remotePeers(filterAddr)
		if len(addresses) == 0 {
			println(fmt.Sprintf("[Global] Can't resend CastSearch (no remote peers) %s", string(m.Content)))
			return
		}

		hops := m.Hops - 1
		if len(addresses) == 1 {
			// send to 1 random neighbour
			address := addresses[r.Intn(len(addresses))]
			p.sendToAddress(*address, NewCastSearchMessage(m.Addr, hops, m.Content))
		} else {
			// send to 2 random neighbours
			hop := []Hops{hops / 2, hops - hops/2}
			perm := r.Perm(len(addresses))
			for i := range hop {
				if hop[i] > 0 {
					p.sendToAddress(*addresses[perm[i]], NewCastSearchMessage(m.Addr, hop[i], m.Content))
				}
			}
		}
	case ActionCastReply:
		p.searchCB(string(m.Content))
	}
}

func (p *Peer) sendToAddress(address Address, m *Message) bool {
	p.m.RLock()
	defer p.m.RUnlock()
	for _, n := range p.nodes {
		if !n.State.Ready() {
			continue
		}
		if *n.PrevNode.addr == address {
			go n.SendPrev(m)
			return true
		} else if *n.NextNode.addr == address {
			go n.SendNext(m)
			return true
		}
	}
	return false
}

func (p *Peer) degreeCB() {
	var oldDegree = p.degree
	p.degree = len(p.remotePeers(nil))
	var dDiff = p.degree - oldDegree
	if dDiff > 0 {
		p.println(2, fmt.Sprintf("[Global] [Degree] Degree changed by +%d", dDiff))
		p.wd1 += float32(dDiff)
		p.wd2 += float32(p.degree*p.degree - oldDegree*oldDegree)
	}
}

func (p *Peer) remotePeers(filter *Address) []*Address {
	p.m.RLock()
	defer p.m.RUnlock()
	// calculate degree (collect all real neighbour addresses)
	var addresses []*Address
	for _, n := range p.nodes {
		if n.State.Ready() {
			addresses = p.addUniqueAddress(addresses, n.PrevNode.addr, filter)
			addresses = p.addUniqueAddress(addresses, n.NextNode.addr, filter)
		}
	}
	return addresses
}

func (p *Peer) fishLoop() {
	// TODO: perhaps adjust timer -> race to equilibrium?
	p.fishticker = time.NewTicker(FISH_INTERVAL)
	for _ = range p.fishticker.C {
		var addresses = p.remotePeers(nil)
		if p.degree != 0 {
			waterpart := p.water / float32(p.degree+1)
			wd1part := p.wd1 / float32(p.degree+1)
			wd2part := p.wd2 / float32(p.degree+1)
			fishpart := p.fish / float32(p.degree+1)
			p.water -= waterpart
			p.wd1 -= wd1part
			p.wd2 -= wd2part
			p.fish -= fishpart

			// TODO: to whom do we want to send it? (only real neighbours? or possibly
			// also to a different nodes of this very peer?)
			// atm send only to different peers
			address := addresses[r.Intn(len(addresses))]
			p.sendToAddress(*address, NewFishMessage(waterpart, wd1part, wd2part, fishpart, p.strength))
		}
	}
}

func (p *Peer) addUniqueAddress(addresses []*Address, address, filter *Address) []*Address {
	if *address == *p.addr || *address == *filter {
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
		p.println(3, fmt.Sprintf("[Global] New Connection from %v", c.RemoteAddr()))

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
		//hops := Hops(6)
		hops := Hops(3*math.Log(float64(p.networksize))) + 1
		if hops > 1 {
			rwm := NewRandomWalkMessage(msg.Addr, msg.Loc, hops)
			// handle as if it came from some random node
			p.messageCB(nil, nil, rwm)
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
			p.println(3, fmt.Sprintf("[Global] from %v, No Free Node: Closing Conn: %s", c.Remote(), msg))
			return
		}

		n = freenodes[r.Intn(len(freenodes))]
	case ActionCastReply:
		c.Close()
		p.messageCB(nil, nil, msg)
		return
	default:
		fmt.Fprintf(os.Stderr, "[Global] invalid from %v: %s\n", c.Remote(), msg)
		//c.Close()
		return
	}

	if n == nil {
		fmt.Fprintf(os.Stderr, "[Global] from %v -> Loc#%d not found: %s\n", c.Remote(), msg.DstLoc, msg)
		//c.Close()
		return
	}

	p.println(3, fmt.Sprintf("[Global] from %v -> Loc#%d: %s", c.Remote(), n.Loc, msg))

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

func (p *Peer) println(verbosity int, a ...interface{}) {
	if p.verbosity >= verbosity {
		fmt.Println(a...)
	}
}
