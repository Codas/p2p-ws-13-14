package main

import "fmt"

const (
	// message actions
	ActionSplitEdge = iota
	ActionMergeEdge
	ActionRedirect
	ActionHelloCW
	ActionHelloCCW
	ActionTryLater
	ActionCancel
	ActionShutdown
	ActionBroadcast
	ActionGraph
	ActionFish
	ActionRandomWalk
)

const (
	// masks
	MaskZip    = (1 << 2) // not supported atm
	MaskAction = (31 << 3)
)

type Action int
type Location uint8
type Hops uint8

// Package type
type Message struct {
	Action Action

	Addr   *Address
	Loc    Location
	SrcLoc Location
	DstLoc Location

	Water    float32
	D1       float32
	D2       float32
	Fish     float32
	Strength uint32

	Hops    Hops
	Content []byte
}

func (m *Message) String() string {
	var me string
	switch m.Action {
	case ActionSplitEdge:
		me = "SplitEdge"
	case ActionMergeEdge:
		me = "MergeEdge"
	case ActionRedirect:
		me = "Redirect"
	case ActionHelloCW:
		me = "HelloCW"
	case ActionHelloCCW:
		me = "HelloCCW"
	case ActionCancel:
		me = "Cancel"
	case ActionTryLater:
		me = "TryLater"
	case ActionShutdown:
		me = "Shutdown"
	case ActionBroadcast:
		me = "Broadcast"
	case ActionGraph:
		me = "Graph"
	case ActionFish:
		me = "Fish"
	case ActionRandomWalk:
		me = "RandomWalk"
	}

	switch m.Action {
	case ActionSplitEdge, ActionMergeEdge, ActionRedirect:
		me += fmt.Sprintf("(%s, %d)", m.Addr, m.Loc)
	case ActionHelloCW, ActionHelloCCW:
		me += fmt.Sprintf("(%s, src=%d, dst=%d)", m.Addr, m.SrcLoc, m.DstLoc)
	case ActionBroadcast:
		me += fmt.Sprintf("(%s, %d, %s)", m.Addr, m.Loc, string(m.Content))
	case ActionGraph:
		me += fmt.Sprintf("(%s, %d, len=%d)", m.Addr, m.Loc, len(m.Content))
	case ActionFish:
		me += fmt.Sprintf("(water=%.2f, d1=%.2f, d2=%.2f, fish=%.2f, str=%d)", m.Water, m.D1, m.D2, m.Fish, m.Strength)
	case ActionRandomWalk:
		me += fmt.Sprintf("(%s, %d, hops=%d)", m.Addr, m.Loc, m.Hops)
	}
	return me
}

func NewSplitEdgeMessage(addr *Address, loc Location) *Message {
	return &Message{
		Action: ActionSplitEdge,
		Addr:   addr,
		Loc:    loc,
	}
}

func NewMergeEdgeMessage(addr *Address, loc Location) *Message {
	return &Message{
		Action: ActionMergeEdge,
		Addr:   addr,
		Loc:    loc,
	}
}

func NewRedirectMessage(addr *Address, loc Location) *Message {
	return &Message{
		Action: ActionRedirect,
		Addr:   addr,
		Loc:    loc,
	}
}

func NewHelloCWMessage(addr *Address, srcLoc Location, dstLoc Location) *Message {
	return &Message{
		Action: ActionHelloCW,
		Addr:   addr,
		SrcLoc: srcLoc,
		DstLoc: dstLoc,
	}
}

func NewHelloCCWMessage(addr *Address, srcLoc Location, dstLoc Location) *Message {
	return &Message{
		Action: ActionHelloCCW,
		Addr:   addr,
		SrcLoc: srcLoc,
		DstLoc: dstLoc,
	}
}

func NewCancelMessage() *Message {
	return &Message{
		Action: ActionCancel,
	}
}

func NewTryLaterMessage() *Message {
	return &Message{
		Action: ActionTryLater,
	}
}

func NewShutdownMessage() *Message {
	return &Message{
		Action: ActionShutdown,
	}
}

func NewBroadcastMessage(addr *Address, loc Location, content []byte) *Message {
	return &Message{
		Action:  ActionBroadcast,
		Addr:    addr,
		Loc:     loc,
		Content: content,
	}
}

func NewGraphMessage(addr *Address, loc Location, content []byte) *Message {
	return &Message{
		Action:  ActionGraph,
		Addr:    addr,
		Loc:     loc,
		Content: content,
	}
}

func NewFishMessage(water float32, d1 float32, d2 float32, fish float32, strength uint32) *Message {
	return &Message{
		Action:   ActionFish,
		Water:    water,
		D1:       water,
		D2:       water,
		Fish:     fish,
		Strength: strength,
	}
}

func NewRandomWalkMessage(addr *Address, loc Location, hops Hops) *Message {
	return &Message{
		Action: ActionRandomWalk,
		Addr:   addr,
		Loc:    loc,
		Hops:   hops,
	}
}
