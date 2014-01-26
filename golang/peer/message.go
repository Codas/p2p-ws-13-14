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
	ActionCastStore
	ActionCastSearch
	ActionCastReply
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
	WD1      float32
	WD2      float32
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
	case ActionCastStore:
		me = "CastStore"
	case ActionCastSearch:
		me = "CastSearch"
	case ActionCastReply:
		me = "CastReply"
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
		me += fmt.Sprintf("(water=%.2f, wd1=%.2f, wd2=%.2f, fish=%.2f, str=%d)", m.Water, m.WD1, m.WD2, m.Fish, m.Strength)
	case ActionRandomWalk:
		me += fmt.Sprintf("(%s, %d, hops=%d)", m.Addr, m.Loc, m.Hops)
	case ActionCastStore:
		me += fmt.Sprintf("(hops=%d, %s)", m.Hops, string(m.Content))
	case ActionCastSearch:
		me += fmt.Sprintf("(%s, hops=%d, %s)", m.Addr, m.Hops, string(m.Content))
	case ActionCastReply:
		me += fmt.Sprintf("(%s)", string(m.Content))
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

func NewFishMessage(water float32, wd1 float32, wd2 float32, fish float32, strength uint32) *Message {
	return &Message{
		Action:   ActionFish,
		Water:    water,
		WD1:      wd1,
		WD2:      wd2,
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

func NewCastStore(hops Hops, content []byte) *Message {
	return &Message{
		Action:  ActionCastStore,
		Hops:    hops,
		Content: content,
	}
}

func NewCastSearch(addr *Address, hops Hops, content []byte) *Message {
	return &Message{
		Action:  ActionCastSearch,
		Addr:    addr,
		Hops:    hops,
		Content: content,
	}
}
func NewCastReply(content []byte) *Message {
	return &Message{
		Action:  ActionCastReply,
		Content: content,
	}
}
