package main

import "strconv"

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
	ActionFishMessage
	ActionJoin
	ActionRandomWalk
)

const (
	// masks
	MaskZip    = (1 << 2) // not supported atm
	MaskAction = (31 << 3)
)

type Action int
type Location uint8
type FishCarry float32
type WaterCarry float32
type Hops uint8

// Package type
type Message struct {
	Action  Action
	Addr    *Address
	Loc     Location
	SrcLoc  Location
	DstLoc  Location
	Fish    FishCarry
	Water   WaterCarry
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
	case ActionFishMessage:
		me = "Fish"
	case ActionJoin:
		me = "Join"
	case ActionRandomWalk:
		me = "RandomWalk"
	}

	switch m.Action {
	case ActionSplitEdge, ActionMergeEdge, ActionRedirect:
		me += "(" + m.Addr.String() + ", " + strconv.Itoa(int(m.Loc)) + ")"
	case ActionHelloCW, ActionHelloCCW:
		me += "(" + m.Addr.String() + ", src=" + strconv.Itoa(int(m.SrcLoc)) + ", dst=" + strconv.Itoa(int(m.DstLoc)) + ")"
	case ActionBroadcast:
		me += "(" + m.Addr.String() + ", " + strconv.Itoa(int(m.Loc)) + ", " + string(m.Content) + ")"
	case ActionGraph:
		me += "(" + m.Addr.String() + ", " + strconv.Itoa(int(m.Loc)) + ", len=" + strconv.Itoa(len(m.Content)) + ")"
	case ActionFishMessage:
		me += "(" + strconv.Itoa(len(m.Content)) + ", " + strconv.Itoa(int(m.Fish)) + ", " + strconv.Itoa(int(m.Water)) + ")"

	case ActionJoin:
		me += "(" + m.Addr.String() + ")"
	case ActionRandomWalk:
		me += "(" + m.Addr.String() + ", " + strconv.Itoa(len(m.Content)) + ")"
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

func NewFishMessage(addr *Address, fish FishCarry, water WaterCarry) *Message {
	return &Message{
		Action: ActionFishMessage,
		Fish:   fish,
		Water:  water,
	}
}

func NewJoinMessage(addr *Address) *Message {
	return &Message{
		Action: ActionJoin,
	}
}

func NewRandomWalkMessage(addr *Address, hops Hops) *Message {
	return &Message{
		Action: ActionRandomWalk,
		Hops:   hops,
	}
}
