package protocol

import (
	"strconv"
)

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
	ActionMessage = 8
)

const (
	// masks
	MaskZip    = (1 << 2) // not supported atm
	MaskAction = (31 << 3)
)

type Action int
type Location uint8

type Address struct {
	ip   string
	port int
}

func NewAddress(ip string, port int) *Address {
	if ip == "localhost" {
		ip = "127.0.0.1"
	}
	return &Address{ip, port}
}

func (a *Address) String() string {
	return a.ip + ":" + strconv.Itoa(a.port)
}

// Package type
type Message struct {
	Action  Action
	Addr    *Address
	Loc     Location
	SrcLoc  Location
	DstLoc  Location
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
	case ActionMessage:
		me = "Message"
	}
	switch m.Action {
	case ActionSplitEdge, ActionMergeEdge, ActionRedirect:
		me += "(Addr=" + m.Addr.String() + ", Loc=" + strconv.Itoa(int(m.Loc)) + ")"
	case ActionHelloCW, ActionHelloCCW:
		me += "(Addr=" + m.Addr.String() + ", srcLoc=" + strconv.Itoa(int(m.SrcLoc)) + ", dstLoc=" + strconv.Itoa(int(m.DstLoc)) + ")"
	case ActionMessage:
		me += "(content=" + string(m.Content) + ")"
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

func NewContentMessage(content []byte) *Message {
	return &Message{
		Action:  ActionTryLater,
		Content: content,
	}
}