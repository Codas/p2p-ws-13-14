package protocol

const (
	// usual message flags
	FlagJoin = iota
	FlagPart
	FlagTopicAsk
	FlagTopicReceive
	FlagMessage
	FlagBinary
	FlagBroadCast
	// admin message flags
	FlagClose       = 16
	FlagDeleteTopic = 17
	FlagKickUser    = 18
	FlagStatistics  = 20
)
const (
	// masks
	MaskZip    = (1 << 2)
	MaskAction = (31 << 3)
)

type Flags int

// Package type
type Packet struct {
	Flags   Flags
	Topics  []string
	Message []byte
}

func (p *Packet) hasTopics() bool {
	return msgHasTopics(p.Flags)
}

func (p *Packet) hasMessage() bool {
	return msgHasMessage(p.Flags)
}

func msgHasTopics(f Flags) bool {
	switch f {
	case FlagJoin, FlagPart, FlagTopicReceive, FlagMessage, FlagBinary, FlagDeleteTopic, FlagKickUser:
		return true
	}
	return false
}

func msgHasMessage(f Flags) bool {
	switch f {
	case FlagMessage, FlagBinary, FlagBroadCast:
		return true
	}
	return false
}
