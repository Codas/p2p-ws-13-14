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
type Package struct {
	Flags   Flags
	Topics  []string
	Message []byte
}

func (p *Package) hasTopics() bool {
	switch p.Flags {
	case FlagJoin, FlagPart, FlagTopicReceive, FlagMessage, FlagBinary, FlagDeleteTopic, FlagKickUser:
		return true
	}
	return false
}

func (p *Package) hasMessage() bool {
	switch p.Flags {
	case FlagMessage, FlagBinary, FlagBroadCast:
		return true
	}
	return false
}
