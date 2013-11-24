package protocol_test

import (
	"bytes"
	"testing"

	. "../protocol"
)

var loremIpsum = `Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet.`

func TestEncoding(t *testing.T) {
	cMsg, compressed := CompressMessage([]byte(loremIpsum))
	if !compressed {
		t.Error("compression failed")
	}
	if loremIpsum != string(DecompressMessage(cMsg)) {
		t.Error("decompress(compress(data)) results in different data")
	}
}

type message struct {
	topics []string
	text   string
}

var testCases = []message{
	{[]string{"food", "drink"}, loremIpsum},
}

type closeBuffer struct {
	bytes.Buffer
}

func (cb *closeBuffer) Close() error {
	return nil
}

func testTopicsIdentical(m message, p *Packet, t *testing.T) {
	if len(m.topics) != len(p.Topics) {
		t.Errorf("number of sent and received topics mismatch: %d != %d", len(m.topics), len(p.Topics))
	}
	for i := 0; i < len(m.topics); i++ {
		if m.topics[i] != p.Topics[i] {
			t.Errorf("sent and received topics mismatched: %s != %s", m.topics[i], p.Topics[i])
		}
	}
}

func TestJoinTopics(t *testing.T) {
	cb := &closeBuffer{}
	c := NewConnection(cb)
	for _, m := range testCases {
		if err := c.WriteJoinTopics(m.topics); err != nil {
			t.Errorf("writing join topics failed: (%s) %#v", err, m)
		}
		p, err := c.ReadPacket()
		if err != nil {
			t.Errorf("reading join topics failed: (%s) %#v", err, m)
		}
		if p.Flags != FlagJoin {
			t.Errorf("flag not correctly set: %d != %d", p.Flags, FlagJoin)
		}
		testTopicsIdentical(m, p, t)
	}
}

func TestPartTopics(t *testing.T) {
	cb := &closeBuffer{}
	c := NewConnection(cb)
	for _, m := range testCases {
		if err := c.WritePartTopics(m.topics); err != nil {
			t.Errorf("writing join topics failed: (%s) %#v", err, m)
		}
		p, err := c.ReadPacket()
		if err != nil {
			t.Errorf("reading join topics failed: (%s) %#v", err, m)
		}
		if p.Flags != FlagPart {
			t.Errorf("flag not correctly set: %d != %d", p.Flags, FlagPart)
		}
		testTopicsIdentical(m, p, t)
	}
}

func TestBroadcast(t *testing.T) {
	cb := &closeBuffer{}
	c := NewConnection(cb)
	for _, m := range testCases {
		if err := c.WriteBroadCast(m.text); err != nil {
			t.Errorf("writing join topics failed: (%s) %#v", err, m)
		}
		p, err := c.ReadPacket()
		if err != nil {
			t.Errorf("reading join topics failed: (%s) %#v", err, m)
		}
		if p.Flags != FlagBroadCast {
			t.Errorf("flag not correctly set: %d != %d", p.Flags, FlagBroadCast)
		}
		if m.text != string(p.Message) {
			t.Errorf("message not correctly transmitted: %s != %s", m.text, string(p.Message))
		}
	}
}

func TestMessage(t *testing.T) {
	cb := &closeBuffer{}
	c := NewConnection(cb)
	for _, m := range testCases {
		if err := c.WriteMessage(m.topics, m.text); err != nil {
			t.Errorf("writing join topics failed: (%s) %#v", err, m)
		}
		p, err := c.ReadPacket()
		if err != nil {
			t.Errorf("reading join topics failed: (%s) %#v", err, m)
		}
		if p.Flags != FlagMessage {
			t.Errorf("flag not correctly set: %d != %d", p.Flags, FlagMessage)
		}
		testTopicsIdentical(m, p, t)
		if m.text != string(p.Message) {
			t.Errorf("message not correctly transmitted: %s != %s", m.text, string(p.Message))
		}
	}
}
