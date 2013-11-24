package protocol_test

import (
	"bytes"
	"testing"

	. "../protocol"
)

var loremIpsum = `Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet.`

func TestEncoding(t *testing.T) {
	cMsg, compressed, err := CompressMessage([]byte(loremIpsum))
	if err != nil {
		t.Error("compressing failed: ", err)
	}
	if !compressed {
		t.Error("compression failed")
	}
	if dMsg, err := DecompressMessage(cMsg); err != nil {
		t.Error("decompressing failed: ", err)
	} else if loremIpsum != string(dMsg) {
		t.Error("decompress(compress(data)) results in different data")
	}
}

func TestLengthEncoding(t *testing.T) {
	numbers := []uint64{0, 10, 23682, 20, 217843682, 234, 1 << 5, 1 << 13, 1 << 21, 1 << 29, 1 << 37, 1 << 45, 1 << 53}
	var b bytes.Buffer
	for _, n := range numbers {
		if err := EncodeLength(&b, n); err != nil {
			t.Errorf("encoding number failed: (%s) %d", err, n)
		}
		l, err := DecodeLength(&b)
		if err != nil {
			t.Errorf("decoding number failed: (%s) %d", err, n)
		}
		if n != l {
			t.Errorf("encode <-> decode length mismatch: %d != %d", n, l)
		}
	}
}

type message struct {
	topics []string
	text   string
}

var testCases = []message{
	{[]string{"food", "drink"}, loremIpsum},
	{[]string{}, loremIpsum},
	{[]string{"crazy channel with whitespaces"}, loremIpsum},
	{[]string{"testTopic"}, "little text"},
	{[]string{"testTopic", "perhaps even unicode character like π, ϖ, Π"}, "little text"},
	{[]string{"", "this was a channel with an empty name"}, ""},
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
		if err := c.Write(FlagJoin, m.topics, ""); err != nil {
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
		if err := c.Write(FlagPart, m.topics, ""); err != nil {
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
		if err := c.Write(FlagBroadCast, nil, m.text); err != nil {
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
		if err := c.Write(FlagMessage, m.topics, m.text); err != nil {
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

func TestWriteAskTopic(t *testing.T) {
	cb := &closeBuffer{}
	c := NewConnection(cb)
	if err := c.Write(FlagTopicAsk, nil, ""); err != nil {
		t.Errorf("writing join topics failed: (%s)", err)
	}
	p, err := c.ReadPacket()
	if err != nil {
		t.Errorf("reading join topics failed: (%s)", err)
	}
	if p.Flags != FlagTopicAsk {
		t.Errorf("flag not correctly set: %d != %d", p.Flags, FlagTopicAsk)
	}
}
