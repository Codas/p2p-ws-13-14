package protocol

import (
	"bufio"
	"io"
)

type Connection struct {
	r *bufio.Reader
	w *bufio.Writer
	c io.Closer
}

func NewConnection(rwc io.ReadWriteCloser) *Connection {
	return &Connection{
		r: bufio.NewReader(rwc),
		w: bufio.NewWriter(rwc),
		c: rwc,
	}
}

func numBytes(length uint64) int {
	// mathematisch bestimmen:
	// numbytes := int(math.Ceil((math.Floor(math.Log2(float64(length))) - 4) / 8)) + 1
	switch {
	case length < 1<<5:
		return 1
	case length < 1<<13:
		return 2
	case length < 1<<21:
		return 3
	case length < 1<<29:
		return 4
	case length < 1<<37:
		return 5
	case length < 1<<45:
		return 6
	case length < 1<<53:
		return 7
	default:
		return 8
	}
}

func (c *Connection) encodeLength(length uint64) error {
	numbytes := numBytes(length)
	for i := 0; i < numbytes; i++ {
		b := byte((length >> (8 * uint(numbytes-i-1))) & 0xFF)
		if i == 0 {
			b |= byte((numbytes - 1) << 5)
		}
		if err := c.w.WriteByte(b); err != nil {
			return err
		}
	}
	return nil
}

func (c *Connection) decodeLength() (length uint64, err error) {
	// read first byte
	b, err := c.r.ReadByte()
	if err != nil {
		return 0, err
	}
	numbytes := int((b&(7<<5))>>5) + 1

	// read extra bytes
	bytes := make([]byte, numbytes)
	// clear out our three length bits
	bytes[0] = b & 31
	_, err = c.r.Read(bytes[1:])
	if err != nil {
		return 0, err
	}

	// convert to uint64
	for i := range bytes {
		length |= (uint64(bytes[i]) << uint(8*(numbytes-i-1)))
	}
	return
}

func (c *Connection) WriteJoinTopics(topics []string) error {
	if err := c.writeFlags(FlagJoin, false); err != nil {
		return err
	}
	if err := c.writeTopics(topics); err != nil {
		return err
	}
	return c.w.Flush()
}

func (c *Connection) WritePartTopics(topics []string) error {
	if err := c.writeFlags(FlagPart, false); err != nil {
		return err
	}
	if err := c.writeTopics(topics); err != nil {
		return err
	}
	return c.w.Flush()
}

func (c *Connection) WriteBroadCast(message string) error {
	msg, compressed := CompressMessage([]byte(message))
	if err := c.writeFlags(FlagBroadCast, compressed); err != nil {
		return err
	}
	if err := c.writeMessage(msg); err != nil {
		return err
	}
	return c.w.Flush()
}

func (c *Connection) WriteMessage(topics []string, message string) error {
	msg, compressed := CompressMessage([]byte(message))
	if err := c.writeFlags(FlagMessage, compressed); err != nil {
		return err
	}
	if err := c.writeTopics(topics); err != nil {
		return err
	}
	if err := c.writeMessage(msg); err != nil {
		return err
	}
	return c.w.Flush()
}

func (c *Connection) WriteAskTopics() error {
	if err := c.writeFlags(FlagTopicAsk, false); err != nil {
		return err
	}
	return c.w.Flush()
}

func (c *Connection) writeFlags(actionFlag Flags, compressed bool) error {
	var code byte = actionFlag << 3
	if compressed {
		code |= MaskZip
	}
	return c.w.WriteByte(code)
}

func (c *Connection) writeTopics(topics []string) error {
	length := uint64(len(topics) - 1)
	for _, t := range topics {
		length += uint64(len(t))
	}
	if err := c.encodeLength(length); err != nil {
		return err
	}
	for i, t := range topics {
		if _, err := c.w.WriteString(t); err != nil {
			return err
		}
		if i < len(topics)-1 {
			if err := c.w.WriteByte(0); err != nil {
				return err
			}
		}
	}
	return nil
}

func (c *Connection) writeMessage(msg []byte) error {
	length := len(msg)
	if err := c.encodeLength(uint64(length)); err != nil {
		return err
	}
	if _, err := c.w.Write(msg); err != nil {
		return err
	}
	return nil
}

func (c *Connection) ReadPacket() (p *Packet, err error) {
	b, err := c.r.ReadByte()
	if err != nil {
		return nil, err
	}

	p = &Packet{}
	p.Flags = Flags((b & MaskAction) >> 3)
	compressed := b&MaskZip != 0
	if p.hasTopics() {
		if p.Topics, err = c.readTopics(); err != nil {
			return nil, err
		}
	}
	if p.hasMessage() {
		if p.Message, err = c.readMessage(compressed); err != nil {
			return nil, err
		}
	}
	return
}

func (c *Connection) readTopics() (topics []string, err error) {
	data, err := c.readMessage(false)
	if err != nil {
		return nil, err
	}
	laststart := 0
	for i, b := range data {
		if b == 0 {
			topics = append(topics, string(data[laststart:i]))
			laststart = i + 1
		}
	}
	topics = append(topics, string(data[laststart:len(data)]))
	return
}

func (c *Connection) readMessage(compressed bool) (msg []byte, err error) {
	length, err := c.decodeLength()
	if err != nil {
		return nil, err
	}
	msg = make([]byte, length)
	if _, err = c.r.Read(msg); err != nil {
		return
	}
	if compressed {
		msg = DecompressMessage(msg)
	}
	return
}

func (c *Connection) Close() error {
	return c.c.Close()
}
