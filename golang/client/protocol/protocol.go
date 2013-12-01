package protocol

import (
	"bufio"
	"bytes"
	"io"
)

type byteReader interface {
	io.Reader
	io.ByteReader
}

type Connection struct {
	b bytes.Buffer
	r *bufio.Reader
	w *bufio.Writer
	c io.Closer
}

func NewConnection(rwc io.ReadWriteCloser) *Connection {
	w := bufio.NewWriter(rwc)
	w.WriteByte(120)
	w.Flush()
	return &Connection{
		r: bufio.NewReader(rwc),
		w: w,
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

func EncodeLength(bw io.ByteWriter, length uint64) error {
	numbytes := numBytes(length)
	for i := 0; i < numbytes; i++ {
		b := byte((length >> (8 * uint(numbytes-i-1))) & 0xFF)
		if i == 0 {
			b |= byte((numbytes - 1) << 5)
		}
		if err := bw.WriteByte(b); err != nil {
			return err
		}
	}
	return nil
}

func DecodeLength(br byteReader) (length uint64, err error) {
	// read first byte
	b, err := br.ReadByte()
	if err != nil {
		return 0, err
	}
	numbytes := int((b&(7<<5))>>5) + 1

	// read extra bytes
	bytes := make([]byte, numbytes)
	// clear out our three length bits
	bytes[0] = b & 31
	_, err = br.Read(bytes[1:])
	if err != nil {
		return 0, err
	}

	// convert to uint64
	for i := range bytes {
		length |= (uint64(bytes[i]) << uint(8*(numbytes-i-1)))
	}
	return
}

func (c *Connection) compressBuffer() (data []byte, compressed bool, err error) {
	data = c.b.Bytes()
	data, compressed, err = CompressMessage(data)
	c.b.Reset()
	return
}

func (c *Connection) writeAll(flag Flags) error {
	data, compressed, err := c.compressBuffer()
	if err != nil {
		return err
	}
	if err := c.writeFlags(flag, compressed); err != nil {
		return err
	}
	length := len(data)
	if length > 0 {
		if compressed {
			if err := EncodeLength(c.w, uint64(length)); err != nil {
				return err
			}
		}
		if _, err := c.w.Write(data); err != nil {
			return err
		}
	}
	return c.w.Flush()
}

func (c *Connection) Write(f Flags, topics []string, message string) error {
	if msgHasTopics(f) {
		if err := c.bufferTopics(topics); err != nil {
			return err
		}
	}
	if msgHasMessage(f) {
		if err := c.bufferMessage([]byte(message)); err != nil {
			return err
		}
	}
	return c.writeAll(f)
}

func (c *Connection) writeFlags(actionFlag Flags, compressed bool) error {
	code := byte(actionFlag << 3)
	if compressed {
		code |= MaskZip
	}
	return c.w.WriteByte(code)
}

func (c *Connection) bufferTopics(topics []string) error {
	var length uint64
	if len(topics) > 1 {
		length = uint64(len(topics) - 1)
	}
	for _, t := range topics {
		length += uint64(len(t))
	}
	if err := EncodeLength(&c.b, length); err != nil {
		return err
	}
	for i, t := range topics {
		if _, err := c.b.WriteString(t); err != nil {
			return err
		}
		if i < len(topics)-1 {
			if err := c.b.WriteByte(0); err != nil {
				return err
			}
		}
	}
	return nil
}

func (c *Connection) bufferMessage(msg []byte) error {
	length := len(msg)
	if err := EncodeLength(&c.b, uint64(length)); err != nil {
		return err
	}
	if _, err := c.b.Write(msg); err != nil {
		return err
	}
	return nil
}

func (c *Connection) ReadPacket() (p *Packet, err error) {
	p, compressed, err := c.readFlags()
	if err != nil {
		return nil, err
	}

	r := byteReader(c.r)
	if compressed {
		var data []byte
		if data, err = c.getMessage(r, compressed); err != nil {
			return nil, err
		}
		r = bytes.NewBuffer(data)
	}

	if p.hasTopics() {
		if p.Topics, err = c.getTopics(r); err != nil {
			return nil, err
		}
	}
	if p.hasMessage() {
		if p.Message, err = c.getMessage(r, false); err != nil {
			return nil, err
		}
	}
	return
}

func (c *Connection) readFlags() (p *Packet, compressed bool, err error) {
	b, err := c.r.ReadByte()
	if err != nil {
		return nil, false, err
	}
	p = &Packet{
		Flags: Flags((b & MaskAction) >> 3),
	}
	return p, b&MaskZip != 0, nil
}

func (c *Connection) getTopics(br byteReader) (topics []string, err error) {
	data, err := c.getMessage(br, false)
	if err != nil {
		return nil, err
	}
	if len(data) == 0 {
		return nil, nil
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

func (c *Connection) getMessage(r byteReader, compressed bool) (msg []byte, err error) {
	length, err := DecodeLength(r)
	if err != nil {
		return nil, err
	}
	if length == 0 {
		return nil, nil
	}
	msg = make([]byte, length)
	if _, err = r.Read(msg); err != nil {
		return
	}
	if compressed {
		msg, err = DecompressMessage(msg)
	}
	return
}

func (c *Connection) Close() error {
	return c.c.Close()
}
