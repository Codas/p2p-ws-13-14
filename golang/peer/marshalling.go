package main

import (
	"bytes"
	"encoding/binary"
	"errors"
)

type byteReader interface {
	Read([]byte) (int, error)
	ReadByte() (byte, error)
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

func parseLength(br byteReader) (length uint64, err error) {

	// read first byte
	b, err := br.ReadByte()
	if err != nil {
		return 0, err
	}
	numbytes := int((b&(7<<5))>>5) + 1

	// read extra bytes
	buf := make([]byte, numbytes)
	// clear out our three length bits
	buf[0] = b & 31
	if n, err := br.Read(buf[1:]); err != nil {
		return 0, err
	} else if n != numbytes-1 {
		return 0, errors.New("Read length missmatch!")
	}

	// convert to uint64
	for i := range buf {
		length |= (uint64(buf[i]) << uint(8*(numbytes-i-1)))
	}
	return
}

func unparseLength(length uint64) []byte {
	numbytes := numBytes(length)
	buf := make([]byte, numbytes)
	for i := range buf {
		buf[i] = byte((length >> (8 * uint(numbytes-i-1))) & 0xFF)
	}
	buf[0] |= byte((numbytes - 1) << 5)
	return buf
}

func parseHeader(br byteReader) (action Action, err error) {
	b, err := br.ReadByte()
	if err != nil {
		return 0, err
	}
	return Action((b & MaskAction) >> 3), nil
}

func unparseHeader(action Action) []byte {
	buf := make([]byte, 1)
	buf[0] = byte(action << 3)
	return buf
}

func parseAddress(br byteReader) (addr *Address, err error) {
	buf, err := readN(br, 6)
	if err != nil {
		return nil, err
	}
	ip := [4]byte{buf[0], buf[1], buf[2], buf[3]}
	port := int(buf[4])<<8 + int(buf[5])
	return NewAddress(ip, port), nil
}

func unparseAddress(addr *Address) []byte {
	buf := make([]byte, 6)
	buf[0] = addr.IP[0]
	buf[1] = addr.IP[1]
	buf[2] = addr.IP[2]
	buf[3] = addr.IP[3]
	buf[4] = byte((addr.Port >> 8) & 0xFF)
	buf[5] = byte(addr.Port & 0xFF)
	return buf
}

func parseLocation(br byteReader) (l Location, err error) {
	b, err := br.ReadByte()
	if err != nil {
		return 0, err
	}
	return Location(b), nil
}

func unparseLocation(l Location) []byte {
	buf := make([]byte, 1)
	buf[0] = byte(l)
	return buf
}

func parseWaterFishStrength(br byteReader) (water float32, fish float32, strength uint32, err error) {
	err = binary.Read(br, binary.LittleEndian, &water)
	if err != nil {
		return
	}
	err = binary.Read(br, binary.LittleEndian, &fish)
	if err != nil {
		return
	}
	err = binary.Read(br, binary.LittleEndian, &strength)
	if err != nil {
		return
	}
	return
}

func unparseWaterFishStrength(water float32, fish float32, strength uint32) []byte {
	buf := bytes.NewBuffer(make([]byte, 12))
	binary.Write(buf, binary.LittleEndian, water)
	binary.Write(buf, binary.LittleEndian, fish)
	binary.Write(buf, binary.LittleEndian, strength)
	return buf.Bytes()
}

func parseHops(br byteReader) (h Hops, err error) {
	b, err := br.ReadByte()
	if err != nil {
		return 0, err
	}
	return Hops(b), nil
}

func unparseHops(h Hops) []byte {
	buf := make([]byte, 1)
	buf[0] = byte(h)
	return buf
}

func parseContent(br byteReader) (c []byte, err error) {
	length, err := parseLength(br)
	return readN(br, int(length))
}

func unparseContent(content []byte) []byte {
	lenbuf := unparseLength(uint64(len(content)))
	buf := make([]byte, len(lenbuf)+len(content))
	copy(buf, lenbuf)
	copy(buf[len(lenbuf):], content)
	return buf
}

func readN(br byteReader, length int) (b []byte, err error) {
	buf := make([]byte, length)
	if n, err := br.Read(buf); err != nil {
		return nil, err
	} else if n != length {
		return nil, errors.New("Read length missmatch!")
	}
	return buf, nil
}
