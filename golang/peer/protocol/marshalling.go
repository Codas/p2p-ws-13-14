package protocol

import (
	"errors"
	"fmt"
	"io"
	"math"
	"strconv"
	"strings"
)

type byteReader interface {
	io.Reader
	io.ByteReader
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
	return &Address{
		ip:   strconv.Itoa(int(buf[0])) + "." + strconv.Itoa(int(buf[1])) + "." + strconv.Itoa(int(buf[2])) + "." + strconv.Itoa(int(buf[3])),
		port: int(buf[4])<<8 + int(buf[5]),
	}, nil
}

func unparseAddress(addr *Address) []byte {
	buf := make([]byte, 6)
	parts := strings.Split(addr.ip, ".")
	if len(parts) != 4 {
		fmt.Println("Error unparsing Address IP: not conforming IPv4:", addr.ip)
		return buf
	}
	for i, p := range parts {
		n, err := strconv.Atoi(p)
		if len(parts) != 4 {
			fmt.Println("Parsing Error when unparsing Address IP: ", err)
			return buf
		}
		buf[i] = byte(n)
	}
	buf[4] = byte((addr.port >> 8) & 0xFF)
	buf[5] = byte(addr.port & 0xFF)
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

func parseHops(br byteReader) (h HopsLeft, err error) {
	b, err := br.ReadByte()
	if err != nil {
		return 0, err
	}
	return HopsLeft(b), nil
}

func unparseHops(h HopsLeft) []byte {
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

func readN(r io.Reader, length int) (b []byte, err error) {
	buf := make([]byte, length)
	if n, err := r.Read(buf); err != nil {
		return nil, err
	} else if n != length {
		return nil, errors.New("Read length missmatch!")
	}
	return buf, nil
}

func parseFishAndWater(br byteReader) (fish FishCarry, water WaterCarry, err error) {
	buf, err := readN(br, 16)

	if err != nil {
		return
	}

	fishMantisse := readBytesAsInt(buf[0:4])
	fishExp := readBytesAsInt(buf[4:8]) - 32768
	waterMantisse := readBytesAsInt(buf[8:12])
	waterExp := readBytesAsInt(buf[12:16]) - 32768

	fish = FishCarry(float32(float64(fishMantisse) * math.Pow(float64(2), float64(fishExp))))
	water = WaterCarry(float32(float64(waterMantisse) * math.Pow(float64(2), float64(waterExp))))
	return
}

func readBytesAsInt(buf []byte) int32 {
	return int32(buf[0])<<24 + int32(buf[1])<<16 + int32(buf[2])<<8 + int32(buf[3])
}

func unparseFishAndWater(fish float32, water float32) []byte {
	buf := make([]byte, 16)
	writeDoubleToByteBuffer(buf[0:8], fish)
	writeDoubleToByteBuffer(buf[8:16], water)

	return buf
}

func writeDoubleToByteBuffer(buf []byte, f float32) {
	tmp := strings.Split(strconv.FormatFloat(float64(f), 'b', -1, 32), "p")
	mantisse, _ := strconv.ParseInt(tmp[0], 10, 32)
	binaryExp, _ := strconv.ParseInt(tmp[1], 10, 32)
	writeIntToByteBuffer(buf[0:4], mantisse)
	writeIntToByteBuffer(buf[4:8], binaryExp+32768) // with bias
}

func writeIntToByteBuffer(buf []byte, n int64) {
	buf[0] = byte((n >> 24) & 0xFF)
	buf[1] = byte((n >> 16) & 0xFF)
	buf[2] = byte((n >> 8) & 0xFF)
	buf[3] = byte(n & 0xFF)
}
