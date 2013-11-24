package protocol_test

import (
	. "../protocol"
	"testing"
)

func TestEncoding(t *testing.T) {
	loremIpsum := `Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet.`
	cMsg, compressed := CompressMessage([]byte(loremIpsum))
	if !compressed {
		t.Error("compression failed")
	}
	if loremIpsum != string(DecompressMessage(cMsg)) {
		t.Error("decompress(compress(data)) results in different data")
	}
}
