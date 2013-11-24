// LZ4 compression and decompression
package protocol

import lz4 "github.com/salviati/go-lz4"

func CompressMessage(msg []byte) (cMsg []byte, compressed bool) {
	length := len(msg)
	if length <= 20 {
		return msg, false
	}
	// compress
	compressBound := lz4.CompressBound(length)
	cMsg = make([]byte, compressBound)
	length = lz4.Compress(msg, cMsg, length)
	return cMsg[:length], true
}

func DecompressMessage(msg []byte) (dMsg []byte) {
	oSize := len(msg) * 4 // guess length ?
	dMsg = make([]byte, oSize)
	length := lz4.DecompressUnknownOutputSize(msg, dMsg, len(msg), oSize)
	return dMsg[:length]
}