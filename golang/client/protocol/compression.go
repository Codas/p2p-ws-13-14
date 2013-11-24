// LZ4 compression and decompression
package protocol

import _ "fmt"

//import lz4 "github.com/salviati/go-lz4"
import "code.google.com/p/snappy-go/snappy"

const CompressionThreshold = 20

func CompressMessage(msg []byte) (cMsg []byte, compressed bool, err error) {
	length := len(msg)
	if length <= CompressionThreshold {
		return msg, false, nil
	}

	cMsg, err = snappy.Encode(nil, msg)
	//fmt.Println("compress ", msg)
	//fmt.Println("      -> ", cMsg)
	return cMsg, true, err

	/*
		// compress
		compressBound := lz4.CompressBound(length)
		cMsg = make([]byte, compressBound)
		length = lz4.Compress(msg, cMsg, length)
		fmt.Println("compress ", msg)
		fmt.Println("      -> ", cMsg[:length])
		return cMsg[:length], true
	*/
}

func DecompressMessage(msg []byte) (dMsg []byte, err error) {
	dMsg, err = snappy.Decode(nil, msg)
	//fmt.Println("decompress ", msg)
	//fmt.Println("        -> ", dMsg)
	return dMsg, err

	/*
		oSize := len(msg) * 4 // guess length ?
		dMsg = make([]byte, oSize)
		length := lz4.DecompressUnknownOutputSize(msg, dMsg, len(msg), oSize)
		fmt.Println("decompress ", msg)
		fmt.Println("        -> ", dMsg[:length])
		return dMsg[:length]
	*/
}
